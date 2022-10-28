#' databusUpload
#'
#' Publishes a dataset on the Databus (energy.databus.dbpedia.org)
#'
#' @param myKey a personal and secret API key (can be found or generated
#' under https://energy.databus.dbpedia.org/{{user}}#settings)
#' @param myData a list or a path to a JSON file with the dataset's metadata
#' (if missing, defaults from the package's inst folder will be taken)
#' @author Anastasis Giannousakis
#' @importFrom httr add_headers PUT GET
#' @importFrom digest digest
#' @importFrom jsonlite read_json
#' @export

databusUpload <- function(myKey, myData = NULL) {

  .getIdString <- function(x) {
    tmp <- NULL
    for(i in x["metadata"]) tmp <- paste0(tmp, names(i), "=", i, collapse = "_")
    idString = paste0(tmp, ".", x[["extension"]])
    return(idString)
  }

  # if no myData object is given, take defaults from the inst folder
  if (is.null(myData)) {
    dataFile <- system.file("extdata", "myData.json", package = "r2databus")
    dataList <- read_json(dataFile)
    dataList[["user"]] <- Sys.info()[["user"]]
    dataList[["file"]] <- "https://www.w3.org/ns/dcat2.rdf"
    dataList[["extension"]] <- "rdf"
    dataList[["license"]] <- "https://www.w3.org/Consortium/Legal/2015/copyright-software-and-document"
  } else if (is.list(myData)) {
    dataList <- myData
  } else if (file.exists(myData)) {
    dataList <- read_json(myData)
  }



  groupidString <- paste0("https://energy.databus.dbpedia.org/",
                          dataList[["user"]],
                          "/",
                          dataList[["group"]])

  groupList <- list("@id" = groupidString,
                    "@type" = "Group",
                    "title" = dataList[["groupTitle"]],
                    "abstract" = dataList[["groupAbstract"]],
                    "description" = dataList[["groupDescription"]])

  groupList <- list(
    "@context" = "https://downloads.dbpedia.org/databus/context.jsonld",
    "@graph" = groupList)



  dataidString <- paste0(groupidString,
                         "/",
                         dataList[["artifact"]],
                         "/",
                         dataList[["version"]])

  resp <- GET(dataList[["file"]])
  sha256sum <- digest(resp[["content"]], algo = "sha256", serialize = FALSE)
  byteSize <- length(resp[["content"]])

  dataidList <- list("@id" = paste0(dataidString, "#Dataset"),
                     "@type" = "Dataset",
                     "hasVersion" = dataList[["version"]],
                     "title" = dataList[["dataidTitle"]],
                     "abstract" = dataList[["dataidAbstract"]],
                     "description" = dataList[["dataidDescription"]],
                     "license" = dataList[["license"]],
                     "distribution" = list("downloadURL" = dataList[["file"]],
                                           "@type" = "Part",
                                           "formatExtension" = dataList[["extension"]],
                                           "compression" = "none",
                                           "byteSize" = byteSize,
                                           "sha256sum" = sha256sum))

  dataidList[["distribution"]]["@id"] <-  paste0(dataidString,
                                                 "#",
                                                 dataList[["artifact"]],
                                                 "_",
                                                 .getIdString(dataList))

  dataidList[["distribution"]]["file"] <-  paste0(dataidString,
                                                  "/",
                                                  dataList[["artifact"]],
                                                  "_",
                                                  .getIdString(dataList))

  names(dataList[["metadata"]]) <- paste0("dcv:", names(dataList[["metadata"]]))
  dataidList[["distribution"]] <- c(dataidList[["distribution"]], dataList[["metadata"]])

  dataidList <- list("@context" = "https://downloads.dbpedia.org/databus/context.jsonld",
                     "@graph" = dataidList)



  message("Deploying Group")
  url <- groupList[["@graph"]][["@id"]]
  PUT(url = url,
      body = groupList,
      encode = "json",
      config = add_headers("X-API-Key" = myKey, "Content-Type" = "application/json")
  )

  message("Deploying DataID")
  url <- sub("#Dataset$", "", dataidList[["@graph"]][["@id"]])
  PUT(url = url,
      body = dataidList,
      encode = "json",
      config = add_headers("X-API-Key" = myKey, "Content-Type" = "application/json")
  )

}
