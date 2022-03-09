#' asJsonldDataID
#'
#' Returns the DataID documentation as JSON-LD string
#'
#' @param x a named list that contains the metadata and link to the dataset
#' that will be published on the Databus
#' @param context string containing the "context" URI of the Databus
#' @param dBusUri string containing the base Databus URI for POST requests
#' @return  the DataID documentation as JSON-LD string
#' @author Anastasis Giannousakis
#' @importFrom jsonlite toJSON
#' @export
#' @seealso \code{\link{asJsonldGroup}}

asJsonldDataID <- function(x, context = "https://downloads.dbpedia.org/databus/context.jsonld",
                           dBusUri = "https://energy.databus.dbpedia.org") {

  distinctCvs <- function(x) {
    distinctCvDefinitions <- list()
    i <- 0
    for (dbfile in x) {
      for (item in names(dbfile[["metadata"]])) {
        i <- i + 1
        distinctCvDefinitions[[i]] <- list(
          "@type" = "rdf:Property",
          "@id" = paste0("dcv:", item),
          "rdfs:subPropertyOf" = list("@id" = "dataid:contentVariant")
        )
      }

    }
    return(unique(distinctCvDefinitions))
  }

  dataid_uri <- getTargetURIDataID(x) # nolint

  group_data_dict <- list( # nolint
    "@context" = context,
    "@graph" = list(
      list(
        "@type" = "dataid:Dataset",
        "@id" = paste0(dataid_uri, "#Dataset"),
        "version" = dataid_uri,
        "artifact" = paste0(dBusUri, "/", x["account_name"], "/", x["group"], "/", x["artifact"]),
        "group" = getTargetURIGroup(x),
        "hasVersion" = x[["version"]],
        "issued" = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        "publisher" = x[["publisher"]],
        "label" = list("@value" = x[["label"]], "@language" = "en"),
        "title" = list("@value" = x[["title"]], "@language" = "en"),
        "comment" = list("@value" = x[["comment"]], "@language" = "en"),
        "abstract" = list("@value" = x[["abstract"]], "@language" = "en"),
        "description" = list("@value" = x[["description"]], "@language" = "en"),
        "license" = list("@id" = x[["license"]]),
        "distribution" = dbfilesToDict(x[["databus_files"]], x[["artifact"]], dataid_uri)
      )
    )
  )
  for (i in distinctCvs(x[["databus_files"]])) group_data_dict[["@graph"]] <- c(group_data_dict[["@graph"]], list(i))
  group_data_dict[["@graph"]] <- c(
    group_data_dict[["@graph"]],
    list(
      list(
        "@id" = paste0(dBusUri, "/", x["account_name"], "/", x["group"], "/", x["artifact"]),
        "@type" = "dataid:Artifact")
      )
    )

  group_data_dict[["@graph"]] <- c(
    group_data_dict[["@graph"]],
    list(
      list(
        "@id" = paste0(dBusUri, "/", x["account_name"], "/", x["group"], "/", x["artifact"], "/", x["version"]),
        "@type" = "dataid:Version")
      )
    )
  return(toJSON(group_data_dict, auto_unbox = TRUE))

}
