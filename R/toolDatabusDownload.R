#' Tool: DatabusDownload
#'
#' This function downloads a dataset and its metadata from the Energy Databus (https://energy.databus.dbpedia.org/)
#'
#' @param get A vector of hash codes which should be replaced
#' @param add Additional entries that should be added to the dictionary. Need to be
#' provided in the form of a named vector with the structure c(<label>=<hash>),
#' e.g. c(h12="62eff8f7")
#' @return A vector with either labels (if available) or hash codes (if no label was available).
#' @author Anastasis Giannousakis
#' @examples
#' toolDatabusDownload("62eff8f7")
#'
#' @export
toolDatabusDownload <- function(path) {

  # read metadata
  download.file(url = path, destfile = "tmpfile", mode = "wb")
  tmpj <- jsonlite::read_json("tmpfile")
  # read file name and download data
  download.file(url = tmpj[["@graph"]][[5]][["downloadURL"]], destfile = "datafile", mode = "wb")

  return(list(url           = tmpj[["@graph"]][[5]][["downloadURL"]],
              doi           = NULL,
              title         = tmpj[["@graph"]][[4]][["abstract"]],
              author        = tmpj[["@graph"]][[4]][["publisher"]],
              version       = tmpj[["@graph"]][[4]][["version"]],
              release_date  = tmpj[["@graph"]][[4]][["issued"]],
              description   = tmpj[["@graph"]][[4]][["description"]],
              license       = tmpj[["@graph"]][[4]][["license"]],
              reference     = NULL)
  )

}
