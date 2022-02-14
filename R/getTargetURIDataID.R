#' getTargetURIDataID
#'
#' Returns the URI used in the POST request that publishes the DataID documentation
#'
#' @param x a list that has to contain the named strings "account_name", "group", "artifact" and "version"
#' @param dBusUri string containing the base Databus URI for POST requests
#' @return the URI used in the POST request that publishes the DataID documentation
#' @author Anastasis Giannousakis
#' @export
#' @seealso \code{\link{getTargetURIGroup}}

getTargetURIDataID <- function(x, dBusUri = "https://dev.databus.dbpedia.org") {
  return(paste0(dBusUri , "/", x["account_name"] , "/", x["group"] , "/", x["artifact"] , "/", x["version"]))
}
