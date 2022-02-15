#' getTargetURIGroup
#'
#' Returns the URI used in the POST request that publishes the Group documentation
#'
#' @param x a list that has to contain the named strings "account_name" and "id"
#' @param dBusUri string containing the base Databus URI for POST requests
#' @return the URI used in the POST request that publishes Group documentation
#' @author Anastasis Giannousakis
#' @export
#' @seealso \code{\link{deploy_to_dev_databus}}

getTargetURIGroup <- function(x, dBusUri = "https://dev.databus.dbpedia.org") {
  return(paste0(dBusUri, "/", x["account_name"], "/", x["id"]))
}
