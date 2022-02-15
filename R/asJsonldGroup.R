#' asJsonldGroup
#'
#' Returns the Group documentation as JSON-LD string
#'
#' @param x a named list that contains the metadata of the Group that will
#' publish data on the Databus
#' @param context string containing the "context" URI of the Databus
#' @return  the Group documentation as JSON-LD string
#' @author Anastasis Giannousakis
#' @importFrom jsonlite toJSON
#' @export
#' @seealso \code{\link{asJsonldDataID}}

asJsonldGroup <- function(x, context = "https://downloads.dbpedia.org/databus/context.jsonld") {

  groupUri <- getTargetURIGroup(x)

  group_data_dict <- list(
    "@context" = context,
    "@graph" = list(
      list(
        "@id" = groupUri,
        "@type" = "dataid:Group",
        "label" = list("@value" = x[["label"]], "@language" = "en"),
        "title" = list("@value" = x[["title"]], "@language" = "en"),
        "comment" = list("@value" = x[["comment"]], "@language" = "en"),
        "abstract" = list("@value" = x[["abstract"]], "@language" = "en"),
        "description" = list("@value" = x[["description"]], "@language" = "en")
      )
    )
  )
  return(toJSON(group_data_dict, auto_unbox = TRUE))

}
