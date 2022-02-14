#' @importFrom jsonlite toJSON

asJsonldGroup <- function(context, x) {

  group_uri = getTargetURIGroup(x)

  group_data_dict = list(
    "@context" = context,
    "@graph"= list(
      list(
        "@id" = group_uri,
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

