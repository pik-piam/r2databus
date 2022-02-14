#' @importFrom jsonlite toJSON

asJsonldDataID <- function(context, x, dBusUri = "https://dev.databus.dbpedia.org") {

  distinct_cvs <- function(x) {
    distinct_cv_definitions <- list()
    i <- 0
    for (dbfile in x) {
      for (item in names(dbfile[["metadata"]])) {
        i <- i + 1
        distinct_cv_definitions[[i]] <- list(
          "@type" = "rdf:Property",
          "@id" = paste0('dcv:', item),
          "rdfs:subPropertyOf" = list('@id' = 'dataid:contentVariant')
        )
      }

    }
    return(unique(distinct_cv_definitions))
  }

  dataid_uri = getTargetURIDataID(x)

  group_data_dict = list(
    "@context" = context,
    "@graph"= list(
      list(
        "@type" = "dataid:Dataset",
        "@id" = paste0(dataid_uri, "#Dataset"),
        "version" = dataid_uri,
        "artifact" = paste0(dBusUri, "/", x["account_name"] , "/", x["group"] , "/", x["artifact"]),
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
        "distribution" = dbfiles_to_dict(x[["databus_files"]], x[["artifact"]], dataid_uri)
      )
    )
  )
  for (i in distinct_cvs(x[["databus_files"]])) group_data_dict[["@graph"]] <- c(group_data_dict[["@graph"]], list(i))
  group_data_dict[["@graph"]] <- c(group_data_dict[["@graph"]], list(list("@id" = paste0(dBusUri, "/", x["account_name"] , "/", x["group"] , "/", x["artifact"]), "@type" = "dataid:Artifact")))
  group_data_dict[["@graph"]] <- c(group_data_dict[["@graph"]], list(list("@id" = paste0(dBusUri, "/", x["account_name"] , "/", x["group"] , "/", x["artifact"], "/", x["version"]), "@type" = "dataid:Version")))
  return(toJSON(group_data_dict, auto_unbox = TRUE))

}

