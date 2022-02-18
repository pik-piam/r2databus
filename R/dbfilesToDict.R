#' dbfilesToDict
#'
#' Returns a named list with special strings for file annotation on the Databus
#'
#' @param x an unnamed list of lists with mandatory names in each of its items: "metadata" and "file"
#' @param artifact string containing the artifact name
#' @param dataid_uri string containing the URI used in the POST request that publishes the DataID documentation,
#' as created by \code{\link{getTargetURIDataID}}
#' @return a named list with special strings for file annotation on the Databus
#' @author Anastasis Giannousakis
#' @export
#' @seealso \code{\link{getDatabusFile}}

dbfilesToDict <- function(x, artifact, dataid_uri) { # nolint
  file_dst <- list() # nolint
  i <- 0
  for (dbfile in x) {
    i <- i + 1
    file_dst[[i]] <- list(
      "@id" = paste0(dataid_uri, "#", getDatabusFile(dbfile)[["id_string"]]),
      "file" = paste0(dataid_uri, "/", artifact, "_", getDatabusFile(dbfile)[["id_string"]]),
      "@type" = "dataid:Part",
      "format" = getDatabusFile(dbfile)[["file_ext"]],
      "compression" = "none",
      "downloadURL" = dbfile[["file"]],
      "byteSize" = as.character(getDatabusFile(dbfile)[["content_length"]]),
      "sha256sum" = getDatabusFile(dbfile)[["sha256sum"]]
    )
    for (kv in dbfile["metadata"]) file_dst[[i]][paste0("dcv:", names(kv))] <- kv
  }

  return(file_dst)
}
