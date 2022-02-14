#' getDatabusFile
#'
#' Fetches the necessary information of a file URI for the deployment to the Databus
#'
#' @param x a list that has to contain the named items "file", "metadata" and "filetype"
#' @return a named list with the information gathered
#' @author Anastasis Giannousakis
#' @importFrom httr GET
#' @importFrom digest digest
#' @export

getDatabusFile <- function(x) {

  resp <- GET(x[["file"]])
  if (resp["status_code"] > 400)
    message("ERROR for {uri} -> Status {str(resp.status_code)}")

  sha256sum <- digest::digest(resp["content"], algo = "sha256")
  content_length <- length(resp[["content"]])
  file_ext <- x[["filetype"]]
  tmp <- NULL
  for(i in x["metadata"]) tmp <- paste0(tmp, names(i), "=", i, collapse = "_")
  id_string = paste0(tmp, ".", file_ext)
  return(list(sha256sum = sha256sum, content_length = content_length, sha256sum = sha256sum, file_ext = file_ext, id_string = id_string))
}
