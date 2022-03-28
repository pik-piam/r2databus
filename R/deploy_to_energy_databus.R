#' deploy_to_energy_databus
#'
#' Function that executes the deployment to the Databus (energy.databus.dbpedia.org)
#'
#' @param api_key a personal and secret API key (can be found or generated
#' under https://energy.databus.dbpedia.org/{{user}}#settings)
#' @param groupJson the Group documentation as JSON-LD string (if missing defaults
#' from the package inst folder will be taken)
#' @param dataidJson the DataID documentation as JSON-LD string (if missing defaults
#' from the package inst folder will be taken)
#' @author Anastasis Giannousakis
#' @importFrom httr add_headers PUT
#' @importFrom jsonlite read_json
#' @export

deploy_to_energy_databus <- function(api_key, groupJson = NULL, dataidJson = NULL) { # nolint

  message(paste0("Deploying Group"))
  if (is.null(groupJson)) {
    groupFile <- system.file("extdata", "group.jsonld", package = "r2databus")
    groupJson <- read_json(groupFile)
  }
  url <- groupJson$`@graph`$`@id`
  PUT(url = url,
      body = groupJson,
      encode = "json",
      config = add_headers("X-API-Key" = api_key, "Content-Type" = "application/json")
  )

  message(paste0("Deploying DataID"))
  if (is.null(dataidJson)) {
    dataFile <- system.file("extdata", "dataid.jsonld", package = "r2databus")
    dataidJson <- read_json(dataFile)
  }
  url <- sub("#Dataset$", "", dataidJson$`@graph`$`@id`)
  PUT(url = url,
      body = dataidJson,
      encode = "json",
      config = add_headers("X-API-Key" = api_key, "Content-Type" = "application/json")
  )

}
