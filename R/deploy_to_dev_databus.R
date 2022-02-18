#' deploy_to_dev_databus
#'
#' Function that executes the deployment to the Databus (dev.databus.dbpedia.org)
#'
#' @param api_key a personal and secret API key (can be found or generated
#' under https://dev.databus.dbpedia.org/{{user}}#settings)
#' @param group the Group documentation as JSON-LD string
#' @param dataid the DataID documentation as JSON-LD string
#' @param post_databus_uri string containing the Databus URI for POST requests
#' @return the POST request response object for further analysis
#' @author Anastasis Giannousakis
#' @importFrom httr add_headers POST
#' @export

deploy_to_dev_databus <- function(api_key, group, dataid, # nolint
                                  post_databus_uri = "https://dev.databus.dbpedia.org/system/publish") { # nolint

  message(paste0("Deploying Group"))
  groupld <- asJsonldGroup(context = context, x = group)
  resp <- POST(url = post_databus_uri,
               body = groupld,
               encode = "raw",
               config = add_headers("X-API-Key" = api_key, "Content-Type" = "application/json")
  )
  print(groupld)
  print(resp)


  message(paste0("Deploying DataID"))
  dataidld <- asJsonldDataID(context = context, x = dataid)
  resp <- POST(url = post_databus_uri,
               body = dataidld,
               encode = "raw",
               config = add_headers("X-API-Key" = api_key, "Content-Type" = "application/json")
  )
  print(dataidld)
  print(resp)

  return(resp)
}
