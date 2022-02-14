getTargetURIDataID <- function(x, dBusUri = "https://dev.databus.dbpedia.org") {
  return(paste0(dBusUri , "/", x["account_name"] , "/", x["group"] , "/", x["artifact"] , "/", x["version"]))
}
