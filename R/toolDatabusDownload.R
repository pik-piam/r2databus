#' Tool: DatabusDownload
#'
#' This function downloads a dataset (file) and its metadata from the Energy Databus
#' (https://energy.databus.dbpedia.org/)
#' Data are organised in "artifacts" on the databus. An artifact contains one or
#' more datasets (files)
#'
#' @param artifact the Databus page hosting the artifact (i.e. one or more datasets
#' and their metadata)
#' @param subtype the specific dataset (file) to download
#' @param databusURL in case another databus hosts the data, e.g. https://databus.dbpedia.org
#' @return a list with the metadata associated with the dataset (file)
#' @importFrom dplyr filter select distinct
#' @importFrom tidyr %>%
#' @importFrom utils download.file
#' @author Anastasis Giannousakis
#' @examples
#' \dontrun{
#' toolDatabusDownload("https://energy.databus.dbpedia.org/giannoupik/GROUPNAME/ARTIFACT/",
#' "https://energy.databus.dbpedia.org/giannoupik/GROUPNAME/ARTIFACT/YOURVERSION/
#' ARTIFACT_FOO=BAR_TYPE=SOMEDATATYPE.rdf")
#' }
#' @export
toolDatabusDownload <- function(artifact, subtype = NULL, databusURL = "https://energy.databus.dbpedia.org" ) {

  # remove trailing slash
  artifact <- sub("/$", "", artifact)

  p <- NULL
  modified <- NULL

  # read file metadata
  q1 <- "PREFIX dataid: <http://dataid.dbpedia.org/ns/core#>
         PREFIX dcv: <http://dataid.dbpedia.org/ns/cv#>
         PREFIX dct:    <http://purl.org/dc/terms/>
         PREFIX dcat:   <http://www.w3.org/ns/dcat#>
         PREFIX rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>

         SELECT ?file ?p ?o WHERE
         {
           GRAPH ?g
           {
             ?dataset dcat:distribution ?distribution .
             ?distribution dataid:file ?file .
             ?distribution ?p ?o .
             ?dataset dataid:artifact <DATASET_URL> .
           }
         }"

  q1 <- sub("DATASET_URL", artifact, q1)
  metaData <- SPARQL::SPARQL(url =  paste0(databusURL, "/sparql"), query = q1)[["results"]]
  metaData[, "file"] <- sub("^<", "", metaData[, "file"])
  metaData[, "file"] <- sub(">$", "", metaData[, "file"])
  metaData <- filter(metaData, file == subtype)  %>% select(c("p", "o"))
  metaData[, "o"] <- sub("^<", "", metaData[, "o"])
  metaData[, "o"] <- sub(">$", "", metaData[, "o"])
  downloadURL <- filter(metaData,
                        p == "<http://www.w3.org/ns/dcat#downloadURL>"
                        ) %>% select("o") %>% as.character()

  try(download.file(url = downloadURL, destfile = "dataBusFile", mode = "wb"))

  # read artifact metada
  q2 <- "PREFIX data: <http://data.odw.tw/>
         PREFIX da: <https://www.wowman.org/index.php?id=1&type=get#>
         PREFIX dc: <http://purl.org/dc/elements/1.1/>
         PREFIX dd: <http://example.org/dummydata/>
         PREFIX dataid: <http://dataid.dbpedia.org/ns/core#>
         PREFIX dcv: <http://dataid.dbpedia.org/ns/cv#>
         PREFIX dct:    <http://purl.org/dc/terms/>
         PREFIX dcat:   <http://www.w3.org/ns/dcat#>
         PREFIX rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>

        SELECT ?dataset ?file ?version ?license ?title ?author ?release_date ?description ?modified WHERE
        {
              ?dataset dcat:distribution ?distribution .
              ?distribution dataid:file ?file .
              ?distribution dct:hasVersion ?version .
              ?dataset dct:license ?license .
              ?dataset dct:title ?title .
              ?dataset dct:publisher ?author .
              ?dataset dct:issued ?release_date .
              ?dataset dct:description ?description .
              ?dataset dct:modified ?modified .
              ?dataset dataid:artifact <DATASET_URL> .

        }"

  q2 <- sub("DATASET_URL", artifact, q2)
  metaData2 <- SPARQL::SPARQL(url =  paste0(databusURL, "/sparql"), query = q2)[["results"]]
  metaData2[, "file"] <- sub("^<", "", metaData2[, "file"])
  metaData2[, "file"] <- sub(">$", "", metaData2[, "file"])
  # filter by subtype and pick the newest version
  metaData2 <- filter(metaData2,
                      file == subtype,
                      modified == max(metaData2$modified))

  releaseDate <- metaData2[, "release_date"] %>% as.numeric() %>% as.POSIXct(origin="1970-01-01") %>% as.Date()

  return(list(url           = downloadURL,
              doi           = NULL,
              title         = metaData2[, "title"],
              author        = metaData2[, "author"],
              version       = metaData2[, "version"],
              release_date  = releaseDate,
              description   = metaData2[, "description"],
              license       = metaData2[, "license"],
              reference     = NULL,
              extraMetaData = metaData)
  )

}
