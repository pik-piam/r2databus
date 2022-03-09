#' databusQuery
#'
#' Function that executes a SPARQL Query on the Databus
#'
#' @param url string containing the Databus URL for SPARQL queries (endpoint)
#' @param q the SPARQL query (prefixes and query)
#' @param metadata the metadata to search for (string)
#' @return the SPARQL query response object for further analysis
#' @author Anastasis Giannousakis
#' @importFrom SPARQL SPARQL
#' @export

databusQuery <- function(url = "https://databus.dbpedia.org/repo/sparql", q = NULL,
                         metadata = "emissiondata") {


  if (is.null(q)) q<- "
  PREFIX dataid: <http://dataid.dbpedia.org/ns/core#>
  PREFIX dct:    <http://purl.org/dc/terms/>
  PREFIX dcat:   <http://www.w3.org/ns/dcat#>
  PREFIX db:     <https://databus.dbpedia.org/>
  PREFIX rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>
  SELECT DISTINCT ?file ?g WHERE {
	  GRAPH ?g {
		  ?dataset dcat:distribution ?distribution .
		  ?distribution dataid:file ?file .


		  { ?distribution <dataid-cv:type> 'METADATASTRING'^^<http://www.w3.org/2001/XMLSchema#string> . }
	  }
  }
  "
  return(SPARQL(url, sub("METADATASTRING", metadata, q)))

}
