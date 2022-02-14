
#' @importFrom httr POST add_headers








deploy_to_dev_databus <- function(api_key, group, dataid) {

    DATABUS_URI_BASE = "https://dev.databus.dbpedia.org"
    post_databus_uri = "https://dev.databus.dbpedia.org/system/publish"


        message(paste0("Deploying Group"))
        groupld = asJsonldGroup(context, group)
        resp <- POST(url = post_databus_uri,
                     body = groupld,
                     encode = "raw",
                     config = add_headers("X-API-Key" = api_key, "Content-Type" = "application/json")
                     )
        print(groupld)
        print(resp)


        message(paste0("Deploying DataID"))
        dataidld = asJsonldDataID(context, dataid)
        resp <- POST(url = post_databus_uri,
                     body = dataidld,
                     encode = "raw",
                     config = httr::add_headers("X-API-Key" = api_key, "Content-Type" = "application/json")
        )
        print(dataidld)
        print(resp)
        #        resp = PUT(getTargetURIGroup(group), headers='{"X-API-Key": api_key, "Content-Type": "application/json"}', data = groupld)
#print(resp)
#system(paste0("curl -H 'x-api-key: ", api_key, "' -X PUT -H 'Content-Type: application/json' -d '", groupld,  "' ", post_databus_uri)) #https://dev.databus.dbpedia.org/giannoupik/general"))
#            if (resp["status_code"] >= 400) {
#            print(f"Response: Status {resp.status_code}; Text: {resp.text}")
#            print(f"Problematic file:\n {submission_data}")
#        }

#        message(paste0("Deploying DataID"))
#        dataid = asJsonld(context, dataid)
#        resp = PUT(getTargetURIGroup(dataid), headers='{"X-API-Key": api_key, "Content-Type": "application/json"}', data = dataid)

#        if (resp["status_code"] >= 400) {
#            print(f"Response: Status {resp.status_code}; Text: {resp.text}")
#            print(f"Problematic file:\n {submission_data}")
#        }

return(resp)
}

context <- "https://downloads.dbpedia.org/databus/context.jsonld"

account_name <- "giannoupik"

group <- "general7R"

artifact <- "testartifact7R"

version <- "2022-02-11"

title <- "Test Title 7R"


# currently its advised to use the internal webid found at https://dev.databus.dbpedia.org/{{user}}#settings
publisher <- paste0("https://dev.databus.dbpedia.org/", account_name, "#this")

label <- "Test Label 7R"

comment <- "This is a short comment about the 7R test."

abstract <- "This a short abstract for the dataset 7R. Since this is only a test it is quite insignificant."

description <- "A bit longer description of the dataset 7R."

license <- "http://this.is.a.license.uri.com/test"

files <- list(
    list("file" = "https://yum-yab.github.io/data/databus-api-test/first/pizza-ont.owl",
         "metadata" = list("type" = "ontology"),
         "filetype" = "owl"),
    list("file" = "https://yum-yab.github.io/data/databus-api-test/first/Sample500.csv",
        "metadata" = list("type" = "randomData"),
        "filetype" = "csv"),
    list("file" = "https://openenergy-platform.org/api/v0/schema/supply/tables/wind_turbine_library/rows/",
         "metadata" = list("type" = "turbineData", "extra" = "external"),
         "filetype" = "json")
)

databus_version = list(
    account_name = account_name,
    id = group,
    group = group,
    artifact = artifact,
    version = version,
    title = title,
    publisher = publisher,
    label = label,
    comment = comment,
    abstract = abstract,
    description = description,
    license = license,
    databus_files = files
)

databus_group = list(
    account_name = account_name,
    id = group,
    label = "Test Group 7R",
    title = "Test Group 7R",
    abstract = "Lorem ipsum dolor sit amet.",
    comment = "Lorem ipsum dolor sit amet.",
    description = "Lorem ipsum dolor sit amet."
)

# For the new version deployed to dev.databus.dbpedia.org
# API KEY can be found or generated under https://dev.databus.dbpedia.org/{{user}}#settings
#deploy_to_dev_databus("my-key", databus_group, databus_version)
