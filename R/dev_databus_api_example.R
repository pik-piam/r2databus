library(jsonlite)
library(httr)

DATABUS_URI_BASE = "https://dev.databus.dbpedia.org"
post_databus_uri = "https://dev.databus.dbpedia.org/system/publish"


as.jsonldGroup <- function(context, x) {

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
    return(jsonlite::toJSON(group_data_dict, auto_unbox = TRUE))

}

as.jsonldDataID <- function(context, x) {

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
                "artifact" = paste0(DATABUS_URI_BASE , "/", x["account_name"] , "/", x["group"] , "/", x["artifact"]),
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
    group_data_dict[["@graph"]] <- c(group_data_dict[["@graph"]], list(list("@id" = paste0(DATABUS_URI_BASE , "/", x["account_name"] , "/", x["group"] , "/", x["artifact"]), "@type" = "dataid:Artifact")))
    group_data_dict[["@graph"]] <- c(group_data_dict[["@graph"]], list(list("@id" = paste0(DATABUS_URI_BASE , "/", x["account_name"] , "/", x["group"] , "/", x["artifact"], "/", x["version"]), "@type" = "dataid:Version")))
    return(jsonlite::toJSON(group_data_dict, auto_unbox = TRUE))

}


dbfiles_to_dict <- function(x, artifact, dataid_uri) {
    file_dst <- list()
    i <- 0
    for (dbfile in x) {
        i <- i + 1
        file_dst[[i]] = list(
            "@id" = paste0(dataid_uri, "#", getDatabusFile(dbfile)[["id_string"]]),
            "file" = paste0(dataid_uri, "/", artifact, "_", getDatabusFile(dbfile)[["id_string"]]),
            "@type" = "dataid:Part",
            "format" = getDatabusFile(dbfile)[["file_ext"]],
            "compression" = "none",
            "downloadURL" = dbfile[["file"]],
            "byteSize" = as.character(getDatabusFile(dbfile)[["content_length"]]),
            "sha256sum" = getDatabusFile(dbfile)[["sha256sum"]]
        )
        for (kv in dbfile["metadata"]) file_dst[[i]][paste0("dcv:", names(kv))] = kv
    }

    return(file_dst)
}


deploy_to_dev_databus <- function(api_key, group, dataid) {

        message(paste0("Deploying Group"))
        groupld = as.jsonldGroup(context, group)
        resp <- POST(url = post_databus_uri,
                     body = groupld,
                     encode = "raw",
                     config = httr::add_headers("X-API-Key" = api_key, "Content-Type" = "application/json")
                     )
        print(groupld)
        print(resp)


        message(paste0("Deploying DataID"))
        dataidld = as.jsonldDataID(context, dataid)
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
#        dataid = as.jsonld(context, dataid)
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
deploy_to_dev_databus("my-key", databus_group, databus_version)
