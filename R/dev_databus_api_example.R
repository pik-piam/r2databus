context <- "https://downloads.dbpedia.org/databus/context.jsonld"

account_name <- "giannoupik" # nolint

group <- "general8R"

artifact <- "testartifact8R"

version <- "2022-02-11"

title <- "Test Title 8R"


# currently its advised to use the internal webid found at https://dev.databus.dbpedia.org/{{user}}#settings
publisher <- paste0("https://dev.databus.dbpedia.org/", account_name, "#this")

label <- "Test Label 8R"

comment <- "This is a short comment about the 8R test."

abstract <- "This a short abstract for the dataset 8R. Since this is only a test it is quite insignificant."

description <- "A bit longer description of the dataset 8R."

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

databus_version <- list( # nolint
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

databus_group <- list( # nolint
    account_name = account_name,
    id = group,
    label = "Test Group 8R",
    title = "Test Group 8R",
    abstract = "Lorem ipsum dolor sit amet.",
    comment = "Lorem ipsum dolor sit amet.",
    description = "Lorem ipsum dolor sit amet."
)
