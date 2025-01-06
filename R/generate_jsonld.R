#' Function to create a JSON-LD document from available metadata
#'
#' @description This function follows a standard JSON-LD template and autopopulates
#' it with metadata from the dataset. The function checks for appropriate files being
#' available before executing. The JSON-LD document is saved to the metadata folder.
#'
#' @returns NULL. Error messages are displayed if dictionary or metadata files are not found.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' generate_jsonld()
#' }
generate_jsonld <- function() {

  dictionary_path <- "data-raw/dictionary.csv"

  if (!file.exists(dictionary_path)) {
    usethis::ui_stop("Dictionary file not found. Please run 'setup_dictionary' to create the dictionary file. Please setup metadata first by running add_metadata and update_metadata as well.")
  }

  dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE)

  dataset <- dictionary |> dplyr::distinct(file_name)

  # Remove file extension
  dataset <- dataset |> dplyr::mutate(file_name = stringr::str_remove(file_name, ".rda"))

  file_name <- dataset$file_name

  dataset_description <- read.dcf("DESCRIPTION")
  pkg_description <- dataset_description[,"Description"]

  if (!dir.exists("data/metadata")) {
    usethis::ui_stop("Metadata folder not found. Please run 'add_metadata' to create the metadata folder and ensure it is updated before running this function.")
  }

  access_metadata <- readr::read_csv("data/metadata/access.csv", show_col_types = FALSE)
  data_url <- access_metadata$contentUrl[1]

  biblio_metadata <- readr::read_csv("data/metadata/biblio.csv", show_col_types = FALSE)
  keywords <- biblio_metadata$keywords
  spatial_coverage <- biblio_metadata$geographicDescription
  start_date <- biblio_metadata$startDate
  end_date <- biblio_metadata$endDate

  creators_metadata <- readr::read_csv("data/metadata/creators.csv", show_col_types = FALSE)

  contact_type <- creators_metadata$name
  email <- creators_metadata$email

  today <- lubridate::today()

  # Construct the JSON-LD document as a list
  json_ld <- list(
    "@context" = "https://openwashdata.org/",
    "@type" = "Dataset",
    "name" = file_name,
    "description" = pkg_description,
    "publisher" = list(
      "@type" = "Organization",
      "name" = "openwashdata @ Global Health Engineering, ETH Zurich",
      "url" = "https://openwashdata.org/"
    ),
    "datePublished" = today,
    "license" = "https://creativecommons.org/licenses/by/4.0/",
    "keywords" = keywords,
    "distribution" = list(
      "@type" = "DataDownload",
      "url" = data_url,
      "contentUrl" = data_url,
      "encodingFormat" = "text/csv"
    ),
    "encodingFormat" = "CSV",
    "version" = "1.0",
    "spatialCoverage" = list(
      "@type" = "Place",
      "name" = spatial_coverage
    ),
    "temporalCoverage" = list(
      "@type" = "DateTime",
      "startDate" = start_date,
      "endDate" = end_date
    ),
    "contactPoint" = list(
      "@type" = "ContactPoint",
      "contactType" = contact_type,
      "email" = email
    )
)
  # Convert the list to a JSON-LD document
  json_ld_json <- jsonlite::toJSON(json_ld, pretty = TRUE, auto_unbox = TRUE)

  # Print the JSON-LD document
  cat(json_ld_json)

  # Save the JSON-LD document to a file
  write(json_ld_json, "inst/extdata/metadata.json")
}
