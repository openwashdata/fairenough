#' Updates metadata google sheet automatically
#'
#' @description This function updates the metadata google sheet with the values from
#' available metadata. Github username is required to update the maintainer field.
#'
#' @param github_profile Github username of the maintainer
#'
#' @returns NULL. Errors if metadata does not exist
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' update_gsheet_metadata("githubusername")
#' }
update_gsheet_metadata <- function(github_profile = "") {

  if (github_profile == "") {
    usethis::ui_stop("Please provide the github profile name for the maintainer.")
  }

  dictionary_path <- "data-raw/dictionary.csv"

  if (!file.exists(dictionary_path)) {
    usethis::ui_stop("Dictionary file not found. Please run 'setup_dictionary' to create the dictionary file.")
  }

  dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE)
  dataset <- dictionary |> dplyr::distinct(file_name)
  # remove extension from file_name
  file_name <- tools::file_path_sans_ext(dataset$file_name)

  maintainer <- github_profile

  id = ""

  source <- ""

  difficulty <- ""

  status <- "in-progress"

  description <- read.dcf("DESCRIPTION")[,"Description"]

  biblio_metadata <- readr::read_csv("data/metadata/biblio.csv", show_col_types = FALSE)

  location <- biblio_metadata$geographicDescription[1]

  date_published <-  ""

  link_github <-  paste0("<https://github.com/openwashdata/", file_name, ">")

  link_website <- paste0("<https://openwashdata.github.io/", file_name, ">")

  doi <- utils::readCitationFile("inst/CITATION")$doi

  temporal_coverage = paste(biblio_metadata$startDate[1], biblio_metadata$endDate[1], sep = "-")

  keywords <- biblio_metadata$keywords[1]

  # Create a dataframe with the metadata
  data <- data.frame(
    id = id,
    file_name = file_name,
    maintainer = maintainer,
    source = source,
    difficulty = difficulty,
    status = status,
    description = description,
    location = location,
    date_published = date_published,
    link_github = link_github,
    link_website = link_website,
    doi = doi,
    temporal_coverage = temporal_coverage,
    keywords = keywords
  )

  # Update these in the google sheet

  workbook = googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/1vtw16vpvJbioDirGTQcy0Ubz01Cz7lcwFVvbxsNPSVM/edit?gid=1694829481#gid=1694829481")

  googlesheets4::sheet_append(data, ss=workbook, sheet="Sheet1")

  cat("Successfully appended data. Please fill in the missing columns manually.")
}

