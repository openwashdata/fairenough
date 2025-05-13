#' Function to update the biblio metadata file
#'
#' @description This function updates the biblio metadata file with the values from the DESCRIPTION file,
#' along with some standard templated values for funder, license, etc.
#'
#' @return NULL. Throws an error if DESCRIPTION does not exist.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' update_biblio()
#' }
update_biblio <- function() {
  # Check if the DESCRIPTION file exists
  if (!file.exists("DESCRIPTION")) {
    stop("DESCRIPTION file not found. Please create and complete a DESCRIPTION first.")
  }

  # Check if the biblio.csv file exists
  biblio_path <- "data/metadata/biblio.csv"
  if (!file.exists(biblio_path)) {
    stop("biblio.csv file not found at 'data/metadata/'. Please run add_metadata first.")
  }

  # Read the DESCRIPTION file
  description <- read.dcf("DESCRIPTION")

  # Ensure required fields exist
  required_fields <- c("Title", "Description", "License")
  missing_fields <- setdiff(required_fields, colnames(description))
  if (length(missing_fields) > 0) {
    stop(paste("Missing fields in DESCRIPTION:", paste(missing_fields, collapse = ", ")))
  }

  # Extract values for the selected fields
  title <- description[,"Title"]
  pkg_description <- description[,"Description"]
  license <- description[,"License"]
  funder <- "https://ethrat.ch/en/eth-domain/open-research-data/"

  # Read and update biblio data
  biblio_data <- readr::read_csv(biblio_path, show_col_types = FALSE)
  biblio_data[1, "title"] <- title
  biblio_data[1, "description"] <- pkg_description
  biblio_data[1, "license"] <- license
  biblio_data[1, "funder"] <- funder

  # Write back to biblio.csv
  readr::write_csv(biblio_data, biblio_path)
  print("Biblio metadata updated! Please complete remaining fields.")
}
