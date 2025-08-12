#' Add contributors to the creators metadata file
#' @param name Name of the contributor
#' @param email Email ID of the contributor
#' @param affiliation Affiliation (Institute/organisation of the contributor)
#' @returns NULL, Errors handled within the function
#' @examples
#' \dontrun{
#' add_creator("Jane Doe", 'janedoeatuniversity.org', affiliation = "University")
#' }
add_creator <- function (name= "", email="", affiliation = "Openwashdata") {


  # Ensure data type consistency

  if (!file.exists("data/metadata/creators.csv")) {
    usethis::ui_stop("creators.csv file not found. Please run `add_metadata()` first.")
  }
  creators <- readr::read_csv("data/metadata/creators.csv", col_types = readr::cols(
    id = readr::col_double(),
    name = readr::col_character(),
    affiliation = readr::col_character(),
    email = readr::col_character()
  ))

  if (name == ""){
    usethis::ui_stop("Name cannot be blank!")
  }

  if (email == ""){
    # Ask if user wants to proceed without email
    email_confirm <- readline("Email is blank. Do you want to proceed without email? (y/n): ")
    if (tolower(email_confirm) != "y") {
      usethis::ui_stop("Please pass an email address.")
    }
  }

  if (affiliation == "Openwashdata") {
    cat("Proceeding with default affiliation: Openwashdata\n")
  }

  id <- nrow(creators) + 1

  # Append new creator to the end of the data frame
  new_row <- tibble::tibble(
    id = id,
    name = name,
    affiliation = affiliation,
    email = email
  )

  creators <- dplyr::bind_rows(creators, new_row)

  # Write back to creators.csv
  readr::write_csv(creators, "data/metadata/creators.csv")

  print("Creator added successfully!")

}
