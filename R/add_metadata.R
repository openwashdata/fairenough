#' Adds metadata files and creates the metadata folder
#'
#' @description
#' Creates the data/metadata directory with the acess, attributes, biblio and
#' creators metadata files. If the directory already exists, the user is prompted
#' to overwrite the existing files.
#'
#'
#' @returns NULL. Error message if the metadata folder already exists and user declines to
#' create new metadata files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_metadata()
#' }
add_metadata <- function(){

  # If metadata folder doesn't exist
  if(!dir.exists("data/metadata")){
    cat("No metadata folder detected")
    dataspice::create_spice()
    cat("\n")
    cat("Metadata files created in data/metadata")
  }
  else {
    cat("Metadata folder already exsits. Overwrite?")
    response <- readline(prompt = "Enter y/n: ")
    if (response == "y"){
      dataspice::create_spice()
      cat("Metadata files created in data/metadata")
    }
    else {
      cat("Failed! Folder already exists.")
  }
  }

  # Add metadata folder to .Rbuildignore

  rbuildignore_path <- ".Rbuildignore"
  new_pattern <- "^data/metadata$"

  # Check if the .Rbuildignore file exists
  if (file.exists(rbuildignore_path)) {
    # Check for existing pattern
    lines <- readLines(rbuildignore_path)
    if (!(new_pattern %in% lines)) {
      # Add the pattern if it doesn't already exist
      writeLines(c(lines, new_pattern), rbuildignore_path)
      cat("\n")
      cat("Pattern added to .Rbuildignore.")
    } else {
      cat("\n")
      cat("Pattern already exists in .Rbuildignore.")
    }
  } else {
    # Notify the user to check for the missing file
    cat("\n")
    cat(".Rbuildignore file does not exist. Please follow all previous steps in the fairenough workflow and re-run this script.")
  }

}
