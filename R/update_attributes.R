#' Update metadata attributes file
#' @description Updates the attributes file in the metadata folder with data from the dictionary
#' file in the data-raw folder. This file is esentially a copy of the dictionary file. If the
#' dictionary file does not exist, the user is asked to create it first. Always overwrites existing
#' attributes file with the latest dictionary file.
#' @returns NULL. Error if dictionary file is not found.
#' @examples
#' \dontrun{
#' update_attributes()
#' }
update_attributes <- function(){

  correct_wd <- is_pkg()
  if (correct_wd){

    if(!file.exists("data-raw/dictionary.csv")){
      usethis::ui_stop("Dictionary file not detected. Please run setup_rawdata first and ensure dictionary is up to date")
    }
    else {
      dictionary <- readr::read_csv("data-raw/dictionary.csv", show_col_types = FALSE)
      dictionary |>  readr::write_csv("data/metadata/attributes.csv")
      cat("Attributes metadata updated!")
    }
  }
  else {
    usethis::ui_stop("You are not in the correct working directory for developing the data package.
                          Please check your working directory.")
  }
}

