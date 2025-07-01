#' Create the data-raw directory with a data-processing.R template
#'
#' @description
#' `setup_rawdata()` creates a directory for raw data and an example script
#' named `data_processing.R` for importing, processing and exporting the tidy data.
#' The template assumes that the dataset name is the same as the data package name.
#'
#' @export
#'
#' @returns NULL. This function will create a directory "data-raw" under the package directory.
#'
#' @examples
#' \dontrun{
#'   setup_rawdata()
#' }
#'
setup_rawdata <- function(){
  correct_wd <- is_pkg()
  if(correct_wd) {
    usethis::use_directory("data-raw", ignore = TRUE)
    r_path <- file.path("data-raw", "data_processing.R")
    name <- basename(getwd())
    usethis::use_template(
      "data_processing.R",
      save_as = r_path,
      data = list(name = name),
      ignore = FALSE,
      open = rlang::is_interactive(),
      package = "fairenough"
    )
  }else{
    usethis::ui_stop("You are not in the correct working directory for developing the data package.
                          Please check your working directory.")
  }
}
