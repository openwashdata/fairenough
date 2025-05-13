#' Updates the access metadata file with links to the downloadable data files
#'
#' @description This function updates the access metadata file with the links to the downloadable data files
#' in xlsx and csv format. Redirects to the github repo raw download links. Meant to be a util function for
#' the `update_metadata` function.
#'
#' @returns NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' update_access()
#' }
#'
update_access <- function(){

extdata_path <- "https://github.com/openwashdata/worldhdi/raw/main/inst/extdata/"

dictionary_path <- "data-raw/dictionary.csv"

dictionary <- readr::read_csv(dictionary_path, show_col_types = FALSE)

dataset <- dictionary |> dplyr::distinct(file_name)

# Remove file extension
dataset <- dataset |> dplyr::mutate(file_name = stringr::str_remove(file_name, ".rda"))

file_name <- dataset$file_name

data <- data.frame(
  fileName = c(paste0(file_name, ".csv"), paste0(file_name, ".xlsx")),
  name = rep(file_name, 2),
  contentUrl = c(
    paste0("https://raw.githubusercontent.com/openwashdata/", file_name,"/main/inst/extdata/",file_name,".csv"),
    paste0("https://raw.githubusercontent.com/openwashdata/", file_name,"/main/inst/extdata/",file_name,".xlsx")
  ),
  encodingFormat = c("csv", "ooxml"),
  stringsAsFactors = FALSE
)

readr::write_csv(data, "data/metadata/access.csv")
cat ("Access metadata updated!")

}
