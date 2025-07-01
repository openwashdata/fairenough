#' Create a dictionary file for tidy data sets
#' @description
#' `setup_dictionary()` generates a dictionary CSV file in the
#' `data/` directory. The dictionary file
#' contains information on the tidy data sets such as directory, file names, variable names,
#' variable types, and descriptions. If tidy data exists, the dictionary is populated with
#' relevant information; otherwise, it creates an empty dictionary CSV file.
#' @returns NULL. Error if raw data is not found or not in a package directory.
#' @examples
#' \dontshow{
#' temppkg <- tempdir()
#' devtools::create(temppkg, open=FALSE)
#' .old_wd <- setwd(temppkg)
#' }
#' \dontrun{
#' setup_rawdata()
#' # Go to data_processing.R, clean the raw data and export tidy data
#' setup_dictionary()
#' }
#' \dontshow{
#' setwd(.old_wd)
#' }
setup_dictionary <- function() {
  # Check working directory
  correct_wd <- is_pkg()
  has_dataraw <- dir.exists(file.path(getwd(), "data-raw"))
  if(correct_wd) {
    if(!has_dataraw){
      usethis::ui_stop("You have not set up the raw data.
                        Please run setup_rawdata() and import raw data files first.")
    }
  } else {
    usethis::ui_stop("You are not in the correct working directory for developing the data package.
                          Please check your working directory.")
  }
  # Check dictionary csvfile existence
  dict_path <- file.path("data-raw", "dictionary.csv")
  if(no_dict(dict_path)){
    dictionary <- fill_dictionary(dict_path, "data/")
  } else {
    usethis::ui_stop(paste("The dictionary CSV file", dict_path, "already exists!"))
  }
}

#' Fill in the dictionary file based on the tidy data information
#' @param dict_path Path to the dictionary csvfile.
#' @param data_dir Path to the directory of the tidy R data objects. Defaults to data/
#' @returns A tibble data frame of dataset dictionary with an empty description column to be written.
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' update_dictionary(dict_path = "data-raw/my-dictionary.csv", data = "data/")
#' }
#' \dontshow{
#' setwd(.old_wd)
#' }
fill_dictionary <- function(dict_path, data_dir){
  # Collect tidy data information
  if(dir.exists(data_dir)){
    tidydata_info <- collect_tidydata_info(data_dir)
  } else {
    # Error because tidy data is not yet available, should complete that first
    usethis::ui_stop("There is no tidy data available. Please use devtools::use_data() to store the tidy data as an R data object first.")
  }
  # Fill in dictionary
  dictionary <- data.frame(directory = data_dir,
                               file_name = tidydata_info$file_name,
                               variable_name = tidydata_info$var_name,
                               variable_type = tidydata_info$var_type,
                               description = NA)
  # Export dictionary
  utils::write.csv(x = dictionary, file = dict_path, na = "", row.names = FALSE)
  # Prompt to complete variable description
  usethis::ui_todo("To complete the dictionary at {dict_path}, please provide the variable descriptions.")
  return(dictionary)
  }

no_dict <- function(dict_path) {
  if(file.exists(dict_path)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

create_empty_dict <- function(dict_path){
  file.create(dict_path)
  writeLines(text = "directory,file_name,variable_name,variable_type,description",
             con = dict_path)
}

collect_tidydata_info <- function(data_dir){
  tidy_data_names <- list.files(data_dir)
  file_name <- c()
  var_name <- c()
  var_type <- c()
  for (d in tidy_data_names){
    tidydata <- load_object(file.path(data_dir, d)) # Read in tidy dataset(s)
    file_name <- c(file_name, rep(d, ncol(tidydata)))
    var_name <-c(var_name, colnames(tidydata))
    var_type <- c(var_type, sapply(tidydata, class))
  }
  var_type <- as.character(var_type)
  return(data.frame(file_name, var_name, var_type))
}
