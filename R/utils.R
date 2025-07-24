#' Utility functions for fairenough

#' Get base path with consistent handling across all functions
#' 
#' This function provides a consistent way to handle base_path across all fairenough functions.
#' If base_path is provided, it sets the global option and returns the normalized path.
#' If base_path is NULL, it checks for the global option, falling back to here::here() or "."
#' 
#' @param base_path Optional base path to set
#' @return Normalized base path
#' @export
get_base_path <- function(base_path = NULL) {
  # If base_path is provided, set it as option and use it
  if (!is.null(base_path)) {
    normalized_path <- normalizePath(base_path, mustWork = TRUE)
    options(fairenough.base_path = normalized_path)
    return(normalized_path)
  }
  
  # Otherwise, check for existing option
  stored_path <- getOption("fairenough.base_path")
  if (!is.null(stored_path)) {
    return(stored_path)
  }
  
  # Fall back to here::here() or current directory
  default_path <- tryCatch(
    here::here(),
    error = function(e) {
      message("No project root found, using current directory")
      "."
    }
  )
  
  normalized_path <- normalizePath(default_path, mustWork = TRUE)
  options(fairenough.base_path = normalized_path)
  return(normalized_path)
}

#' Read data from various sources
#' 
#' This function reads data from a data frame, CSV file, or Excel file.
#' It handles all validation and provides consistent error messages.
#' 
#' @param data Either a data frame or a path to a CSV/Excel file
#' @param show_messages Whether to show informational messages (default TRUE)
#' @return A data frame
#' @export
read_data <- function(data, show_messages = TRUE) {
  # If already a data frame, validate and return
  if (is.data.frame(data)) {
    if (nrow(data) == 0) {
      cli::cli_abort("Data frame must have at least one row")
    }
    return(data)
  }
  
  # Handle file path input
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) {
      cli::cli_abort("File not found: {data}")
    }
    
    # Determine file type and read accordingly
    file_ext <- tolower(tools::file_ext(data))
    
    if (file_ext == "csv") {
      if (show_messages) cli::cli_alert_info("Reading CSV file: {data}")
      data <- readr::read_csv(data, show_col_types = FALSE)
    } else if (file_ext %in% c("xlsx", "xls")) {
      # Ensure readxl is available
      if (!requireNamespace("readxl", quietly = TRUE)) {
        cli::cli_abort(c(
          "Package {.pkg readxl} is required to read Excel files",
          "i" = "Install it with: install.packages('readxl')"
        ))
      }
      if (show_messages) cli::cli_alert_info("Reading Excel file: {data}")
      data <- readxl::read_excel(data)
    } else {
      cli::cli_abort("Unsupported file type: {file_ext}. Supported types: csv, xlsx, xls")
    }
    
    # Validate the loaded data
    if (!is.data.frame(data)) {
      cli::cli_abort("Failed to read file as a data frame")
    }
    
    if (nrow(data) == 0) {
      cli::cli_abort("File contains no data rows")
    }
    
    return(data)
  }
  
  # If we get here, input is invalid
  cli::cli_abort("Input must be a data frame or a path to a CSV/Excel file")
}