#' Utility functions for fairenough

# Supported file extensions for data processing
SUPPORTED_EXTENSIONS <- c("csv", "xlsx", "xls")
SUPPORTED_EXTENSIONS_DOT <- c(".csv", ".xlsx", ".xls")

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
    return(validate_data_frame(data))
  }
  
  # Must be a file path - validate it
  data_path <- validate_file_path(data)
  
  # Determine file type and read accordingly
  file_ext <- tolower(tools::file_ext(data_path))
  
  if (file_ext == "csv") {
    if (show_messages) cli::cli_alert_info("Reading CSV file: {data_path}")
    data <- readr::read_csv(data_path, show_col_types = FALSE)
  } else if (file_ext %in% c("xlsx", "xls")) {
    # Ensure readxl is available
    if (!requireNamespace("readxl", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg readxl} is required to read Excel files",
        "i" = "Install it with: install.packages('readxl')"
      ))
    }
    if (show_messages) cli::cli_alert_info("Reading Excel file: {data_path}")
    data <- readxl::read_excel(data_path)
  }
  
  # Validate and return the loaded data
  return(validate_data_frame(data))
}

#' Get supported file extensions
#' @return Character vector of supported file extensions (without dots)
#' @export
get_supported_extensions <- function() {
  return(SUPPORTED_EXTENSIONS)
}

#' Check if file type is supported
#' @param file_path Path to file
#' @return Logical indicating if file type is supported
#' @export
is_supported_file_type <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    return(FALSE)
  }
  file_ext <- tolower(tools::file_ext(file_path))
  return(file_ext %in% SUPPORTED_EXTENSIONS)
}

#' Filter files by supported extensions
#' @param file_paths Character vector of file paths
#' @return Character vector of files with supported extensions
#' @export
filter_supported_files <- function(file_paths) {
  if (length(file_paths) == 0) {
    return(character(0))
  }
  
  supported_files <- character(0)
  for (ext in SUPPORTED_EXTENSIONS_DOT) {
    pattern <- paste0("\\", ext, "$")
    matching_files <- file_paths[grepl(pattern, file_paths, ignore.case = TRUE)]
    supported_files <- c(supported_files, matching_files)
  }
  
  return(supported_files)
}

#' Validate file path
#' @param file_path Path to file
#' @return The validated file path (throws error if invalid)
#' @export
validate_file_path <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    cli::cli_abort("File path must be a single character string")
  }
  
  if (!file.exists(file_path)) {
    cli::cli_abort("File not found: {file_path}")
  }
  
  if (!is_supported_file_type(file_path)) {
    file_ext <- tools::file_ext(file_path)
    cli::cli_abort("Unsupported file type: {file_ext}. Supported types: {paste(SUPPORTED_EXTENSIONS, collapse=', ')}")
  }
  
  return(file_path)
}

#' Validate data frame
#' @param data Data frame to validate
#' @param min_rows Minimum number of rows required (default 1)
#' @return The validated data frame (throws error if invalid)
#' @export
validate_data_frame <- function(data, min_rows = 1) {
  if (!is.data.frame(data)) {
    cli::cli_abort("Input must be a data frame")
  }
  
  if (nrow(data) < min_rows) {
    cli::cli_abort("Data frame must have at least {min_rows} row{?s}")
  }
  
  return(data)
}

#' Ensure directory exists with messaging
#' @param dir_path Path to directory
#' @param description Optional description for messages
#' @param recursive Whether to create parent directories (default TRUE)
#' @return The directory path
#' @export
ensure_directory <- function(dir_path, description = NULL, recursive = TRUE) {
  if (is.null(description)) {
    description <- "Directory"
  }
  
  if (fs::dir_exists(dir_path)) {
    cli::cli_alert_info("{description} '{dir_path}' already exists")
  } else {
    fs::dir_create(dir_path, recurse = recursive)
    cli::cli_alert_success("Created {description}: {dir_path}")
  }
  
  return(dir_path)
}