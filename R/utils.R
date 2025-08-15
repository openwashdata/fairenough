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

#' Get raw directory path with consistent handling across all functions
#' 
#' This function provides a consistent way to handle raw_dir across all fairenough functions.
#' If raw_dir is provided, it sets the global option and returns the normalized path.
#' If raw_dir is NULL, it checks for the global option, falling back to "data_raw"
#' 
#' @param raw_dir Optional base path to set
#' @return Normalized base path
#' @export
get_raw_dir <- function(raw_dir = NULL) {
  # If raw_dir is provided, set it as option and use it
  if (!is.null(raw_dir)) {
    normalized_path <- normalizePath(raw_dir, mustWork = TRUE)
    options(fairenough.raw_dir = normalized_path)
    return(normalized_path)
  }
  
  # Otherwise, check for existing option
  stored_path <- getOption("fairenough.raw_dir")
  if (!is.null(stored_path)) {
    return(stored_path)
  }
  
  # Fall back to data_raw
  default_path <- "data_raw"
  
  normalized_path <- normalizePath(default_path, mustWork = TRUE)
  options(fairenough.raw_dir = normalized_path)
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
#' @param verbose Whether to show messages (default TRUE)
#' @return The directory path
#' @export
ensure_directory <- function(dir_path, description = NULL, recursive = TRUE, verbose = TRUE) {
  if (is.null(description)) {
    description <- "Directory"
  }
  
  if (fs::dir_exists(dir_path)) {
    if (verbose) cli::cli_alert_info("{description} '{dir_path}' already exists")
  } else {
    fs::dir_create(dir_path, recurse = recursive)
    if (verbose) cli::cli_alert_success("Created {description}: {dir_path}")
  }
  
  return(dir_path)
}

#' Use a template file with data substitution
#' 
#' Similar to usethis::use_template but respects base_path.
#' Reads a template from the package, substitutes data, and writes to target location.
#' 
#' @param template Name of template file in inst/templates
#' @param save_as Path to save file relative to base_path
#' @param data List of data for substitution
#' @param base_path Base path for the project
#' @param package Package containing the template
#' @param open Whether to open the file after creation
#' @param verbose Whether to show messages
#' @return Path to created file
#' @export
use_template <- function(template,
                        save_as = template,
                        data = list(),
                        base_path = NULL,
                        package = "fairenough",
                        open = FALSE,
                        verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  # Get template from package
  template_path <- system.file("templates", template, package = package)
  if (template_path == "") {
    cli::cli_abort("Template {.file {template}} not found in package {.pkg {package}}")
  }
  
  # Read template
  template_content <- paste(readLines(template_path), collapse = "\n")
  
  # Use whisker for template rendering (same as usethis)
  if (!requireNamespace("whisker", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg whisker} is required. Install it with: install.packages('whisker')")
  }
  
  # Render template with whisker
  rendered_content <- whisker::whisker.render(template_content, data)
  
  # Create output path
  output_path <- file.path(base_path, save_as)
  
  # Ensure directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Check if file exists and prompt
  if (file.exists(output_path)) {
    if (interactive()) {
      if (!usethis::ui_yeah("Overwrite pre-existing file {usethis::ui_path(save_as)}?")) {
        return(invisible(NULL))
      }
    }
  }
  
  # Write file
  writeLines(rendered_content, output_path)
  
  if (verbose) {
    if (file.exists(output_path)) {
      cli::cli_alert_success("Writing {.path {save_as}}")
    } else {
      cli::cli_alert_success("Creating {.path {save_as}}")
    }
  }
  
  # Open file if requested
  if (open && interactive()) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::navigateToFile(output_path)
    } else {
      utils::file.edit(output_path)
    }
  }
  
  return(invisible(output_path))
}