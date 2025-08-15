#' Process all data files
#' 
#' High-level function to read, clean, and export all data files.
#' Processes all files in the raw data directory and exports them as .rda and CSV.
#' 
#' @param raw_dir Directory containing raw data files (default: "data_raw")
#' @param auto_clean Whether to automatically clean data (default: TRUE)
#' @param overwrite Whether to overwrite existing files (default: TRUE)
#' @param base_path Base path for the project (default: uses get_base_path())
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @return Invisibly returns a list of processed files
#' @export
#' @examples
#' \dontrun{
#' # Basic processing
#' process()
#' 
#' # Skip auto-cleaning
#' process(auto_clean = FALSE)
#' 
#' # Preserve existing files
#' process(overwrite = FALSE)
#' }
process <- function(raw_dir = NULL,
                   auto_clean = TRUE,
                   overwrite = TRUE,
                   base_path = NULL,
                   verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  raw_dir <- get_raw_dir(raw_dir)
  raw_path <- file.path(base_path, raw_dir)
  
  if (verbose) cli::cli_h1("Processing data files")
  
  # Check if data_raw directory exists
  if (!fs::dir_exists(raw_path)) {
    cli::cli_alert_warning("Directory {.path {raw_dir}} not found")
    cli::cli_alert_info("Run {.fn setup} first to create project structure")
    return(invisible(NULL))
  }
  
  # Find all data files
  all_files <- list.files(raw_path, full.names = TRUE)
  data_files <- filter_supported_files(all_files)
  
  if (length(data_files) == 0) {
    cli::cli_alert_warning("No data files found in {.path {raw_dir}}")
    supported <- paste(get_supported_extensions(), collapse = ", ")
    cli::cli_alert_info("Supported formats: {supported}")
    return(invisible(NULL))
  }
  
  if (verbose) cli::cli_alert_info("Found {length(data_files)} file{?s} to process")
  
  processed_files <- list()
  
  # Process each file
  for (file_path in data_files) {
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    if (verbose) cli::cli_alert_info("Processing {.file {basename(file_path)}}")
    
    tryCatch({
      # Read data
      data <- read_data(file_path, show_messages = FALSE)
      
      # Clean if requested
      if (auto_clean) {
        data <- clean_data(data, verbose = FALSE)
      }
      
      # Export to both formats
      export_rda(data, file_name, base_path, overwrite, verbose = FALSE)
      export_csv(data, file_name, base_path, overwrite, verbose = FALSE)
      
      processed_files[[file_name]] <- list(
        source = file_path,
        rows = nrow(data),
        cols = ncol(data)
      )
      
      if (verbose) {
        cli::cli_alert_success("Processed {.file {file_name}}: {nrow(data)} rows, {ncol(data)} columns")
      }
      
    }, error = function(e) {
      cli::cli_alert_danger("Failed to process {.file {basename(file_path)}}: {e$message}")
    })
  }
  
  # Save dataset names to metadata.json
  if (length(processed_files) > 0) {
    dataset_names <- list(names(processed_files))
    update_metadata("datasets", dataset_names, base_path, verbose = verbose)
  }
  
  if (verbose) {
    cli::cli_alert_success("Processing complete: {length(processed_files)}/{length(data_files)} files")
    cli::cli_alert_info("Next step: Run {.fn document} to generate documentation")
  }
  
  invisible(processed_files)
}

#' Clean data with standard transformations
#' 
#' Applies standard cleaning operations to a data frame:
#' - Clean column names (janitor::clean_names)
#' - Convert string nulls to NA
#' - Optimize numeric types
#' 
#' @param data Data frame to clean
#' @param clean_names Whether to clean column names (default: TRUE)
#' @param string_nulls Character values to convert to NA (default: c("null", "NA", ""))
#' @param optimize_types Whether to optimize numeric types (default: TRUE)
#' @param verbose Whether to show messages (default: TRUE)
#' @return Cleaned data frame
#' @export
#' @examples
#' \dontrun{
#' # Basic cleaning
#' clean_data(my_data)
#' 
#' # Keep original column names
#' clean_data(my_data, clean_names = FALSE)
#' 
#' # Custom null values
#' clean_data(my_data, string_nulls = c("N/A", "missing", ""))
#' }
clean_data <- function(data,
                      clean_names = TRUE,
                      string_nulls = c("null", "NA", ""),
                      optimize_types = TRUE,
                      verbose = TRUE) {
  
  original_rows <- nrow(data)
  original_cols <- ncol(data)
  
  # Clean column names
  if (clean_names) {
    data <- janitor::clean_names(data)
    if (verbose) cli::cli_alert_info("Cleaned column names")
  }
  
  # Convert string nulls to NA
  if (length(string_nulls) > 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        dplyr::where(is.character),
        ~ ifelse(trimws(.) %in% string_nulls, NA, .)
      ))
    if (verbose) cli::cli_alert_info("Converted string nulls to NA")
  }
  
  # Optimize numeric types (convert to integer where possible)
  if (optimize_types) {
    data <- data |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ {
        if (all(is.na(.) | . == as.integer(.), na.rm = TRUE)) {
          as.integer(.)
        } else {
          .
        }
      }))
    if (verbose) cli::cli_alert_info("Optimized numeric types")
  }
  
  if (verbose) {
    cli::cli_alert_success("Data cleaned: {original_rows} rows, {original_cols} columns")
  }
  
  return(data)
}

#' Export data as RDA file
#' 
#' Saves a data frame as an .rda file in the data directory.
#' 
#' @param data Data frame to export
#' @param name Name for the data object (without extension)
#' @param base_path Base path for the project
#' @param overwrite Whether to overwrite existing file (default: TRUE)
#' @param verbose Whether to show messages (default: TRUE)
#' @return Path to the created file
#' @export
export_rda <- function(data,
                      name,
                      base_path = NULL,
                      overwrite = TRUE,
                      verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  # Ensure data directory exists
  data_dir <- file.path(base_path, "data")
  if (!fs::dir_exists(data_dir)) {
    fs::dir_create(data_dir)
  }
  
  # Create file path
  rda_path <- file.path(data_dir, paste0(name, ".rda"))
  
  # Check if file exists
  if (file.exists(rda_path) && !overwrite) {
    if (verbose) cli::cli_alert_info("Skipped {.file {name}.rda} (already exists)")
    return(invisible(rda_path))
  }
  
  # Save the data with the correct object name
  assign(name, data)
  save(list = name, file = rda_path)
  
  if (verbose) cli::cli_alert_success("Exported {.file data/{name}.rda}")
  
  return(invisible(rda_path))
}

#' Export data as CSV file
#' 
#' Saves a data frame as a CSV file in the inst/extdata directory.
#' 
#' @param data Data frame to export
#' @param name Name for the file (without extension)
#' @param base_path Base path for the project
#' @param overwrite Whether to overwrite existing file (default: TRUE)
#' @param verbose Whether to show messages (default: TRUE)
#' @return Path to the created file
#' @export
export_csv <- function(data,
                      name,
                      base_path = NULL,
                      overwrite = TRUE,
                      verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  # Ensure inst/extdata directory exists
  extdata_dir <- file.path(base_path, "inst", "extdata")
  if (!fs::dir_exists(extdata_dir)) {
    fs::dir_create(extdata_dir, recurse = TRUE)
  }
  
  # Create file path
  csv_path <- file.path(extdata_dir, paste0(name, ".csv"))
  
  # Check if file exists
  if (file.exists(csv_path) && !overwrite) {
    if (verbose) cli::cli_alert_info("Skipped {.file {name}.csv} (already exists)")
    return(invisible(csv_path))
  }
  
  # Write CSV
  readr::write_csv(data, csv_path)
  
  if (verbose) cli::cli_alert_success("Exported {.file inst/extdata/{name}.csv}")
  
  return(invisible(csv_path))
}