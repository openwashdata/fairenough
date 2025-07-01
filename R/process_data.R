#' Process raw data files for package creation
#'
#' Stage 1 of the fairenough pipeline. Processes all CSV files in the raw data
#' directory, cleans them, and prepares them for package inclusion.
#'
#' @param raw_data_path Character string. Path to directory containing raw CSV files.
#' @param overwrite_rda Logical. Whether to overwrite existing .rda files. Default TRUE.
#' @param overwrite_csv Logical. Whether to overwrite existing .csv files. Default TRUE.
#' @param verbose Logical. Show progress messages. Default TRUE.
#'
#' @return List containing processing results and file paths.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- process_data("data-raw/")
#' }
process_data <- function(
  raw_data_path,
  overwrite_rda = TRUE,
  overwrite_csv = TRUE,
  verbose = TRUE
) {
  
  if (verbose) cli::cli_alert_info("Starting data processing...")
  
  # Find all CSV files in the raw data directory
  csv_files <- list.files(
    path = raw_data_path,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(csv_files) == 0) {
    cli::cli_alert_warning("No CSV files found in {raw_data_path}")
    return(list(
      processed_files = character(0),
      datasets = character(0),
      errors = character(0)
    ))
  }
  
  if (verbose) {
    cli::cli_alert_info("Found {length(csv_files)} CSV file{?s} to process")
    cli::cli_ul(basename(csv_files))
  }
  
  # Process each CSV file
  processed_files <- character()
  datasets <- character()
  errors <- character()
  
  for (csv_file in csv_files) {
    if (verbose) {
      cli::cli_alert("Processing {basename(csv_file)}...")
    }
    
    tryCatch({
      # Use existing process_csv function
      result <- process_csv(
        file_path = csv_file,
        overwrite_rda = overwrite_rda,
        overwrite_csv = overwrite_csv
      )
      
      if (!is.null(result)) {
        processed_files <- c(processed_files, csv_file)
        datasets <- c(datasets, tools::file_path_sans_ext(basename(csv_file)))
        if (verbose) cli::cli_alert_success("Processed {basename(csv_file)}")
      }
      
    }, error = function(e) {
      error_msg <- paste(basename(csv_file), ":", e$message)
      errors <- c(errors, error_msg)
      if (verbose) cli::cli_alert_danger("Failed to process {basename(csv_file)}: {e$message}")
    })
  }
  
  # Setup data-raw directory structure if needed
  if (!dir.exists("data-raw")) {
    if (verbose) cli::cli_alert_info("Setting up data-raw directory...")
    tryCatch({
      setup_rawdata()
      if (verbose) cli::cli_alert_success("Created data-raw directory structure")
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("Could not setup data-raw: {e$message}")
    })
  }
  
  # Summary
  if (verbose) {
    cli::cli_alert_success("Data processing complete!")
    cli::cli_alert_info("Successfully processed: {length(processed_files)} file{?s}")
    if (length(errors) > 0) {
      cli::cli_alert_warning("Errors encountered: {length(errors)}")
    }
  }
  
  return(list(
    processed_files = processed_files,
    datasets = datasets,
    errors = errors,
    csv_count = length(csv_files),
    success_count = length(processed_files),
    error_count = length(errors)
  ))
}