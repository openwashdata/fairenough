#' Initialize all data processing steps
#' 
#' @param data_raw_dir Directory name for raw data files
#' @param gitignore Whether to add data_raw to .gitignore
#' @param overwrite_rda Whether to overwrite existing .rda files
#' @param auto_clean Whether to auto-clean data during export
#' @param base_path Base path for the project (defaults to here::here())
#' @return Invisibly returns the base_path
#' @export
init_all <- function(data_raw_dir = "data_raw", gitignore = TRUE, overwrite_rda = TRUE, auto_clean = TRUE, base_path = NULL) {
  # Use the utility function for consistent base_path handling
  base_path <- get_base_path(base_path)

  # Initialise raw data directory
  init_data_raw(data_raw_dir, gitignore, base_path)

  # Check if the data_raw directory exists
  data_raw_path <- file.path(base_path, data_raw_dir)
  if (!fs::dir_exists(data_raw_path)) {
    message(paste("Error: Directory '", data_raw_path, "' not found. Please create it or adjust the path."))
  } else {
    # List all files in the data_raw directory
    all_files_in_data_raw <- list.files(
      path = data_raw_path,
      full.names = TRUE,
      recursive = FALSE
    )

    # Define target extensions
    target_extensions <- c(".csv", ".xls", ".xlsx")
    files_to_process <- character(0)

    # Filter files by target extensions
    for (ext in target_extensions) {
      files_to_process <- c(files_to_process,
                            all_files_in_data_raw[grepl(paste0("\\", ext, "$"), all_files_in_data_raw, ignore.case = TRUE)])
    }

    if (length(files_to_process) == 0) {
      message(paste("No ", target_extensions, " files found in '", data_raw_path, "' to process."))
    } else {
      message(paste0("Found ", length(files_to_process), " files to process in '", data_raw_path, "':"))
      print(basename(files_to_process))

      processed_data_list <- list()

      # Loop through each file and process it
      for (file_path in files_to_process) {
        # Call init_export for each file
        init_export(
          data_file = file_path,
          overwrite_rda = overwrite_rda,
          auto_clean = auto_clean,
          base_path = base_path
        )
      }

      message("All raw data files have been processed.")
      message("\nTo create a data dictionary, use doc_dictionary() after init_all.")
      message("For automatic description generation, pass an ellmer::Chat object to doc_dictionary().")
    }
  }
  
  # Return the base_path so users can reuse it
  invisible(base_path)
}