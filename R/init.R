init <- function(data_raw_dir = "data_raw", gitignore = TRUE, overwrite_rda = TRUE, auto_clean = TRUE) {

  # Initialise raw data directory
  init_data_raw()

  # Check if the data_raw directory exists
  if (!fs::dir_exists(data_raw_dir)) {
    message(paste("Error: Directory '", data_raw_dir, "' not found. Please create it or adjust the path."))
  } else {
    # List all files in the data_raw directory
    all_files_in_data_raw <- list.files(
      path = data_raw_dir,
      full.names = TRUE,
      recursive = FALSE
    )

    # Define target extensions
    target_extensions <- c(".csv", ".xls", ".xlsx")
    files_to_process <- character(0)

    # Filter files by target extensions
    for (ext in target_extensions) {
      files_to_process <- c(files_to_process,
                            all_files_in_data_raw[grepl(paste0("\\.", ext, "$"), all_files_in_data_raw, ignore.case = TRUE)])
    }

    if (length(files_to_process) == 0) {
      message(paste("No ", target_extensions, " files found in '", data_raw_dir, "' to process."))
    } else {
      message(paste0("Found ", length(files_to_process), " files to process in '", data_raw_dir, "':"))
      print(basename(files_to_process))

      processed_data_list <- list()

      # Loop through each file and process it
      for (file_path in files_to_process) {
        # Call init_export for each file
        init_export(
          file = file_path,
          overwrite_rda = overwrite_rda,
          auto_clean = auto_clean
        )
      }

      message("All raw data files have been processed.")
    }
  }
}