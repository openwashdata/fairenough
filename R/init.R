init <- function(data_raw_dir = "data_raw", gitignore = TRUE, overwrite_rda = TRUE, auto_clean = TRUE, base_path = NULL) {
  # Smart default: try here::here(), fall back to current directory
  if (is.null(base_path)) {
    base_path <- tryCatch(
      here::here(),
      error = function(e) {
        message("No project root found, using current directory")
        "."
      }
    )
  }
  
  # Normalize the base path
  base_path <- normalizePath(base_path, mustWork = TRUE)

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
                            all_files_in_data_raw[grepl(paste0("\\.", ext, "$"), all_files_in_data_raw, ignore.case = TRUE)])
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
          file = file_path,
          overwrite_rda = overwrite_rda,
          auto_clean = auto_clean,
          base_path = base_path
        )
      }

      message("All raw data files have been processed.")
    }
  }
}