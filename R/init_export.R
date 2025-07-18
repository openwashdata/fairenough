init_export <- function(file, overwrite_rda = TRUE, auto_clean = TRUE) {
  if (!fs::file_exists(file)) {
    message(paste("Error: File not found at", file))
    return(invisible(NULL))
  }

  file_name <- tools::file_path_sans_ext(basename(file))
  file_extension <- tools::file_ext(file)
  message(paste("Reading and processing:", file_name, " (.", file_extension, ")"))

  raw_data <- NULL

  # Read data based on file extension
  if (tolower(file_extension) == "csv") {
    raw_data <- readr::read_csv(file, show_col_types = FALSE)
  } else if (tolower(file_extension) == "xlsx") {
    # Ensure readxl is installed and loaded or use readxl::read_excel
    if (!requireNamespace("readxl", quietly = TRUE)) {
      message("Error: 'readxl' package is required for .xlsx files but not installed.")
      message("Please install it with: install.packages('readxl')")
      return(invisible(NULL))
    }
    raw_data <- readxl::read_excel(file)
  } else {
    message(paste("Error: Unsupported file type for processing:", file_extension))
    message("Only .csv and .xlsx files are currently supported.")
    return(invisible(NULL))
  }

  if (is.null(raw_data)) {
    message(paste("Error: Could not read data from", file))
    return(invisible(NULL))
  }

  # Apply cleaning and transformation steps conditionally
  if (auto_clean) {
    cleaned_data <- raw_data |>
      janitor::clean_names()

    final_data <- cleaned_data |>
      dplyr::mutate(across(
        where(is.character),
        ~ ifelse(trimws(.) %in% c("null", "NA", ""), NA, .)
      ))

    final_data <- final_data |>
      dplyr::mutate(across(where(is.numeric), ~ {
        if (all(is.na(.) | . == as.integer(.), na.rm = TRUE)) {
          as.integer(.)
        } else {
          .
        }
      }))
  } else {
    final_data <- raw_data # If auto_clean is FALSE, use the raw data
    message("Skipping automatic data cleaning and type conversion.")
  }


  # Export Data --------------------------------------------------------------

  # Create the desired object name for .rda (same as file_name)
  data_object_name <- file_name
  assign(data_object_name, final_data, envir = .GlobalEnv)

  # Ensure the 'data' directory exists for saving R data
  data_dir <- file.path(getwd(), "data")
  if (!fs::dir_exists(data_dir)) {
    fs::dir_create(data_dir)
    message(paste("Created directory:", data_dir))
  }

  # Save the data object using base R's save() for reliability
  rda_path <- file.path(data_dir, paste0(data_object_name, ".rda"))

  if (!overwrite_rda && fs::file_exists(rda_path)) {
    message(paste(
      "Skipping R data object export (file already exists and overwrite_rda = FALSE):",
      paste0(data_object_name, ".rda")
    ))
  } else {
    save(list = data_object_name, file = rda_path, envir = .GlobalEnv)
    message(paste("Exported R data object:", paste0(data_object_name, ".rda")))
  }

  # Export as CSV to inst/extdata
  extdata_dir <- file.path(getwd(), "inst", "extdata")
  fs::dir_create(extdata_dir, recurse = TRUE)
  csv_export_path <- file.path(extdata_dir, paste0(file_name, ".csv"))
  readr::write_csv(final_data, csv_export_path)
  message(paste("Exported CSV:", csv_export_path))

  message(paste("Processing of", file_name, "complete."))
}
