#' Process CSV files for data package creation
#' Reads a CSV file, cleans column names, handles NA values, converts numeric
#' columns to integers, and exports the data in both .rda and .csv formats.
#' @param file_path Character string. Path to the CSV file to process.
#' @param overwrite_rda Logical. Whether to overwrite existing .rda file. Default is TRUE.
#' @param overwrite_csv Logical. Whether to overwrite existing .csv file. Default is TRUE.
#' @return The processed data frame (invisibly).
#' @examples
#' \dontrun{
#' process_csv("data-raw/mydata.csv")
#' }
process_csv <- function(file_path, overwrite_rda = TRUE, overwrite_csv = TRUE) {
  if (!fs::file_exists(file_path)) {
    message(paste("Error: File not found at", file_path))
    return(invisible(NULL)) # Return NULL invisibly if file doesn't exist
  }

  file_name <- tools::file_path_sans_ext(basename(file_path))
  message(paste("Reading and processing:", file_name))

  # Read data
  raw_data <- readr::read_csv(file_path, show_col_types = FALSE)

  # Apply janitor::clean_names()
  cleaned_data <- raw_data |>
    janitor::clean_names()

  # Replace "null" / "NA" / "" strings with actual NA values
  final_data <- cleaned_data |>
    dplyr::mutate(across(
      where(is.character),
      ~ ifelse(trimws(.) %in% c("null", "NA", ""), NA, .)
    ))

  # Integer columns
  final_data <- cleaned_data |>
    dplyr::mutate(across(where(is.numeric), ~ as.integer(.)))

  # Export Data --------------------------------------------------------------

  # Create the desired object name for .rda (same as file_name)
  data_object_name <- file_name
  assign(data_object_name, final_data, envir = .GlobalEnv) # Ensure available globally

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
      "Skipping R data object export (file already exists and overwrite = FALSE):",
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
  readr::write_csv(final_data, csv_export_path, append = !overwrite_csv)
  message(paste("Exported CSV:", csv_export_path))

  message(paste("Processing of", file_name, "complete."))
  return(invisible(final_data)) # Invisibly return the processed data frame
}
