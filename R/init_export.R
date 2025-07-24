#' Export data file to package data
#' 
#' @param data_file Path to a CSV/Excel file
#' @param overwrite_rda Whether to overwrite existing .rda files
#' @param auto_clean Whether to auto-clean data
#' @param base_path Base path for the project
#' @export
init_export <- function(data_file, overwrite_rda = TRUE, auto_clean = TRUE, base_path = NULL) {
  # Use the utility function for consistent base_path handling
  base_path <- get_base_path(base_path)
  
  # Validate input is a file path
  if (is.data.frame(data_file)) {
    cli::cli_abort("init_export requires a file path, not a data frame. Use a CSV or Excel file path.")
  }
  
  if (!is.character(data_file) || length(data_file) != 1) {
    cli::cli_abort("data_file must be a single file path")
  }
  
  file_name <- tools::file_path_sans_ext(basename(data_file))
  message(paste("Processing:", basename(data_file)))
  
  # Read data using the utility function
  raw_data <- tryCatch({
    read_data(data_file, show_messages = TRUE)
  }, error = function(e) {
    message(paste("Error:", e$message))
    return(NULL)
  })
  
  if (is.null(raw_data)) {
    return(invisible(NULL))
  }

  # Apply cleaning and transformation steps conditionally
  if (auto_clean) {
    cleaned_data <- raw_data |>
      janitor::clean_names()

    final_data <- cleaned_data |>
      dplyr::mutate(dplyr::across(
        dplyr::where(is.character),
        ~ ifelse(trimws(.) %in% c("null", "NA", ""), NA, .)
      ))

    final_data <- final_data |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ {
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
  data_dir <- file.path(base_path, "data")
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
  extdata_dir <- file.path(base_path, "inst", "extdata")
  fs::dir_create(extdata_dir, recurse = TRUE)
  csv_export_path <- file.path(extdata_dir, paste0(file_name, ".csv"))
  readr::write_csv(final_data, csv_export_path)
  message(paste("Exported CSV:", csv_export_path))

  message(paste("Processing of", file_name, "complete."))
}
