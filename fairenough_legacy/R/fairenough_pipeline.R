#' Complete automated data package creation pipeline
#'
#' This is the main function that orchestrates the entire fairenough workflow
#' from raw data to published package. It combines data processing, documentation
#' generation, package building, and publishing in a single automated pipeline.
#'
#' @param raw_data_path Character string. Path to directory containing raw CSV files.
#' @param package_name Character string. Name for the data package.
#' @param title Character string. Package title.
#' @param description Character string. Package description.
#' @param author_name Character string. Primary author name.
#' @param author_email Character string. Primary author email.
#' @param author_orcid Character string. Primary author ORCID (optional).
#' @param authors List of lists. Additional authors with given, family, role, email, ORCID, affiliation fields (optional).
#' @param organization Character string. Organization name.
#' @param api_key Character string. API key for AI documentation generation (optional).
#' @param skip_build Logical. Skip package building stage. Default FALSE.
#' @param skip_publish Logical. Skip publishing stage. Default FALSE.
#' @param verbose Logical. Show detailed progress messages. Default TRUE.
#'
#' @return List with results from each pipeline stage.
#'
#'
#' @examples
#' \dontrun{
#' result <- fairenough_pipeline(
#'   raw_data_path = "data-raw/",
#'   package_name = "mydata",
#'   title = "My Dataset Package",
#'   description = "A comprehensive dataset for analysis",
#'   author_name = "John Doe",
#'   author_email = "john@example.com",
#'   organization = "My Organization"
#' )
#' }
fairenough_pipeline <- function(
  raw_data_path,
  package_name,
  title,
  description,
  author_name,
  author_email,
  author_orcid = NULL,
  authors = NULL,
  organization,
  api_key = NULL,
  skip_build = FALSE,
  skip_publish = FALSE,
  verbose = TRUE
) {
  
  if (verbose) cli::cli_h1("fairenough Pipeline")
  
  # Validate inputs
  if (!dir.exists(raw_data_path)) {
    cli::cli_abort("Raw data path does not exist: {raw_data_path}")
  }
  
  # Initialize results list
  results <- list()
  start_time <- Sys.time()
  
  # Stage 1: Process Data
  if (verbose) cli::cli_h2("Stage 1: Processing Data")
  results$process <- process_data(
    raw_data_path = raw_data_path,
    verbose = verbose
  )
  
  # Stage 2: Generate Documentation
  if (verbose) cli::cli_h2("Stage 2: Generating Documentation")
  results$documentation <- generate_documentation(
    package_name = package_name,
    title = title,
    description = description,
    author_name = author_name,
    author_email = author_email,
    author_orcid = author_orcid,
    organization = organization,
    api_key = api_key,
    verbose = verbose
  )
  
  # Stage 3: Build Package
  if (!skip_build) {
    if (verbose) cli::cli_h2("Stage 3: Building Package")
    results$build <- build_package(verbose = verbose)
  }
  
  # Stage 4: Publish Package
  if (!skip_publish) {
    if (verbose) cli::cli_h2("Stage 4: Publishing Package")
    results$publish <- publish_package(
      package_name = package_name,
      author_name = author_name,
      author_email = author_email,
      author_orcid = author_orcid,
      verbose = verbose
    )
  }
  
  # Summary
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  if (verbose) {
    cli::cli_h2("Pipeline Complete!")
    cli::cli_alert_success("Total time: {round(duration, 2)} minutes")
    cli::cli_alert_info("Package {package_name} is ready!")
  }
  
  results$summary <- list(
    package_name = package_name,
    duration_minutes = duration,
    stages_completed = length(results),
    timestamp = end_time
  )
  
  invisible(results)
}