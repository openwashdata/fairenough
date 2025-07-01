#' Generate comprehensive package documentation
#' Stage 2 of the fairenough pipeline. Creates data dictionaries, README files,
#' and package website using AI-powered documentation generation where available.
#' @param package_name Character string. Name of the package.
#' @param title Character string. Package title.
#' @param description Character string. Package description.
#' @param author_name Character string. Primary author name.
#' @param author_email Character string. Primary author email.
#' @param author_orcid Character string. Primary author ORCID (optional).
#' @param organization Character string. Organization name.
#' @param api_key Character string. API key for AI documentation generation (optional).
#' @param verbose Logical. Show progress messages. Default TRUE.
#' @return List containing documentation generation results.
#' @examples
#' \dontrun{
#' result <- generate_documentation(
#'   package_name = "mydata",
#'   title = "My Dataset",
#'   description = "A great dataset",
#'   author_name = "John Doe",
#'   author_email = "john@example.com",
#'   organization = "My Org"
#' )
#' }
generate_documentation <- function(
  package_name,
  title,
  description,
  author_name,
  author_email,
  author_orcid = NULL,
  organization,
  api_key = NULL,
  verbose = TRUE
) {
  
  if (verbose) cli::cli_alert_info("Starting documentation generation...")
  
  results <- list()
  
  # Setup roxygen documentation first (as per pipeline.qmd)
  if (verbose) cli::cli_alert("Setting up roxygen documentation...")
  tryCatch({
    setup_roxygen(title = title, description = description)
    results$roxygen_setup <- TRUE
    if (verbose) cli::cli_alert_success("Roxygen documentation setup complete")
  }, error = function(e) {
    results$roxygen_setup <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to setup roxygen: {e$message}")
  })
  
  # Clear existing authors and add new ones (as per pipeline.qmd)
  if (verbose) cli::cli_alert("Setting up package authors...")
  tryCatch({
    # Clear existing authors
    desc::desc_del_author()
    
    # Add primary author
    usethis::use_author(
      given = strsplit(author_name, " ")[[1]][1],
      family = strsplit(author_name, " ")[[1]][2],
      role = c("aut", "cre"),
      email = author_email,
      comment = if (!is.null(author_orcid)) c(ORCID = author_orcid) else NULL
    )
    
    results$authors_setup <- TRUE
    if (verbose) cli::cli_alert_success("Authors setup complete")
  }, error = function(e) {
    results$authors_setup <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to setup authors: {e$message}")
  })

  # Update DESCRIPTION file (after author setup)
  if (verbose) cli::cli_alert("Updating DESCRIPTION file...")
  tryCatch({
    update_description(
      organisation = organization,
      title = title,
      description = description
    )
    results$description_updated <- TRUE
    if (verbose) cli::cli_alert_success("DESCRIPTION file updated")
  }, error = function(e) {
    results$description_updated <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to update DESCRIPTION: {e$message}")
  })
  
  # Setup and generate data dictionaries
  if (verbose) cli::cli_alert("Setting up data dictionaries...")
  tryCatch({
    setup_dictionary()
    results$dictionaries_setup <- TRUE
    if (verbose) cli::cli_alert_success("Data dictionaries setup complete")
    
    # Try AI-powered dictionary generation if API key provided
    if (!is.null(api_key)) {
      if (verbose) cli::cli_alert("Generating AI-powered dictionaries...")
      tryCatch({
        # This will call gendict() once implemented
        # For now, we'll note that it's available but not implemented
        results$ai_dictionaries <- "pending_implementation"
        if (verbose) cli::cli_alert_info("AI dictionary generation: implementation pending")
      }, error = function(e) {
        results$ai_dictionaries <- FALSE
        if (verbose) cli::cli_alert_warning("AI dictionary generation failed: {e$message}")
      })
    } else {
      results$ai_dictionaries <- "no_api_key"
      if (verbose) cli::cli_alert_info("AI dictionary generation skipped (no API key)")
    }
    
  }, error = function(e) {
    results$dictionaries_setup <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to setup dictionaries: {e$message}")
  })
  
  # Generate README
  if (verbose) cli::cli_alert("Generating README...")
  tryCatch({
    setup_readme(package = package_name)
    results$readme_generated <- TRUE
    if (verbose) cli::cli_alert_success("README generated")
  }, error = function(e) {
    results$readme_generated <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to generate README: {e$message}")
  })
  
  # Setup website
  if (verbose) cli::cli_alert("Setting up package website...")
  tryCatch({
    setup_website()
    results$website_setup <- TRUE
    if (verbose) cli::cli_alert_success("Package website setup complete")
  }, error = function(e) {
    results$website_setup <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to setup website: {e$message}")
  })
  
  # Add metadata (following pipeline.qmd order)
  if (verbose) cli::cli_alert("Adding package metadata...")
  tryCatch({
    add_metadata()
    update_metadata()
    results$metadata_added <- TRUE
    if (verbose) cli::cli_alert_success("Package metadata added")
  }, error = function(e) {
    results$metadata_added <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to add metadata: {e$message}")
  })
  
  # Add creator information (as per pipeline.qmd)
  if (verbose) cli::cli_alert("Adding creator information...")
  tryCatch({
    add_creator(
      name = author_name,
      email = author_email,
      affiliation = organization
    )
    results$creator_added <- TRUE
    if (verbose) cli::cli_alert_success("Creator information added")
  }, error = function(e) {
    results$creator_added <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to add creator: {e$message}")
  })
  
  # Generate JSON-LD (as per pipeline.qmd)
  if (verbose) cli::cli_alert("Generating JSON-LD metadata...")
  tryCatch({
    generate_jsonld()
    results$jsonld_generated <- TRUE
    if (verbose) cli::cli_alert_success("JSON-LD metadata generated")
  }, error = function(e) {
    results$jsonld_generated <- FALSE
    if (verbose) cli::cli_alert_warning("Failed to generate JSON-LD: {e$message}")
  })
  
  # Summary
  successful_steps <- sum(unlist(results[sapply(results, is.logical)]))
  total_steps <- length(results[sapply(results, is.logical)])
  
  if (verbose) {
    cli::cli_alert_success("Documentation generation complete!")
    cli::cli_alert_info("Successful steps: {successful_steps}/{total_steps}")
  }
  
  results$summary <- list(
    successful_steps = successful_steps,
    total_steps = total_steps,
    success_rate = successful_steps / total_steps
  )
  
  return(results)
}