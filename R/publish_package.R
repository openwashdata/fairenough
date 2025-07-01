#' Publish and archive the data package
#'
#' Stage 4 of the fairenough pipeline. Handles package publication including
#' Zenodo metadata creation, git operations, and preparation for distribution.
#'
#' @param package_name Character string. Name of the package.
#' @param author_name Character string. Primary author name.
#' @param author_email Character string. Primary author email.
#' @param author_orcid Character string. Primary author ORCID (optional).
#' @param create_zenodo Logical. Whether to create Zenodo metadata. Default TRUE.
#' @param commit_changes Logical. Whether to commit changes to git. Default TRUE.
#' @param create_release Logical. Whether to create a git tag for release. Default FALSE.
#' @param push_to_remote Logical. Whether to push to remote repository. Default FALSE.
#' @param doi Character string. DOI for citation updates (optional).
#' @param verbose Logical. Show progress messages. Default TRUE.
#'
#' @return List containing publishing results and metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- publish_package(
#'   package_name = "mydata",
#'   author_name = "John Doe",
#'   author_email = "john@example.com"
#' )
#' }
publish_package <- function(
  package_name,
  author_name,
  author_email,
  author_orcid = NULL,
  create_zenodo = TRUE,
  commit_changes = TRUE,
  create_release = FALSE,
  push_to_remote = FALSE,
  doi = NULL,
  verbose = TRUE
) {
  
  if (verbose) cli::cli_alert_info("Starting package publishing...")
  
  results <- list()
  
  # Step 1: Create Zenodo metadata
  if (create_zenodo) {
    if (verbose) cli::cli_alert("Creating Zenodo metadata...")
    tryCatch({
      # Get package information
      desc_info <- desc::desc_get_deps(".")
      pkg_desc <- desc::desc_get("Description", ".")
      pkg_title <- desc::desc_get("Title", ".")
      
      # Prepare creator information
      creators <- list(
        list(
          name = paste0(strsplit(author_name, " ")[[1]][2], ", ", 
                       strsplit(author_name, " ")[[1]][1]),
          affiliation = "Data Provider"
        )
      )
      
      if (!is.null(author_orcid)) {
        creators[[1]]$orcid <- author_orcid
      }
      
      # Create Zenodo JSON
      create_zenodo_json(
        creators = creators,
        title = paste(package_name, "-", pkg_title),
        license = "CC-BY-4.0",
        keywords = c("data", "research", package_name),
        communities = list(
          list(identifier = "open-research-data")
        )
      )
      
      results$zenodo_created <- TRUE
      if (verbose) cli::cli_alert_success("Zenodo metadata created")
      
    }, error = function(e) {
      results$zenodo_created <- FALSE
      results$zenodo_error <- e$message
      if (verbose) cli::cli_alert_warning("Failed to create Zenodo metadata: {e$message}")
    })
  } else {
    results$zenodo_created <- "skipped"
    if (verbose) cli::cli_alert_info("Zenodo metadata creation skipped")
  }
  
  # Step 2: Git operations
  if (commit_changes) {
    if (verbose) cli::cli_alert("Committing changes to git...")
    tryCatch({
      # Check if we're in a git repository
      if (!dir.exists(".git")) {
        if (verbose) cli::cli_alert_info("Initializing git repository...")
        system("git init")
      }
      
      # Add all files
      system("git add .")
      
      # Create commit message
      commit_msg <- sprintf("chore: automated package build for %s", package_name)
      
      # Commit changes
      commit_result <- system(sprintf('git commit -m "%s"', commit_msg), 
                             ignore.stdout = !verbose)
      
      results$git_committed <- (commit_result == 0)
      
      if (results$git_committed) {
        if (verbose) cli::cli_alert_success("Changes committed to git")
      } else {
        if (verbose) cli::cli_alert_info("No changes to commit or commit failed")
      }
      
    }, error = function(e) {
      results$git_committed <- FALSE
      results$git_error <- e$message
      if (verbose) cli::cli_alert_warning("Git commit failed: {e$message}")
    })
  } else {
    results$git_committed <- "skipped"
    if (verbose) cli::cli_alert_info("Git commit skipped")
  }
  
  # Step 3: Create release tag
  if (create_release && results$git_committed == TRUE) {
    if (verbose) cli::cli_alert("Creating release tag...")
    tryCatch({
      # Get package version
      pkg_version <- desc::desc_get("Version", ".")
      tag_name <- paste0("v", pkg_version)
      
      # Create git tag
      tag_result <- system(sprintf('git tag -a %s -m "Release %s"', 
                                  tag_name, pkg_version), 
                          ignore.stdout = !verbose)
      
      results$release_tagged <- (tag_result == 0)
      results$release_tag <- tag_name
      
      if (results$release_tagged) {
        if (verbose) cli::cli_alert_success("Release tag created: {tag_name}")
      } else {
        if (verbose) cli::cli_alert_warning("Failed to create release tag")
      }
      
    }, error = function(e) {
      results$release_tagged <- FALSE
      results$tag_error <- e$message
      if (verbose) cli::cli_alert_warning("Release tagging failed: {e$message}")
    })
  } else {
    results$release_tagged <- "skipped"
    if (verbose) cli::cli_alert_info("Release tagging skipped")
  }
  
  # Step 4: Push to remote
  if (push_to_remote && results$git_committed == TRUE) {
    if (verbose) cli::cli_alert("Pushing to remote repository...")
    tryCatch({
      # Push commits
      push_result <- system("git push", ignore.stdout = !verbose)
      
      # Push tags if they exist
      if (results$release_tagged == TRUE) {
        system("git push --tags", ignore.stdout = !verbose)
      }
      
      results$pushed_to_remote <- (push_result == 0)
      
      if (results$pushed_to_remote) {
        if (verbose) cli::cli_alert_success("Pushed to remote repository")
      } else {
        if (verbose) cli::cli_alert_warning("Failed to push to remote repository")
      }
      
    }, error = function(e) {
      results$pushed_to_remote <- FALSE
      results$push_error <- e$message
      if (verbose) cli::cli_alert_warning("Push to remote failed: {e$message}")
    })
  } else {
    results$pushed_to_remote <- "skipped"
    if (verbose) cli::cli_alert_info("Push to remote skipped")
  }
  
  # Step 5: Update citation with DOI (as per pipeline.qmd)
  if (!is.null(doi)) {
    if (verbose) cli::cli_alert("Updating citation with DOI...")
    tryCatch({
      update_citation(doi)
      results$citation_updated <- TRUE
      if (verbose) cli::cli_alert_success("Citation updated with DOI: {doi}")
    }, error = function(e) {
      results$citation_updated <- FALSE
      results$citation_error <- e$message
      if (verbose) cli::cli_alert_warning("Failed to update citation: {e$message}")
    })
  } else {
    results$citation_updated <- "skipped"
    if (verbose) cli::cli_alert_info("Citation update skipped (no DOI provided)")
  }
  
  # Step 6: Generate publication summary
  if (verbose) cli::cli_alert("Generating publication summary...")
  
  publication_info <- list(
    package_name = package_name,
    author = author_name,
    email = author_email,
    orcid = author_orcid,
    version = desc::desc_get("Version", "."),
    title = desc::desc_get("Title", "."),
    timestamp = Sys.time()
  )
  
  # Create a simple publication report
  report_content <- sprintf(
    "# Publication Summary for %s\n\n- **Package**: %s\n- **Version**: %s\n- **Author**: %s\n- **Email**: %s\n- **Date**: %s\n\n## Status\n- Zenodo metadata: %s\n- Git commit: %s\n- Release tag: %s\n- Remote push: %s\n",
    package_name,
    package_name,
    publication_info$version,
    author_name,
    author_email,
    format(publication_info$timestamp, "%Y-%m-%d %H:%M:%S"),
    ifelse(results$zenodo_created == TRUE, "✓", "✗"),
    ifelse(results$git_committed == TRUE, "✓", "✗"),
    ifelse(results$release_tagged == TRUE, "✓", "✗"),
    ifelse(results$pushed_to_remote == TRUE, "✓", "✗")
  )
  
  writeLines(report_content, "PUBLICATION_SUMMARY.md")
  results$publication_report <- "PUBLICATION_SUMMARY.md"
  
  if (verbose) {
    cli::cli_alert_success("Package publishing complete!")
    cli::cli_alert_info("Publication summary: PUBLICATION_SUMMARY.md")
  }
  
  results$publication_info <- publication_info
  
  return(results)
}