#' Build and validate the data package
#' Stage 3 of the fairenough pipeline. Compiles documentation, runs package
#' checks, and builds the final package ready for distribution.
#' @param run_tests Logical. Whether to run package tests. Default TRUE.
#' @param run_check Logical. Whether to run R CMD check. Default TRUE.
#' @param build_vignettes Logical. Whether to build vignettes. Default TRUE.
#' @param check_cran Logical. Whether to use CRAN-like checks. Default FALSE.
#' @param verbose Logical. Show progress messages. Default TRUE.
#' @return List containing build results and package information.
#' @examples
#' \dontrun{
#' result <- build_package()
#' }
build_package <- function(
  run_tests = TRUE,
  run_check = TRUE,
  build_vignettes = TRUE,
  check_cran = FALSE,
  verbose = TRUE
) {
  
  if (verbose) cli::cli_alert_info("Starting package build...")
  
  results <- list()
  start_time <- Sys.time()
  
  # Step 1: Document the package
  if (verbose) cli::cli_alert("Generating package documentation...")
  tryCatch({
    devtools::document()
    results$documentation_built <- TRUE
    if (verbose) cli::cli_alert_success("Package documentation generated")
  }, error = function(e) {
    results$documentation_built <- FALSE
    if (verbose) cli::cli_alert_danger("Failed to generate documentation: {e$message}")
    return(results)
  })
  
  # Step 2: Load package for testing
  if (verbose) cli::cli_alert("Loading package...")
  tryCatch({
    devtools::load_all()
    results$package_loaded <- TRUE
    if (verbose) cli::cli_alert_success("Package loaded successfully")
  }, error = function(e) {
    results$package_loaded <- FALSE
    if (verbose) cli::cli_alert_warning("Package loading issues: {e$message}")
  })
  
  # Step 3: Run tests
  if (run_tests) {
    if (verbose) cli::cli_alert("Running package tests...")
    tryCatch({
      test_results <- devtools::test()
      results$tests_passed <- all(test_results$failed == 0)
      results$test_summary <- list(
        failed = sum(test_results$failed),
        warning = sum(test_results$warning),
        skipped = sum(test_results$skipped),
        passed = sum(test_results$passed)
      )
      
      if (results$tests_passed) {
        if (verbose) cli::cli_alert_success("All tests passed!")
      } else {
        if (verbose) cli::cli_alert_warning("Some tests failed: {sum(test_results$failed)} failures")
      }
    }, error = function(e) {
      results$tests_passed <- FALSE
      results$test_error <- e$message
      if (verbose) cli::cli_alert_warning("Test execution failed: {e$message}")
    })
  } else {
    results$tests_passed <- "skipped"
    if (verbose) cli::cli_alert_info("Tests skipped")
  }
  
  # Step 4: Build README (as per pipeline.qmd)
  if (verbose) cli::cli_alert("Building README...")
  tryCatch({
    devtools::build_readme()
    results$readme_built <- TRUE
    if (verbose) cli::cli_alert_success("README built")
  }, error = function(e) {
    results$readme_built <- FALSE
    if (verbose) cli::cli_alert_warning("README build failed: {e$message}")
  })
  
  # Step 5: Build website (as per pipeline.qmd)
  if (build_vignettes && file.exists("_pkgdown.yml")) {
    if (verbose) cli::cli_alert("Building package website...")
    tryCatch({
      devtools::build_site()  # Use devtools::build_site as in pipeline.qmd
      results$website_built <- TRUE
      if (verbose) cli::cli_alert_success("Package website built")
    }, error = function(e) {
      results$website_built <- FALSE
      if (verbose) cli::cli_alert_warning("Website build failed: {e$message}")
    })
  } else {
    results$website_built <- "skipped"
    if (verbose) cli::cli_alert_info("Website build skipped")
  }
  
  # Step 5: Run R CMD check
  if (run_check) {
    if (verbose) cli::cli_alert("Running R CMD check...")
    tryCatch({
      check_results <- devtools::check(
        cran = check_cran,
        remote = TRUE,
        manual = FALSE,
        run_dont_test = FALSE
      )
      
      results$check_passed <- length(check_results$errors) == 0
      results$check_summary <- list(
        errors = length(check_results$errors),
        warnings = length(check_results$warnings),
        notes = length(check_results$notes)
      )
      
      if (results$check_passed) {
        if (verbose) cli::cli_alert_success("R CMD check passed!")
      } else {
        if (verbose) cli::cli_alert_danger("R CMD check failed with {length(check_results$errors)} error{?s}")
      }
      
      if (length(check_results$warnings) > 0) {
        if (verbose) cli::cli_alert_warning("R CMD check warnings: {length(check_results$warnings)}")
      }
      
    }, error = function(e) {
      results$check_passed <- FALSE
      results$check_error <- e$message
      if (verbose) cli::cli_alert_danger("R CMD check execution failed: {e$message}")
    })
  } else {
    results$check_passed <- "skipped"
    if (verbose) cli::cli_alert_info("R CMD check skipped")
  }
  
  # Step 6: Build package tarball
  if (verbose) cli::cli_alert("Building package tarball...")
  tryCatch({
    built_path <- devtools::build()
    results$package_built <- TRUE
    results$tarball_path <- built_path
    if (verbose) cli::cli_alert_success("Package built: {basename(built_path)}")
  }, error = function(e) {
    results$package_built <- FALSE
    results$build_error <- e$message
    if (verbose) cli::cli_alert_danger("Package build failed: {e$message}")
  })
  
  # Summary
  end_time <- Sys.time()
  build_duration <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  # Calculate overall success
  success_indicators <- c(
    results$documentation_built,
    results$package_loaded,
    ifelse(is.logical(results$tests_passed), results$tests_passed, TRUE),
    ifelse(is.logical(results$check_passed), results$check_passed, TRUE),
    results$package_built
  )
  
  overall_success <- all(success_indicators, na.rm = TRUE)
  
  if (verbose) {
    cli::cli_alert_success("Package build complete!")
    cli::cli_alert_info("Build time: {round(build_duration, 2)} minutes")
    if (overall_success) {
      cli::cli_alert_success("Package is ready for distribution!")
    } else {
      cli::cli_alert_warning("Package build completed with issues")
    }
  }
  
  results$summary <- list(
    overall_success = overall_success,
    build_duration_minutes = build_duration,
    timestamp = end_time
  )
  
  return(results)
}