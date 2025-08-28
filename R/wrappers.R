#' Setup wrapper
#'
#' Wrapper function that initializes a fairenough data package project
#' with clear step messaging.
#'
#' @param raw_dir Name of the raw data directory (default: "data_raw")
#' @param gitignore Whether to add data_raw to .gitignore (default: TRUE)
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @return Invisibly returns a list with setup results
#' @export
setup <- function(
  raw_dir = "data_raw",
  gitignore = TRUE,
  base_path = NULL,
  verbose = TRUE,
  overwrite = FALSE
) {
  base_path <- get_base_path(base_path)

  # Check if setup is already completed
  if (!overwrite) {
    validation <- validate_setup_completed(base_path)
    
    if (validation$all_required_passed) {
      if (verbose) {
        show_checklist(validation, "Setup already completed", verbose)
        cli::cli_alert_info("Use {.code overwrite = TRUE} to force setup")
      }
      return(invisible(list(base_path = base_path, skipped = TRUE, validation = validation)))
    }
  }

  if (verbose) {
    cli::cli_h1("Setting up fairenough project")
  }

  # Initialize project structure
  if (verbose) {
    cli::cli_h2("Step 1: Initializing project structure")
  }

  result <- setup_package(
    raw_dir = raw_dir,
    gitignore = gitignore,
    base_path = base_path,
    verbose = verbose,
    overwrite = overwrite
  )

  # Show final validation
  if (verbose) {
    final_validation <- validate_setup_completed(base_path)
    show_checklist(final_validation, "Setup completed", verbose)
  }

  if (verbose) {
    cli::cli_alert_success("Project setup completed!")
  }

  invisible(result)
}

#' Process data wrapper
#'
#' Wrapper function that processes all data files in the raw data directory
#' with clear step messaging.
#'
#' @param raw_dir Directory containing raw data files (default: "data_raw")
#' @param auto_clean Whether to automatically clean data (default: TRUE)
#' @param overwrite Whether to overwrite existing files (default: TRUE)
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @return Invisibly returns a list of processed files
#' @export
process <- function(
  raw_dir = NULL,
  auto_clean = TRUE,
  overwrite = TRUE,
  base_path = NULL,
  verbose = TRUE
) {
  base_path <- get_base_path(base_path)

  # Check if processing is already completed
  if (!overwrite) {
    validation <- validate_processing_completed(base_path)
    
    if (validation$all_required_passed) {
      if (verbose) {
        show_checklist(validation, "Data processing already completed", verbose)
        cli::cli_alert_info("Use {.code overwrite = TRUE} to force processing")
      }
      return(invisible(list(skipped = TRUE, validation = validation)))
    }
  }

  if (verbose) {
    cli::cli_h1("Processing all data files")
  }

  # Process all data files
  if (verbose) {
    cli::cli_h2("Step 1: Processing all data files")
  }

  result <- process_data(
    raw_dir = raw_dir,
    auto_clean = auto_clean,
    overwrite = overwrite,
    base_path = base_path,
    verbose = verbose
  )

  # Show final validation
  if (verbose) {
    final_validation <- validate_processing_completed(base_path)
    show_checklist(final_validation, "Data processing completed", verbose)
  }

  if (verbose) {
    cli::cli_alert_success("Data processing completed!")
  }

  invisible(result)
}

#' Collect metadata wrapper
#'
#' Wrapper function that collects comprehensive metadata for R data packages
#' with clear step messaging.
#'
#' @param extended Whether to prompt for extended metadata fields (default: FALSE)
#' @param interactive Whether to use interactive prompts (default: TRUE)
#' @param save_to_desc Whether to save to DESCRIPTION file (default: TRUE)
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @param overwrite Whether to overwrite existing metadata (default: FALSE)
#' @param ... Additional arguments passed to collect_metadata
#' @return List containing all metadata organized by category
#' @export
collect <- function(
  extended = FALSE,
  interactive = TRUE,
  save_to_desc = TRUE,
  base_path = NULL,
  verbose = TRUE,
  overwrite = FALSE,
  ...
) {
  base_path <- get_base_path(base_path)

  # Check if metadata collection is already completed
  if (!overwrite) {
    validation <- validate_metadata_collected(base_path)
    
    if (validation$all_required_passed) {
      if (verbose) {
        show_checklist(validation, "Metadata collection already completed", verbose)
        cli::cli_alert_info("Use {.code overwrite = TRUE} to force collection")
      }
      # Return existing metadata
      existing_meta <- get_metadata(base_path)
      return(invisible(existing_meta))
    }
  }

  if (verbose) {
    cli::cli_h1("Collecting package metadata")
  }

  # Collect comprehensive metadata
  if (verbose) {
    cli::cli_h2("Step 1: Collecting comprehensive metadata")
  }

  result <- collect_metadata(
    extended = extended,
    interactive = interactive,
    save_to_desc = save_to_desc,
    base_path = base_path,
    overwrite = overwrite,
    ...
  )

  # Show final validation
  if (verbose) {
    final_validation <- validate_metadata_collected(base_path)
    show_checklist(final_validation, "Metadata collection completed", verbose)
  }

  if (verbose) {
    cli::cli_alert_success("Metadata collection completed!")
  }

  invisible(result)
}

#' Generation of documentation for datasets
#'
#' Wrapper function that generates a data dictionary for all data files
#' in the package with clear step messaging.
#'
#' @param chat Optional chat object for LLM-based generation
#' @param context Optional context for LLM generation
#' @param overwrite Whether to overwrite existing dictionary (default: FALSE)
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @param ... Additional arguments passed to gendict
#' @return Data frame containing the dictionary
#' @export
generate <- function(
  chat = NULL,
  context = NULL,
  overwrite = FALSE,
  base_path = NULL,
  verbose = TRUE,
  ...
) {
  base_path <- get_base_path(base_path)

  # Check if dictionary generation is already completed (descriptions exist)
  if (!overwrite) {
    validation <- validate_dictionary_completed(base_path)
    
    if (validation$all_required_passed) {
      if (verbose) {
        show_checklist(validation, "Dictionary generation already completed", verbose)
        cli::cli_alert_info("Use {.code overwrite = TRUE} to force generation")
      }
      # Return existing dictionary if it exists
      dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
      if (file.exists(dict_path)) {
        existing_dict <- utils::read.csv(dict_path)
        return(invisible(existing_dict))
      }
      return(invisible(NULL))
    }
  }

  if (verbose) {
    cli::cli_h1("Generating data dictionary")
  }

  # Generate data dictionary
  if (verbose) {
    cli::cli_h2("Step 1: Generating data dictionary")
  }

  result <- generate_dictionary(
    chat = chat,
    context = context,
    overwrite = overwrite,
    base_path = base_path,
    verbose = verbose,
    ...
  )

  # Show final validation
  if (verbose) {
    final_validation <- validate_dictionary_completed(base_path)
    show_checklist(final_validation, "Dictionary generation completed", verbose)
  }

  if (verbose) {
    cli::cli_alert_success("Data dictionary generation completed!")
  }

  invisible(result)
}

#' Build package components wrapper
#'
#' Wrapper function that runs all build functions in sequence.
#' Generates roxygen docs, citation files, builds the package,
#' creates README, and builds the website.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @param overwrite Whether to overwrite existing files (default: TRUE)
#' @param validate Whether to validate the CITATION file (default: TRUE)
#' @param preview Whether to preview the site after building (default: TRUE)
#' @return List with results from each build step
#' @export
build <- function(
  base_path = NULL,
  verbose = TRUE,
  overwrite = TRUE,
  good_practice = FALSE,
  validate = TRUE,
  preview = TRUE
) {
  base_path <- get_base_path(base_path)

  # Check if build is already completed
  if (!overwrite) {
    validation <- validate_build_completed(base_path)
    
    if (validation$all_required_passed) {
      if (verbose) {
        show_checklist(validation, "Build already completed", verbose)
        cli::cli_alert_info("Use {.code overwrite = TRUE} to force build")
      }
      return(invisible(list(skipped = TRUE, validation = validation)))
    }
  }

  if (verbose) {
    cli::cli_h1("Building all package components")
  }

  results <- list()

  # 1. Build roxygen documentation
  if (verbose) {
    cli::cli_h2("Step 1: Building roxygen documentation")
  }
  results$roxygen <- build_roxygen(
    type = "dataset",
    base_path = base_path,
    verbose = verbose
  )

  # 2. Build citation
  if (verbose) {
    cli::cli_h2("Step 2: Building citation")
  }
  results$citation <- build_citation(
    base_path = base_path,
    verbose = verbose,
    validate = validate,
    overwrite = overwrite
  )

  # 3. Build package
  if (verbose) {
    cli::cli_h2("Step 3: Building package")
  }
  results$package <- build_package(
    base_path = base_path,
    verbose = verbose,
    overwrite = overwrite
  )

  # 4. Build README
  if (verbose) {
    cli::cli_h2("Step 4: Building README")
  }
  results$readme <- build_readme(
    base_path = base_path,
    verbose = verbose,
    overwrite = overwrite
  )

  # 5. Build site
  if (verbose) {
    cli::cli_h2("Step 5: Building website")
  }
  results$site <- build_site(
    base_path = base_path,
    verbose = verbose,
    preview = preview,
    install = TRUE
  )

  # Show final validation
  if (verbose) {
    final_validation <- validate_build_completed(base_path)
    show_checklist(final_validation, "Build completed", verbose)
  }

  if (verbose) {
    cli::cli_alert_success("All build steps completed!")
  }

  invisible(results)
}

#' Main fairenough wrapper
#'
#' Complete automated R data package creation pipeline. Runs all steps
#' from setup to final build in sequence. For more granular control,
#' use individual wrapper functions: setup(), process(), collect(),
#' generate(), and build().
#'
#' @param chat Chat object for LLM-based generation (optional)
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @param base_path Base path for the project
#' @param ... Additional arguments passed to collect() and generate() function
#' @return List containing results from all pipeline steps
#' @export
fairenough <- function(
  chat = NULL,
  verbose = TRUE,
  overwrite = FALSE,
  base_path = NULL,
  ...
) {
  # Warning about granular control
  if (verbose) {
    message(
      "For more granular control, use individual functions: setup(), process(), collect(), generate(), build()"
    )
  }

  results <- list()

  # Run complete pipeline with smart overwrite behavior
  # Each step checks its own completion state and skips if already done (unless overwrite=TRUE)
  
  results$setup <- setup(
    verbose = verbose,
    overwrite = overwrite,
    base_path = base_path
  )

  results$process <- process(
    verbose = verbose,
    overwrite = overwrite,
    base_path = base_path
  )

  results$collect <- collect(
    verbose = verbose,
    overwrite = overwrite,
    base_path = base_path,
    ...
  )

  results$generate <- generate(
    chat = chat,
    verbose = verbose,
    overwrite = overwrite,
    base_path = base_path,
    ...
  )

  results$build <- build(
    verbose = verbose,
    overwrite = overwrite,
    base_path = base_path
  )

  invisible(results)
}
