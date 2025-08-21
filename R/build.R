#' Build package structure and core files
#'
#' Creates the essential R package structure using modern development tools.
#' Generates roxygen docs for dataset functions, LICENSE, and validates structure.
#' Note: This assumes usethis::create_package() has already been run in setup().
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages
#' @param overwrite Whether to overwrite existing files
#' @return Logical indicating success
#' @export
build_package <- function(
  base_path = NULL,
  verbose = TRUE,
  overwrite = TRUE
) {
  base_path <- get_base_path(base_path)

  # Generate automatic roxygen documentation for dataset functions in R/
  if (verbose) {
    cli::cli_alert_info("Generating dataset documentation")
  }
  doc_result <- build_roxygen(
    type = "dataset",
    base_path = base_path,
    verbose = verbose
  )

  # Create LICENSE file
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (file.exists(desc_path)) {
    license <- desc::desc_get_field("License")
    license_file <- file.path(base_path, "LICENSE")
    license_md_file <- file.path(base_path, "LICENSE.md")

    # Check if any license file exists
    if (
      overwrite || (!file.exists(license_file) || !file.exists(license_md_file))
    ) {
      if (verbose) {
        cli::cli_alert_info("Creating LICENSE file")
      }

      usethis::with_project(
        base_path,
        {
          build_license(license)
        },
        force = TRUE
      )
    }
  }

  # Package validation and documentation with devtools
  if (verbose) {
    cli::cli_alert_info("Running roxygen2 and validating package structure")
  }
  tryCatch(
    {
      devtools::document(base_path)
      if (verbose) {
        cli::cli_alert_success("Package structure validation complete")
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Package validation issues: {e$message}")
    }
  )

  if (verbose) {
    cli::cli_alert_success("Package build step complete!")
  }

  invisible(TRUE)
}

#' Validate package with modern tools
#'
#' Runs comprehensive package validation including R CMD check,
#' spell checking, and best practices analysis.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages
#' @param spell_check Whether to run spell check (default: TRUE)
#' @param good_practice Whether to run good practice checks (default: FALSE, as it's slow)
#' @return List with validation results
#' @export
validate_package <- function(
  base_path = NULL,
  verbose = TRUE,
  spell_check = TRUE,
  good_practice = FALSE
) {
  base_path <- get_base_path(base_path)

  if (verbose) {
    cli::cli_h2("Step 3: Validating Package")
  }

  results <- list()

  # 1. Run standard R CMD check
  if (verbose) {
    cli::cli_alert_info("Running R CMD check")
  }
  results$check <- run_checks(base_path = base_path, verbose = verbose)

  # 2. Spell check (if requested)
  if (spell_check) {
    if (verbose) {
      cli::cli_alert_info("Running spell check")
    }
    tryCatch(
      {
        # Setup spell check if not already configured
        wordlist_file <- file.path(base_path, "inst", "WORDLIST")
        if (!file.exists(wordlist_file)) {
          usethis::use_spell_check()
        }

        # Run spell check
        if (requireNamespace("spelling", quietly = TRUE)) {
          spell_errors <- spelling::spell_check_package(base_path)
          if (nrow(spell_errors) > 0) {
            if (verbose) {
              cli::cli_alert_warning(
                "Found {nrow(spell_errors)} potential spelling issue{?s}"
              )
              cli::cli_alert_info(
                "Run spelling::spell_check_package() for details"
              )
            }
            results$spelling <- spell_errors
          } else {
            if (verbose) {
              cli::cli_alert_success("No spelling issues found")
            }
            results$spelling <- NULL
          }
        }
      },
      error = function(e) {
        cli::cli_alert_warning("Spell check failed: {e$message}")
        results$spelling <- NA
      }
    )
  }

  # 3. Good practices check (if requested - can be slow)
  if (good_practice) {
    if (verbose) {
      cli::cli_alert_info(
        "Running good practices check (this may take a while)"
      )
    }

    if (requireNamespace("goodpractice", quietly = TRUE)) {
      tryCatch(
        {
          gp_result <- goodpractice::gp(base_path, quiet = !verbose)
          results$good_practice <- gp_result

          if (verbose) {
            # Show summary of issues
            if (length(goodpractice::failed_checks(gp_result)) > 0) {
              cli::cli_alert_warning("Some good practice checks failed")
              cli::cli_alert_info("Run goodpractice::gp() for details")
            } else {
              cli::cli_alert_success("All good practice checks passed")
            }
          }
        },
        error = function(e) {
          cli::cli_alert_warning("Good practice check failed: {e$message}")
          results$good_practice <- NA
        }
      )
    } else {
      cli::cli_alert_warning("Package {.pkg goodpractice} not installed")
      cli::cli_alert_info("Install it with: install.packages('goodpractice')")
      results$good_practice <- NA
    }
  }

  if (verbose) {
    cli::cli_alert_success("Package validation complete!")
  }

  invisible(results)
}

#' Generate roxygen documentation for datasets
#'
#' Creates R documentation files for each dataset using the data dictionary.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Logical indicating success
#' @export
build_roxygen <- function(type = "dataset", base_path = NULL, verbose = TRUE) {
  base_path <- get_base_path(base_path)

  # Check for dictionary
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
  if (!file.exists(dict_path)) {
    cli::cli_alert_warning(
      "No dictionary found at {.path inst/extdata/dictionary.csv}"
    )
    cli::cli_alert_info("Run {.fn document} first to create dictionary")
    return(invisible(FALSE))
  }

  # Check for metadata
  metadata <- get_metadata_from_desc(base_path)
  if (is.null(metadata)) {
    cli::cli_alert_warning("No metadata found")
    cli::cli_alert_info("Run {.fn collect_metadata} first to collect metadata")
    return(invisible(FALSE))
  }

  # Load dictionary
  dictionary <- fairenough::read_data(dict_path)

  # Get unique datasets
  datasets <- unique(dictionary$file_name)

  # Ensure R directory exists
  r_dir <- file.path(base_path, "R")
  if (!dir.exists(r_dir)) {
    dir.create(r_dir)
  }

  # Generate documentation for each dataset
  for (dataset_name in datasets) {
    # Remove .rda extension if present
    clean_name <- sub("\\.rda$", "", dataset_name)

    # Filter dictionary for this dataset
    dataset_dict <- dictionary[dictionary$file_name == dataset_name, ]

    # Load the actual data to get dimensions
    data_file <- file.path(base_path, "data", paste0(clean_name, ".rda"))
    if (!file.exists(data_file)) {
      if (verbose) {
        cli::cli_alert_warning("Data file not found: {.path {data_file}}")
      }
      next
    }

    # Load data to get dimensions
    temp_env <- new.env()
    load(data_file, envir = temp_env)
    data_obj <- get(ls(temp_env)[1], envir = temp_env)
    n_rows <- nrow(data_obj)
    n_cols <- ncol(data_obj)

    # Create roxygen documentation
    doc_lines <- character()

    # Title and description
    doc_lines <- c(
      doc_lines,
      paste0("#' ", clean_name, ": ", metadata$package$title),
      "#'",
      paste0("#' ", metadata$package$description),
      "#'"
    )

    # Format line
    doc_lines <- c(
      doc_lines,
      paste0(
        "#' @format A data frame with ",
        n_rows,
        " rows and ",
        n_cols,
        " variables:"
      )
    )

    # Describe block
    doc_lines <- c(doc_lines, "#' \\describe{")

    for (i in seq_len(nrow(dataset_dict))) {
      var_name <- dataset_dict$variable_name[i]
      var_desc <- dataset_dict$description[i]
      if (is.na(var_desc) || var_desc == "") {
        var_desc <- paste0(dataset_dict$variable_type[i], " variable")
      }
      doc_lines <- c(
        doc_lines,
        paste0("#'   \\item{", var_name, "}{", var_desc, "}")
      )
    }

    doc_lines <- c(doc_lines, "#' }")

    # Add the dataset name as a string at the end
    doc_lines <- c(doc_lines, paste0('"', clean_name, '"'))

    # Write to R file
    output_file <- file.path(r_dir, paste0(clean_name, ".R"))
    writeLines(doc_lines, output_file)

    if (verbose) {
      cli::cli_alert_success("Created documentation for {.file {clean_name}}")
    }
  }

  return(invisible(TRUE))
}

#' Build README from inst/templates/README.Rmd
#'
#' Build README.md from the README.Rmd template, incorporating package metadata.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @param overwrite Whether to overwrite README.Rmd found at base_path
#' @return Path to generated README
#' @export
build_readme <- function(base_path = NULL, verbose = TRUE, overwrite = TRUE) {
  base_path <- get_base_path(base_path)

  # Create README.Rmd from template if it doesn't exist
  readme_rmd <- file.path(base_path, "README.Rmd")
  template_path <- system.file(
    "templates",
    "README.Rmd",
    package = "fairenough"
  )
  fs::file_copy(template_path, readme_rmd, overwrite = overwrite)

  # Check if rmarkdown is available
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    tryCatch(
      {
        # Render README with proper working directory
        readme_md <- file.path(base_path, "README.md")
        rmarkdown::render(
          readme_rmd,
          output_format = "github_document",
          output_file = readme_md,
          quiet = !verbose
        )
        if (verbose) cli::cli_alert_success("Generated README.md")
      },
      error = function(e) {
        cli::cli_alert_warning("Could not render README: {e$message}")
      }
    )
  } else {
    cli::cli_alert_warning("Package {.pkg rmarkdown} not installed")
    cli::cli_alert_info("Install it with: install.packages('rmarkdown')")
  }

  return(invisible(file.path(base_path, "README.md")))
}

#' Build website with pkgdown
#'
#' Configures pkgdown for building the package's website.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages (default: TRUE)
#' @param preview Whether to preview the site (default: TRUE)
#' @param install Whether to install the package (default: TRUE)
#' @return Logical indicating success
#' @export
build_site <- function(
  base_path = NULL,
  verbose = TRUE,
  preview = TRUE,
  install = TRUE
) {
  base_path <- get_base_path(base_path)

  # Check if pkgdown is available
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    cli::cli_alert_warning("Package {.pkg pkgdown} not installed")
    cli::cli_alert_info("Install it with: install.packages('pkgdown')")
    return(invisible(FALSE))
  }

  # Create _pkgdown.yml from template if it doesn't exist
  pkgdown_yml <- file.path(base_path, "_pkgdown.yml")

  if (!file.exists(pkgdown_yml)) {
    # Load metadata for template data
    metadata <- get_metadata_from_desc(base_path)

    if (!is.null(metadata)) {
      # Prepare authors data for Mustache iteration
      # Add a flag for comma placement between authors
      if (!is.null(metadata$authors) && length(metadata$authors) > 0) {
        # Ensure authors is a proper list structure
        authors_list <- list()
        for (i in seq_along(metadata$authors)) {
          author <- metadata$authors[[i]]
          if (is.list(author)) {
            # Extract fields safely
            given <- if (!is.null(author$given)) {
              author$given
            } else if (!is.null(author[["given"]])) {
              author[["given"]]
            } else {
              ""
            }
            family <- if (!is.null(author$family)) {
              author$family
            } else if (!is.null(author[["family"]])) {
              author[["family"]]
            } else {
              ""
            }

            # Only add if we have valid names
            if (given != "" && family != "") {
              authors_list[[length(authors_list) + 1]] <- list(
                given = given,
                family = family,
                not_last = i < length(metadata$authors)
              )
            }
          }
        }
        metadata$authors <- authors_list
      }
    } else {
      metadata <- list()
    }

    # Use our own template function that respects base_path
    use_template(
      template = "_pkgdown.yml",
      save_as = "_pkgdown.yml",
      data = metadata,
      base_path = base_path,
      package = "fairenough",
      verbose = verbose
    )
  }

  # Build site
  if (verbose) {
    cli::cli_alert_info("Building package website...")
  }

  tryCatch(
    {
      # pkgdown::build_site() needs to know the package path
      if (verbose) {
        pkgdown::build_site(
          pkg = base_path,
          preview = preview,
          install = install
        )
      } else {
        suppressMessages(pkgdown::build_site(
          pkg = base_path,
          preview = preview,
          install = install
        ))
      }
      if (verbose) cli::cli_alert_success("Website built in {.path docs/}")
    },
    error = function(e) {
      cli::cli_alert_warning("Could not build website: {e$message}")
      return(invisible(FALSE))
    }
  )

  return(invisible(TRUE))
}

#' Run package checks
#'
#' Runs R CMD check on the package.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Logical indicating if checks passed
#' @export
run_checks <- function(base_path = NULL, verbose = TRUE) {
  base_path <- get_base_path(base_path)

  if (!requireNamespace("devtools", quietly = TRUE)) {
    cli::cli_alert_warning("Package {.pkg devtools} not installed")
    cli::cli_alert_info("Install it with: install.packages('devtools')")
    return(invisible(NA))
  }

  if (verbose) {
    cli::cli_alert_info("Running R CMD check...")
  }

  check_result <- tryCatch(
    {
      devtools::check(
        pkg = base_path,
        quiet = !verbose,
        args = "--no-manual"
      )
    },
    error = function(e) {
      cli::cli_alert_warning("Check failed: {e$message}")
      return(NULL)
    }
  )

  if (!is.null(check_result)) {
    errors <- length(check_result$errors)
    warnings <- length(check_result$warnings)
    notes <- length(check_result$notes)

    if (errors == 0 && warnings == 0) {
      if (verbose) {
        cli::cli_alert_success("Package checks passed!")
      }
      return(invisible(TRUE))
    } else {
      if (verbose) {
        cli::cli_alert_warning(
          "Check complete: {errors} error{?s}, {warnings} warning{?s}, {notes} note{?s}"
        )
      }
      return(invisible(FALSE))
    }
  }

  return(invisible(NA))
}


#' Build CITATION file using cffr
#'
#' Builds a CITATION file from DESCRIPTION using the cffr package.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @param validate Whether to validate the CITATION file
#' @param overwrite Whether to overwrite the CITATION file
#' @export
build_citation <- function(
  base_path = NULL,
  verbose = TRUE,
  validate = TRUE,
  overwrite = TRUE
) {
  base_path <- get_base_path(base_path)

  # Check if cffr is available
  if (!requireNamespace("cffr", quietly = TRUE)) {
    cli::cli_alert_warning("Package {.pkg cffr} not installed")
    cli::cli_alert_info("Install it with: install.packages('cffr')")
    return(invisible(FALSE))
  }

  tryCatch(
    {
      desc_path <- file.path(base_path, "DESCRIPTION")
      if (file.exists(desc_path)) {
        # Use cffr to create citation from DESCRIPTION
        citation_cff_file <- file.path(base_path, "CITATION.cff")
        citation_file <- file.path(base_path, "inst", "CITATION")

        if (
          overwrite ||
            (!file.exists(citation_cff_file) || !file.exists(citation_cff_file))
        ) {
          # Create CITATION.cff in base_path
          cff <- cffr::cff_write(
            x = desc_path,
            outfile = file.path(base_path, "CITATION.cff"),
            validate = validate,
            verbose = verbose,
          )
          cffr::cff_write_citation(
            cff,
            file = file.path(base_path, "inst", "CITATION"),
          )
        } else {
          cli::cli_alert_info(
            "Use {.code overwrite = TRUE} to overwrite existing CITATION."
          )
        }
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Could not create citation: {e$message}")
      return(invisible(FALSE))
    }
  )
}
