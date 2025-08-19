#' Build package structure and core files
#' 
#' Creates the essential R package structure using modern development tools.
#' Generates DESCRIPTION, NAMESPACE, roxygen docs, LICENSE, and validates structure.
#' 
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages
#' @return Logical indicating success
#' @export
build_package <- function(base_path = NULL, verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  # Store current project before changing
  old_proj <- tryCatch(usethis::proj_get(), error = function(e) NULL)
  
  # Ensure .Rproj file exists and set as active project
  ensure_rproj(base_path, verbose)
  
  # Restore previous project on exit
  on.exit({
    if (!is.null(old_proj)) usethis::proj_set(old_proj)
  }, add = TRUE)
  
  if (verbose) cli::cli_h2("Step 1: Building Package Structure")
  
  # 1. Apply metadata to DESCRIPTION
  if (verbose) cli::cli_alert_info("Applying metadata to DESCRIPTION")
  apply_result <- apply_metadata(base_path = base_path, verbose = verbose)
  
  # 2. Generate data documentation files in R/
  if (verbose) cli::cli_alert_info("Generating dataset documentation")  
  doc_result <- generate_data_documentation(base_path = base_path, verbose = verbose)
  
  # 3. Generate NAMESPACE and .Rd files using roxygen2
  if (verbose) cli::cli_alert_info("Running roxygen2 to generate NAMESPACE and man pages")
  tryCatch({
    roxygen2::roxygenise(base_path)
    if (verbose) cli::cli_alert_success("Generated NAMESPACE and .Rd files")
  }, error = function(e) {
    cli::cli_alert_warning("Roxygen2 failed: {e$message}")
    return(FALSE)
  })
  
  # 4. Create LICENSE file if needed
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (file.exists(desc_path)) {
    desc_obj <- desc::desc(file = desc_path)
    license <- desc_obj$get_field("License")
    license_file <- file.path(base_path, "LICENSE")
    license_md_file <- file.path(base_path, "LICENSE.md")
    
    # Check if any license file exists
    if (!file.exists(license_file) && !file.exists(license_md_file)) {
      if (verbose) cli::cli_alert_info("Creating LICENSE file for {license}")
      
      tryCatch({
        # Match license type and use appropriate usethis function
        license_created <- FALSE
        
        if (grepl("CC-BY-4\\.0|CC BY 4\\.0", license, ignore.case = TRUE)) {
          usethis::use_ccby_license()
          license_created <- TRUE
        } else if (grepl("CC0|CC-0", license, ignore.case = TRUE)) {
          usethis::use_cc0_license()
          license_created <- TRUE
        } else if (grepl("MIT", license, ignore.case = TRUE)) {
          usethis::use_mit_license()
          license_created <- TRUE
        } else if (grepl("GPL-3|GPL \\(>= 3\\)", license, ignore.case = TRUE)) {
          usethis::use_gpl_license(version = 3)
          license_created <- TRUE
        } else if (grepl("GPL-2|GPL \\(>= 2\\)", license, ignore.case = TRUE)) {
          usethis::use_gpl_license(version = 2)
          license_created <- TRUE
        } else if (grepl("Apache-2\\.0|Apache 2\\.0", license, ignore.case = TRUE)) {
          usethis::use_apache_license(version = "2.0")
          license_created <- TRUE
        } else if (grepl("AGPL-3|AGPL \\(>= 3\\)", license, ignore.case = TRUE)) {
          usethis::use_agpl_license(version = 3)
          license_created <- TRUE
        } else if (grepl("LGPL-3|LGPL \\(>= 3\\)", license, ignore.case = TRUE)) {
          usethis::use_lgpl_license(version = 3)
          license_created <- TRUE
        } else if (grepl("LGPL-2\\.1|LGPL \\(>= 2\\.1\\)", license, ignore.case = TRUE)) {
          usethis::use_lgpl_license(version = "2.1")
          license_created <- TRUE
        } else if (grepl("Proprietary", license, ignore.case = TRUE)) {
          usethis::use_proprietary_license()
          license_created <- TRUE
        }
        
        if (license_created) {
          if (verbose) cli::cli_alert_success("Created LICENSE file")
        } else {
          if (verbose) cli::cli_alert_warning("Unknown license type: {license}")
          if (verbose) cli::cli_alert_info("Consider using one of: CC-BY-4.0, CC0-1.0, MIT, GPL-3, GPL-2, Apache-2.0, AGPL-3, LGPL-3, LGPL-2.1")
        }
      }, error = function(e) {
        cli::cli_alert_warning("Could not create LICENSE: {e$message}")
      })
    }
  }
  
  # 5. Ensure standard .gitignore exists
  gitignore_file <- file.path(base_path, ".gitignore")
  if (!file.exists(gitignore_file)) {
    if (verbose) cli::cli_alert_info("Creating .gitignore")
    tryCatch({

      usethis::use_git_ignore(c("*.DS_Store", ".Rhistory", ".RData", ".Ruserdata"))
      if (verbose) cli::cli_alert_success("Created .gitignore")
    }, error = function(e) {
      cli::cli_alert_warning("Could not create .gitignore: {e$message}")
    })
  }
  
  # 6. Final package validation with devtools
  if (verbose) cli::cli_alert_info("Validating package structure")
  tryCatch({
    devtools::document(base_path)
    if (verbose) cli::cli_alert_success("Package structure validation complete")
  }, error = function(e) {
    cli::cli_alert_warning("Package validation issues: {e$message}")
  })
  
  if (verbose) cli::cli_alert_success("Package build step complete!")
  
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
validate_package <- function(base_path = NULL, 
                            verbose = TRUE,
                            spell_check = TRUE,
                            good_practice = FALSE) {
  
  base_path <- get_base_path(base_path)
  
  if (verbose) cli::cli_h2("Step 3: Validating Package")
  
  results <- list()
  
  # 1. Run standard R CMD check
  if (verbose) cli::cli_alert_info("Running R CMD check")
  results$check <- run_checks(base_path = base_path, verbose = verbose)
  
  # 2. Spell check (if requested)
  if (spell_check) {
    if (verbose) cli::cli_alert_info("Running spell check")
    tryCatch({
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
            cli::cli_alert_warning("Found {nrow(spell_errors)} potential spelling issue{?s}")
            cli::cli_alert_info("Run spelling::spell_check_package() for details")
          }
          results$spelling <- spell_errors
        } else {
          if (verbose) cli::cli_alert_success("No spelling issues found")
          results$spelling <- NULL
        }
      }
    }, error = function(e) {
      cli::cli_alert_warning("Spell check failed: {e$message}")
      results$spelling <- NA
    })
  }
  
  # 3. Good practices check (if requested - can be slow)
  if (good_practice) {
    if (verbose) cli::cli_alert_info("Running good practices check (this may take a while)")
    
    if (requireNamespace("goodpractice", quietly = TRUE)) {
      tryCatch({
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
      }, error = function(e) {
        cli::cli_alert_warning("Good practice check failed: {e$message}")
        results$good_practice <- NA
      })
    } else {
      cli::cli_alert_warning("Package {.pkg goodpractice} not installed")
      cli::cli_alert_info("Install it with: install.packages('goodpractice')")
      results$good_practice <- NA
    }
  }
  
  if (verbose) cli::cli_alert_success("Package validation complete!")
  
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
generate_data_documentation <- function(base_path = NULL,
                                       verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  # Check for dictionary
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
  if (!file.exists(dict_path)) {
    cli::cli_alert_warning("No dictionary found at {.path inst/extdata/dictionary.csv}")
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
      if (verbose) cli::cli_alert_warning("Data file not found: {.path {data_file}}")
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
    doc_lines <- c(doc_lines, 
                   paste0("#' ", clean_name, ": ", metadata$package$title),
                   "#'",
                   paste0("#' ", metadata$package$description),
                   "#'")
    
    # Format line
    doc_lines <- c(doc_lines,
                   paste0("#' @format A data frame with ", n_rows, " rows and ", n_cols, " variables:"))
    
    # Describe block
    doc_lines <- c(doc_lines, "#' \\describe{")
    
    for (i in seq_len(nrow(dataset_dict))) {
      var_name <- dataset_dict$variable_name[i]
      var_desc <- dataset_dict$description[i]
      if (is.na(var_desc) || var_desc == "") {
        var_desc <- paste0(dataset_dict$variable_type[i], " variable")
      }
      doc_lines <- c(doc_lines,
                     paste0("#'   \\item{", var_name, "}{", var_desc, "}"))
    }
    
    doc_lines <- c(doc_lines, "#' }")
    
    # Add the dataset name as a string at the end
    doc_lines <- c(doc_lines, paste0('"', clean_name, '"'))
    
    # Write to R file
    output_file <- file.path(r_dir, paste0(clean_name, ".R"))
    writeLines(doc_lines, output_file)
    
    if (verbose) cli::cli_alert_success("Created documentation for {.file {clean_name}}")
  }
  
  return(invisible(TRUE))
}

#' Generate README from template
#' 
#' Creates README.md from a template, incorporating package metadata.
#' 
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Path to generated README
#' @export
generate_readme <- function(base_path = NULL,
                           verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  # Load metadata
  metadata <- get_metadata_from_desc(base_path)
  if (is.null(metadata)) {
    metadata <- list()
  }
  
  # Load dictionary
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
  dictionary <- fairenough::read_data(dict_path)
  
  # No need for template data - README will read from DESCRIPTION directly
  template_data <- list()
  
  # Create README.Rmd from template if it doesn't exist
  readme_rmd <- file.path(base_path, "README.Rmd")
  
  if (!file.exists(readme_rmd)) {
    # Use our template function
    use_template(
      template = "README.Rmd",
      save_as = "README.Rmd",
      data = template_data,
      base_path = base_path,
      package = "fairenough",
      verbose = FALSE  # Suppress the success message since we'll show our own
    )
  }
  
  # Check if rmarkdown is available
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    tryCatch({
      # Render README with proper working directory
      withr::with_dir(base_path, {
        rmarkdown::render(
          "README.Rmd",
          output_format = "github_document",
          output_file = "README.md",
          quiet = !verbose
        )
      })
      if (verbose) cli::cli_alert_success("Generated README.md")
    }, error = function(e) {
      cli::cli_alert_warning("Could not render README: {e$message}")
    })
  } else {
    cli::cli_alert_warning("Package {.pkg rmarkdown} not installed")
    cli::cli_alert_info("Install it with: install.packages('rmarkdown')")
  }
  
  return(invisible(file.path(base_path, "README.md")))
}

#' Setup package website
#' 
#' Configures pkgdown for creating a package website.
#' 
#' @param base_path Base path for the project
#' @param verbose Whether to show messages (default: TRUE)
#' @param install Whether to install the package before building site (default: TRUE)
#' @param preview Whether to preview the site (default: TRUE)
#' @return Logical indicating success
#' @export
setup_website <- function(base_path = NULL,
                         verbose = TRUE,
                         install = TRUE,
                         preview = TRUE) {
  
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
            given <- if (!is.null(author$given)) author$given else if (!is.null(author[["given"]])) author[["given"]] else ""
            family <- if (!is.null(author$family)) author$family else if (!is.null(author[["family"]])) author[["family"]] else ""
            
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
  if (verbose) cli::cli_alert_info("Building package website...")
  
  tryCatch({
    # pkgdown::build_site() needs to know the package path
    if (verbose) {
      pkgdown::build_site(pkg = base_path, preview = preview, install = install)
    } else {
      suppressMessages(pkgdown::build_site(pkg = base_path, preview = preview, install = install))
    }
    if (verbose) cli::cli_alert_success("Website built in {.path docs/}")
  }, error = function(e) {
    cli::cli_alert_warning("Could not build website: {e$message}")
    return(invisible(FALSE))
  })
  
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
run_checks <- function(base_path = NULL,
                      verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  if (!requireNamespace("devtools", quietly = TRUE)) {
    cli::cli_alert_warning("Package {.pkg devtools} not installed")
    cli::cli_alert_info("Install it with: install.packages('devtools')")
    return(invisible(NA))
  }
  
  if (verbose) cli::cli_alert_info("Running R CMD check...")
  
  check_result <- tryCatch({
    devtools::check(
      pkg = base_path,
      quiet = !verbose,
      args = "--no-manual"
    )
  }, error = function(e) {
    cli::cli_alert_warning("Check failed: {e$message}")
    return(NULL)
  })
  
  if (!is.null(check_result)) {
    errors <- length(check_result$errors)
    warnings <- length(check_result$warnings)
    notes <- length(check_result$notes)
    
    if (errors == 0 && warnings == 0) {
      if (verbose) cli::cli_alert_success("Package checks passed!")
      return(invisible(TRUE))
    } else {
      if (verbose) {
        cli::cli_alert_warning("Check complete: {errors} error{?s}, {warnings} warning{?s}, {notes} note{?s}")
      }
      return(invisible(FALSE))
    }
  }
  
  return(invisible(NA))
}


#' Create CITATION file using cffr
#' 
#' Creates a CITATION file from DESCRIPTION using the cffr package.
#' 
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @param overwrite Whether to overwrite the CITATION file
#' @export
create_citation <- function(base_path = NULL, verbose = TRUE, validate = FALSE, overwrite = TRUE) {
  base_path <- get_base_path(base_path)
  
  # Check if cffr is available
  if (!requireNamespace("cffr", quietly = TRUE)) {
    cli::cli_alert_warning("Package {.pkg cffr} not installed")
    cli::cli_alert_info("Install it with: install.packages('cffr')")
    return(invisible(FALSE))
  }
  
  tryCatch({
    # Use cffr to create citation from DESCRIPTION
    desc_path <- file.path(base_path, "DESCRIPTION")
    if (file.exists(desc_path)) {
      # Create CITATION.cff in base_path
      cff <- cffr::cff_write(x = desc_path, 
                            outfile = file.path(base_path, "CITATION.cff"), 
                            validate = validate, 
                            verbose = verbose,
                            overwrite = overwrite)
      cffr::cff_write_citation(cff, 
                              file = file.path(base_path, "inst", "CITATION"),
                              overwrite = overwrite)
    }
  }, error = function(e) {
    cli::cli_alert_warning("Could not create citation: {e$message}")
    return(invisible(FALSE))
  })
}