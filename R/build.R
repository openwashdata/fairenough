#' Build R data package in modern 3-step process
#' 
#' Orchestrates complete package build using modern R development tools.
#' Separates package structure, documentation, and validation for better control.
#' 
#' @param step Which build step to run: "all" (default), "package", "docs", or "validate"  
#' @param base_path Base path for the project (default: uses get_base_path())
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @return Invisibly returns build status
#' @export
#' @examples
#' \dontrun{
#' # Full 3-step build
#' build()
#' 
#' # Just build package structure
#' build(step = "package")
#' 
#' # Build docs and website only
#' build(step = "docs")
#' }
build <- function(step = "all",
                 base_path = NULL,
                 verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  if (verbose) cli::cli_h1("Building data package with modern workflow")
  
  # Execute build steps based on parameter
  package_result <- NULL
  docs_result <- NULL
  validation_result <- NULL
  
  if (step %in% c("all", "package")) {
    package_result <- build_package(base_path = base_path, verbose = verbose)
  }
  
  if (step %in% c("all", "docs")) {
    docs_result <- build_docs(base_path = base_path, verbose = verbose)
  }
  
  if (step %in% c("all", "validate")) {
    validation_result <- validate_package(base_path = base_path, verbose = verbose)
  }
  
  if (verbose && step == "all") {
    cli::cli_alert_success("Modern 3-step build complete!")
    cli::cli_alert_info("Package structure, documentation, and validation finished")
  }
  
  invisible(list(
    base_path = base_path,
    step = step,
    package_result = package_result,
    docs_result = docs_result,
    validation_result = validation_result
  ))
}

#' Ensure .Rproj file exists for proper usethis context
#' 
#' Creates an .Rproj file if one doesn't exist in the project directory.
#' This ensures usethis functions work correctly with the project context.
#' 
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Path to .Rproj file (invisibly)
#' @noRd
ensure_rproj <- function(base_path, verbose = TRUE) {
  
  # Get package name from DESCRIPTION if it exists
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (file.exists(desc_path)) {
    desc_obj <- desc::desc(file = desc_path)
    proj_name <- tryCatch(
      desc_obj$get_field("Package"),
      error = function(e) basename(base_path)
    )
  } else {
    proj_name <- basename(base_path)
  }
  
  # Check for existing .Rproj file
  existing_rproj <- list.files(base_path, pattern = "\\.Rproj$", full.names = TRUE)
  
  if (length(existing_rproj) == 0) {
    # Use usethis to create project properly if no .Rproj exists
    if (verbose) cli::cli_alert_info("Creating project file for {.path {proj_name}}")
    
    # Temporarily suppress usethis messages if not verbose
    withr::local_options(list(usethis.quiet = !verbose))
    
    # Create the project (this also sets it as active)
    tryCatch({
      # This creates .Rproj and sets it as the active project
      usethis::create_project(path = base_path, open = FALSE, rstudio = TRUE)
      if (verbose) cli::cli_alert_success("Created and activated project at {.path {base_path}}")
    }, error = function(e) {
      # If create_project fails (e.g., already exists), just set the project
      usethis::proj_set(base_path)
      if (verbose) cli::cli_alert_info("Set active project to {.path {base_path}}")
    })
    
    invisible(file.path(base_path, paste0(proj_name, ".Rproj")))
  } else {
    # If .Rproj exists, just set it as active
    usethis::proj_set(base_path)
    if (verbose) cli::cli_alert_info("Using existing project at {.path {base_path}}")
    invisible(existing_rproj[1])
  }
}

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

#' Build documentation and website
#' 
#' Generates README and pkgdown website using modern usethis helpers.
#' Since both use inst/templates/README.Rmd, they are built together.
#' 
#' @param base_path Base path for the project
#' @param verbose Whether to show detailed messages
#' @return Logical indicating success
#' @export
build_docs <- function(base_path = NULL, verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  # Store current project before changing
  old_proj <- tryCatch(usethis::proj_get(), error = function(e) NULL)
  
  # Ensure .Rproj file exists and set as active project
  ensure_rproj(base_path, verbose)
  
  # Restore previous project on exit
  on.exit({
    if (!is.null(old_proj)) usethis::proj_set(old_proj)
  }, add = TRUE)
  
  if (verbose) cli::cli_h2("Step 2: Building Documentation & Website")
  
  # 1. Generate README from template
  if (verbose) cli::cli_alert_info("Generating README from template")
  readme_result <- generate_readme(base_path = base_path, verbose = verbose)
  
  # 2. Setup pkgdown website with modern approach
  if (verbose) cli::cli_alert_info("Setting up pkgdown website")
  
  # Check if pkgdown is available
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    cli::cli_alert_warning("Package {.pkg pkgdown} not installed")
    cli::cli_alert_info("Install it with: install.packages('pkgdown')")
    return(invisible(FALSE))
  }
  
  # Use modern usethis helper for GitHub Pages setup if not already configured
  gh_pages_file <- file.path(base_path, ".github", "workflows", "pkgdown.yaml")
  if (!file.exists(gh_pages_file)) {
    if (verbose) cli::cli_alert_info("Setting up GitHub Pages deployment")
    tryCatch({
      usethis::use_pkgdown_github_pages()
      if (verbose) cli::cli_alert_success("GitHub Pages deployment configured")
    }, error = function(e) {
      # Fall back to regular setup if GitHub Pages fails
      if (verbose) cli::cli_alert_info("Setting up standard pkgdown site")
      tryCatch({
        usethis::use_pkgdown()
        if (verbose) cli::cli_alert_success("Pkgdown site configured")
      }, error = function(e2) {
        cli::cli_alert_warning("Could not setup pkgdown: {e2$message}")
      })
    })
  }
  
  # 3. Generate the website using existing setup_website function
  website_result <- setup_website(base_path = base_path, verbose = verbose)
  
  if (verbose) cli::cli_alert_success("Documentation and website build complete!")
  
  invisible(list(
    readme = readme_result,
    website = website_result
  ))
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
  metadata_path <- file.path(base_path, "inst", "extdata", "metadata.json")
  if (!file.exists(metadata_path)) {
    cli::cli_alert_warning("No metadata found")
    cli::cli_alert_info("Run {.fn document} first to collect metadata")
    return(invisible(FALSE))
  }
  
  # Load dictionary and metadata
  dictionary <- utils::read.csv(dict_path)
  metadata <- jsonlite::fromJSON(metadata_path, simplifyDataFrame = FALSE)
  
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

#' Apply metadata to package files
#' 
#' Updates DESCRIPTION file and other package files with collected metadata.
#' 
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Logical indicating success
#' @export
apply_metadata <- function(base_path = NULL,
                          verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  # Load metadata
  metadata_path <- file.path(base_path, "inst", "extdata", "metadata.json")
  
  if (!file.exists(metadata_path)) {
    cli::cli_alert_warning("No metadata found at {.path inst/extdata/metadata.json}")
    cli::cli_alert_info("Run {.fn document} first to collect metadata")
    return(invisible(FALSE))
  }
  
  metadata <- jsonlite::fromJSON(metadata_path, simplifyDataFrame = FALSE)
  
  # Update DESCRIPTION file
  desc_path <- file.path(base_path, "DESCRIPTION")
  
  if (file.exists(desc_path)) {
    desc_obj <- desc::desc(file = desc_path)
  } else {
    # Create minimal DESCRIPTION if it doesn't exist
    desc_obj <- desc::desc(text = "Package: placeholder\nVersion: 0.0.1\n")
  }
  
  # Apply package metadata
  if (!is.null(metadata$package$name)) {
    desc_obj$set("Package", metadata$package$name)
  }
  
  if (!is.null(metadata$package$title)) {
    desc_obj$set("Title", metadata$package$title)
  }
  
  if (!is.null(metadata$package$description)) {
    desc_obj$set("Description", metadata$package$description)
  }
  
  if (!is.null(metadata$package$version)) {
    desc_obj$set("Version", metadata$package$version)
  }
  
  if (!is.null(metadata$package$github_user)) {
    desc_obj$set("github_user", metadata$package$github_user)
  }
  
  # Apply authors using desc package methods
  if (!is.null(metadata$authors) && length(metadata$authors) > 0) {
    # Clear all existing authors
    tryCatch({
      desc_obj$del_author()
    }, error = function(e) {
      # Ignore if no authors to remove
    })
    
    # Add each author using desc package approach
    for (i in seq_along(metadata$authors)) {
      author <- metadata$authors[[i]]
      
      # Debug: check author structure
      if (verbose) {
        cli::cli_alert_info("Processing author {i}: {class(author)}")
      }
      
      # Handle different author structures
      if (is.list(author)) {
        # Author is a proper list - try both $ and [[ access
        given <- if (!is.null(author$given)) author$given else author[["given"]]
        family <- if (!is.null(author$family)) author$family else author[["family"]]
        email <- if (!is.null(author$email)) author$email else author[["email"]]
        orcid <- if (!is.null(author$orcid)) author$orcid else author[["orcid"]]
        affiliation <- if (!is.null(author$affiliation)) author$affiliation else author[["affiliation"]]
        roles <- if (!is.null(author$roles)) author$roles else author[["roles"]]
      } else {
        # Skip malformed author entries
        if (verbose) cli::cli_alert_warning("Skipping malformed author entry {i}")
        next
      }
      
      # Build comment field safely
      comment <- character()
      if (!is.null(orcid) && orcid != "") {
        comment <- c(ORCID = orcid)
      }
      if (!is.null(affiliation) && affiliation != "") {
        comment <- c(comment, affiliation = affiliation)
      }
      
      # Handle roles - convert string to character vector if needed
      if (is.character(roles) && length(roles) == 1) {
        # Split by comma if it's a comma-separated string
        roles <- trimws(strsplit(roles, ",")[[1]])
      } else if (is.null(roles) || length(roles) == 0) {
        # Default roles if none specified
        roles <- c("aut")
      }
      
      # Skip invalid entries
      if (is.null(given) || given == "" || is.null(family) || family == "") {
        if (verbose) cli::cli_alert_warning("Skipping author with missing name")
        next
      }
      
      # Validate ORCID format
      if (!is.null(orcid) && orcid != "") {
        # Check if it's the placeholder ORCID
        if (orcid == "0000-0000-0000-0000") {
          orcid <- NULL  # Remove invalid placeholder
        } else if (!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", orcid)) {
          if (verbose) cli::cli_alert_warning("Invalid ORCID format for {given} {family}, skipping ORCID")
          orcid <- NULL
        }
      }
      
      # Rebuild comment field after ORCID validation
      comment <- character()
      if (!is.null(orcid) && orcid != "") {
        comment <- c(ORCID = orcid)
      }
      if (!is.null(affiliation) && affiliation != "") {
        comment <- c(comment, affiliation = affiliation)
      }
      
      desc_obj$add_author(
        given = given,
        family = family,
        email = email,
        role = roles,
        comment = if (length(comment) > 0) comment else NULL
      )
    }
  }
  
  # Apply license
  if (!is.null(metadata$license$id)) {
    desc_obj$set("License", metadata$license$id)
  }
  
  # Add dependencies
  desc_obj$set_dep("R", type = "Depends", version = ">= 3.5.0")
  desc_obj$set_dep("readr", type = "Imports")
  desc_obj$set_dep("dplyr", type = "Imports")
  desc_obj$set_dep("janitor", type = "Imports")
  desc_obj$set_dep("fs", type = "Imports")
  desc_obj$set_dep("cli", type = "Imports")
  desc_obj$set_dep("here", type = "Imports")
  
  # Save DESCRIPTION
  desc_obj$write(file = desc_path)
  
  if (verbose) {
    cli::cli_alert_success("Updated DESCRIPTION file")
  }
  
  # Create or update CITATION file
  create_citation(metadata, base_path, verbose)
  
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
  metadata_path <- file.path(base_path, "inst", "extdata", "metadata.json")
  metadata <- if (file.exists(metadata_path)) {
    jsonlite::fromJSON(metadata_path, simplifyDataFrame = FALSE)
  } else {
    list()
  }
  
  # Load dictionary
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
  dictionary <- if (file.exists(dict_path)) {
    utils::read.csv(dict_path)
  } else {
    NULL
  }
  
  # Prepare template data (though README.Rmd doesn't use substitution, keeping for consistency)
  template_data <- list(
    package_name = if (!is.null(metadata$package$name)) metadata$package$name else "package",
    github_user = if (!is.null(metadata$package$github_user)) metadata$package$github_user else "openwashdata"
  )
  
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
      rmarkdown::render(
        readme_rmd,
        output_format = "github_document",
        output_file = "README.md",
        quiet = !verbose
      )
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
#' @param verbose Whether to show messages
#' @return Logical indicating success
#' @export
setup_website <- function(base_path = NULL,
                         verbose = TRUE) {
  
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
    metadata_path <- file.path(base_path, "inst", "extdata", "metadata.json")
    
    if (file.exists(metadata_path)) {
      # Use simplifyVector = FALSE to preserve list structure for whisker templating
      metadata <- jsonlite::fromJSON(metadata_path, simplifyVector = FALSE)
      
      # Prepare authors data for Mustache iteration
      # Add a flag for comma placement between authors
      if (!is.null(metadata$authors) && length(metadata$authors) > 0) {
        for (i in seq_along(metadata$authors)) {
          # Add flag for whether this is NOT the last author (for comma placement)
          metadata$authors[[i]]$not_last <- i < length(metadata$authors)
        }
      }
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
      pkgdown::build_site(pkg = base_path, preview = FALSE)
    } else {
      suppressMessages(pkgdown::build_site(pkg = base_path, preview = FALSE))
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
#' Creates a CITATION file from metadata using the cffr package.
#' 
#' @param metadata List containing metadata
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @noRd
create_citation <- function(metadata, base_path, verbose = TRUE) {
  
  if (is.null(metadata$authors) || length(metadata$authors) == 0) {
    return(invisible(NULL))
  }
  
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
      # Create CITATION.cff in root
      cffr::cff_write(x = base_path)
      
      # Also create inst/CITATION file for R
      citation_dir <- file.path(base_path, "inst")
      if (!dir.exists(citation_dir)) {
        dir.create(citation_dir, recursive = TRUE)
      }
      
      # Generate R citation format
      cit <- utils::citation(auto = desc::desc(file = desc_path))
      citation_file <- file.path(citation_dir, "CITATION")
      
      # Write the CITATION file
      sink(citation_file)
      print(cit, style = "R")
      sink()
      
      if (verbose) {
        cli::cli_alert_success("Created CITATION.cff and inst/CITATION files")
      }
    }
  }, error = function(e) {
    cli::cli_alert_warning("Could not create citation: {e$message}")
    return(invisible(FALSE))
  })
}