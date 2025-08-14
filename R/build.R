#' Build data package
#' 
#' High-level function to build the complete data package.
#' Applies metadata to DESCRIPTION, generates README, and sets up website.
#' 
#' @param check Whether to run R CMD check (default: TRUE)
#' @param website Whether to setup pkgdown website (default: TRUE)
#' @param base_path Base path for the project (default: uses get_base_path())
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @return Invisibly returns build status
#' @export
#' @examples
#' \dontrun{
#' # Full build with checks
#' build()
#' 
#' # Quick build without checks
#' build(check = FALSE)
#' 
#' # Build without website
#' build(website = FALSE)
#' }
build <- function(check = TRUE,
                 website = TRUE,
                 base_path = NULL,
                 verbose = TRUE) {
  
  base_path <- get_base_path(base_path)

  # Use withr/usethis to temporarily set project path for this function
  # This ensures use_template writes to the correct location
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::local_options(list(usethis.quiet = TRUE))
  }
  usethis::local_project(base_path, setwd = FALSE)
  
  if (verbose) cli::cli_h1("Building data package")
  
  # Apply metadata to DESCRIPTION and other files
  if (verbose) cli::cli_h2("Applying Metadata")
  apply_metadata(base_path = base_path, verbose = verbose)
  
  # Generate data documentation
  if (verbose) cli::cli_h2("Generating Data Documentation")
  generate_data_documentation(base_path = base_path, verbose = verbose)
  
  # Generate README from template
  if (verbose) cli::cli_h2("Generating README")
  generate_readme(base_path = base_path, verbose = verbose)
  
  # Setup website if requested
  if (website) {
    if (verbose) cli::cli_h2("Setting up Website")
    setup_website(base_path = base_path, verbose = verbose)
  }
  
  # Run checks if requested
  if (check) {
    if (verbose) cli::cli_h2("Running Package Checks")
    check_result <- run_checks(base_path = base_path, verbose = verbose)
  }
  
  if (verbose) {
    cli::cli_alert_success("Package build complete!")
    cli::cli_alert_info("Next step: Run {.fn publish} to prepare for distribution")
  }
  
  invisible(list(
    base_path = base_path,
    check_passed = if (check) check_result else NA,
    website_setup = website
  ))
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