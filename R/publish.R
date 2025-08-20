#' Publish data package
#'
#' High-level function to prepare package for publication.
#' Creates Zenodo metadata and validates the package.
#'
#' @param platforms Character vector of platforms to prepare for (default: c("zenodo", "github"))
#' @param validate Whether to validate the package (default: TRUE)
#' @param base_path Base path for the project (default: uses get_base_path())
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @return Invisibly returns publication status
#' @export
#' @examples
#' \dontrun{
#' # Prepare for all platforms
#' publish()
#'
#' # Only prepare for GitHub
#' publish(platforms = "github")
#'
#' # Skip validation
#' publish(validate = FALSE)
#' }
publish <- function(
  platforms = c("zenodo", "github"),
  validate = TRUE,
  base_path = NULL,
  verbose = TRUE
) {
  base_path <- get_base_path(base_path)

  if (verbose) {
    cli::cli_h1("Preparing package for publication")
  }

  # Validate package if requested
  if (validate) {
    if (verbose) {
      cli::cli_h2("Validating Package")
    }
    validation_result <- validate_package(
      base_path = base_path,
      verbose = verbose
    )

    if (!validation_result$is_valid) {
      cli::cli_alert_warning("Package validation failed")
      cli::cli_alert_info("Fix the issues above before publishing")
      return(invisible(list(
        validation = validation_result,
        published = FALSE
      )))
    }
  }

  # Prepare for each platform
  results <- list()

  if ("zenodo" %in% platforms) {
    if (verbose) {
      cli::cli_h2("Preparing for Zenodo")
    }
    results$zenodo <- create_zenodo_json(
      base_path = base_path,
      verbose = verbose
    )
  }

  if ("github" %in% platforms) {
    if (verbose) {
      cli::cli_h2("Preparing for GitHub")
    }
    results$github <- prepare_github(base_path = base_path, verbose = verbose)
  }

  if ("cran" %in% platforms) {
    if (verbose) {
      cli::cli_h2("Preparing for CRAN")
    }
    results$cran <- prepare_cran(base_path = base_path, verbose = verbose)
  }

  if (verbose) {
    cli::cli_alert_success("Package prepared for publication!")
    cli::cli_alert_info(
      "Review the generated files and proceed with platform-specific submission"
    )

    if ("zenodo" %in% platforms) {
      cli::cli_alert_info(
        "Zenodo: Upload {.path .zenodo.json} with your repository"
      )
    }
    if ("github" %in% platforms) {
      cli::cli_alert_info("GitHub: Push to repository and create a release")
    }
    if ("cran" %in% platforms) {
      cli::cli_alert_info("CRAN: Run {.code devtools::release()} when ready")
    }
  }

  invisible(list(
    validation = if (validate) validation_result else NULL,
    results = results,
    published = TRUE
  ))
}

#' Create Zenodo metadata file
#'
#' Creates a .zenodo.json file with metadata for Zenodo repository.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Path to created file
#' @export
create_zenodo_json <- function(base_path = NULL, verbose = TRUE) {
  base_path <- get_base_path(base_path)

  # Load metadata
  metadata_path <- file.path(base_path, "inst", "extdata", "metadata.json")

  if (!file.exists(metadata_path)) {
    cli::cli_alert_warning("No metadata found")
    cli::cli_alert_info("Run {.fn document} first to collect metadata")
    return(invisible(NULL))
  }

  metadata <- jsonlite::fromJSON(metadata_path)

  # Build Zenodo metadata
  zenodo_meta <- list(
    title = metadata$package$title,
    description = metadata$package$description,
    version = metadata$package$version,
    language = metadata$package$language,
    upload_type = "dataset",
    access_right = "open"
  )

  # Add license
  if (!is.null(metadata$license$id)) {
    zenodo_meta$license <- tolower(metadata$license$id)
  }

  # Add creators
  if (!is.null(metadata$authors) && length(metadata$authors) > 0) {
    zenodo_meta$creators <- lapply(metadata$authors, function(author) {
      creator <- list(
        name = paste(author$family, author$given, sep = ", ")
      )
      if (!is.null(author$orcid)) {
        creator$orcid <- author$orcid
      }
      if (!is.null(author$affiliation)) {
        creator$affiliation <- author$affiliation
      }
      return(creator)
    })
  }

  # Add keywords
  if (!is.null(metadata$publication$keywords)) {
    zenodo_meta$keywords <- metadata$publication$keywords
  }

  # Add grants
  if (!is.null(metadata$publication$grant_id)) {
    zenodo_meta$grants <- list(
      list(id = metadata$publication$grant_id)
    )
  }

  # Add communities
  if (!is.null(metadata$publication$communities)) {
    zenodo_meta$communities <- lapply(
      metadata$publication$communities,
      function(x) list(identifier = x)
    )
  }

  # Add related identifiers
  if (!is.null(metadata$related) && length(metadata$related) > 0) {
    zenodo_meta$related_identifiers <- metadata$related
  }

  # Save as .zenodo.json
  zenodo_path <- file.path(base_path, ".zenodo.json")
  jsonlite::write_json(
    zenodo_meta,
    zenodo_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )

  if (verbose) {
    cli::cli_alert_success("Created {.path .zenodo.json}")
  }

  return(invisible(zenodo_path))
}

#' Validate package for publication
#'
#' Checks that the package meets requirements for publication.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return List with validation results
#' @export
validate_package <- function(base_path = NULL, verbose = TRUE) {
  base_path <- get_base_path(base_path)

  checks <- list()
  issues <- character()

  # Check DESCRIPTION file
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (file.exists(desc_path)) {
    checks$description <- TRUE

    desc_obj <- desc::desc(file = desc_path)

    # Check required fields
    required_fields <- c(
      "Package",
      "Title",
      "Description",
      "Version",
      "Authors@R",
      "License"
    )
    for (field in required_fields) {
      if (is.na(desc_obj$get(field))) {
        checks[[paste0("description_", tolower(field))]] <- FALSE
        issues <- c(issues, paste("DESCRIPTION missing:", field))
      }
    }
  } else {
    checks$description <- FALSE
    issues <- c(issues, "DESCRIPTION file not found")
  }

  # Check data files
  data_dir <- file.path(base_path, "data")
  if (fs::dir_exists(data_dir)) {
    rda_files <- list.files(data_dir, pattern = "\\.rda$")
    checks$data <- length(rda_files) > 0
    if (!checks$data) {
      issues <- c(issues, "No .rda files in data/")
    }
  } else {
    checks$data <- FALSE
    issues <- c(issues, "data/ directory not found")
  }

  # Check documentation
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
  checks$dictionary <- file.exists(dict_path)
  if (!checks$dictionary) {
    issues <- c(issues, "Data dictionary not found")
  }

  # Check README
  readme_path <- file.path(base_path, "README.md")
  checks$readme <- file.exists(readme_path)
  if (!checks$readme) {
    issues <- c(issues, "README.md not found")
  }

  # Check metadata
  metadata_path <- file.path(base_path, "inst", "extdata", "metadata.json")
  checks$metadata <- file.exists(metadata_path)
  if (!checks$metadata) {
    issues <- c(issues, "Metadata not found")
  }

  # Overall validation
  is_valid <- all(unlist(checks))

  if (verbose) {
    if (is_valid) {
      cli::cli_alert_success("Package validation passed!")
    } else {
      cli::cli_alert_warning("Package validation found issues:")
      for (issue in issues) {
        cli::cli_alert_danger(issue)
      }
    }
  }

  return(list(
    is_valid = is_valid,
    checks = checks,
    issues = issues
  ))
}

#' Prepare package for GitHub
#'
#' Creates or updates GitHub-specific files.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Logical indicating success
#' @export
prepare_github <- function(base_path = NULL, verbose = TRUE) {
  base_path <- get_base_path(base_path)

  # Create .github directory if needed
  github_dir <- file.path(base_path, ".github")
  if (!fs::dir_exists(github_dir)) {
    fs::dir_create(github_dir)
  }

  # Create CONTRIBUTING.md if it doesn't exist
  contrib_path <- file.path(base_path, "CONTRIBUTING.md")
  if (!file.exists(contrib_path)) {
    contrib_content <- "# Contributing

Thank you for your interest in contributing to this project!

## How to contribute

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## Code of Conduct

Please be respectful and constructive in all interactions.
"
    writeLines(contrib_content, contrib_path)
    if (verbose) cli::cli_alert_success("Created CONTRIBUTING.md")
  }

  # Create CODE_OF_CONDUCT.md if it doesn't exist
  coc_path <- file.path(base_path, "CODE_OF_CONDUCT.md")
  if (!file.exists(coc_path)) {
    coc_content <- "# Contributor Code of Conduct

As contributors and maintainers of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities.

We are committed to making participation in this project a harassment-free experience for everyone, regardless of level of experience, gender, gender identity and expression, sexual orientation, disability, personal appearance, body size, race, ethnicity, age, or religion.

Examples of unacceptable behavior by participants include the use of sexual language or imagery, derogatory comments or personal attacks, trolling, public or private harassment, insults, or other unprofessional conduct.

Project maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct. Project maintainers who do not follow the Code of Conduct may be removed from the project team.

This code of conduct applies both within project spaces and in public spaces when an individual is representing the project or its community.

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or contacting one or more of the project maintainers.

This Code of Conduct is adapted from the Contributor Covenant, version 1.1.0, available at https://contributor-covenant.org/version/1/1/0/
"
    writeLines(coc_content, coc_path)
    if (verbose) cli::cli_alert_success("Created CODE_OF_CONDUCT.md")
  }

  if (verbose) {
    cli::cli_alert_success("GitHub files prepared")
  }

  return(invisible(TRUE))
}

#' Prepare package for CRAN
#'
#' Prepares package for CRAN submission.
#'
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Logical indicating readiness
#' @export
prepare_cran <- function(base_path = NULL, verbose = TRUE) {
  base_path <- get_base_path(base_path)

  # Create cran-comments.md
  comments_path <- file.path(base_path, "cran-comments.md")
  if (!file.exists(comments_path)) {
    comments_content <- "## Test environments
* local OS X install, R 4.3.0
* ubuntu 20.04 (on GitHub Actions), R 4.3.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package.
"
    writeLines(comments_content, comments_path)
    if (verbose) cli::cli_alert_success("Created cran-comments.md")
  }

  # Create NEWS.md if it doesn't exist
  news_path <- file.path(base_path, "NEWS.md")
  if (!file.exists(news_path)) {
    # Get version from DESCRIPTION
    desc_obj <- desc::desc(file = file.path(base_path, "DESCRIPTION"))
    version <- desc_obj$get_version()

    news_content <- sprintf(
      "# %s %s

* Initial release
",
      desc_obj$get("Package"),
      version
    )

    writeLines(news_content, news_path)
    if (verbose) cli::cli_alert_success("Created NEWS.md")
  }

  if (verbose) {
    cli::cli_alert_success("CRAN preparation complete")
    cli::cli_alert_info("Run {.code devtools::check()} for final checks")
    cli::cli_alert_info("Run {.code devtools::release()} when ready to submit")
  }

  return(invisible(TRUE))
}
