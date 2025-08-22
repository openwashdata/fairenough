#' Collect comprehensive metadata interactively
#'
#' @description
#' Collects metadata for R data packages using interactive prompts.
#' Supports partial pre-filling where provided values are used and
#' missing required fields are prompted. Uses the general prompt.R utilities.
#' By default, collects only essential metadata. Set extended=TRUE to collect
#' additional optional metadata for publication and archiving.
#'
#' @param pkg_name Package name
#' @param title Package title
#' @param description Package description (one paragraph)
#' @param version Package version (default: "0.0.1")
#' @param language Language code (default: "en-GB")
#' @param github_user GitHub username or organization
#' @param authors List of author information
#' @param license License identifier (e.g., "CC-BY-4.0")
#' @param keywords Character vector of keywords (only prompted if extended=TRUE)
#' @param funder Funding organization (only prompted if extended=TRUE)
#' @param grant_id Grant identifier (only prompted if extended=TRUE)
#' @param temporal_start Start date of data coverage (only prompted if extended=TRUE)
#' @param temporal_end End date of data coverage (only prompted if extended=TRUE)
#' @param spatial_description Geographic coverage description (only prompted if extended=TRUE)
#' @param spatial_coordinates List with lat and lon (only prompted if extended=TRUE)
#' @param related_identifiers List of related publications (only prompted if extended=TRUE)
#' @param communities Character vector of Zenodo communities (only prompted if extended=TRUE)
#' @param extended Whether to prompt for extended metadata fields (default: FALSE)
#' @param interactive Whether to use interactive prompts (default: TRUE)
#' @param save_to_desc Whether to save to DESCRIPTION file (default: TRUE)
#' @param base_path Base path for the project
#' @param overwrite Whether to overwrite existing metadata (default: FALSE)
#'
#' @return List containing all metadata organized by category
#' @export
collect_metadata <- function(
  pkg_name = NULL,
  title = NULL,
  description = NULL,
  version = "0.0.1",
  language = "en-GB",
  github_user = NULL,
  authors = NULL,
  license = NULL,
  keywords = NULL,
  funder = NULL,
  grant_id = NULL,
  temporal_start = NULL,
  temporal_end = NULL,
  spatial_description = NULL,
  spatial_coordinates = NULL,
  related_identifiers = NULL,
  communities = NULL,
  extended = FALSE,
  interactive = TRUE,
  save_to_desc = TRUE,
  base_path = NULL,
  overwrite = FALSE
) {
  # Get base path if needed
  if (!is.null(base_path)) {
    base_path <- get_base_path(base_path)
  } else if (save_to_desc) {
    base_path <- get_base_path()
  }

  # Check for existing metadata in DESCRIPTION
  if (save_to_desc && !overwrite) {
    desc_path <- file.path(base_path, "DESCRIPTION")
    if (file.exists(desc_path)) {
      existing_meta <- tryCatch(
        get_metadata_from_desc(base_path),
        error = function(e) NULL
      )

      if (!is.null(existing_meta)) {
        if (interactive) {
          cli::cli_alert_info("Metadata already exists in DESCRIPTION")
          if (
            !prompt_confirm("Do you want to overwrite it?", default = FALSE)
          ) {
            return(existing_meta)
          }
        } else {
          return(existing_meta)
        }
      }
    }
  }

  cli::cli_h1("Collecting metadata for R data package")

  # Package Information - Required fields
  cli::cli_h2("Package Information")

  pkg_name <- prompt_input(
    "Package name",
    value = pkg_name,
    required = TRUE,
    validator = validate_package_name,
    validator_message = "Package name must start with a letter, contain only letters, numbers, and dots"
  )

  title <- prompt_input(
    "Title",
    value = title,
    required = TRUE
  )

  description <- prompt_input(
    "Description (one paragraph)",
    value = description,
    required = TRUE
  )

  version <- prompt_input(
    "Version",
    value = version,
    default = "0.0.1"
  )

  language <- prompt_menu(
    choices = c(
      "English (GB)" = "en-GB",
      "English (US)" = "en-US",
      "German" = "de",
      "French" = "fr",
      "Spanish" = "es"
    ),
    title = "Select language",
    value = language,
    default = "en-GB",
    allow_other = TRUE
  )

  github_user <- prompt_input(
    "GitHub user/organization",
    value = github_user
  )

  # Authors - Complex handling
  cli::cli_h2("Authors")

  if (is.null(authors) || length(authors) == 0) {
    authors <- list()
    if (interactive) {
      add_more <- TRUE
      author_num <- 1

      while (add_more) {
        cli::cli_alert_info(paste("Author", author_num))

        given <- prompt_input("  Given name", required = TRUE)
        family <- prompt_input("  Family name", required = TRUE)
        email <- prompt_input(
          "  Email",
          required = TRUE,
          validator = validate_email,
          validator_message = "Please enter a valid email address"
        )
        orcid <- prompt_input(
          "  ORCID (0000-0000-0000-0000)",
          validator = validate_orcid,
          validator_message = "ORCID must be in format 0000-0000-0000-000X"
        )
        affiliation <- prompt_input("  Affiliation", default = "ETH Zurich")

        # Use multi-select for roles
        roles <- prompt_multi_select(
          choices = c(
            "Author (wrote the package)" = "aut",
            "Creator/Maintainer (maintains the package)" = "cre",
            "Contributor (made contributions)" = "ctb",
            "Funder (funded the work)" = "fnd",
            "Thesis advisor (supervised the work)" = "ths"
          ),
          title = "Select author roles",
          default = c("aut", "cre"),
          min_choices = 1
        )

        authors[[author_num]] <- list(
          given = given,
          family = family,
          email = email,
          orcid = if (!is.null(orcid) && orcid != "") orcid else NULL,
          affiliation = affiliation,
          roles = roles
        )

        author_num <- author_num + 1
        add_more <- prompt_confirm("\nAdd another author?", default = FALSE)
      }
    }
  }

  # License
  cli::cli_h2("License")

  # Generate license options from LICENSE_CONFIG
  license_choices <- get_available_licenses("both")
  
  # Match existing license value if provided
  if (!is.null(license)) {
    matched_license <- match_license(license)
    if (!is.null(matched_license)) {
      license <- matched_license
    }
  }

  license <- prompt_menu(
    choices = license_choices,
    title = "Select a license",
    value = license,
    default = "CC-BY",
    allow_other = TRUE
  )
  
  # If user provided a custom license, try to match it
  if (!is.null(license) && !(license %in% names(license_choices))) {
    matched_license <- match_license(license)
    if (!is.null(matched_license)) {
      license <- matched_license
    }
  }

  # Get license URL based on canonical name
  license_urls <- list(
    "CC-BY" = "https://creativecommons.org/licenses/by/4.0/",
    "CC0" = "https://creativecommons.org/publicdomain/zero/1.0/",
    "MIT" = "https://opensource.org/licenses/MIT",
    "GPL-2" = "https://www.gnu.org/licenses/gpl-2.0.html",
    "GPL-3" = "https://www.gnu.org/licenses/gpl-3.0.html",
    "LGPL-2.1" = "https://www.gnu.org/licenses/lgpl-2.1.html",
    "LGPL-3" = "https://www.gnu.org/licenses/lgpl-3.0.html",
    "AGPL-3" = "https://www.gnu.org/licenses/agpl-3.0.html",
    "Apache-2.0" = "https://www.apache.org/licenses/LICENSE-2.0",
    "Proprietary" = NULL
  )

  license_url <- if (!is.null(license) && license %in% names(license_urls)) {
    license_urls[[license]]
  } else {
    NULL
  }

  # Publication Information - Optional fields (only if extended)
  if (extended) {
    cli::cli_h2("Publication Information")

    # Keywords
    if (is.null(keywords) && interactive) {
      keywords_input <- prompt_input("Keywords (comma-separated)")
      if (!is.null(keywords_input) && keywords_input != "") {
        keywords <- trimws(strsplit(keywords_input, ",")[[1]])
      }
    }

    funder <- prompt_input(
      "Funding organization",
      value = funder,
      default = if (interactive) "ETH Domain ORD Programme" else NULL
    )

    grant_id <- prompt_input("Grant ID", value = grant_id)

    # Communities
    if (is.null(communities) && interactive) {
      use_communities <- prompt_confirm(
        "Add to Zenodo communities?",
        default = TRUE
      )
      if (use_communities) {
        communities <- prompt_multi_select(
          choices = c(
            "Global Health Engineering" = "global-health-engineering",
            "Environmental Science" = "environmental-science",
            "Data Science" = "data-science",
            "Open Science" = "open-science"
          ),
          title = "Select Zenodo communities",
          allow_other = TRUE
        )
      }
    }
  }

  # Coverage - Optional (only if extended)
  if (extended) {
    cli::cli_h2("Data Coverage")

    # Temporal coverage
    if (interactive && (is.null(temporal_start) || is.null(temporal_end))) {
      has_temporal <- prompt_confirm(
        "Does the data have temporal coverage?",
        default = TRUE
      )
      if (has_temporal) {
        temporal_start <- prompt_input(
          "Start date (YYYY or YYYY-MM-DD)",
          value = temporal_start,
          validator = validate_date,
          validator_message = "Date must be YYYY or YYYY-MM-DD format"
        )
        temporal_end <- prompt_input(
          "End date (YYYY or YYYY-MM-DD)",
          value = temporal_end,
          validator = validate_date,
          validator_message = "Date must be YYYY or YYYY-MM-DD format"
        )
      }
    }

    # Spatial coverage
    if (interactive && is.null(spatial_description)) {
      has_spatial <- prompt_confirm(
        "Does the data have geographic coverage?",
        default = FALSE
      )
      if (has_spatial) {
        spatial_description <- prompt_input("Geographic description")

        has_coords <- prompt_confirm("Add coordinates?", default = FALSE)
        if (has_coords && is.null(spatial_coordinates)) {
          lat <- as.numeric(prompt_input("Latitude", required = TRUE))
          lon <- as.numeric(prompt_input("Longitude", required = TRUE))
          spatial_coordinates <- list(lat = lat, lon = lon)
        }
      }
    }
  }

  # Related identifiers (only if extended)
  if (extended) {
    cli::cli_h2("Related Publications")

    if (is.null(related_identifiers) && interactive) {
      has_related <- prompt_confirm(
        "Are there related publications?",
        default = FALSE
      )
      if (has_related) {
        related_identifiers <- list()
        add_more <- TRUE
        rel_num <- 1

        while (add_more) {
          doi <- prompt_input(paste("  DOI", rel_num))
          if (!is.null(doi) && doi != "") {
            related_identifiers[[rel_num]] <- list(
              scheme = "doi",
              identifier = doi,
              relation = "isDocumentedBy",
              resource_type = "publication-article"
            )
            rel_num <- rel_num + 1
            add_more <- prompt_confirm("  Add another?", default = FALSE)
          } else {
            add_more <- FALSE
          }
        }
      }
    }
  }

  # Construct metadata list
  metadata <- list(
    package = list(
      name = pkg_name,
      title = title,
      description = description,
      version = version,
      language = language,
      github_user = github_user
    ),
    authors = authors,
    license = list(
      id = license,
      url = license_url
    ),
    publication = list(
      keywords = keywords,
      funder = if (!is.null(funder) && funder != "") funder else NULL,
      grant_id = if (!is.null(grant_id) && grant_id != "") grant_id else NULL,
      communities = communities
    ),
    coverage = list(
      temporal = if (!is.null(temporal_start) || !is.null(temporal_end)) {
        list(start = temporal_start, end = temporal_end)
      } else {
        NULL
      },
      spatial = if (!is.null(spatial_description)) {
        list(
          description = spatial_description,
          coordinates = spatial_coordinates
        )
      } else {
        NULL
      }
    ),
    related = related_identifiers
  )

  # Remove NULL elements from nested lists
  metadata <- lapply(metadata, function(x) {
    if (is.list(x) && !is.data.frame(x)) {
      x[!vapply(x, is.null, logical(1))]
    } else {
      x
    }
  })

  # Ensure UTF-8 encoding
  ensure_utf8 <- function(x) {
    rapply(
      x,
      function(y) {
        if (is.character(y)) enc2utf8(y) else y
      },
      how = "replace"
    )
  }

  metadata <- ensure_utf8(metadata)

  cli::cli_alert_success("Metadata collection complete!")

  # Show summary
  if (interactive) {
    show_summary <- prompt_confirm("\nShow metadata summary?", default = TRUE)
    if (show_summary) {
      cli::cli_h2("Metadata Summary")
      cli::cli_text("Title: {metadata$package$title}")
      cli::cli_text("Authors: {length(metadata$authors)}")
      cli::cli_text("License: {metadata$license$id}")
      if (!is.null(metadata$publication$keywords)) {
        cli::cli_text(
          "Keywords: {paste(metadata$publication$keywords, collapse = ', ')}"
        )
      }
    }
  }

  # Save to DESCRIPTION if requested
  if (save_to_desc) {
    write_metadata_to_desc(
      metadata,
      base_path,
      overwrite = TRUE, # Always overwrite when collecting fresh metadata
      verbose = TRUE
    )
  }

  return(metadata)
}
