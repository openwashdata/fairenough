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
        get_metadata(base_path),
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

  # Package Information - Required fields
  cli::cli_h3("Package Information")

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
  cli::cli_h3("Authors")

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
  cli::cli_h3("License")

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
    cli::cli_h3("Publication Information")

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
    cli::cli_h3("Data Coverage")

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
    cli::cli_h3("Related Publications")

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
      cli::cli_h3("Metadata Summary")
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
    save_metadata(
      metadata,
      base_path,
      overwrite = TRUE, # Always overwrite when collecting fresh metadata
      verbose = TRUE
    )
  }

  return(metadata)
}

#' Save metadata to DESCRIPTION file
#'
#' Saves metadata structure to DESCRIPTION file using appropriate fields.
#' Handles both creating new metadata and updating existing metadata.
#'
#' @param metadata List containing metadata
#' @param base_path Base path for the project
#' @param overwrite Whether to overwrite existing metadata fields
#' @param create Whether to create DESCRIPTION file if it doesn't exist
#' @param verbose Whether to show messages
#' @return NULL (invisibly)
#' @export
save_metadata <- function(
  metadata,
  base_path = NULL,
  overwrite = TRUE,
  create = TRUE,
  verbose = TRUE
) {
  base_path <- get_base_path(base_path)
  desc_path <- file.path(base_path, "DESCRIPTION")

  if (!file.exists(desc_path)) {
    if (create) {
      # Create a minimal DESCRIPTION file
      if (verbose) {
        cli::cli_alert_info("Creating DESCRIPTION file")
      }

      # Initialize with minimal required fields
      desc_content <- c(
        "Package: placeholder",
        "Title: Placeholder Title",
        "Version: 0.0.1",
        "Authors@R: person('First', 'Last', email = 'email@example.com', role = c('aut', 'cre'))",
        "Description: Placeholder description.",
        "License: MIT",
        "Encoding: UTF-8"
      )

      writeLines(desc_content, desc_path)
    } else {
      cli::cli_abort("No DESCRIPTION file found at {.path {desc_path}}")
    }
  }

  # Create desc object
  d <- desc::desc(file = desc_path)

  # Helper function to conditionally set fields
  set_if_new <- function(field, value) {
    if (!is.null(value)) {
      existing <- d$get(field)[[1]]
      if (overwrite || is.null(existing)) {
        d$set(field, value)
      }
    }
  }

  # Set standard fields
  if (!is.null(metadata$package)) {
    set_if_new("Package", metadata$package$name)
    set_if_new("Title", metadata$package$title)
    set_if_new("Description", metadata$package$description)
    set_if_new("Version", metadata$package$version)
    set_if_new("Language", metadata$package$language)

    # Always set Date to current date for citation purposes
    set_if_new("Date", Sys.Date())

    # Build URLs from github_user if available
    if (
      !is.null(metadata$package$github_user) && !is.null(metadata$package$name)
    ) {
      # Store github_user as a custom field for easy access
      set_if_new("github_user", metadata$package$github_user)

      # Build and set URLs
      github_url <- paste0(
        "https://github.com/",
        metadata$package$github_user,
        "/",
        metadata$package$name
      )
      package_url <- paste0(
        "https://",
        metadata$package$github_user,
        ".github.io/",
        metadata$package$name
      )
      d$set_urls(c(package_url, github_url))
    }
  }

  # Set authors (only if overwrite or no existing authors)
  existing_authors <- d$get_authors()
  if (
    !is.null(metadata$authors) &&
      length(metadata$authors) > 0 &&
      (overwrite || length(existing_authors) == 0)
  ) {
    # Convert to person objects
    authors_list <- lapply(metadata$authors, function(author) {
      person(
        given = author$given,
        family = author$family,
        email = author$email,
        role = if (!is.null(author$roles)) author$roles else c("aut"),
        comment = if (!is.null(author$orcid)) c(ORCID = author$orcid) else NULL
      )
    })

    # Combine into Authors@R field
    authors_r <- do.call("c", authors_list)
    d$set_authors(authors_r)
  }

  # Set license
  set_if_new("License", metadata$license$id)

  # Set custom fields with Config/ prefix
  if (!is.null(metadata$publication)) {
    if (!is.null(metadata$publication$keywords)) {
      set_if_new(
        "Config/Keywords",
        paste(metadata$publication$keywords, collapse = ", ")
      )
    }
    set_if_new("Config/Funder", metadata$publication$funder)
    set_if_new("Config/GrantID", metadata$publication$grant_id)
    if (!is.null(metadata$publication$communities)) {
      set_if_new(
        "Config/Communities",
        paste(metadata$publication$communities, collapse = ", ")
      )
    }
  }

  # Coverage (store as JSON for complex structure)
  if (!is.null(metadata$coverage)) {
    if (!is.null(metadata$coverage$temporal)) {
      set_if_new(
        "Config/TemporalCoverage",
        jsonlite::toJSON(metadata$coverage$temporal, auto_unbox = TRUE)
      )
    }
    if (!is.null(metadata$coverage$spatial)) {
      set_if_new(
        "Config/SpatialCoverage",
        jsonlite::toJSON(metadata$coverage$spatial, auto_unbox = TRUE)
      )
    }
  }

  # Related identifiers (store as JSON)
  if (!is.null(metadata$related) && length(metadata$related) > 0) {
    set_if_new(
      "Config/RelatedIdentifiers",
      jsonlite::toJSON(metadata$related, auto_unbox = TRUE)
    )
  }

  # Handle any other custom fields generically
  standard_fields <- c(
    "package",
    "authors",
    "license",
    "publication",
    "coverage",
    "related"
  )
  other_fields <- setdiff(names(metadata), standard_fields)

  for (field in other_fields) {
    if (!is.null(metadata[[field]])) {
      # Convert field name to Config/ format
      config_field <- paste0("Config/", tools::toTitleCase(field))

      # Store as JSON if it's a complex structure, otherwise as string
      if (is.list(metadata[[field]]) && !is.data.frame(metadata[[field]])) {
        set_if_new(
          config_field,
          jsonlite::toJSON(metadata[[field]], auto_unbox = TRUE)
        )
      } else if (
        is.vector(metadata[[field]]) && length(metadata[[field]]) > 1
      ) {
        # Multiple values - join with commas
        set_if_new(config_field, paste(metadata[[field]], collapse = ", "))
      } else {
        # Single value
        set_if_new(config_field, as.character(metadata[[field]]))
      }
    }
  }

  # Write back to file
  d$write(file = desc_path)

  if (verbose) {
    cli::cli_alert_success("Metadata saved to DESCRIPTION file")
  }

  invisible(NULL)
}

#' Get metadata from DESCRIPTION file
#'
#' Reads DESCRIPTION file and reconstructs metadata structure
#'
#' @param base_path Base path for the project
#' @return List containing metadata
#' @export
get_metadata <- function(base_path = NULL) {
  base_path <- get_base_path(base_path)
  desc_path <- file.path(base_path, "DESCRIPTION")

  if (!file.exists(desc_path)) {
    return(NULL)
  }

  # Read DESCRIPTION
  d <- desc::desc(file = desc_path)

  # Initialize metadata structure
  metadata <- list()

  # Helper to safely get field value
  safe_get <- function(field) {
    val <- d$get(field)
    if (is.null(val) || length(val) == 0) NULL else val[[1]]
  }

  # Package metadata
  metadata$package <- list(
    name = safe_get("Package"),
    title = safe_get("Title"),
    description = safe_get("Description"),
    version = safe_get("Version"),
    language = safe_get("Language")
  )

  # Extract github_user from URL if present
  urls <- d$get_urls()
  if (length(urls) > 0) {
    github_pattern <- "https://github.com/([^/]+)/"
    if (grepl(github_pattern, urls[1])) {
      metadata$package$github_user <- sub(
        paste0(github_pattern, ".*"),
        "\\1",
        urls[1]
      )
    }
  }

  # Authors
  authors_r <- d$get_authors()
  if (length(authors_r) > 0) {
    metadata$authors <- lapply(authors_r, function(author) {
      list(
        given = author$given,
        family = author$family,
        email = author$email,
        orcid = author$comment["ORCID"],
        affiliation = NULL, # Not stored in standard person object
        roles = author$role
      )
    })
  }

  # License
  license_id <- safe_get("License")
  if (!is.null(license_id)) {
    metadata$license <- list(id = license_id)

    # Add standard license URLs
    license_urls <- list(
      "CC-BY-4.0" = "https://creativecommons.org/licenses/by/4.0/",
      "CC BY 4.0" = "https://creativecommons.org/licenses/by/4.0/",
      "CC0-1.0" = "https://creativecommons.org/publicdomain/zero/1.0/",
      "MIT" = "https://opensource.org/licenses/MIT",
      "GPL-3" = "https://www.gnu.org/licenses/gpl-3.0.html",
      "Apache-2.0" = "https://www.apache.org/licenses/LICENSE-2.0"
    )

    if (license_id %in% names(license_urls)) {
      metadata$license$url <- license_urls[[license_id]]
    }
  }

  # Publication metadata from custom fields
  metadata$publication <- list()

  keywords <- safe_get("Config/Keywords")
  if (!is.null(keywords)) {
    metadata$publication$keywords <- trimws(strsplit(keywords, ",")[[1]])
  }

  funder <- safe_get("Config/Funder")
  if (!is.null(funder)) {
    metadata$publication$funder <- funder
  }

  grant_id <- safe_get("Config/GrantID")
  if (!is.null(grant_id)) {
    metadata$publication$grant_id <- grant_id
  }

  communities <- safe_get("Config/Communities")
  if (!is.null(communities)) {
    metadata$publication$communities <- trimws(strsplit(communities, ",")[[1]])
  }

  # Helper function to safely parse JSON
  safe_json_parse <- function(json_string) {
    if (is.null(json_string) || json_string == "" || is.na(json_string)) {
      return(NULL)
    }
    tryCatch(
      {
        jsonlite::fromJSON(json_string, simplifyVector = FALSE)
      },
      error = function(e) {
        NULL
      }
    )
  }

  # Coverage from custom fields (parse JSON)
  metadata$coverage <- list()

  temporal <- safe_get("Config/TemporalCoverage")
  parsed_temporal <- safe_json_parse(temporal)
  if (!is.null(parsed_temporal)) {
    metadata$coverage$temporal <- parsed_temporal
  }

  spatial <- safe_get("Config/SpatialCoverage")
  parsed_spatial <- safe_json_parse(spatial)
  if (!is.null(parsed_spatial)) {
    metadata$coverage$spatial <- parsed_spatial
  }

  # Related identifiers (parse JSON)
  related <- safe_get("Config/RelatedIdentifiers")
  parsed_related <- safe_json_parse(related)
  if (!is.null(parsed_related)) {
    metadata$related <- parsed_related
  }

  # Datasets (parse JSON)
  datasets <- safe_get("Config/Datasets")
  parsed_datasets <- safe_json_parse(datasets)
  if (!is.null(parsed_datasets)) {
    metadata$datasets <- parsed_datasets
  }

  # Clean up empty elements
  metadata <- lapply(metadata, function(x) {
    if (is.list(x) && length(x) > 0) {
      x[!vapply(x, is.null, logical(1))]
    } else {
      x
    }
  })

  # Remove empty top-level elements
  metadata[vapply(metadata, function(x) length(x) > 0, logical(1))]
}

#' Get key from DESCRIPTION file
#'
#' Reads and returns key from DESCRIPTION file
#'
#' @param key Optional key to retrieve specific data (e.g., "datasets")
#' @param base_path Base path for the project
#' @return Metadata list or specific key data if requested
#' @export
get_key <- function(key = NULL, base_path = NULL) {
  base_path <- get_base_path(base_path)
  metadata <- get_metadata(base_path)

  if (!is.null(key)) {
    return(metadata[[key]])
  }

  return(metadata)
}
