#' Prompt for comprehensive metadata interactively
#' 
#' @description
#' General-purpose interactive function to collect metadata for datasets.
#' Supports interactive prompts, partial pre-filling, or full specification.
#' This function is independent of any specific package structure.
#' 
#' @param pkg_name Character string. Package name.
#' @param title Character string. Package title.
#' @param description Character string. Package description (one paragraph).
#' @param version Character string. Package version. Default is "0.0.1".
#' @param language Character string. Language code. Default is "en-GB".
#' @param github_user Character string. GitHub username or organization.
#' @param authors List of lists. Author information including given, family, email, 
#'   orcid, affiliation, and roles.
#' @param license Character string. License identifier (e.g., "CC-BY-4.0", "MIT", "GPL-3").
#' @param keywords Character vector. Keywords describing the dataset.
#' @param funder Character string. Funding organization.
#' @param grant_id Character string. Grant identifier.
#' @param temporal_start Character string. Start date of data coverage (YYYY or YYYY-MM-DD).
#' @param temporal_end Character string. End date of data coverage (YYYY or YYYY-MM-DD).
#' @param spatial_description Character string. Geographic coverage description.
#' @param spatial_coordinates List with lat and lon. Geographic coordinates.
#' @param related_identifiers List of lists. Related publications (DOIs, URLs).
#' @param communities Character vector. Zenodo communities (e.g., "global-health-engineering").
#' @param interactive Logical. Whether to use interactive prompts. Default is TRUE.
#' @param overwrite Logical. Whether to overwrite existing metadata file. Default is FALSE.
#' @param save_to_desc Logical. Whether to save metadata to DESCRIPTION file. Default is TRUE.
#' 
#' @return A list containing all metadata organized by category:
#'   - package: title, description, version, language
#'   - authors: list of author information
#'   - license: id and url
#'   - publication: keywords, funder, grant_id, communities
#'   - coverage: temporal and spatial information
#'   - related: related identifiers
#'   
#' @details
#' This function collects metadata and can optionally save it to the DESCRIPTION file.
#' Standard fields are saved to their respective DESCRIPTION fields, while custom
#' metadata is saved with the Config/ prefix following R package conventions.
#' 
#' @examples
#' \dontrun{
#' # Interactive mode (default)
#' metadata <- prompt4metadata()
#' 
#' # Partial pre-fill
#' metadata <- prompt4metadata(
#'   title = "My Dataset",
#'   authors = list(
#'     list(given = "Jane", family = "Doe", email = "jane@example.com")
#'   )
#' )
#' 
#' # Full specification
#' metadata <- prompt4metadata(
#'   pkg_name = "mydata",
#'   title = "My Dataset",
#'   description = "A comprehensive dataset about...",
#'   interactive = FALSE
#' )
#' 
#' }
#' @export
prompt4metadata <- function(
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
  interactive = TRUE,
  overwrite = FALSE,
  save_to_desc = TRUE,
  base_path = NULL
) {
  
  # Helper function for yes/no prompts
  prompt_yes_no <- function(question, default = TRUE) {
    if (!interactive) return(default)
    
    default_text <- if (default) "Y/n" else "y/N"
    response <- tolower(readline(paste0(question, " [", default_text, "]: ")))
    
    if (response == "") return(default)
    return(response %in% c("y", "yes"))
  }
  
  # Get base path
  if (!is.null(base_path)) {
    base_path <- get_base_path(base_path)
  } else if (save_to_desc) {
    base_path <- get_base_path()
  }
  
  # Check for existing metadata in DESCRIPTION if save_to_desc is TRUE
  if (save_to_desc && !overwrite) {
    desc_path <- file.path(base_path, "DESCRIPTION")
    if (file.exists(desc_path)) {
      # Try to read existing metadata from DESCRIPTION
      existing_meta <- tryCatch({
        get_metadata_from_desc(base_path)
      }, error = function(e) NULL)
      
      if (!is.null(existing_meta) && interactive) {
        cli::cli_alert_info("Metadata already exists in DESCRIPTION")
        if (!prompt_yes_no("Do you want to overwrite it?", default = FALSE)) {
          return(existing_meta)
        }
      } else if (!is.null(existing_meta) && !interactive) {
        # In non-interactive mode, return existing metadata
        return(existing_meta)
      }
    }
  }
  
  # Helper function for interactive prompts
  prompt_field <- function(field_name, current_value, default = NULL, required = FALSE) {
    if (!interactive || (!is.null(current_value) && current_value != "")) {
      return(current_value)
    }
    
    prompt_text <- field_name
    if (!is.null(default)) {
      prompt_text <- paste0(prompt_text, " [", default, "]")
    }
    if (required) {
      prompt_text <- paste0(prompt_text, " (required)")
    }
    prompt_text <- paste0(prompt_text, ": ")
    
    response <- readline(prompt_text)
    
    if (response == "" && !is.null(default)) {
      return(default)
    } else if (response == "" && required) {
      cli::cli_alert_warning("This field is required. Please provide a value.")
      return(prompt_field(field_name, NULL, default, required))
    }
    
    return(response)
  }
  
  cli::cli_h1("Collecting metadata for R data package")
  
  # Package metadata
  cli::cli_h2("Package Information")
  pkg_name <- prompt_field("Package name", pkg_name, required = TRUE)
  title <- prompt_field("Title", title, required = TRUE)
  description <- prompt_field("Description (one paragraph)", description, required = TRUE)
  version <- prompt_field("Version", version, default = version)
  language <- prompt_field("Language", language, default = language)
  github_user <- prompt_field("GitHub user/organization: ", github_user, default = github_user)
  
  # Authors
  cli::cli_h2("Authors")
  if (is.null(authors)) {
    authors <- list()
    add_more <- TRUE
    author_num <- 1
    
    while (add_more && interactive) {
      cli::cli_alert_info(paste("Author", author_num))
      
      given <- prompt_field("  Given name", NULL, required = TRUE)
      family <- prompt_field("  Family name", NULL, required = TRUE)
      email <- prompt_field("  Email", NULL, required = TRUE)
      orcid <- prompt_field("  ORCID (0000-0000-0000-0000)", NULL)
      affiliation <- prompt_field("  Affiliation", NULL, default = "ETH Zurich")
      
      # Role selection
      cli::cli_text("  Roles (select all that apply):")
      cli::cli_text("    [1] aut (Author)")
      cli::cli_text("    [2] cre (Creator/Maintainer)")
      cli::cli_text("    [3] ctb (Contributor)")
      cli::cli_text("    [4] fnd (Funder)")
      cli::cli_text("    [5] ths (Thesis advisor)")
      role_input <- readline("  Enter numbers separated by commas [1,2]: ")
      
      if (role_input == "") role_input <- "1,2"
      role_map <- c("aut", "cre", "ctb", "fnd", "ths")
      selected_roles <- as.numeric(strsplit(role_input, ",")[[1]])
      roles <- role_map[selected_roles]
      
      authors[[author_num]] <- list(
        given = given,
        family = family,
        email = email,
        orcid = if (orcid != "") orcid else NULL,
        affiliation = affiliation,
        roles = roles
      )
      
      author_num <- author_num + 1
      add_more <- prompt_yes_no("\nAdd another author?", default = FALSE)
    }
  }
  
  # License
  cli::cli_h2("License")
  if (is.null(license) && interactive) {
    cli::cli_text("Select a license:")
    cli::cli_text("  [1] CC-BY-4.0 (Creative Commons Attribution)")
    cli::cli_text("  [2] CC0-1.0 (Public Domain)")
    cli::cli_text("  [3] MIT")
    cli::cli_text("  [4] GPL-3")
    cli::cli_text("  [5] Apache-2.0")
    cli::cli_text("  [6] Other")
    
    license_choice <- readline("Enter number [1]: ")
    if (license_choice == "") license_choice <- "1"
    
    license_map <- c("CC-BY-4.0", "CC0-1.0", "MIT", "GPL-3", "Apache-2.0")
    
    if (license_choice == "6") {
      license <- prompt_field("Enter license identifier", NULL, required = TRUE)
    } else {
      license <- license_map[as.numeric(license_choice)]
    }
  }
  
  # Get license URL
  license_urls <- list(
    "CC-BY-4.0" = "https://creativecommons.org/licenses/by/4.0/",
    "CC0-1.0" = "https://creativecommons.org/publicdomain/zero/1.0/",
    "MIT" = "https://opensource.org/licenses/MIT",
    "GPL-3" = "https://www.gnu.org/licenses/gpl-3.0.html",
    "Apache-2.0" = "https://www.apache.org/licenses/LICENSE-2.0"
  )
  
  # Use the URL if it exists, otherwise NULL
  license_url <- if (!is.null(license) && license %in% names(license_urls)) {
    license_urls[[license]]
  } else {
    NULL
  }
  
  # Publication metadata
  cli::cli_h2("Publication Information")
  
  # Keywords
  if (is.null(keywords) && interactive) {
    keywords_input <- prompt_field("Keywords (comma-separated)", NULL)
    if (keywords_input != "") {
      keywords <- trimws(strsplit(keywords_input, ",")[[1]])
    }
  }
  
  funder <- prompt_field("Funding organization", funder, 
                        default = "ETH Domain ORD Programme")
  grant_id <- prompt_field("Grant ID", grant_id)
  
  # Communities
  if (is.null(communities) && interactive) {
    use_communities <- prompt_yes_no("Add to Zenodo communities?", default = TRUE)
    if (use_communities) {
      communities_input <- prompt_field("Communities (comma-separated)", NULL,
                                      default = "global-health-engineering")
      if (communities_input != "") {
        communities <- trimws(strsplit(communities_input, ",")[[1]])
      }
    }
  }
  
  # Coverage
  cli::cli_h2("Data Coverage")
  
  # Temporal
  if (interactive && (is.null(temporal_start) || is.null(temporal_end))) {
    has_temporal <- prompt_yes_no("Does the data have temporal coverage?", default = TRUE)
    if (has_temporal) {
      temporal_start <- prompt_field("Start date (YYYY or YYYY-MM-DD)", temporal_start)
      temporal_end <- prompt_field("End date (YYYY or YYYY-MM-DD)", temporal_end)
    }
  }
  
  # Spatial
  if (interactive && is.null(spatial_description)) {
    has_spatial <- prompt_yes_no("Does the data have geographic coverage?", default = FALSE)
    if (has_spatial) {
      spatial_description <- prompt_field("Geographic description", spatial_description)
      
      has_coords <- prompt_yes_no("Add coordinates?", default = FALSE)
      if (has_coords && is.null(spatial_coordinates)) {
        lat <- as.numeric(prompt_field("Latitude", NULL))
        lon <- as.numeric(prompt_field("Longitude", NULL))
        spatial_coordinates <- list(lat = lat, lon = lon)
      }
    }
  }
  
  # Related identifiers
  cli::cli_h2("Related Publications")
  if (is.null(related_identifiers) && interactive) {
    has_related <- prompt_yes_no("Are there related publications?", default = FALSE)
    if (has_related) {
      related_identifiers <- list()
      add_more <- TRUE
      rel_num <- 1
      
      while (add_more) {
        doi <- prompt_field(paste("  DOI", rel_num), NULL)
        if (doi != "") {
          related_identifiers[[rel_num]] <- list(
            scheme = "doi",
            identifier = doi,
            relation = "isDocumentedBy",
            resource_type = "publication-article"
          )
          rel_num <- rel_num + 1
          add_more <- prompt_yes_no("  Add another?", default = FALSE)
        } else {
          add_more <- FALSE
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
      } else NULL,
      spatial = if (!is.null(spatial_description)) {
        list(
          description = spatial_description,
          coordinates = spatial_coordinates
        )
      } else NULL
    ),
    related = related_identifiers
  )
  
  # Remove NULL elements from nested lists
  metadata <- lapply(metadata, function(x) {
    if (is.list(x) && !is.data.frame(x)) {
      x[!sapply(x, is.null)]
    } else {
      x
    }
  })
  
  cli::cli_alert_success("Metadata collection complete!")
  
  # Show summary
  if (interactive) {
    show_summary <- prompt_yes_no("\nShow metadata summary?", default = TRUE)
    if (show_summary) {
      cli::cli_h2("Metadata Summary")
      cli::cli_text("Title: {metadata$package$title}")
      cli::cli_text("Authors: {length(metadata$authors)}")
      cli::cli_text("License: {metadata$license$id}")
      if (!is.null(metadata$publication$keywords)) {
        cli::cli_text("Keywords: {paste(metadata$publication$keywords, collapse = ', ')}")
      }
    }
  }
  
  # Save metadata to DESCRIPTION if requested
  if (save_to_desc) {
    write_metadata_to_desc(metadata, base_path, overwrite = overwrite, verbose = TRUE)
  }
  
  return(metadata)
}