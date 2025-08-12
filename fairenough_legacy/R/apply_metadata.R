#' Apply metadata to all package files
#' 
#' @description
#' Takes metadata collected by doc_metadata() and applies it to all relevant
#' package files including DESCRIPTION, metadata CSVs, JSON-LD, and Zenodo JSON.
#' 
#' @param metadata List. Metadata object returned by doc_metadata().
#' @param overwrite Logical. Whether to overwrite existing metadata files. Default is TRUE.
#' @param create_zenodo Logical. Whether to create .zenodo.json file. Default is TRUE.
#' 
#' @return NULL (invisibly). Updates files as side effects.
#' 
#' @examples
#' \dontrun{
#' # First collect metadata
#' metadata <- doc_metadata()
#' 
#' # Then apply it to all files
#' apply_metadata(metadata)
#' }
#' @export
apply_metadata <- function(metadata, overwrite = TRUE, create_zenodo = TRUE) {
  
  # Validate metadata structure
  required_fields <- c("package", "authors", "license", "publication", "coverage")
  missing_fields <- setdiff(required_fields, names(metadata))
  if (length(missing_fields) > 0) {
    cli::cli_abort("Missing required metadata fields: {missing_fields}")
  }
  
  cli::cli_h1("Applying metadata to package files")
  
  # 1. Update DESCRIPTION file
  cli::cli_h2("Updating DESCRIPTION")
  tryCatch({
    update_description_from_metadata(metadata)
    cli::cli_alert_success("DESCRIPTION file updated")
  }, error = function(e) {
    cli::cli_alert_warning("Failed to update DESCRIPTION: {e$message}")
  })
  
  # 2. Create/update metadata directory structure
  cli::cli_h2("Creating metadata structure")
  tryCatch({
    if (!dir.exists("data/metadata") || overwrite) {
      create_metadata_structure_from_metadata(metadata)
      cli::cli_alert_success("Metadata structure created")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Failed to create metadata structure: {e$message}")
  })
  
  # 3. Update metadata CSV files
  cli::cli_h2("Updating metadata files")
  
  # Update creators.csv
  tryCatch({
    update_creators_csv(metadata$authors)
    cli::cli_alert_success("creators.csv updated")
  }, error = function(e) {
    cli::cli_alert_warning("Failed to update creators.csv: {e$message}")
  })
  
  # Update biblio.csv
  tryCatch({
    update_biblio_csv(metadata)
    cli::cli_alert_success("biblio.csv updated")
  }, error = function(e) {
    cli::cli_alert_warning("Failed to update biblio.csv: {e$message}")
  })
  
  # 4. Create .zenodo.json if requested
  if (create_zenodo) {
    cli::cli_h2("Creating Zenodo metadata")
    tryCatch({
      create_zenodo_from_metadata(metadata)
      cli::cli_alert_success(".zenodo.json created")
    }, error = function(e) {
      cli::cli_alert_warning("Failed to create .zenodo.json: {e$message}")
    })
  }
  
  # 5. Add to .Rbuildignore
  tryCatch({
    add_metadata_to_rbuildignore()
  }, error = function(e) {
    cli::cli_alert_warning("Failed to update .Rbuildignore: {e$message}")
  })
  
  cli::cli_alert_success("Metadata application complete!")
  
  return(invisible(NULL))
}

# Helper function to update DESCRIPTION from metadata
update_description_from_metadata <- function(metadata) {
  
  # Check if DESCRIPTION exists
  if (!file.exists("DESCRIPTION")) {
    cli::cli_abort("DESCRIPTION file not found. Run setup_roxygen() first.")
  }
  
  # Update basic fields
  desc::desc_set("Title", metadata$package$title)
  desc::desc_set("Description", metadata$package$description)
  desc::desc_set("Version", metadata$package$version)
  desc::desc_set("Language", metadata$package$language)
  desc::desc_set("Date", Sys.Date())
  
  # Handle license
  if (!is.null(metadata$license$id)) {
    if (metadata$license$id == "CC-BY-4.0") {
      usethis::use_ccby_license()
    } else {
      desc::desc_set("License", metadata$license$id)
    }
  }
  
  # Update authors
  desc::desc_del_author()
  for (author in metadata$authors) {
    usethis::use_author(
      given = author$given,
      family = author$family,
      role = author$roles,
      email = author$email,
      comment = if (!is.null(author$orcid)) c(ORCID = author$orcid) else NULL
    )
  }
  
  # Add custom fields
  if (!is.null(metadata$publication$funder)) {
    desc::desc_set("Funder", metadata$publication$funder)
  }
  
  # Add LazyData
  desc::desc_set("LazyData", "true")
  desc::desc_set("Config/Needs/website", "rmarkdown")
}

# Helper function to create metadata structure
create_metadata_structure_from_metadata <- function(metadata) {
  
  # Create metadata directory
  metadata_dir <- "data/metadata"
  fs::dir_create(metadata_dir, recurse = TRUE)
  
  # Create empty CSV files with proper structure
  # These will be populated by the update functions
  
  # attributes.csv
  attributes_df <- tibble::tibble(
    fileName = character(0),
    variableName = character(0),
    description = character(0),
    unitText = character(0)
  )
  readr::write_csv(attributes_df, file.path(metadata_dir, "attributes.csv"))
  
  # access.csv
  access_df <- tibble::tibble(
    fileName = character(0),
    name = character(0),
    contentUrl = character(0),
    fileFormat = character(0)
  )
  readr::write_csv(access_df, file.path(metadata_dir, "access.csv"))
  
  # creators.csv - will be populated by update_creators_csv
  creators_df <- tibble::tibble(
    id = numeric(0),
    name = character(0),
    affiliation = character(0),
    email = character(0)
  )
  readr::write_csv(creators_df, file.path(metadata_dir, "creators.csv"))
  
  # biblio.csv - will be populated by update_biblio_csv
  biblio_df <- tibble::tibble(
    title = character(0),
    description = character(0),
    datePublished = character(0),
    citation = character(0),
    keywords = character(0),
    license = character(0),
    funder = character(0),
    temporalCoverage = character(0),
    spatialCoverage = character(0)
  )
  readr::write_csv(biblio_df, file.path(metadata_dir, "biblio.csv"))
}

# Helper function to update creators.csv
update_creators_csv <- function(authors) {
  
  creators_path <- "data/metadata/creators.csv"
  
  # Create data frame from authors list
  creators_df <- tibble::tibble(
    id = seq_along(authors),
    name = sapply(authors, function(a) paste(a$given, a$family)),
    affiliation = sapply(authors, function(a) a$affiliation %||% ""),
    email = sapply(authors, function(a) a$email %||% "")
  )
  
  # Write to CSV
  readr::write_csv(creators_df, creators_path)
}

# Helper function to update biblio.csv
update_biblio_csv <- function(metadata) {
  
  biblio_path <- "data/metadata/biblio.csv"
  
  # Extract temporal coverage string
  temporal_coverage <- NULL
  if (!is.null(metadata$coverage$temporal)) {
    temporal_coverage <- paste(
      metadata$coverage$temporal$start,
      metadata$coverage$temporal$end,
      sep = " - "
    )
  }
  
  # Extract spatial coverage string
  spatial_coverage <- NULL
  if (!is.null(metadata$coverage$spatial)) {
    spatial_coverage <- metadata$coverage$spatial$description
  }
  
  # Create biblio data frame
  biblio_df <- tibble::tibble(
    title = metadata$package$title,
    description = metadata$package$description,
    datePublished = as.character(Sys.Date()),
    citation = "",  # Will be filled by update_citation()
    keywords = if (!is.null(metadata$publication$keywords)) {
      paste(metadata$publication$keywords, collapse = ", ")
    } else "",
    license = metadata$license$id,
    funder = metadata$publication$funder %||% "",
    temporalCoverage = temporal_coverage %||% "",
    spatialCoverage = spatial_coverage %||% ""
  )
  
  # Write to CSV
  readr::write_csv(biblio_df, biblio_path)
}

# Helper function to create .zenodo.json
create_zenodo_from_metadata <- function(metadata) {
  
  # Transform authors to Zenodo format
  creators <- lapply(metadata$authors, function(author) {
    list(
      name = paste(author$family, author$given, sep = ", "),
      affiliation = author$affiliation,
      orcid = author$orcid
    )
  })
  
  # Transform communities
  communities <- NULL
  if (!is.null(metadata$publication$communities)) {
    communities <- lapply(metadata$publication$communities, function(comm) {
      list(identifier = comm)
    })
  }
  
  # Transform related identifiers
  related_identifiers <- metadata$related
  
  # Create Zenodo JSON
  create_zenodo_json(
    creators = creators,
    license = metadata$license$id,
    title = metadata$package$title,
    related_identifiers = related_identifiers,
    keywords = metadata$publication$keywords,
    communities = communities,
    grants = if (!is.null(metadata$publication$grant_id)) {
      list(list(id = metadata$publication$grant_id))
    } else NULL
  )
}

# Helper function to add metadata to .Rbuildignore
add_metadata_to_rbuildignore <- function() {
  
  rbuildignore_path <- ".Rbuildignore"
  new_pattern <- "^data/metadata$"
  
  if (file.exists(rbuildignore_path)) {
    lines <- readLines(rbuildignore_path)
    if (!(new_pattern %in% lines)) {
      writeLines(c(lines, new_pattern), rbuildignore_path)
    }
  }
}

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x