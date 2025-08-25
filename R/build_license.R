# =============================================================================
# usethis License Wrapper - Regex Pattern Matching Approach
# A unified interface for usethis license functions with regex-based matching
# =============================================================================

#' @importFrom utils getFromNamespace
#' @importFrom stats setNames
NULL

#' License Configuration
#'
#' Centralized configuration for all supported licenses.
#' Makes it easy to add/remove licenses in one place.
#'
#' @format A list containing license metadata
LICENSE_CONFIG <- list(
  # License definitions with metadata
  definitions = list(
    MIT = list(
      canonical = "MIT",
      patterns = c("MIT", "mit license"),
      usethis_func = "use_mit_license",
      requires_copyright = TRUE,
      description = "MIT License"
    ),

    `Apache-2.0` = list(
      canonical = "Apache-2.0",
      patterns = c("APACHE", "apache 2", "apache-2.0"),
      usethis_func = "use_apache_license",
      requires_version = TRUE,
      description = "Apache License 2.0"
    ),

    `GPL-2` = list(
      canonical = "GPL-2",
      patterns = c("GPL.*2", "gnu gpl v?2"),
      usethis_func = "use_gpl_license",
      version = 2,
      description = "GNU General Public License v2"
    ),

    `GPL-3` = list(
      canonical = "GPL-3",
      patterns = c("GPL(?!.*2)", "gnu gpl v?3", "gpl$"),
      usethis_func = "use_gpl_license",
      version = 3,
      description = "GNU General Public License v3"
    ),

    `AGPL-3` = list(
      canonical = "AGPL-3",
      patterns = c("AGPL", "affero gpl"),
      usethis_func = "use_agpl_license",
      version = 3,
      description = "GNU Affero General Public License v3"
    ),

    `LGPL-2.1` = list(
      canonical = "LGPL-2.1",
      patterns = c("LGPL.*2", "lesser gpl.*2"),
      usethis_func = "use_lgpl_license",
      version = 2.1,
      description = "GNU Lesser General Public License v2.1"
    ),

    `LGPL-3` = list(
      canonical = "LGPL-3",
      patterns = c("LGPL(?!.*2)", "lesser gpl.*3", "lgpl$"),
      usethis_func = "use_lgpl_license",
      version = 3,
      description = "GNU Lesser General Public License v3"
    ),

    CC0 = list(
      canonical = "CC0",
      patterns = c("CC-?0", "CC-?ZERO", "public domain"),
      usethis_func = "use_cc0_license",
      description = "Creative Commons Zero"
    ),

    `CC-BY` = list(
      canonical = "CC-BY",
      patterns = c("CC-?BY", "creative commons by", "cc by 4"),
      usethis_func = "use_ccby_license",
      description = "Creative Commons Attribution"
    ),

    Proprietary = list(
      canonical = "Proprietary",
      patterns = c("PROPRIETARY", "CLOSED", "PRIVATE", "ALL RIGHTS RESERVED"),
      usethis_func = "use_proprietary_license",
      requires_copyright = TRUE,
      description = "Proprietary License"
    )
  )
)

#' Get Supported License Names
#'
#' Extract supported license names from configuration.
#'
#' @param format Character. Either "canonical" (default), "description", or "both"
#' @return Character vector of license names
#' @export
get_available_licenses <- function(
  format = c("canonical", "description", "both")
) {
  format <- match.arg(format)

  defs <- LICENSE_CONFIG$definitions

  switch(
    format,
    canonical = names(defs),
    description = vapply(defs, function(x) x$description, character(1)),
    both = {
      descriptions <- vapply(defs, function(x) x$description, character(1))
      setNames(names(defs), descriptions)
    }
  )
}

#' Get License Configuration
#'
#' Internal helper to get license configuration by canonical name.
#'
#' @param license Character string of canonical license name
#' @return List with license configuration, or NULL if not found
get_license_config <- function(license) {
  LICENSE_CONFIG$definitions[[license]]
}

#' Match License to License Config
#'
#' This is the core pattern matching function used by build_license().
#' Uses the centralized configuration for pattern matching.
#'
#' @param license Character string to match
#' @return Character string matching a canonical license, or NULL if no match
#' @export
#' @examples
#' match_license("MIT")           # "MIT"
#' match_license("gpl v3")        # "GPL-3"
#' match_license("apache 2.0")    # "Apache-2.0"
match_license <- function(license) {
  if (is.null(license) || is.na(license) || trimws(license) == "") {
    return(NULL)
  }

  # Clean input
  license_clean <- trimws(license)

  # Check each license definition
  for (canonical_name in names(LICENSE_CONFIG$definitions)) {
    config <- LICENSE_CONFIG$definitions[[canonical_name]]

    # Check against all patterns for this license
    for (pattern in config$patterns) {
      if (grepl(pattern, license_clean, ignore.case = TRUE, perl = TRUE)) {
        return(canonical_name)
      }
    }
  }

  return(NULL)
}

#' Build License
#'
#' A unified interface to usethis license functions. Uses match_license() to map
#' license input to a supported license, then calls the appropriate usethis function.
#'
#' @param license Character string specifying the license name
#' @param ... Additional arguments passed to underlying usethis function
#' @return Invisible TRUE if successful
#' @export
build_license <- function(license, ...) {
  # Check if usethis is available
  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop("The 'usethis' package is required but not installed.", call. = FALSE)
  }

  # Validate and match license
  if (is.null(license) || is.na(license) || trimws(license) == "") {
    stop("License cannot be NULL, NA, or empty string.", call. = FALSE)
  }

  matched_license <- match_license(license)
  if (is.null(matched_license)) {
    available <- paste(get_available_licenses("description"), collapse = "\n  ")
    stop(
      "Unknown license: '",
      license,
      "'.\n",
      "Available licenses:\n  ",
      available,
      call. = FALSE
    )
  }

  # Get configuration and call appropriate function
  config <- get_license_config(matched_license)
  func_name <- config$usethis_func

  # Build arguments based on configuration
  args <- list(...)

  # Add version if specified in config
  if (!is.null(config$version) && is.null(args$version)) {
    args$version <- config$version
  }

  # Call the usethis function
  do.call(getFromNamespace(func_name, "usethis"), args)

  invisible(TRUE)
}

#' Validate License Configuration
#'
#' Internal function to validate the license configuration structure.
#' Useful for validation of changes in LICENSE_CONFIG and development.
#'
#' @return Logical indicating if configuration is valid
#' @keywords internal
validate_license_config <- function() {
  required_fields <- c("canonical", "patterns", "usethis_func", "description")

  for (license_name in names(LICENSE_CONFIG$definitions)) {
    config <- LICENSE_CONFIG$definitions[[license_name]]

    # Check required fields exist
    missing_fields <- setdiff(required_fields, names(config))
    if (length(missing_fields) > 0) {
      warning(
        "License '",
        license_name,
        "' missing fields: ",
        paste(missing_fields, collapse = ", ")
      )
      return(FALSE)
    }

    # Validate canonical name matches key
    if (config$canonical != license_name) {
      warning("License '", license_name, "' canonical name mismatch")
      return(FALSE)
    }
  }

  TRUE
}
