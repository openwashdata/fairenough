# =============================================================================
# usethis License Wrapper - Regex Pattern Matching Approach
# A unified interface for usethis license functions with regex-based matching
# =============================================================================

#' Supported License Names
#'
#' A character vector of canonical license names supported by this wrapper.
#'
#' @export
SUPPORTED_LICENSES <- c(
  "MIT",
  "Apache-2.0",
  "GPL-2",
  "GPL-3",
  "AGPL-3",
  "LGPL-2.1",
  "LGPL-3",
  "CC0",
  "CC-BY",
  "Proprietary"
)

#' Get Available License Names
#'
#' Returns a character vector of all supported license names.
#'
#' @return Character vector of supported license names
#' @export
#' @examples
#' get_available_licenses()
get_available_licenses <- function() {
  SUPPORTED_LICENSES
}

#' Build License
#'
#' A unified interface to usethis license functions. Uses match_license() to map
#' license input to a supported license, then calls the appropriate usethis function.
#'
#' @param license Character string specifying the license name.
#'   Supports common variations and is case-insensitive.
#' @param copyright_holder Character string specifying the copyright holder.
#'   For MIT and proprietary licenses. Defaults to "{package name} authors".
#' @param version Numeric version number for GPL, AGPL, LGPL, and Apache licenses.
#'   Defaults to the latest version for each license type.
#' @param include_future Logical indicating whether to include future versions.
#'   Only applicable to GPL, AGPL, LGPL, and Apache licenses. Default is TRUE.
#' @param ... Additional arguments passed to the underlying usethis function.
#'
#' @return Invisible TRUE if successful, stops with error if license not found.
#'
#' @export
build_license <- function(
  license,
  copyright_holder = NULL,
  version = NULL,
  include_future = TRUE,
  ...
) {
  # Check if usethis is available
  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop(
      "The 'usethis' package is required but not installed. Please install it with: install.packages('usethis')",
      call. = FALSE
    )
  }

  # Validate input
  if (is.null(license) || is.na(license) || trimws(license) == "") {
    stop("License cannot be NULL, NA, or empty string.", call. = FALSE)
  }

  # Use match_license to map to a supported license
  matched_license <- match_license(license)

  # If no match found, provide helpful error
  if (is.null(matched_license)) {
    available <- paste(get_available_licenses(), collapse = ", ")
    stop(
      "Unknown license: '",
      license,
      "'.\n",
      "Available licenses: ",
      available,
      "\n",
      "Use get_available_licenses() to see all supported licenses.",
      call. = FALSE
    )
  }

  # Call appropriate usethis function based on matched license
  switch(
    matched_license,
    "MIT" = {
      usethis::use_mit_license(copyright_holder = copyright_holder, ...)
    },

    "Apache-2.0" = {
      version <- version %||% 2
      usethis::use_apache_license(
        version = version,
        include_future = include_future,
        ...
      )
    },

    "GPL-2" = {
      usethis::use_gpl_license(
        version = 2,
        include_future = include_future,
        ...
      )
    },

    "GPL-3" = {
      usethis::use_gpl_license(
        version = 3,
        include_future = include_future,
        ...
      )
    },

    "AGPL-3" = {
      version <- version %||% 3
      usethis::use_agpl_license(
        version = version,
        include_future = include_future,
        ...
      )
    },

    "LGPL-2.1" = {
      usethis::use_lgpl_license(
        version = 2.1,
        include_future = include_future,
        ...
      )
    },

    "LGPL-3" = {
      version <- version %||% 3
      usethis::use_lgpl_license(
        version = version,
        include_future = include_future,
        ...
      )
    },

    "CC0" = {
      usethis::use_cc0_license(...)
    },

    "CC-BY" = {
      usethis::use_ccby_license(...)
    },

    "Proprietary" = {
      usethis::use_proprietary_license(copyright_holder = copyright_holder, ...)
    },

    # This should never happen since match_license only returns valid licenses or NULL
    stop(
      "Internal error: matched license '",
      matched_license,
      "' not implemented.",
      call. = FALSE
    )
  )

  invisible(TRUE)
}

#' Match License to Supported License
#'
#' Helper function that maps a license string to one of the SUPPORTED_LICENSES elements.
#' This is the core pattern matching function used by build_license().
#'
#' @param license Character string to match
#' @return Character string matching one of SUPPORTED_LICENSES, or NULL if no match found
#'
#' @examples
#' match_license("MIT")           # "MIT"
#' match_license("gpl v3")        # "GPL-3"
#' match_license("cc-by-4.0")     # "CC-BY"
#' match_license("unknown")       # NULL
match_license <- function(license) {
  if (is.null(license) || is.na(license) || trimws(license) == "") {
    return(NULL)
  }

  # Pattern matching logic - order matters for precedence
  if (
    grepl(
      "CC-?BY-?4(\\.0)?|CC BY 4(\\.0)?|creative commons by",
      license,
      ignore.case = TRUE
    )
  ) {
    return("CC-BY")
  } else if (
    grepl("CC-?0|CC-?ZERO|public domain", license, ignore.case = TRUE)
  ) {
    return("CC0")
  } else if (grepl("MIT", license, ignore.case = TRUE)) {
    return("MIT")
  } else if (grepl("APACHE", license, ignore.case = TRUE)) {
    return("Apache-2.0")
  } else if (grepl("AGPL", license, ignore.case = TRUE)) {
    return("AGPL-3")
  } else if (grepl("LGPL", license, ignore.case = TRUE)) {
    if (grepl("2", license)) {
      return("LGPL-2.1")
    } else {
      return("LGPL-3")
    }
  } else if (grepl("GPL", license, ignore.case = TRUE)) {
    if (grepl("2", license)) {
      return("GPL-2")
    } else {
      return("GPL-3")
    }
  } else if (
    grepl(
      "PROPRIETARY|CLOSED|PRIVATE|ALL RIGHTS RESERVED",
      license,
      ignore.case = TRUE
    )
  ) {
    return("Proprietary")
  }

  return(NULL)
}
