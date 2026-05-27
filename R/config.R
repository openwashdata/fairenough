# Package-private state and path/option helpers shared across fairenough
# calls within a session.

# Package-private cache for path state shared across fairenough calls within
# a session. CRAN forbids mutating global options() for persistent state, so
# we keep it in the package namespace instead.
.fairenough_state <- new.env(parent = emptyenv())

#' Get base path with consistent handling across all functions
#'
#' Provides a consistent way to handle `base_path` across all fairenough
#' functions. If `base_path` is provided, it is normalized, cached in a
#' package-private environment, and returned. If `base_path` is `NULL`, the
#' cached value is returned when set, otherwise the function falls back to
#' [here::here()] or the current directory.
#'
#' @param base_path Optional base path to set
#' @return Normalized base path
#' @keywords internal
get_base_path <- function(base_path = NULL) {
  if (!is.null(base_path)) {
    normalized_path <- normalizePath(base_path, mustWork = TRUE)
    .fairenough_state$base_path <- normalized_path
    return(normalized_path)
  }

  stored_path <- .fairenough_state$base_path
  if (!is.null(stored_path)) {
    return(stored_path)
  }

  default_path <- tryCatch(
    here::here(),
    error = function(e) {
      message("No project root found, using current directory")
      "."
    }
  )

  normalized_path <- normalizePath(default_path, mustWork = TRUE)
  .fairenough_state$base_path <- normalized_path
  return(normalized_path)
}

#' Get raw directory path with consistent handling across all functions
#'
#' Provides a consistent way to handle `raw_dir` across all fairenough
#' functions. If `raw_dir` is provided, it is cached in a package-private
#' environment and returned. If `raw_dir` is `NULL`, the cached value is
#' returned when set, otherwise `"data_raw"` is used as the default.
#'
#' @param raw_dir Optional base path to set
#' @return Normalized base path
#' @keywords internal
get_raw_dir <- function(raw_dir = NULL) {
  if (!is.null(raw_dir)) {
    .fairenough_state$raw_dir <- raw_dir
    return(raw_dir)
  }

  stored_path <- .fairenough_state$raw_dir
  if (!is.null(stored_path)) {
    return(stored_path)
  }

  default_path <- "data_raw"
  .fairenough_state$raw_dir <- default_path
  return(default_path)
}
