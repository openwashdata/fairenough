#' Utility functions for fairenough

#' Get base path with consistent handling across all functions
#' 
#' This function provides a consistent way to handle base_path across all fairenough functions.
#' If base_path is provided, it sets the global option and returns the normalized path.
#' If base_path is NULL, it checks for the global option, falling back to here::here() or "."
#' 
#' @param base_path Optional base path to set
#' @return Normalized base path
#' @export
get_base_path <- function(base_path = NULL) {
  # If base_path is provided, set it as option and use it
  if (!is.null(base_path)) {
    normalized_path <- normalizePath(base_path, mustWork = TRUE)
    options(fairenough.base_path = normalized_path)
    return(normalized_path)
  }
  
  # Otherwise, check for existing option
  stored_path <- getOption("fairenough.base_path")
  if (!is.null(stored_path)) {
    return(stored_path)
  }
  
  # Fall back to here::here() or current directory
  default_path <- tryCatch(
    here::here(),
    error = function(e) {
      message("No project root found, using current directory")
      "."
    }
  )
  
  normalized_path <- normalizePath(default_path, mustWork = TRUE)
  options(fairenough.base_path = normalized_path)
  return(normalized_path)
}