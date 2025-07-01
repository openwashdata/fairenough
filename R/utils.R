#' Utility functions for fairenough

#' Load object from file
#' 
#' @importFrom utils head
#' 
#' @param file Path to the R data file
#' @return The loaded object
load_object <- function(file) {
  tmp_env <- new.env()
  load(file = file, envir = tmp_env)
  tmp_env[[ls(tmp_env)[1]]]
}

#' Check if current directory is a package directory
#' 
#' @return Logical indicating if current directory contains DESCRIPTION and NAMESPACE files
is_pkg <- function(){
  return(file.exists(file.path(getwd(), "DESCRIPTION")) &&
           file.exists(file.path(getwd(), "NAMESPACE"))
  )
}