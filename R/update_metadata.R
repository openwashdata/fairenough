
#' Updates all metadata files simultaneously
#'
#' @description This function updates all metadata files simultaneously by calling helper
#' functions. Ideally, this function is used rather than individual helpers.
#'
#' @returns NULL. Error handling is done in the helper functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' update_metadata()
#' }
#'
update_metadata <- function(){

  update_biblio()
  cat("-------------------------------------------------------------------------")
  update_attributes()
}
