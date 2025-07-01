create_zenodo_json <- function(
  creators = NULL,
  license = NULL,
  title = NULL,
  related_identifiers = NULL,
  keywords = NULL,
  communities = NULL,
  grants = NULL,
  filename = ".zenodo.json"
) {
  
  # Check if creators is a list, if not, convert it to a list
  if (!is.null(creators) && !is.list(creators)) {
    stop("Creators must be a list of lists")
  }
  
  # Check if related_identifiers is a list, if not, convert it to a list
  if (!is.null(related_identifiers) && !is.list(related_identifiers)) {
    stop("Related identifiers must be a list of lists")
  }
  
  # Check if communities is a list, if not, convert it to a list
  if (!is.null(communities) && !is.list(communities)) {
    stop("Communities must be a list of lists")
  }
  
  # Check if grants is a list, if not, convert it to a list
  if (!is.null(grants) && !is.list(grants)) {
    stop("Grants must be a list of lists")
  }
  
  # Create the zenodo json object
  zenodo_json <- list(
    creators = creators,
    license = license,
    title = title,
    related_identifiers = related_identifiers,
    keywords = keywords,
    communities = communities,
    grants = grants
  )
  
  # Remove any NULL values from the zenodo json object
  zenodo_json <- zenodo_json[sapply(zenodo_json, function(x) !is.null(x))]
  
  # Write the zenodo json object to a file
  jsonlite::write_json(zenodo_json, filename, pretty = TRUE, auto_unbox = TRUE)
}

# creators <- list(
#   list(orcid = aut1$ORCID[1], affiliation = aut1$affiliation, name = paste0(aut1$given, ", ", aut1$family))
# )
# 
# related_identifiers <- list(
#   list(scheme = "doi", identifier = "10.1234/software.paper.5678", relation = "isDocumentedBy", resource_type = "publication-article")
# )
# 
# communities <- list(
#   list(identifier = "global-health-engineering")
# )
# # maybe?
# 
# grants <- list(
#   list(id = "777541")
# )