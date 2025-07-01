#' Update the citation file for the dataset.
#' @description
#' Create a citation *.cff file for the released dataset from a given DOI(Digital
#' Object Identifier). It adds the DOI badge to the README RMarkdown file and
#' re-build the README.md and pkgdown website if exists.
#' @param doi DOI(Digital Object Identifier), e.g., 10.5281/zenodo.11185699
#' @returns NULL. A citation .cff file is written under the root directory.
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#'   update_citation(doi = "10.5281/zenodo.11185699")
#' }
#' \dontshow{
#' setwd(.old_wd)
#' }
update_citation <- function(doi){
  # Creates CFF with all author roles
  mod_cff <- cffr::cff_create("DESCRIPTION",
                        dependencies = FALSE,
                        keys = list("doi" = doi,
                                    "date-released" = desc::desc_get("Date")))

  # Remove the preferred-citation key
  mod_cff$`preferred-citation` <- NULL

  # Writes the CFF file
  cffr::cff_write(mod_cff)

  # Now write a CITATION file from the CITATION.cff file
  # Use inst/CITATION instead (the default if not provided)
  path_cit <- file.path("inst/CITATION")

  a_cff <- cffr::cff_read(path = "CITATION.cff")

  cffr::cff_write_citation(a_cff, file = path_cit)

  # Modify README and pkgdown
  if(file.exists(file.path("README.Rmd"))){
    add_citation_badge(doi)
    devtools::build_readme()
  }

  if(dir.exists(file.path("docs"))){
    devtools::build_site()
  }

  # By last, read the citation
  usethis::ui_todo("Proofread your citation file at {usethis::ui_value(path_cit)}")
}

add_citation_badge <- function(doi) {
  badge_icon <- paste0("https://zenodo.org/badge/DOI/", doi, ".svg")
  zenodo_link <- paste0("https://zenodo.org/doi/", doi)
  badge_str <- sprintf("[![DOI](%s)](%s)", badge_icon, zenodo_link) # Badge string
  readme_rmd_path <- file.path("README.Rmd")
  readme_rmd <- readLines(readme_rmd_path)

  # Find the position of the <!-- badges: end --> marker
  i <- 1
  line <- readme_rmd[1]
  while (!startsWith(line, prefix = "<!-- badges: end -->")) {
    i <- i + 1
    line <- readme_rmd[i]
  }

  # Add the badge before the marker and a newline
  new_readme_rmd <- c(readme_rmd[1:(i - 1)], badge_str, "", readme_rmd[i:length(readme_rmd)])
  writeLines(new_readme_rmd, readme_rmd_path)
}

