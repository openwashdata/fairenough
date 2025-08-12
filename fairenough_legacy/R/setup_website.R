#' Set up a pkgdown website for the data package
#' @description
#' `setup_website()` uses the openwashdata pkgdown template to create a website for the data package
#' based on its README.md file. The website provides a structured and visually appealing presentation
#' of the package's documentation.
#' @param has_example Logical. Should the pkgdown website include a vignette page
#' for writing an example? Defaults to FALSE.
#' @returns NULL. Error if no README file is found.
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' # Set up the pkgdown website including a vignette page
#'  setup_website(has_example = TRUE)
#' }
#' \dontshow{
#' setwd(.old_wd)
#' }
setup_website <- function(has_example=FALSE){
  # Check on README file
  if (is_readme_available()) {
    # Add configuration file from fairenough templates
    name <- desc::desc_get("Package")[[1]]
    configpath <- "_pkgdown.yml"
    if (file.exists(configpath) && dir.exists("docs/")) {
      usethis::ui_info("Pkgdown folder exists. This run will update the README and pkgdown website.")
    } else {
      usethis::use_pkgdown(config_file = configpath)
      file.remove(configpath)
      usethis::use_template(template = "_pkgdown.yml",
                            save_as = configpath,
                            data = list(name = name),
                            ignore = FALSE,
                            open = FALSE,
                            package = "fairenough")
    }

    # Create example vignettes
    if (has_example) {
      usethis::use_article("examples")
      devtools::build_rmd("vignettes/articles/examples.Rmd")
    }
    pkgdown::build_site()
    # remove docs/ in gitignore
    gitignorepath <- file.path(".gitignore")
    gitignore <- readLines(gitignorepath)
    writeLines(setdiff(gitignore, "docs"),
               gitignorepath)
  } else {
    usethis::ui_stop("No README.md exists. Consider to set up and write README first. You may use fairenough::setup_readme()")
  }
}

is_readme_available <- function(){
  is_available <- file.exists(file.path(getwd(), "README.md"))
  return(is_available)
}
