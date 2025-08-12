#' Generate the README RMarkdown file
#' @description
#' `setup_readme()` uses the openwashdata README template to generate README files based on datasets
#' retrieved from the `data/` directory. It helps in creating consistent and informative README documentation
#' for your data packages.
#' @returns NULL. This function creates a README.Rmd under the package directory.
#' @examples
#' \dontrun{
#' # Generate the README file after setting up the dictionary
#' setup_dictionary()
#' # Complete and save the dictionary CSV file with variable descriptions
#' setup_readme()
#' }
setup_readme <- function(){
  # Get metadata
  readmermd_path <- file.path("README.Rmd")
  pkgname <- desc::desc_get("Package")
  dataname <- strsplit(basename(list.files("data")[1]), ".rda")[[1]]
  if (is.na(dataname)) {
    warning("No tidy data found. Please revise the section DATA in README!")
  }
  # Create README RMarkdown with a template
  usethis::use_readme_rmd(open = FALSE)
  file.remove(readmermd_path)
  usethis::use_template(template = "README.Rmd",
                        save_as = readmermd_path,
                        data = list(packagename = pkgname,
                                    dataname = dataname),
                        open = rlang::is_interactive(),
                        package = "fairenough")
  usethis::ui_todo("Finish the writing of README and run devtools::build_readme() in console.")
}

