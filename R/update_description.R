#' Update the DESCRIPTION file to conform with standards
#' Updates key fields in the DESCRIPTION file including title, description,
#' and other metadata to conform with R package standards.
#' @param organisation Character string. Organization name.
#' @param title Character string. Package title.
#' @param description Character string. Package description.
#' @param file Character string. Path to package directory. Default is ".".
#' @param language Character string. Language code. Default is "en-GB".
#' @param lazydata Character string. Whether to use lazy data loading. Default is "true".
#' @return NULL (invisibly). Updates DESCRIPTION file as a side effect.
#' @examples
#' \dontrun{
#' update_description(
#'   organisation = "My Organization",
#'   title = "My Package Title",
#'   description = "This package does amazing things."
#' )
#' }
update_description <- function(
  organisation,
  title,
  description,
  file = ".",
  language = "en-GB",
  lazydata = "true"
) {
  if (!file.exists(file.path(getwd(), "DESCRIPTION"))) {
    usethis::ui_stop("No DESCRIPTION file found!")
  }
  # package
  pkgname <- desc::desc_get("Package", file = file)[[1]]
  # title
  desc::desc_set("Title", title, file = file)
  # version
  # fetch latest release from GitHub
  url <- paste0(
    "https://api.github.com/repos/",
    gsub("https://github.com/", "", organisation),
    "/",
    pkgname,
    "/releases"
  )
  response <- httr::GET(url)
  if (httr::status_code(response) == 200) {
    releases <- jsonlite::fromJSON(httr::content(response, "text"))["tag_name"]
    if (length(releases) > 0) {
      versions <- gsub("v", "", releases$tag_name)
      # pick latest
      version_parts <- strsplit(versions, "\\.")[[1]]
      # bump version
      version_parts[3] <- as.character(as.integer(version_parts[3]) + 1)
      new_version <- paste(version_parts, collapse = ".")
      desc::desc_set("Version", new_version, file = file)
    } else {
      desc::desc_set("Version", "0.0.0", file = file)
    }
  } else {
    desc::desc_set("Version", "0.0.0", file = file)
  }
  # organisation
  desc::desc_set("Organisation", organisation, file = file)
  # authors
  # description
  desc::desc_set("Description", description, file = file)
  # license
  usethis::use_ccby_license()
  # language
  desc::desc_set("Language", language, file = file)
  # depends
  # Other Fields
  desc::desc_set("LazyData", lazydata, file = file)
  desc::desc_set("Config/Needs/website", "rmarkdown", file = file)
  # Date
  desc::desc_set("Date", Sys.Date(), file = file)
  # URL
  desc::desc_set_urls(
    urls = c(paste0("https://github.com/", organisation, "/", pkgname)),
    file = file
  )
  # Bug Reports
  desc::desc_set(
    "BugReports",
    paste0("https://github.com/", organisation, "/", pkgname, "/", "issues"),
    file = file
  )
}
