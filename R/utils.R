#' @importFrom utils person
NULL

# Supported file extensions for data processing
SUPPORTED_EXTENSIONS <- c("csv", "xlsx", "xls")
SUPPORTED_EXTENSIONS_DOT <- c(".csv", ".xlsx", ".xls")

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

#' Get raw directory path with consistent handling across all functions
#'
#' This function provides a consistent way to handle raw_dir across all fairenough functions.
#' If raw_dir is provided, it sets the global option and returns the normalized path.
#' If raw_dir is NULL, it checks for the global option, falling back to "data_raw"
#'
#' @param raw_dir Optional base path to set
#' @return Normalized base path
#' @export
get_raw_dir <- function(raw_dir = NULL) {
  # If raw_dir is provided, set it as option and use it
  if (!is.null(raw_dir)) {
    options(fairenough.raw_dir = raw_dir)
    return(raw_dir)
  }

  # Otherwise, check for existing option
  stored_path <- getOption("fairenough.raw_dir")
  if (!is.null(stored_path)) {
    return(stored_path)
  }

  # Fall back to data_raw
  default_path <- "data_raw"

  options(fairenough.raw_dir = default_path)
  return(default_path)
}

#' Read data from various sources
#'
#' This function reads data from a data frame, CSV file, or Excel file.
#' It handles all validation and provides consistent error messages.
#'
#' @param data Either a data frame or a path to a CSV/Excel file
#' @param show_messages Whether to show informational messages (default TRUE)
#' @return A data frame
#' @export
read_data <- function(data, show_messages = TRUE) {
  # If already a data frame, validate and return
  if (is.data.frame(data)) {
    return(validate_data_frame(data))
  }

  # Must be a file path - validate it
  data_path <- validate_file_path(data)

  # Determine file type and read accordingly
  file_ext <- tolower(tools::file_ext(data_path))

  if (file_ext == "csv") {
    if (show_messages) {
      cli::cli_alert_info("Reading CSV file: {data_path}")
    }
    data <- readr::read_csv(data_path, show_col_types = FALSE)
  } else if (file_ext %in% c("xlsx", "xls")) {
    # Ensure readxl is available
    if (!requireNamespace("readxl", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg readxl} is required to read Excel files",
        "i" = "Install it with: install.packages('readxl')"
      ))
    }
    if (show_messages) {
      cli::cli_alert_info("Reading Excel file: {data_path}")
    }
    data <- readxl::read_excel(data_path)
  }

  # Convert tibble to regular data frame to avoid issues
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Validate and return the loaded data
  return(validate_data_frame(data))
}

#' Get supported file extensions
#' @return Character vector of supported file extensions (without dots)
#' @export
get_supported_extensions <- function() {
  return(SUPPORTED_EXTENSIONS)
}

#' Check if file type is supported
#' @param file_path Path to file
#' @return Logical indicating if file type is supported
#' @export
is_supported_file_type <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    return(FALSE)
  }
  file_ext <- tolower(tools::file_ext(file_path))
  return(file_ext %in% SUPPORTED_EXTENSIONS)
}

#' Filter files by supported extensions
#' @param file_paths Character vector of file paths
#' @return Character vector of files with supported extensions
#' @export
filter_supported_files <- function(file_paths) {
  if (length(file_paths) == 0) {
    return(character(0))
  }

  supported_files <- character(0)
  for (ext in SUPPORTED_EXTENSIONS_DOT) {
    pattern <- paste0("\\", ext, "$")
    matching_files <- file_paths[grepl(pattern, file_paths, ignore.case = TRUE)]
    supported_files <- c(supported_files, matching_files)
  }

  return(supported_files)
}

#' Validate file path
#' @param file_path Path to file
#' @return The validated file path (throws error if invalid)
#' @export
validate_file_path <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    cli::cli_abort("File path must be a single character string")
  }

  if (!file.exists(file_path)) {
    cli::cli_abort("File not found: {file_path}")
  }

  if (!is_supported_file_type(file_path)) {
    file_ext <- tools::file_ext(file_path)
    cli::cli_abort(
      "Unsupported file type: {file_ext}. Supported types: {paste(SUPPORTED_EXTENSIONS, collapse=', ')}"
    )
  }

  return(file_path)
}

#' Validate data frame
#' @param data Data frame to validate
#' @param min_rows Minimum number of rows required (default 1)
#' @return The validated data frame (throws error if invalid)
#' @export
validate_data_frame <- function(data, min_rows = 1) {
  if (!is.data.frame(data)) {
    cli::cli_abort("Input must be a data frame")
  }

  if (nrow(data) < min_rows) {
    cli::cli_abort("Data frame must have at least {min_rows} row{?s}")
  }

  return(data)
}

#' Ensure directory exists with messaging
#' @param dir_path Path to directory
#' @param description Optional description for messages
#' @param recursive Whether to create parent directories (default TRUE)
#' @param verbose Whether to show messages (default TRUE)
#' @return The directory path
#' @export
ensure_directory <- function(
  dir_path,
  description = NULL,
  recursive = TRUE,
  verbose = TRUE
) {
  if (is.null(description)) {
    description <- "Directory"
  }

  if (fs::dir_exists(dir_path)) {
    if (verbose) {
      cli::cli_alert_info("{description} '{dir_path}' already exists")
    }
  } else {
    fs::dir_create(dir_path, recurse = recursive)
    if (verbose) cli::cli_alert_success("Created {description}: {dir_path}")
  }

  return(dir_path)
}

#' Use a template file with data substitution
#'
#' Similar to usethis::use_template but respects base_path.
#' Reads a template from the package, substitutes data, and writes to target location.
#'
#' @param template Name of template file in inst/templates
#' @param save_as Path to save file relative to base_path
#' @param data List of data for substitution
#' @param base_path Base path for the project
#' @param package Package containing the template
#' @param open Whether to open the file after creation
#' @param verbose Whether to show messages
#' @return Path to created file
#' @export
use_template <- function(
  template,
  save_as = template,
  data = list(),
  base_path = NULL,
  package = "fairenough",
  open = FALSE,
  verbose = TRUE
) {
  base_path <- get_base_path(base_path)

  # Get template from package
  template_path <- system.file("templates", template, package = package)
  if (template_path == "") {
    cli::cli_abort(
      "Template {.file {template}} not found in package {.pkg {package}}"
    )
  }

  # Read template
  template_content <- paste(readLines(template_path), collapse = "\n")

  # Use whisker for template rendering (same as usethis)
  if (!requireNamespace("whisker", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg whisker} is required. Install it with: install.packages('whisker')"
    )
  }

  # Render template with whisker
  rendered_content <- whisker::whisker.render(template_content, data)

  # Create output path
  output_path <- file.path(base_path, save_as)

  # Ensure directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Check if file exists and prompt
  if (file.exists(output_path)) {
    if (interactive()) {
      if (
        !usethis::ui_yeah(
          "Overwrite pre-existing file {usethis::ui_path(save_as)}?"
        )
      ) {
        return(invisible(NULL))
      }
    }
  }

  # Write file
  writeLines(rendered_content, output_path)

  if (verbose) {
    if (file.exists(output_path)) {
      cli::cli_alert_success("Writing {.path {save_as}}")
    } else {
      cli::cli_alert_success("Creating {.path {save_as}}")
    }
  }

  # Open file if requested
  if (open && interactive()) {
    if (
      requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()
    ) {
      rstudioapi::navigateToFile(output_path)
    } else {
      utils::file.edit(output_path)
    }
  }

  return(invisible(output_path))
}

#' Validation system for fairenough pipeline steps
#' 
#' These functions validate actual completion status using comprehensive checklists.
#' Each validation returns detailed results about what's complete and what's missing.
#'

# Simple validation helper functions for setup
check_description_exists <- function(base_path) {
  file.exists(file.path(base_path, "DESCRIPTION"))
}

check_rproj_exists <- function(base_path) {
  length(list.files(base_path, pattern = "\\.Rproj$")) > 0
}

check_dirs_exist <- function(base_path) {
  required_dirs <- c("data", "data_raw", "inst/extdata")
  all(dir.exists(file.path(base_path, required_dirs)))
}

check_gitignore_configured <- function(base_path) {
  gitignore_path <- file.path(base_path, ".gitignore")
  if (!file.exists(gitignore_path)) return(FALSE)
  
  gitignore_content <- readLines(gitignore_path)
  required_entries <- c("data_raw/", ".Rhistory", ".RData")
  all(sapply(required_entries, function(entry) any(grepl(entry, gitignore_content, fixed = TRUE))))
}

# Validation helpers for data processing
check_raw_data_files <- function(base_path) {
  raw_dir <- file.path(base_path, get_raw_dir())
  if (!dir.exists(raw_dir)) return(FALSE)
  
  all_files <- list.files(raw_dir, full.names = TRUE)
  data_files <- filter_supported_files(all_files)
  length(data_files) > 0
}

check_rda_files <- function(base_path) {
  data_dir <- file.path(base_path, "data")
  if (!dir.exists(data_dir)) return(FALSE)
  
  rda_files <- list.files(data_dir, pattern = "\\.rda$")
  length(rda_files) > 0
}

check_csv_files <- function(base_path) {
  extdata_dir <- file.path(base_path, "inst", "extdata")
  if (!dir.exists(extdata_dir)) return(FALSE)
  
  csv_files <- list.files(extdata_dir, pattern = "\\.csv$")
  length(csv_files) > 0
}

check_file_consistency <- function(base_path) {
  raw_dir <- file.path(base_path, get_raw_dir())
  data_dir <- file.path(base_path, "data")
  extdata_dir <- file.path(base_path, "inst", "extdata")
  
  if (!all(dir.exists(c(raw_dir, data_dir, extdata_dir)))) return(FALSE)
  
  raw_files <- filter_supported_files(list.files(raw_dir, full.names = TRUE))
  if (length(raw_files) == 0) return(TRUE) # No raw files yet
  
  rda_files <- list.files(data_dir, pattern = "\\.rda$")
  # Exclude dictionary.csv from consistency check
  csv_files <- list.files(extdata_dir, pattern = "\\.csv$")
  csv_files <- csv_files[csv_files != "dictionary.csv"]
  
  raw_names <- tools::file_path_sans_ext(basename(raw_files))
  rda_names <- tools::file_path_sans_ext(rda_files)
  csv_names <- tools::file_path_sans_ext(csv_files)
  
  length(raw_names) == length(rda_names) && 
  length(raw_names) == length(csv_names) &&
  all(sort(raw_names) == sort(rda_names)) &&
  all(sort(raw_names) == sort(csv_names))
}

# Validation helpers for metadata
check_package_name <- function(base_path) {
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (!file.exists(desc_path)) return(FALSE)
  
  tryCatch({
    d <- desc::desc(file = desc_path)
    pkg_name <- d$get("Package")[[1]]
    !is.null(pkg_name) && pkg_name != "placeholder" && nchar(pkg_name) > 0
  }, error = function(e) FALSE)
}

check_package_title <- function(base_path) {
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (!file.exists(desc_path)) return(FALSE)
  
  tryCatch({
    d <- desc::desc(file = desc_path)
    title <- d$get("Title")[[1]]
    !is.null(title) && !grepl("Placeholder|What the Package Does", title, ignore.case = TRUE) && nchar(title) > 0
  }, error = function(e) FALSE)
}

check_package_description <- function(base_path) {
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (!file.exists(desc_path)) return(FALSE)
  
  tryCatch({
    d <- desc::desc(file = desc_path)
    description <- d$get("Description")[[1]]
    !is.null(description) && !grepl("Placeholder|What the package does", description, ignore.case = TRUE) && nchar(description) > 0
  }, error = function(e) FALSE)
}

check_package_authors <- function(base_path) {
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (!file.exists(desc_path)) return(FALSE)
  
  tryCatch({
    d <- desc::desc(file = desc_path)
    authors <- d$get_authors()
    
    if (length(authors) == 0) return(FALSE)
    
    has_maintainer <- any(sapply(authors, function(author) "cre" %in% author$role))
    meaningful_authors <- any(sapply(authors, function(author) {
      !is.null(author$given) && !is.null(author$family) &&
      !grepl("First|Last", paste(author$given, author$family), ignore.case = TRUE)
    }))
    
    has_maintainer && meaningful_authors
  }, error = function(e) FALSE)
}

check_package_license <- function(base_path) {
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (!file.exists(desc_path)) return(FALSE)
  
  tryCatch({
    d <- desc::desc(file = desc_path)
    license <- d$get("License")[[1]]
    !is.null(license) && !grepl("use_.*_license|pick a license", license, ignore.case = TRUE) && nchar(license) > 0
  }, error = function(e) FALSE)
}

check_package_urls <- function(base_path) {
  desc_path <- file.path(base_path, "DESCRIPTION")
  if (!file.exists(desc_path)) return(FALSE)
  
  tryCatch({
    d <- desc::desc(file = desc_path)
    urls <- d$get_urls()
    
    github_user <- tryCatch(d$get("github_user")[[1]], error = function(e) NULL)
    if (is.null(github_user) || github_user == "") return(TRUE) # URLs optional if no github_user
    
    length(urls) > 0
  }, error = function(e) FALSE)
}

# Validation helpers for dictionary
check_dictionary_file <- function(base_path) {
  file.exists(file.path(base_path, "inst", "extdata", "dictionary.csv"))
}

check_variables_documented <- function(base_path) {
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
  if (!file.exists(dict_path)) return(FALSE)
  
  data_dir <- file.path(base_path, "data")
  if (!dir.exists(data_dir)) return(TRUE) # No data files yet
  
  rda_files <- list.files(data_dir, pattern = "\\.rda$", full.names = TRUE)
  if (length(rda_files) == 0) return(TRUE) # No data files yet
  
  all_vars <- character(0)
  for (rda_file in rda_files) {
    temp_env <- new.env()
    tryCatch({
      load(rda_file, envir = temp_env)
      data_name <- ls(envir = temp_env)[1]
      data_obj <- get(data_name, envir = temp_env)
      all_vars <- c(all_vars, names(data_obj))
    }, error = function(e) NULL)
  }
  
  dict <- utils::read.csv(dict_path)
  dict_vars <- dict$variable_name
  length(setdiff(all_vars, dict_vars)) == 0
}

check_descriptions_filled <- function(base_path) {
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
  if (!file.exists(dict_path)) return(FALSE)
  
  dict <- utils::read.csv(dict_path)
  if (nrow(dict) == 0) return(TRUE)
  
  !any(is.na(dict$description) | dict$description == "" | trimws(dict$description) == "")
}

check_dictionary_structure <- function(base_path) {
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")
  if (!file.exists(dict_path)) return(FALSE)
  
  dict <- utils::read.csv(dict_path)
  required_cols <- c("directory", "file_name", "variable_name", "variable_type", "description")
  all(required_cols %in% names(dict))
}

# Validation helpers for build
check_license_file <- function(base_path) {
  license_files <- c("LICENSE", "LICENSE.md")
  license_paths <- file.path(base_path, license_files)
  any(file.exists(license_paths))
}

check_citation_files <- function(base_path) {
  citation_cff <- file.path(base_path, "CITATION.cff")
  citation_inst <- file.path(base_path, "inst", "CITATION")
  file.exists(citation_cff) && file.exists(citation_inst)
}

check_readme_file <- function(base_path) {
  file.exists(file.path(base_path, "README.md"))
}

check_documentation <- function(base_path) {
  man_dir <- file.path(base_path, "man")
  if (!dir.exists(man_dir)) return(FALSE)
  
  rd_files <- list.files(man_dir, pattern = "\\.Rd$")
  length(rd_files) > 0
}

check_website <- function(base_path) {
  docs_dir <- file.path(base_path, "docs")
  if (!dir.exists(docs_dir)) return(FALSE)
  
  index_file <- file.path(docs_dir, "index.html")
  file.exists(index_file)
}

check_package_installable <- function(base_path) {
  desc_path <- file.path(base_path, "DESCRIPTION")
  namespace_path <- file.path(base_path, "NAMESPACE") 
  r_dir <- file.path(base_path, "R")
  
  file.exists(desc_path) && file.exists(namespace_path) && dir.exists(r_dir)
}

#' Process checklist into structured results
#' @param checklist List of check items with name, description, check, required fields
#' @return Formatted checklist results
format_checklist_results <- function(checklist) {
  results <- list()
  passed <- 0
  total <- length(checklist)
  
  for (i in seq_along(checklist)) {
    item <- checklist[[i]]
    error_msg <- NULL
    
    check_passed <- tryCatch(
      item$check,
      error = function(e) {
        error_msg <- e$message
        FALSE
      }
    )
    
    results[[i]] <- list(
      name = item$name,
      description = item$description,
      passed = check_passed,
      required = if (is.null(item$required)) TRUE else item$required,
      details = if (!check_passed && !is.null(item$details)) item$details else NULL,
      error = if (!is.null(error_msg)) error_msg else NULL
    )
    
    if (check_passed) passed <- passed + 1
  }
  
  list(
    all_passed = passed == total,
    all_required_passed = all(sapply(results, function(x) !x$required || x$passed)),
    total_checks = total,
    passed_checks = passed,
    failed_checks = total - passed,
    items = results
  )
}

#' Display formatted checklist to user
#' @param validation Validation results from format_checklist_results
#' @param title Title for the checklist display
#' @param verbose Whether to show detailed output
show_checklist <- function(validation, title, verbose = TRUE) {
  if (!verbose) return(invisible(NULL))
  
  # Show summary
  status <- if (validation$all_required_passed) "✅" else "❌"
  cli::cli_h3("{status} {title}: {validation$passed_checks}/{validation$total_checks} checks passed")
  
  # Show individual items
  for (item in validation$items) {
    icon <- if (item$passed) "✅" else "❌"
    
    cli::cli_text("{icon} {item$name}: {item$description}")
    
    if (!item$passed && !is.null(item$details)) {
      cli::cli_alert_info("  → {item$details}")
    }
    
    if (!item$passed && !is.null(item$error)) {
      cli::cli_alert_warning("  → Error: {item$error}")
    }
  }
  
  invisible(validation)
}

#' Get summary string for validation results
#' @param validation Validation results
#' @return Character string with summary
checklist_summary <- function(validation) {
  paste0(validation$passed_checks, "/", validation$total_checks, " checks passed")
}

#' Validate setup completion status
#' @param base_path Base path for the project
#' @return Formatted checklist results
#' @export
validate_setup_completed <- function(base_path = NULL) {
  base_path <- get_base_path(base_path)
  
  checklist <- list(
    list(
      name = "R package structure",
      description = "DESCRIPTION file exists",
      check = check_description_exists(base_path),
      required = TRUE,
      details = "Run setup() to create R package structure"
    ),
    list(
      name = "RStudio project",
      description = ".Rproj file exists",
      check = check_rproj_exists(base_path),
      required = TRUE,
      details = "Run setup() to create RStudio project file"
    ),
    list(
      name = "Data directories", 
      description = "Required directories exist (data/, data_raw/, inst/extdata/)",
      check = check_dirs_exist(base_path),
      required = TRUE,
      details = "Run setup() to create required directory structure"
    ),
    list(
      name = "Git configuration",
      description = ".gitignore configured properly",
      check = check_gitignore_configured(base_path),
      required = FALSE,
      details = "Run setup() to configure .gitignore properly"
    )
  )
  
  format_checklist_results(checklist)
}

validate_processing_completed <- function(base_path = NULL) {
  base_path <- get_base_path(base_path)
  
  checklist <- list(
    list(
      name = "Raw data files",
      description = "Data files found in data_raw/ directory",
      check = check_raw_data_files(base_path),
      required = TRUE,
      details = "Place CSV/Excel files in data_raw/ directory"
    ),
    list(
      name = "Processed RDA files",
      description = ".rda files exist in data/ directory",
      check = check_rda_files(base_path),
      required = TRUE,
      details = "Run process() to convert raw data to .rda format"
    ),
    list(
      name = "CSV exports",
      description = ".csv files exist in inst/extdata/ directory",
      check = check_csv_files(base_path),
      required = TRUE,
      details = "Run process() to export CSV files to inst/extdata/"
    ),
    list(
      name = "File consistency",
      description = "Raw, RDA, and CSV files match in count and naming",
      check = check_file_consistency(base_path),
      required = FALSE,
      details = "Run process() to ensure consistent file processing"
    )
  )
  
  format_checklist_results(checklist)
}

validate_metadata_collected <- function(base_path = NULL) {
  base_path <- get_base_path(base_path)
  
  checklist <- list(
    list(
      name = "DESCRIPTION file",
      description = "DESCRIPTION file exists",
      check = check_description_exists(base_path),
      required = TRUE,
      details = "Run setup() first to create DESCRIPTION file"
    ),
    list(
      name = "Package name",
      description = "Package field in DESCRIPTION is meaningful",
      check = check_package_name(base_path),
      required = TRUE,
      details = "Run collect() to set proper package name"
    ),
    list(
      name = "Package title",
      description = "Title field in DESCRIPTION is meaningful", 
      check = check_package_title(base_path),
      required = TRUE,
      details = "Run collect() to set meaningful package title"
    ),
    list(
      name = "Package description",
      description = "Description field in DESCRIPTION is meaningful",
      check = check_package_description(base_path),
      required = TRUE,
      details = "Run collect() to set meaningful package description"
    ),
    list(
      name = "Authors", 
      description = "Authors@R field with maintainer (cre role)",
      check = check_package_authors(base_path),
      required = TRUE,
      details = "Run collect() to set proper authors with maintainer"
    ),
    list(
      name = "License",
      description = "License field is properly set",
      check = check_package_license(base_path),
      required = TRUE,
      details = "Run collect() to set proper license"
    ),
    list(
      name = "URLs",
      description = "URL fields configured (if github_user provided)",
      check = check_package_urls(base_path),
      required = FALSE,
      details = "URLs will be set automatically if github_user is provided"
    )
  )
  
  format_checklist_results(checklist)
}

validate_dictionary_completed <- function(base_path = NULL) {
  base_path <- get_base_path(base_path)
  
  checklist <- list(
    list(
      name = "Dictionary file",
      description = "dictionary.csv exists in inst/extdata/",
      check = check_dictionary_file(base_path),
      required = TRUE,
      details = "Run generate() to create dictionary file"
    ),
    list(
      name = "Variables documented",
      description = "All variables from data files are in dictionary",
      check = check_variables_documented(base_path),
      required = TRUE,
      details = "Run generate() to document all variables"
    ),
    list(
      name = "Descriptions filled",
      description = "All variable descriptions are complete (not empty or NA)",
      check = check_descriptions_filled(base_path),
      required = TRUE,
      details = "Run generate() with a chat object or fill descriptions manually"
    ),
    list(
      name = "Dictionary structure",
      description = "Dictionary has proper structure and required columns",
      check = check_dictionary_structure(base_path),
      required = TRUE,
      details = "Run generate() to create properly structured dictionary"
    )
  )
  
  format_checklist_results(checklist)
}

validate_build_completed <- function(base_path = NULL) {
  base_path <- get_base_path(base_path)
  
  checklist <- list(
    list(
      name = "LICENSE file",
      description = "LICENSE or LICENSE.md file exists",
      check = check_license_file(base_path),
      required = TRUE,
      details = "Run build() to create LICENSE file"
    ),
    list(
      name = "CITATION files",
      description = "Citation files exist (CITATION.cff and inst/CITATION)",
      check = check_citation_files(base_path),
      required = TRUE,
      details = "Run build() to create citation files"
    ),
    list(
      name = "README file",
      description = "README.md file exists",
      check = check_readme_file(base_path),
      required = TRUE,
      details = "Run build() to generate README.md"
    ),
    list(
      name = "Documentation",
      description = "man/ directory with .Rd files exists",
      check = check_documentation(base_path),
      required = TRUE,
      details = "Run build() to generate roxygen documentation"
    ),
    list(
      name = "Package website",
      description = "Website built in docs/ directory",
      check = check_website(base_path),
      required = FALSE,
      details = "Run build() to generate package website"
    ),
    list(
      name = "Package installable",
      description = "Package structure is valid and installable",
      check = check_package_installable(base_path),
      required = TRUE,
      details = "Run build() to ensure package is properly structured"
    )
  )
  
  format_checklist_results(checklist)
}