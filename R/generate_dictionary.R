#' Generate data dictionary
#'
#' Creates a data dictionary for all data files in the package.
#' Can use LLM for automatic description generation. Works best when
#' package metadata exists in DESCRIPTION as it uses the package description for context.
#'
#' @param chat Optional chat object for LLM-based generation
#' @param context Optional context for LLM generation
#' @param overwrite Whether to overwrite existing dictionary
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @param ... Additional arguments passed to gendict
#' @return Data frame containing the dictionary
#' @export
#' @examples
#' \dontrun{
#' # Generate empty dictionary structure (no LLM)
#' generate_dictionary()
#'
#' # Generate dictionary with LLM
#' library(ellmer)
#' chat <- chat_openai(model = "gpt-4")
#' generate_dictionary(chat = chat, overwrite = TRUE)
#' }
generate_dictionary <- function(
  chat = NULL,
  context = NULL,
  overwrite = FALSE,
  base_path = NULL,
  verbose = TRUE,
  ...
) {
  base_path <- get_base_path(base_path)

  # Check for existing dictionary
  dict_path <- file.path(base_path, "inst", "extdata", "dictionary.csv")

  if (file.exists(dict_path) && !overwrite) {
    if (verbose) {
      cli::cli_alert_info(
        "Dictionary already exists at {.path inst/extdata/dictionary.csv}"
      )
      cli::cli_alert_info("Use {.code overwrite = TRUE} to regenerate")
    }
    return(invisible(utils::read.csv(dict_path)))
  }

  # Collect variable information using the utility function
  datasets_info <- .collect_dataset_info(base_path)

  if (is.null(datasets_info)) {
    cli::cli_alert_warning("No .rda files found in {.path data/}")
    cli::cli_alert_info("Run {.fn process} first to process your data files")
    return(invisible(NULL))
  }

  # Convert datasets_info to the all_vars format needed for dictionary
  all_vars <- data.frame()

  for (data_name in names(datasets_info)) {
    dataset <- datasets_info[[data_name]]

    # Create variable info
    var_info <- data.frame(
      file_name = data_name,
      variable_name = dataset$variables$names,
      variable_type = dataset$variables$types,
      stringsAsFactors = FALSE
    )

    all_vars <- rbind(all_vars, var_info)
  }

  # Check for metadata in DESCRIPTION to enhance context
  if (is.null(context)) {
    metadata <- get_metadata(base_path)
    if (!is.null(metadata) && !is.null(metadata$package$description)) {
      context <- paste("Data's general context:", metadata$package$description)
      if (verbose && !is.null(chat)) {
        cli::cli_alert_info(
          "Using package description from DESCRIPTION as context for LLM"
        )
      }
    } else if (!is.null(chat) && verbose) {
      cli::cli_alert_info("No package description found in DESCRIPTION")
      cli::cli_alert_info(
        "Consider running {.fn collect_metadata} first for improved LLM results"
      )
    }
  }

  # Add descriptions
  all_vars$description <- NA

  if (!is.null(chat)) {
    if (verbose) {
      cli::cli_alert_info("Using LLM to generate descriptions...")
    }

    # Process each unique file
    unique_files <- unique(all_vars$file_name)

    for (file_name in unique_files) {
      # Look for corresponding CSV in inst/extdata
      csv_path <- file.path(
        base_path,
        "inst",
        "extdata",
        paste0(file_name, ".csv")
      )

      if (file.exists(csv_path)) {
        if (verbose) {
          cli::cli_alert_info("Generating descriptions for {.file {file_name}}")
        }

        # Use gendict to generate descriptions
        gendict_result <- tryCatch(
          {
            gendict(data = csv_path, chat = chat, context = context, ...)
          },
          error = function(e) {
            cli::cli_alert_warning(
              "Failed to generate descriptions for {file_name}: {e$message}"
            )
            NULL
          }
        )

        if (!is.null(gendict_result)) {
          # Match descriptions to variables
          for (i in seq_len(nrow(gendict_result))) {
            var_name <- gendict_result$variable[i]
            desc <- gendict_result$description[i]

            # Update description in all_vars
            mask <- all_vars$file_name == file_name &
              all_vars$variable_name == var_name
            all_vars$description[mask] <- desc
          }
        }
      }
    }
  } else {
    if (verbose) {
      cli::cli_alert_info(
        "No chat object provided - descriptions will be empty"
      )
      cli::cli_alert_info(
        "To generate descriptions, pass an ellmer chat object"
      )
    }
  }

  # Format final dictionary
  dictionary <- data.frame(
    directory = "data",
    file_name = all_vars$file_name,
    variable_name = all_vars$variable_name,
    variable_type = all_vars$variable_type,
    description = all_vars$description
  )

  # Save dictionary
  ensure_directory(file.path(base_path, "inst", "extdata"), verbose = FALSE)
  utils::write.csv(dictionary, dict_path, row.names = FALSE, na = "")

  if (verbose) {
    cli::cli_alert_success(
      "Dictionary saved to {.path inst/extdata/dictionary.csv}"
    )
    if (any(is.na(dictionary$description))) {
      cli::cli_alert_info("Edit the CSV file to complete missing descriptions")
    }
  }

  return(invisible(dictionary))
}

#' Collect dataset information from .rda files
#'
#' Scans data directory for .rda files and collects variable information
#'
#' @param base_path Base path for the project
#' @return Data frame with dataset information
.collect_dataset_info <- function(base_path = NULL) {
  base_path <- get_base_path(base_path)

  # Check data directory
  data_dir <- file.path(base_path, "data")
  if (!fs::dir_exists(data_dir)) {
    return(NULL)
  }

  # Find all .rda files
  rda_files <- list.files(data_dir, pattern = "\\.rda$", full.names = TRUE)

  if (length(rda_files) == 0) {
    return(NULL)
  }

  # Collect dataset information
  datasets <- list()

  for (rda_file in rda_files) {
    # Load data
    temp_env <- new.env()
    load(rda_file, envir = temp_env)
    data_name <- ls(envir = temp_env)[1]
    data <- get(data_name, envir = temp_env)

    # Create dataset info
    datasets[[data_name]] <- list(
      rows = nrow(data),
      cols = ncol(data),
      variables = list(
        names = names(data),
        types = vapply(
          data,
          function(x) class(x)[1],
          character(1),
          USE.NAMES = FALSE
        )
      )
    )
  }

  return(datasets)
}
