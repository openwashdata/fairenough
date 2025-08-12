#' Set up roxygen documentation for all tidy data sets using the dictionary

setup_roxygen <- function(
  title = "Title goes here ...",
  description = "Description here ..."
) {
  # Check dictionary existence
  input_file_path <- file.path(getwd(), "data-raw", "dictionary.csv")
  if (!file.exists(input_file_path)) {
    usethis::ui_stop(
      "Data dictionary does not exist in the data-raw/ directory. Please set up the raw data or create a dictionary first."
    )
  }
  # Check R/ existence
  output_file_dir <- file.path(getwd(), "R")
  if (!dir.exists(output_file_dir)) {
    usethis::use_r(open = FALSE)
  }
  # Check data/ existence
  tidy_datasets <- list.files(path = file.path(getwd(), "data"))
  num_tidy_datasets <- length(tidy_datasets)
  # Write roxygen doc for each tidy dataset
  if (num_tidy_datasets == 0) {
    usethis::ui_stop(
      "No tidy data sets are available in the data/ directory.
                     Please complete data processing and export tidy data first."
    )
  } else {
    for (d in tidy_datasets) {
      # Check if the file ends with .rda
      if (grepl("\\.rda$", d)) {
        # Update output_file_path to have the same name as df_name with .R extension
        df_name <- strsplit(basename(file.path(d)), ".rda")[[1]]
        output_file_path <- file.path(output_file_dir, paste0(df_name, ".R"))
        generate_roxygen_docs(
          input_file_path = input_file_path,
          output_file_path = output_file_path,
          title = title,
          description = description,
          df_name = df_name
        )
        usethis::ui_todo(
          "Please write the title and description for \n {usethis::ui_value(output_file_path)}"
        )
      }
    }
  }
}

generate_roxygen_docs <- function(
  input_file_path,
  output_file_path,
  title,
  description,
  df_name = NULL
) {
  # Read input CSV file
  dict <- utils::read.csv(input_file_path)
  ## If an empty csv should quit with error: Cannot generate roxygen file with an empty dictionary
  # Check if df_name is provided and not NULL, then filter input_df
  dict <- subset(dict, dict$file_name == paste0(df_name, ".rda"))
  if (file.exists(output_file_path)) {
    head <- get_roxygen_head(output_file_path)
  } else {
    head <- create_roxygen_head(df_name, title, description)
  }
  body <- create_roxygen_body(dict)
  output <- c(head, body)
  # Label dataset
  output <- c(output, paste0('"', df_name, '"'))
  # Write output to file
  writeLines(output, output_file_path)
  return(output)
}

create_roxygen_head <- function(df_name, title, description) {
  # Create title and description
  roxygen_head <- c(
    paste0("#' ", df_name, ": ", title),
    "#' ",
    paste0("#' ", description),
    "#' "
  )
  return(roxygen_head)
}

get_roxygen_head <- function(roxygen_file_path) {
  roxygen_head <- character()
  roxygen_text <- readLines(roxygen_file_path)
  i <- 1
  line <- roxygen_text[1]
  while (!startsWith(line, prefix = "#' @format")) {
    roxygen_head <- c(roxygen_head, roxygen_text[i])
    i <- i + 1
    line <- roxygen_text[i]
  }
  return(roxygen_head)
}

load_object <- function(file) {
  tmp_env <- new.env()
  load(file = file, envir = tmp_env)
  tmp_env[[ls(tmp_env)[1]]]
}

create_roxygen_body <- function(dict) {
  # Create format line
  dataobj <- file.path(getwd(), "data", dict$file_name[1])
  n_rows <- nrow(load_object(dataobj)) #TODO: Load the data object
  n_vars <- nrow(dict)
  format_line <- paste0(
    "#' @format A tibble with ",
    n_rows,
    " rows and ",
    n_vars,
    " variables"
  )

  # Create \describe block
  block <- create_describe_block(dict)
  output <- c(format_line, block)
  return(output)
}

create_describe_block <- function(dict) {
  block <- character()
  block <- c(block, paste0("#' ", "\\describe{"))

  # Iterate over input rows and create \item blocks
  for (i in seq_len(nrow(dict))) {
    variable_name <- dict[i, "variable_name"]
    description <- dict[i, "description"]

    # Create \item block
    item <- paste0("#'   ", "\\item{", variable_name, "}{", description, "}")

    # Append to output
    block <- c(block, item)
  }

  # Close \describe block
  block <- c(block, "#' }")
  return(block)
}
