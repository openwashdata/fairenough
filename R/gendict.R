# User setup:
#
# library(ellmer)
# Sys.setenv(OPENAI_API_KEY = "YOUR_BLABLADOR_TOKEN") # or setup .Renviron
# # cat("OPENAI_API_KEY=YOUR_BLABLADOR_TOKEN\n", file = file.path(Sys.getenv("HOME"), ".Renviron"), append = TRUE)
# chat <- chat_openai(
# base_url = "https://api.helmholtz-blablador.fz-juelich.de/v1",
# model = "alias-fast" # Or other Blablador aliases like "alias-code", "alias-large", etc.
# ) 

gendict <- function(data, context = NULL, sample_size = 5) {
  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("data must be a data frame")
  }

  if (nrow(data) == 0) {
    cli::cli_abort("data must have at least one row")
  }

  cli::cli_alert_info("Generating dictionary for {ncol(data)} variable{?s}...")

  # Prepare context from DESCRIPTION file
  context_from_desc <- ""
  tryCatch({
    desc_obj <- desc::desc()
    title_val <- desc_obj$get_field("Title")
    description_val <- desc_obj$get_field("Description")

    if (!is.null(title_val) && nzchar(title_val)) {
      context_from_desc <- paste0(context_from_desc, "Package Title: ", title_val, "\n")
    }
    if (!is.null(description_val) && nzchar(description_val)) {
      context_from_desc <- paste0(context_from_desc, "Package Description: ", description_val, "\n")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Could not read DESCRIPTION file using 'desc' package: {e$message}")
  })

  context_part <- ""
  if (!is.null(context) && nzchar(context_from_desc)) {
    context_part <- paste0("Dataset context: ", context, "\n\n", "Additional package context:\n", context_from_desc, "\n")
  } else if (!is.null(context)) {
    context_part <- paste0("Dataset context: ", context, "\n\n")
  } else if (nzchar(context_from_desc)) {
    context_part <- paste0("Dataset context:\n", context_from_desc, "\n")
  }

  # Define the allowed data types
  allowed_data_types <- c(
    "logical", "integer", "numeric", "text", "datetime",
    "categorical", "identifier", "spatial", "binary", "json", "array"
  )
  allowed_types_str <- paste(allowed_data_types, collapse = ", ")

  # Prepare a list of prompts, one for each column, including the sampled data
  column_prompts <- list()
  col_names <- names(data)

  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    column_data <- data[[col_name]]
    unique_values <- unique(column_data)

    # Re-introducing the sampling logic for each column
    if (length(unique_values) <= sample_size) {
      sampled_values <- unique_values
    } else {
      sampled_values <- sample(unique_values, sample_size, replace = FALSE)
      sampled_values <- c(sampled_values, "...")
    }

    # Convert sampled values to a string for the prompt
    sample_str <- paste(sapply(sampled_values, as.character), collapse = ", ")

    # Construct the prompt for llm_vec_custom, now with sample_str included
    column_prompts[[i]] <- glue::glue(
      "{context_part}I need a data dictionary entry for a variable.
      Variable Name: {col_name}
      Example Values: {sample_str}

      Based on the variable name and example values, please provide the following details:
      1.  **Data Type**: Choose one from: {allowed_types_str}.
      2.  **Description**: A clear, concise description (1-2 sentences) of what this variable represents.
      3.  **Unit**: The unit of measurement (if applicable, otherwise 'N/A').

      Provide your answer in the exact format: 'Data Type | Description | Unit'.
      For example: 'categorical | Gender of the respondent | N/A'"
    )
  }

  # Convert list of prompts to a character vector for llm_vec_custom
  prompts_vector <- unlist(column_prompts)

  tryCatch(
    {
      cli::cli_alert_info("Sending {length(prompts_vector)} prompts to LLM using llm_vec_custom...")
      # Use llm_vec_custom to get a vector of responses
      llm_responses <- mall::llm_vec_custom(x = prompts_vector)

      # Initialize lists to store parsed data
      variable_names <- col_names
      types <- character(length(llm_responses))
      descriptions <- character(length(llm_responses))
      units <- character(length(llm_responses))

      for (i in seq_along(llm_responses)) {
        response_text <- llm_responses[i]
        parsed_parts <- strsplit(response_text, "\\|")[[1]]
        parsed_parts <- trimws(parsed_parts)

        if (length(parsed_parts) == 3) {
          types[i] <- parsed_parts[1]
          descriptions[i] <- parsed_parts[2]
          units[i] <- if (parsed_parts[3] == "N/A") "" else parsed_parts[3]
        } else {
          cli::cli_alert_warning("Unexpected LLM response format for '{variable_names[i]}': '{response_text}'. Using fallback.")
          types[i] <- class(data[[variable_names[i]]])[1]
          descriptions[i] <- paste("Could not determine description for", variable_names[i])
          units[i] <- ""
        }
      }

      final_dict <- tibble::tibble(
        variable = variable_names,
        type = types,
        description = descriptions,
        unit = units
      )

      cli::cli_alert_success("Dictionary generated successfully!")
      return(final_dict)
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to generate dictionary: {e$message}")

      # Return a basic fallback dictionary
      fallback_dict <- tibble::tibble(
        variable = names(data),
        description = paste("Variable:", names(data)),
        type = sapply(data, function(x) class(x)[1]),
        unit = ""
      )

      cli::cli_alert_info("Returning basic dictionary structure")
      return(fallback_dict)
    }
  )
}