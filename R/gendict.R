# User setup
# library(mall)
# Sys.setenv(OPENAI_API_KEY = "YOUR_BLABLADOR_TOKEN") # or setup .Renviron
# chat <- chat_openai(
#   base_url = "https://api.helmholtz-blablador.fz-juelich.de/v1",
#   model = "alias-fast" # Or other Blablador aliases like "alias-code", "alias-large", etc.
# )
# llm_use(chat)

gendict <- function(data, context = NULL, sample_size = 5) {
  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("data must be a data frame")
  }

  if (nrow(data) == 0) {
    cli::cli_abort("data must have at least one row")
  }

  cli::cli_alert_info("Generating dictionary for {ncol(data)} variable{?s}...")

  # Prepare data sample for analysis
  data_sample_list <- list()

  # Iterate through each column to get a comprehensive sample of unique values
  for (col_name in names(data)) {
    column_data <- data[[col_name]]

    # Get unique values for the current column
    unique_values <- unique(column_data)

    # If the number of unique values is less than or equal to sample_size,
    # just take all unique values.
    # Otherwise, sample 'sample_size' unique values.
    if (length(unique_values) <= sample_size) {
      sampled_values <- unique_values
    } else {
      # Sample 'sample_size' unique values
      sampled_values <- sample(unique_values, sample_size, replace = FALSE)
      # Add "..." to indicate that there are more unique values not shown
      sampled_values <- c(sampled_values, "...")
    }

    # Add the sampled values to our list, ensuring we keep the original column type
    data_sample_list[[col_name]] <- sampled_values
  }

  # Convert the list of sampled unique values directly to JSON for the prompt
  # This avoids creating an intermediate data frame with potentially unequal column lengths.
  data_json <- jsonlite::toJSON(data_sample_list, auto_unbox = TRUE, pretty = TRUE)

  # Define the allowed data types as a separate variable
  allowed_data_types <- c(
    "logical",
    "integer",
    "numeric",
    "text",
    "datetime",
    "categorical",
    "identifier",
    "spatial",
    "binary",
    "json",
    "array"
  )

  # Define the system message
  system_message <- paste0(
    "You are a data science expert specializing in creating clear, comprehensive data dictionaries. ",
    "Your task is to analyze a dataset sample and generate descriptive metadata for each variable/column. ",
    "Follow these steps for each variable:\n\n",
    "1. **Analyze the column name:** What does the name itself suggest about the data it might contain?\n",
    "2. **Examine the provided example values:** Look for patterns, units, categories, or ranges that can help you understand the nature of the data. Pay attention to the presence of 'null' or 'None' values, indicating missing data.\n",
    "3. **Infer the likely meaning and context:** Based on the column name and example values, what real-world concept or measurement does this column likely represent? Try to infer the broader domain or field of study this data might belong to (e.g., environmental science, social surveys, medical records).\n",
    "4. **Determine the data type:** Clearly identify the likely data type of the column. Use precise terms like '",
    paste(allowed_data_types, collapse = "', '"),
    "' as per the schema.\n",
    "5. **Write a concise description:** Combine your inferences into a brief description (1-2 sentences) that explains what the column *represents* in the real world and its inferred data type. Use clear and accessible language, avoiding overly technical jargon unless essential.\n\n",
  )

  context_from_desc <- ""
  # The 'desc' package is assumed to be a dependency.
  tryCatch({
    # Create a desc object for the current project's DESCRIPTION file
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

  # Prioritize user-provided context if both exist, or combine them
  context_part <- ""
  if (!is.null(context) && nzchar(context_from_desc)) {
    context_part <- paste0("Dataset context: ", context, "\n\n", "Additional package context:\n", context_from_desc, "\n")
  } else if (!is.null(context)) {
    context_part <- paste0("Dataset context: ", context, "\n\n")
  } else if (nzchar(context_from_desc)) {
    context_part <- paste0("Dataset context:\n", context_from_desc, "\n")
  }

  user_message <- glue::glue(
    "
{context_part}Please analyze this dataset sample and create a data dictionary:

{data_json}

For each variable/column, provide:
1. [description] A clear, descriptive explanation of what the variable represents
2. [data type] The appropriate data type
3. [unit] Units of measurement (if applicable)

Focus on being descriptive and helpful for someone who will use this data and format it as:

[data type], [description], [unit].
"
  )

  # Combine system and user messages into a single prompt for mall::llm_custom
  # mall::llm_custom takes a single prompt, so we embed the system message within it.
  full_prompt <- paste0(system_message, "\n\n", user_message)

  tryCatch(
    {
      # mall::llm_custom expects a dataframe and a column to apply the prompt to.
      # Since we're generating a dictionary for the entire dataframe, we can
      # create a dummy dataframe with one row and apply the prompt to a dummy column.
      # The actual data sample is embedded in the prompt.
      dummy_data <- data.frame(dummy_col = "trigger_llm", stringsAsFactors = FALSE)

      # Use llm_custom and specify that the output should be JSON.
      # We instruct the LLM within the prompt to produce JSON.
      # mall will return the raw text output, which we then need to parse.
      result_raw <- mall::llm_custom(
        .data = dummy_data,
        col = dummy_col,
        prompt = full_prompt
      )

      # Extract the prediction and parse it as JSON
      # The result will be in the '.pred' column by default.
      json_string <- result_raw$.pred[1]
      result_parsed <- jsonlite::fromJSON(json_string, simplifyVector = TRUE)

      # Convert the list of lists/vectors (from JSON parsing) into a tibble
      # Ensure column order and types match the original intent
      final_result <- tibble::as_tibble(result_parsed)

      cli::cli_alert_success("Dictionary generated successfully!")
      return(final_result)
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