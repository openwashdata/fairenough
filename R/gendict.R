gendict <- function(data, chat, context = NULL, sample_size = 5) {
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

  # Define the expected output structure
  output_type_spec <- ellmer::type_array(
    items = ellmer::type_object(
      variable = ellmer::type_string("The variable/column name"),
      type = ellmer::type_enum(
        "The data type of the variable",
        values = c(
          "numeric",
          "character",
          "logical",
          "factor",
          "date",
          "datetime"
        )
      ),
      description = ellmer::type_string(
        "A clear, concise description of what this variable represents"
      ),
      unit = ellmer::type_string(
        "The unit of measurement (if applicable, otherwise empty string)"
      )
    )
  )

  # Create the prompt
  system_message <- paste0(
    "You are a data science expert specializing in creating clear, comprehensive data dictionaries. ",
    "Your task is to analyze a dataset sample and generate descriptive metadata for each variable/column. ",
    "Follow these steps for each variable:\n\n",
    "1. **Analyze the column name:** What does the name itself suggest about the data it might contain?\n",
    "2. **Examine the provided example values:** Look for patterns, units, categories, or ranges that can help you understand the nature of the data. Pay attention to the presence of 'null' or 'None' values, indicating missing data.\n",
    "3. **Infer the likely meaning and context:** Based on the column name and example values, what real-world concept or measurement does this column likely represent? Try to infer the broader domain or field of study this data might belong to (e.g., environmental science, social surveys, medical records).\n",
    "4. **Determine the data type:** Clearly identify the likely data type of the column. Use precise terms like 'numeric', 'character', 'logical', 'factor', 'date', or 'datetime' as per the schema.\n", # Match enum
    "5. **Write a concise description:** Combine your inferences into a brief description (1-2 sentences) that explains what the column *represents* in the real world and its inferred data type. Use clear and accessible language, avoiding overly technical jargon unless essential."
  )

  context_part <- if (!is.null(context)) {
    paste0("Dataset context: ", context, "\n\n")
  } else {
    ""
  }

  user_message <- glue::glue(
    "
{context_part}Please analyze this dataset sample and create a data dictionary:

{data_json}

For each variable/column, provide:
1. A clear, descriptive explanation of what the variable represents
2. The appropriate data type
3. Units of measurement (if applicable)

Focus on being descriptive and helpful for someone who will use this data.
"
  )

  messages <- list(
    list(role = "system", content = system_message),
    list(role = "user", content = user_message)
  )

  # Use the pre-existing chat object
  tryCatch(
    {
      result <- chat$chat_structured(
        # Use the provided chat object
        messages = messages,
        type = output_type_spec
      )

      cli::cli_alert_success("Dictionary generated successfully!")
      return(result)
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