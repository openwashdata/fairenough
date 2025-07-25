# User setup:
#
# library(ellmer)
# Sys.setenv(OPENAI_API_KEY = "YOUR_BLABLADOR_TOKEN") # or setup .Renviron
# # cat("OPENAI_API_KEY=YOUR_BLABLADOR_TOKEN\n", file = file.path(Sys.getenv("HOME"), ".Renviron"), append = TRUE)
# chat <- chat_openai(
# base_url = "https://api.helmholtz-blablador.fz-juelich.de/v1",
# model = "alias-fast" # Or other Blablador aliases like "alias-code", "alias-large", etc.
# )

# Helper function: Display progress and results in a clean format
.gendict_display_progress <- function(col_names, responses, current_index = NULL) {
  if (!is.null(current_index)) {
    # Display single result
    cli::cli_alert_success("{col_names[current_index]}: {responses[current_index]}")
    
    # Show progress every 5 items or at the end
    if (current_index %% 5 == 0 || current_index == length(col_names)) {
      pct <- round(current_index / length(col_names) * 100)
      cli::cli_alert_info("Progress: {current_index}/{length(col_names)} ({pct}%)")
    }
  } else {
    # Display all results
    for (i in seq_along(col_names)) {
      cli::cli_alert_success("{col_names[i]}: {responses[i]}")
    }
    cli::cli_alert_info("Completed: {length(col_names)}/{length(col_names)} (100%)")
  }
}

# Helper function: Sequential chat (one at a time)
.gendict_sequential_chat <- function(chat, prompts, col_names, delay = 0) {
  llm_responses <- vector("character", length(prompts))
  
  cli::cli_alert_info("Processing {length(prompts)} columns sequentially...")
  
  for (i in seq_along(prompts)) {
    # Add a small delay between requests to avoid rate limiting
    if (i > 1 && delay > 0) {
      Sys.sleep(delay)
    }
    
    response <- tryCatch({
      chat$chat(prompts[[i]])
    }, error = function(e) {
      cli::cli_alert_warning("Failed to get response for {col_names[i]}: {e$message}")
      paste("(unable to generate) Description unavailable for", col_names[i])
    })
    
    llm_responses[i] <- response
    
    # Display progress
    .gendict_display_progress(col_names, llm_responses, i)
  }
  
  return(llm_responses)
}

# Helper function: Parallel chat using ellmer::parallel_chat
.gendict_parallel_chat <- function(chat, prompts, col_names) {
  cli::cli_alert_info("Processing {length(prompts)} columns in parallel...")
  
  llm_responses <- tryCatch({
    ellmer::parallel_chat(
      chat = chat,
      prompts = prompts,
      max_active = 10
    )
  }, error = function(e) {
    cli::cli_alert_warning("parallel_chat failed: {e$message}. Falling back to sequential processing.")
    return(.gendict_sequential_chat(chat, prompts, col_names))
  })
  
  # Extract text content if response is a list
  if (is.list(llm_responses)) {
    llm_responses <- sapply(seq_along(llm_responses), function(i) {
      x <- llm_responses[[i]]
      
      if (is.character(x) && length(x) == 1) return(x)
      
      if (is.list(x)) {
        # Try common response fields
        for (field in c("content", "text", "message", "response", "output")) {
          if (!is.null(x[[field]])) return(x[[field]])
        }
        
        # If unnamed list with single character element
        if (length(x) == 1 && is.null(names(x)) && is.character(x[[1]])) {
          return(x[[1]])
        }
      }
      
      cli::cli_alert_warning("Could not extract response for {col_names[i]}")
      return(paste("(unable to extract) Description unavailable for", col_names[i]))
    })
  }
  
  # Display all results at once for parallel processing
  .gendict_display_progress(col_names, llm_responses)
  
  return(llm_responses)
}

# Helper function: Sequential with fresh connection for each request
.gendict_sequential_fresh <- function(chat_config, prompts, col_names) {
  llm_responses <- vector("character", length(prompts))
  
  cli::cli_alert_info("Processing {length(prompts)} columns (fresh connection each time)...")
  
  for (i in seq_along(prompts)) {
    response <- tryCatch({
      # Create a fresh chat connection for each request
      fresh_chat <- ellmer::chat_openai(
        base_url = chat_config$base_url,
        api_key = chat_config$api_key,
        model = chat_config$model
      )
      
      # Send the request with the fresh connection
      result <- fresh_chat$chat(prompts[[i]])
      
      # Ensure we got a response
      if (!is.null(result) && nchar(result) > 0) {
        result
      } else {
        paste("(empty response) Description unavailable for", col_names[i])
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to get response for {col_names[i]}: {e$message}")
      paste("(unable to generate) Description unavailable for", col_names[i])
    })
    
    llm_responses[i] <- response
    
    # Display progress
    .gendict_display_progress(col_names, llm_responses, i)
    
    # Small delay between requests
    if (i < length(prompts)) {
      Sys.sleep(0.5)
    }
  }
  
  return(llm_responses)
}

# Helper function: Generate sample values or range for a column
.generate_samples <- function(column_data, sample_size = 5) {
  # Check if continuous
  if (.is_continuous(column_data)) {
    # For continuous variables, return min and max as a list
    non_na_data <- na.omit(column_data)
    if (length(non_na_data) == 0) {
      return(list(type = "continuous", min = NA, max = NA))
    }
    return(list(
      type = "continuous",
      min = min(non_na_data),
      max = max(non_na_data)
    ))
  } else {
    # For categorical/discrete variables, return sampled values
    unique_values <- unique(column_data)
    
    # Sample values
    if (length(unique_values) <= sample_size) {
      sampled_values <- unique_values
    } else {
      sampled_values <- sample(unique_values, sample_size, replace = FALSE)
    }
    
    return(list(
      type = "categorical",
      values = sampled_values,
      has_more = length(unique_values) > sample_size
    ))
  }
}

# Helper function: Check if a variable is continuous
.is_continuous <- function(x, max_levels = 10) {
  # Check if numeric
  if (!is.numeric(x)) return(FALSE)
  
  # Remove NA values for unique count
  x_no_na <- na.omit(x)
  n_unique <- length(unique(x_no_na))
  
  # Consider continuous if:
  # 1. More unique values than max_levels threshold
  # 2. Or if it contains decimal values (not just integers)
  is_continuous <- n_unique > max_levels || any(x_no_na != floor(x_no_na))
  
  return(is_continuous)
}

# Helper function: Add examples or ranges to descriptions
.add_examples_to_descriptions <- function(descriptions, sample_data, col_names) {
  sapply(seq_along(col_names), function(i) {
    samples <- sample_data[[i]]
    
    if (samples$type == "continuous") {
      # For continuous variables, add the range
      if (is.na(samples$min) && is.na(samples$max)) {
        paste0(descriptions[i], " (All NA)")
      } else {
        paste0(descriptions[i], " (Range: ", samples$min, " to ", samples$max, ")")
      }
    } else {
      # For categorical/discrete variables, get first 3 examples
      example_values <- head(samples$values, 3)
      # Handle NA values
      example_strings <- sapply(example_values, function(x) {
        if (is.na(x)) return("NA")
        as.character(x)
      })
      examples_str <- paste(example_strings, collapse = ", ")
      
      # Combine description with examples
      paste0(descriptions[i], " (Examples: ", examples_str, ")")
    }
  })
}

#' Generate data dictionary using LLM
#' 
#' @param data Data frame or path to CSV/Excel file
#' @param chat Chat object from ellmer package
#' @param context Optional context for generation
#' @param sample_size Number of sample values to show
#' @param method Processing method (sequential, sequential_fresh, parallel)
#' @param test_llm_connection Whether to test LLM connection first
#' @return Tibble with variable names and descriptions
#' @export
gendict <- function(data, chat, context = NULL, sample_size = 5, method = "sequential", test_llm_connection = FALSE) {
  # Validate method parameter
  method <- match.arg(method, c("sequential", "sequential_fresh", "parallel"))
  
  # Use utility function to read and validate data
  data <- read_data(data)

  cli::cli_alert_info("Generating dictionary for {ncol(data)} variable{?s}...")
  
  # Test LLM connection first
  if (test_llm_connection) {
    cli::cli_alert_info("Testing LLM connection...")
    test_response <- tryCatch({
      chat$chat("Hello, please respond with 'Connection successful'")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to connect to LLM: {e$message}")
      return(NULL)
    })
    
    if (!is.null(test_response)) {
      cli::cli_alert_success("LLM connection test: {substr(test_response, 1, 50)}...")
    } else {
      cli::cli_abort("Cannot proceed without LLM connection")
    }
  }

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

  # Define the allowed data types
  allowed_data_types <- c(
    "logical", "integer", "numeric", "text", "datetime",
    "categorical", "identifier", "spatial", "binary", "json", "array"
  )

  # Prepare a list of prompts, one for each column, including the sampled data
  column_prompts <- list()
  sample_data <- list()  # Store sample data for later use
  sample_strings <- list()  # Store formatted sample strings
  continuity_flags <- list()  # Store whether each column is continuous
  col_names <- names(data)

  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    column_data <- data[[col_name]]
    
    # Generate samples using helper function
    samples <- .generate_samples(column_data, sample_size)
    sample_data[[i]] <- samples
    
    # Check if continuous and store flag
    is_cont <- samples$type == "continuous"
    continuity_flags[[i]] <- is_cont
    
    # Convert samples to string for prompt
    if (is_cont) {
      if (is.na(samples$min) && is.na(samples$max)) {
        sample_str <- "All NA"
      } else {
        sample_str <- paste0(samples$min, " to ", samples$max)
      }
    } else {
      # Handle NA values in categorical samples
      sample_values <- sapply(samples$values, function(x) {
        if (is.na(x)) return("NA")
        as.character(x)
      })
      sample_str <- paste(sample_values, collapse = ", ")
      if (samples$has_more) {
        sample_str <- paste0(sample_str, ", ...")
      }
    }
    
    # Store the formatted string for later use
    sample_strings[[i]] <- sample_str

    # Construct a comprehensive prompt for better descriptions
    prompt_context <- ""
    if (!is.null(context)) {
      prompt_context <- paste0("Dataset context: ", context, ".")
    } else {
      # Prepare context from DESCRIPTION file
      tryCatch({
        desc_obj <- desc::desc()
        title_val <- desc_obj$get_field("Title")
        description_val <- desc_obj$get_field("Description")

        if (!is.null(title_val) && nzchar(title_val)) {
          prompt_context <- paste0(prompt_context, "Dataset Title: ", title_val, ".")
        }
        if (!is.null(description_val) && nzchar(description_val)) {
          prompt_context <- paste0(prompt_context, "Dataset Description: ", description_val, ".")
        }
      }, error = function(e) {
        cli::cli_alert_warning("Could not read DESCRIPTION file using 'desc' package: {e$message}")
      })
    }
    
    # Adjust instructions based on whether variable is continuous
    variable_type <- if (is_cont) "continuous numeric" else "categorical/discrete"
    
    column_prompts[[i]] <- glue::glue(
      "You are a data dictionary expert. Write a precise, single-sentence description for this {variable_type} variable: {col_name}.

CONTEXT:
{prompt_context}
Variable: {col_name}
Variable Type: {variable_type}
{if (is_cont) 'Value range' else 'Sample values'}: {sample_str}
Other columns: {paste(col_names[col_names != col_name], collapse=\", \")}

INSTRUCTIONS:
1. Analyze the variable name and {if (is_cont) 'value range' else 'sample values'} to understand what is being measured
2. Include units of measurement if apparent from the name or values
3. Be specific about what entity is being described and what attribute is measured
4. Write ONLY the description sentence - no variable name, no 'The variable X represents', no examples, no additional text
{if (is_cont) '5. For continuous variables, focus on what is being measured rather than specific values' else ''}

REQUIRED FORMAT: [Entity] [attribute/measurement] [units if applicable].

EXAMPLES (follow this exact format for style and length):
- Body mass of the penguin measured in grams.
- Species classification of the penguin.
- Length of the penguin's flipper measured in millimeters.
- Year when the observation was recorded.
- Biological sex of the penguin.

CRITICAL: Output only the single description sentence. Do not include the variable name, do not add examples, do not add any prefacing words.

DESCRIPTION:")
  }

  tryCatch(
    {
      cli::cli_alert_info("Using {method} method to process {length(column_prompts)} prompts...")
      
      # Extract chat configuration for fresh connections
      chat_config <- list(
        base_url = "https://api.helmholtz-blablador.fz-juelich.de/v1",
        api_key = Sys.getenv("OPENAI_API_KEY"),
        model = "alias-fast"  # Use same model as in setup
      )
      
      # Use the appropriate helper function based on method
      llm_responses <- switch(method,
        sequential = .gendict_sequential_chat(chat, column_prompts, col_names),
        sequential_fresh = .gendict_sequential_fresh(chat_config, column_prompts, col_names),
        parallel = .gendict_parallel_chat(chat, column_prompts, col_names),
      )

      # Create the final dictionary with variable names, descriptions, and examples/ranges
      descriptions_with_examples <- .add_examples_to_descriptions(
        llm_responses, 
        sample_data, 
        col_names
      )
      
      final_dict <- tibble::tibble(
        variable = col_names,
        description = descriptions_with_examples
      )

      cli::cli_alert_success("Dictionary generated successfully!")
      return(final_dict)
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to generate dictionary: {e$message}")

      # Return a basic fallback dictionary
      fallback_dict <- tibble::tibble(
        variable = names(data),
        description = paste("Variable:", names(data))
      )

      cli::cli_alert_info("Returning basic dictionary structure")
      return(fallback_dict)
    }
  )
}