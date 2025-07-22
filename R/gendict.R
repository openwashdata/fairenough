# User setup:
#
# library(ellmer)
# Sys.setenv(OPENAI_API_KEY = "YOUR_BLABLADOR_TOKEN") # or setup .Renviron
# # cat("OPENAI_API_KEY=YOUR_BLABLADOR_TOKEN\n", file = file.path(Sys.getenv("HOME"), ".Renviron"), append = TRUE)
# chat <- chat_openai(
# base_url = "https://api.helmholtz-blablador.fz-juelich.de/v1",
# model = "alias-fast" # Or other Blablador aliases like "alias-code", "alias-large", etc.
# )

# Helper function: Sequential chat (one at a time)
.gendict_sequential_chat <- function(chat, prompts, col_names, delay = 0.5) {
  llm_responses <- vector("character", length(prompts))
  
  cli::cli_progress_bar("Processing columns sequentially", total = length(prompts))
  
  for (i in seq_along(prompts)) {
    cli::cli_progress_update()
    
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
  }
  
  cli::cli_progress_done()
  return(llm_responses)
}

# Helper function: Parallel chat using ellmer::parallel_chat
.gendict_parallel_chat <- function(chat, prompts, col_names) {
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
  
  return(llm_responses)
}

# Helper function: Sequential with fresh connection for each request
.gendict_sequential_wait <- function(chat_config, prompts, col_names) {
  llm_responses <- vector("character", length(prompts))
  
  cli::cli_progress_bar("Processing columns (fresh connection each time)", total = length(prompts))
  
  for (i in seq_along(prompts)) {
    cli::cli_progress_update()
    
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
    
    # Small delay between requests
    if (i < length(prompts)) {
      Sys.sleep(0.5)
    }
  }
  
  cli::cli_progress_done()
  return(llm_responses)
}

# Helper function: Batch processing (all prompts in one request)
.gendict_batch_chat <- function(chat, prompts, col_names) {
  # Combine all prompts into one with clear separators
  combined_prompt <- paste0(
    "Please provide descriptions for the following variables. ",
    "Respond with one line per variable in the exact order given:",
    paste(sapply(seq_along(prompts), function(i) {
      paste0("VARIABLE ", i, ":\n", prompts[[i]], "\n")
    }), collapse = "\n"),
    "\nProvide exactly ", length(prompts), " responses, one per line."
  )
  
  response <- tryCatch({
    chat$chat(combined_prompt)
  }, error = function(e) {
    cli::cli_alert_warning("Batch processing failed: {e$message}")
    return(NULL)
  })
  
  if (is.null(response)) {
    # Fall back to sequential
    return(.gendict_sequential_chat(chat, prompts, col_names))
  }
  
  # Split response by lines and match to variables
  response_lines <- trimws(strsplit(response, "\n")[[1]])
  response_lines <- response_lines[nchar(response_lines) > 0]
  
  if (length(response_lines) != length(prompts)) {
    cli::cli_alert_warning("Batch response count mismatch. Expected {length(prompts)}, got {length(response_lines)}")
    # Pad or truncate as needed
    if (length(response_lines) < length(prompts)) {
      response_lines <- c(response_lines, rep("(missing) Description unavailable", length(prompts) - length(response_lines)))
    } else {
      response_lines <- response_lines[1:length(prompts)]
    }
  }
  
  return(response_lines)
} 

gendict <- function(data, chat, context = NULL, sample_size = 5, method = "sequential_wait", test_llm_connection = FALSE) {
  # Validate method parameter
  method <- match.arg(method, c("sequential", "sequential_wait", "parallel", "batch"))
  # Handle file path input
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) {
      cli::cli_abort("File not found: {data}")
    }
    
    # Determine file type and read accordingly
    file_ext <- tolower(tools::file_ext(data))
    
    if (file_ext == "csv") {
      cli::cli_alert_info("Reading CSV file: {data}")
      data <- readr::read_csv(data, show_col_types = FALSE)
    } else if (file_ext %in% c("xlsx", "xls")) {
      cli::cli_alert_info("Reading Excel file: {data}")
      data <- readxl::read_excel(data)
    } else {
      cli::cli_abort("Unsupported file type: {file_ext}. Supported types: csv, xlsx, xls")
    }
  }
  
  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("data must be a data frame or a path to a CSV/Excel file")
  }

  if (nrow(data) == 0) {
    cli::cli_abort("data must have at least one row")
  }

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
  sample_strings <- list()  # Store sample strings for later use
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
    # Handle NA values and special characters
    sample_str <- paste(sapply(sampled_values, function(x) {
      if (is.na(x)) return("NA")
      as.character(x)
    }), collapse = ", ")
    
    # Store the sample string for later use
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
          prompt_context <- paste0(prompt_context, "Package Title: ", title_val, ".")
        }
        if (!is.null(description_val) && nzchar(description_val)) {
          prompt_context <- paste0(prompt_context, "Package Description: ", description_val, ".")
        }
      }, error = function(e) {
        cli::cli_alert_warning("Could not read DESCRIPTION file using 'desc' package: {e$message}")
      })
    }
    
    # Add dataset overview for context
    columns_context <- paste0("This dataset contains columns: ", paste(col_names, collapse=", "), ".")
    
    column_prompts[[i]] <- glue::glue(
      "You are a highly skilled data science expert specializing in data understanding.
      Your task is to analyze the provided information and generate only the requested details in the specified
      format: Description (Example values).
      Do not include any conversational filler, explanations, or additional text.
      Provide only ONE response in the specified format.
    
      {prompt_context}
      {columns_context}
      Variable: {col_name}.
      Example values from {col_name}: {paste0(sample_str, collapse = \", \")}.
      Think very hard and based on the variable name, example values, and dataset context.
      Provide a clear, concise description of 1 sentence of what this variable represents.
    
      For example:
      Body mass of the penguin in grams.
    
      Response:
      ")
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
        sequential_wait = .gendict_sequential_wait(chat_config, column_prompts, col_names),
        parallel = .gendict_parallel_chat(chat, column_prompts, col_names),
        batch = .gendict_batch_chat(chat, column_prompts, col_names)
      )

      # Create the final dictionary with variable names, descriptions, and examples
      descriptions_with_examples <- sapply(seq_along(col_names), function(i) {
        # Get first 3 examples from the stored sample strings
        sample_values <- strsplit(sample_strings[[i]], ", ")[[1]]
        first_three <- head(sample_values, 3)
        examples_str <- paste(first_three, collapse = ", ")
        
        # Combine description with examples
        paste0(llm_responses[i], " (Examples: ", examples_str, ")")
      })
      
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