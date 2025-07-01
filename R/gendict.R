#' Generate data dictionary using AI
#'
#' Uses AI (via ellmer package) to automatically generate comprehensive
#' data dictionaries for datasets. This function analyzes column names
#' and sample data to create meaningful descriptions.
#'
#' @param data Data frame. The dataset to generate dictionary for.
#' @param api_key Character string. OpenAI API key. If NULL, will look for OPENAI_API_KEY env var.
#' @param model Character string. AI model to use. Default is "gpt-4o-mini".
#' @param context Character string. Additional context about the dataset.
#'
#' @return Data frame with columns: variable, description, type, example
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple example
#' df <- data.frame(
#'   age = c(25, 30, 35),
#'   income = c(50000, 60000, 70000),
#'   city = c("New York", "Boston", "Chicago")
#' )
#' 
#' dictionary <- gendict(df, api_key = "your-api-key")
#' }
gendict <- function(data, api_key = NULL, model = "gpt-4o-mini", context = NULL) {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("data must be a data frame")
  }
  
  if (nrow(data) == 0) {
    cli::cli_abort("data must have at least one row")
  }
  
  # Check for API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") {
      cli::cli_abort("API key not found. Set OPENAI_API_KEY environment variable or provide api_key parameter.")
    }
  }
  
  # Set API key for ellmer
  Sys.setenv(OPENAI_API_KEY = api_key)
  
  cli::cli_alert_info("Generating dictionary for {ncol(data)} variable{?s}...")
  
  # Prepare data sample for analysis
  sample_size <- min(5, nrow(data))
  data_sample <- data[1:sample_size, ]
  
  # Convert to JSON for the prompt
  data_json <- jsonlite::toJSON(data_sample, auto_unbox = TRUE, pretty = TRUE)
  
  # Define the expected output structure
  output_type_spec <- ellmer::type_array(
    items = ellmer::type_object(
      variable = ellmer::type_string("The variable/column name"),
      description = ellmer::type_string("A clear, concise description of what this variable represents"),
      type = ellmer::type_enum(
        "The data type of the variable",
        values = c("numeric", "character", "logical", "factor", "date", "datetime")
      ),
      unit = ellmer::type_string("The unit of measurement (if applicable, otherwise empty string)")
    )
  )
  
  # Create the prompt
  system_message <- "You are a data science expert specializing in creating clear, comprehensive data dictionaries. Your task is to analyze a dataset sample and generate descriptive metadata for each variable/column."
  
  context_part <- if (!is.null(context)) paste0("Dataset context: ", context, "\n\n") else ""
  
  user_message <- glue::glue("
{context_part}Please analyze this dataset sample and create a data dictionary:

{data_json}

For each variable/column, provide:
1. A clear, descriptive explanation of what the variable represents
2. The appropriate data type
3. Units of measurement (if applicable)

Focus on being descriptive and helpful for someone who will use this data.
")
  
  messages <- list(
    list(role = "system", content = system_message),
    list(role = "user", content = user_message)
  )
  
  # Create chat object and generate dictionary
  tryCatch({
    chat <- ellmer::chat_openai(model = model)
    
    result <- chat$chat_structured(
      messages = messages,
      type = output_type_spec
    )
    
    cli::cli_alert_success("Dictionary generated successfully!")
    return(result)
    
  }, error = function(e) {
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
  })
}

#' Example function showing ellmer usage for sentiment analysis
#'
#' This is an example function demonstrating how to use ellmer for 
#' structured AI text analysis. It's not exported and serves as
#' a template for other AI-powered functions.
#'
#' @param texts Character vector. Text strings to analyze.
#' @param api_key Character string. OpenAI API key.
#' @param model Character string. AI model to use.
#'
#' @return Data frame with sentiment analysis results.
#'
#' @examples
#' \dontrun{
#' texts <- c("I love this!", "This is terrible", "It's okay")
#' sentiment_analysis_example(texts, api_key = "your-key")
#' }
sentiment_analysis_example <- function(texts, api_key = NULL, model = "gpt-4o-mini") {
  
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") {
      stop("API key required")
    }
  }
  
  # Set API key
  Sys.setenv(OPENAI_API_KEY = api_key)
  
  # Format input data
  batched_input_list <- lapply(seq_along(texts), function(i) {
    list(id = i, text = texts[i])
  })
  batched_input_json <- jsonlite::toJSON(batched_input_list, auto_unbox = TRUE, pretty = TRUE)
  
  # Define output structure
  output_type_spec <- ellmer::type_array(
    items = ellmer::type_object(
      id = ellmer::type_integer("The ID of the original input item."),
      sentiment = ellmer::type_enum(
        "The sentiment of the text.",
        values = c("positive", "negative", "neutral")
      ),
      reason = ellmer::type_string("A brief reason for the sentiment classification.")
    )
  )
  
  # Create messages
  system_message <- "You are a helpful text analysis assistant. Analyze sentiment and provide brief reasoning."
  user_message_template <- "Here are the texts for sentiment analysis:\n\n{{texts_json}}"
  
  messages <- list(
    list(role = "system", content = system_message),
    list(role = "user", content = glue::glue(user_message_template, texts_json = batched_input_json))
  )
  
  # Analyze
  chat <- ellmer::chat_openai(model = model)
  results <- chat$chat_structured(
    messages = messages,
    type = output_type_spec
  )
  
  return(results)
}