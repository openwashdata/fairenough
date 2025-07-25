# Minimal test function to isolate the issue
test_gendict_minimal <- function(chat) {
  # Test 1: Direct chat call
  cli::cli_h2("Test 1: Direct chat call")
  response1 <- tryCatch({
    chat$chat("Describe the variable 'rowid' with examples: 174, 170, 336")
  }, error = function(e) {
    cli::cli_alert_danger("Direct call failed: {e$message}")
    return(NULL)
  })
  
  if (!is.null(response1)) {
    cli::cli_alert_success("Direct call succeeded: {substr(response1, 1, 50)}...")
  }
  
  # Test 2: Chat call from within a list/loop
  cli::cli_h2("Test 2: Chat call from list")
  prompts <- list("Describe the variable 'rowid' with examples: 174, 170, 336")
  
  response2 <- tryCatch({
    chat$chat(prompts[[1]])
  }, error = function(e) {
    cli::cli_alert_danger("List call failed: {e$message}")
    return(NULL)
  })
  
  if (!is.null(response2)) {
    cli::cli_alert_success("List call succeeded: {substr(response2, 1, 50)}...")
  }
  
  # Test 3: Chat call with glue
  cli::cli_h2("Test 3: Chat call with glue")
  col_name <- "rowid"
  sample_str <- "174, 170, 336"
  
  prompt3 <- glue::glue("Describe the variable '{col_name}' with examples: {sample_str}")
  
  response3 <- tryCatch({
    chat$chat(prompt3)
  }, error = function(e) {
    cli::cli_alert_danger("Glue call failed: {e$message}")
    return(NULL)
  })
  
  if (!is.null(response3)) {
    cli::cli_alert_success("Glue call succeeded: {substr(response3, 1, 50)}...")
  }
  
  # Test 4: Chat call with paste0
  cli::cli_h2("Test 4: Chat call with paste0")
  prompt4 <- paste0("Describe the variable '", col_name, "' with examples: ", sample_str)
  
  response4 <- tryCatch({
    chat$chat(prompt4)
  }, error = function(e) {
    cli::cli_alert_danger("Paste0 call failed: {e$message}")
    return(NULL)
  })
  
  if (!is.null(response4)) {
    cli::cli_alert_success("Paste0 call succeeded: {substr(response4, 1, 50)}...")
  }
  
  # Test 5: Check prompt class
  cli::cli_h2("Test 5: Checking prompt classes")
  cli::cli_alert_info("Direct string class: {class('test')}")
  cli::cli_alert_info("Glue class: {class(prompt3)}")
  cli::cli_alert_info("Paste0 class: {class(prompt4)}")
  
  # Test 6: Convert glue to character
  cli::cli_h2("Test 6: Convert glue to character")
  prompt6 <- as.character(glue::glue("Describe the variable '{col_name}' with examples: {sample_str}"))
  
  response6 <- tryCatch({
    chat$chat(prompt6)
  }, error = function(e) {
    cli::cli_alert_danger("Character conversion failed: {e$message}")
    return(NULL)
  })
  
  if (!is.null(response6)) {
    cli::cli_alert_success("Character conversion succeeded: {substr(response6, 1, 50)}...")
  }
}

# Run the test
# test_gendict_minimal(chat)