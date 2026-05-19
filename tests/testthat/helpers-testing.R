# Test helpers for fairenough

create_test_data <- function(temp_dir) {
  # Create simple test datasets
  iris_path <- file.path(temp_dir, "iris.csv")
  mtcars_path <- file.path(temp_dir, "mtcars.csv")
  
  write.csv(iris, iris_path, row.names = FALSE)
  write.csv(mtcars, mtcars_path, row.names = FALSE)
  
  return(c(iris_path, mtcars_path))
}

create_messy_data <- function(temp_dir) {
  # Create data with column names that need janitor cleaning
  messy_data <- data.frame(
    `Bad Name!` = 1:3,
    `Another Bad Name` = 4:6,
    `spaces and symbols@#` = 7:9,
    check.names = FALSE
  )
  
  messy_path <- file.path(temp_dir, "messy.csv")
  write.csv(messy_data, messy_path, row.names = FALSE)
  
  return(messy_path)
}

cleanup_temp_dir <- function(temp_dir) {
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE, force = TRUE)
  }
}

# Duck-typed stand-in for an ellmer chat object. gendict()'s sequential
# path only calls chat$chat(prompt) and expects a character scalar back,
# so a list with a $chat() closure is enough for testing without an
# API key. `responses` cycles through canned replies; if NULL, a
# deterministic templated string is returned. `$calls()` exposes the
# prompts received so tests can assert on them.
make_fake_chat <- function(responses = NULL) {
  state <- new.env(parent = emptyenv())
  state$i <- 0L
  state$calls <- list()

  list(
    chat = function(prompt) {
      state$i <- state$i + 1L
      state$calls[[length(state$calls) + 1L]] <- as.character(prompt)
      if (is.null(responses)) {
        paste0("Mock description #", state$i)
      } else {
        responses[[((state$i - 1L) %% length(responses)) + 1L]]
      }
    },
    calls = function() vapply(state$calls, identity, character(1))
  )
}

# Skip pattern for tests that need a real LLM provider. Place at the
# top of any test that calls a non-mocked ellmer chat.
skip_without_openai <- function() {
  testthat::skip_if(
    identical(Sys.getenv("OPENAI_API_KEY"), ""),
    "No OPENAI_API_KEY in environment"
  )
}