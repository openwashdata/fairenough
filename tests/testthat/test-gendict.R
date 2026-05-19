# Test gendict() against the fake chat mock from helpers-testing.R
# These tests exercise the sequential code path (default) and never
# hit a real LLM provider.

test_that("gendict returns a tibble with variable + description columns", {
  data <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  chat <- make_fake_chat()

  result <- suppressMessages(gendict(data, chat = chat))

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("variable", "description"))
  expect_equal(nrow(result), ncol(data))
})

test_that("gendict variable column matches input column names in order", {
  data <- data.frame(alpha = 1:3, beta = 4:6, gamma = 7:9)
  chat <- make_fake_chat()

  result <- suppressMessages(gendict(data, chat = chat))

  expect_equal(result$variable, names(data))
})

test_that("gendict description column is non-empty for every variable", {
  data <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  chat <- make_fake_chat()

  result <- suppressMessages(gendict(data, chat = chat))

  expect_true(all(nzchar(result$description)))
})

test_that("gendict sends one prompt per input column to the chat", {
  data <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  chat <- make_fake_chat()

  suppressMessages(gendict(data, chat = chat))

  expect_length(chat$calls(), ncol(data))
})
