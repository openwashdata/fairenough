options(usethis.quiet = TRUE)
# TEST setup_roxygen --------------------------------------------------------
test_that("setup_roxygen throws error when no tidy dataset available in data/", {
  create_local_package()
  rlang::local_interactive(FALSE)
  fairenough::setup_rawdata()
  file.create("data-raw/dictionary.csv")
  expect_error(setup_roxygen())
})

test_that("setup_roxygen throws error when no dictionary data/", {
  create_local_package()
  rlang::local_interactive(FALSE)
  fairenough::setup_rawdata()
  d1 <- data.frame(id = 1:3, name = c("A", "B", "C"))
  usethis::use_data(d1)
  expect_error(setup_roxygen())
})


test_that("re-run setup_roxygen on existing roxygen files won't change the title and description fields", {
  create_local_package()
  rlang::local_interactive(FALSE)
  fairenough::setup_rawdata()
  d1 <- data.frame(id = 1:3, name = c("A", "B", "C"))
  usethis::use_data(d1)
  fairenough::setup_dictionary()
  fairenough::setup_roxygen()
  doc <- readLines(file.path(getwd(), "R", "d1.R"))
  doc[1] <- "#' d1: Test dataset"
  writeLines(doc, file.path(getwd(), "R", "d1.R"))

  dict_path <- file.path(getwd(), "data-raw", "dictionary.csv")
  dict <- read.csv(dict_path)
  file.remove(dict_path)
  dict$description[1] <- "ID Number"
  write.csv(dict, dict_path)
  fairenough::setup_roxygen()
  expect_true(startsWith(readLines(file.path(getwd(), "R", "d1.R"))[1], "#' d1: Test dataset"))
})

test_that("setup_roxygen works well", {
  create_local_package()
  rlang::local_interactive(FALSE)
  fairenough::setup_rawdata()
  d1 <- data.frame(id = 1:3, name = c("A", "B", "C"))
  usethis::use_data(d1)
  fairenough::setup_dictionary()
  fairenough::setup_roxygen()
  expect_true(file.exists(file.path(getwd(), "R", "d1.R")))
})
