options(usethis.quiet = TRUE)
# TEST setup_dictionary --------------------------------------------------------
test_that("setup_dictionary throws error when not in correct working directory", {
  # Mocking the current working directory without DESCRIPTION and NAMESPACE
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  tempdir <- tempdir()
  setwd(tempdir)
  expect_error(setup_dictionary(), "You are not in the correct working directory")
})

test_that("setup_dictionary throws error when data-raw directory does not exist", {
  # Mocking the current working directory with DESCRIPTION file but without data-raw directory
  create_local_package()
  rlang::local_interactive(FALSE)
  expect_error(setup_dictionary())
})

test_that("setup_dictionary throws error when tidy data does not exist", {
  # Mocking the current working directory with DESCRIPTION file but without data-raw directory
  create_local_package()
  rlang::local_interactive(FALSE)
  fairenough::setup_rawdata()
  expect_error(setup_dictionary())
})

test_that("setup_dictionary throws no error", {
  # Mocking the current working directory with DESCRIPTION file but without data-raw directory
  create_local_package()
  rlang::local_interactive(FALSE)
  fairenough::setup_rawdata()
  mockdata <- data.frame(id = 1:3, name = c("A", "B", "C"))
  usethis::use_data(mockdata)
  expect_no_error(setup_dictionary())
})

test_that("setup_dictionary sets up a dictionary with correct values", {
  create_local_package()
  rlang::local_interactive(FALSE)
  fairenough::setup_rawdata()
  mockdata <- data.frame(id = 1:3, name = c("A", "B", "C"),
                         category = factor("dog", "cat", "dog"),
                         measure = c(3.123, 39.1, 5.3),
                         here = c(TRUE, FALSE, FALSE))
  usethis::use_data(mockdata)
  fairenough::setup_dictionary()
  expect_true(file.exists(file.path("data-raw", "dictionary.csv")))
  dict <- read.csv(file.path("data-raw", "dictionary.csv"))

  expect_equal(dict$file_name[1], "mockdata.rda")
  expect_equal(dict$variable_name[1], "id")
  expect_equal(dict$variable_type[1], "integer")
  expect_equal(dict$variable_name[2], "name")
  expect_equal(dict$variable_type[2], "character")
  expect_equal(dict$variable_type[3], "factor")
  expect_equal(dict$variable_type[4], "numeric")
  expect_equal(dict$variable_type[5], "logical")

})

#TEST fill_dictionary ----------------------------------------------------------
test_that("setup_dictionary throws error when not in correct working directory", {
  create_local_package()
  rlang::local_interactive(FALSE)
  expect_error(fill_dictionary(dict_path = "dict.csv"))
})

