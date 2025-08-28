# Test validation system

test_that("validation functions return consistent structure", {
  validation_functions <- list(
    validate_setup_completed(),
    validate_processing_completed(),
    validate_metadata_collected(),
    validate_dictionary_completed(),
    validate_build_completed()
  )
  
  expected_names <- c("all_passed", "all_required_passed", "total_checks", 
                     "passed_checks", "failed_checks", "items")
  
  for (validation in validation_functions) {
    expect_type(validation, "list")
    expect_named(validation, expected_names)
    expect_type(validation$all_passed, "logical")
    expect_type(validation$all_required_passed, "logical")
    expect_type(validation$items, "list")
  }
})

test_that("validation helpers work correctly", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Test description check
  expect_false(check_description_exists(temp_dir))
  writeLines("Package: test", file.path(temp_dir, "DESCRIPTION"))
  expect_true(check_description_exists(temp_dir))
  
  # Test directory check
  expect_false(check_dirs_exist(temp_dir))
  dir.create(file.path(temp_dir, "data"))
  dir.create(file.path(temp_dir, "data_raw"))  
  dir.create(file.path(temp_dir, "inst", "extdata"), recursive = TRUE)
  expect_true(check_dirs_exist(temp_dir))
})

test_that("file existence checks work", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Test data file checks
  expect_false(check_raw_data_files(temp_dir))
  expect_false(check_rda_files(temp_dir))
  expect_false(check_csv_files(temp_dir))
  
  # Create directories
  dir.create(file.path(temp_dir, "data_raw"))
  dir.create(file.path(temp_dir, "data"))
  dir.create(file.path(temp_dir, "inst", "extdata"), recursive = TRUE)
  
  # Add files
  write.csv(iris, file.path(temp_dir, "data_raw", "test.csv"))
  save(iris, file = file.path(temp_dir, "data", "test.rda"))
  write.csv(iris, file.path(temp_dir, "inst", "extdata", "test.csv"))
  
  expect_true(check_raw_data_files(temp_dir))
  expect_true(check_rda_files(temp_dir))
  expect_true(check_csv_files(temp_dir))
})

test_that("dictionary checks work", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  expect_false(check_dictionary_file(temp_dir))
  
  # Create dictionary
  dir.create(file.path(temp_dir, "inst", "extdata"), recursive = TRUE)
  dict <- data.frame(
    directory = "data",
    file_name = "test.rda", 
    variable_name = "x",
    variable_type = "integer",
    description = "test variable"
  )
  write.csv(dict, file.path(temp_dir, "inst", "extdata", "dictionary.csv"), row.names = FALSE)
  
  expect_true(check_dictionary_file(temp_dir))
  expect_true(check_dictionary_structure(temp_dir))
  expect_true(check_descriptions_filled(temp_dir))
})