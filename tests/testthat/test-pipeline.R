# Test pipeline steps using existing validation system

test_that("setup creates proper structure", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Run setup
  suppressMessages({
    result <- setup(base_path = temp_dir, verbose = FALSE)
  })
  
  # Check core components (skip .Rproj which may not be created in temp dirs)
  expect_true(check_description_exists(temp_dir))
  expect_true(check_dirs_exist(temp_dir))
  
  # Check that most validations pass
  validation <- validate_setup_completed(temp_dir)
  expect_gte(validation$passed_checks, 3) # At least 3 out of 4 should pass
})

test_that("process handles data files correctly", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Create test data and setup
  create_test_data(temp_dir)
  suppressMessages(setup(base_path = temp_dir, verbose = FALSE))
  
  # Run process
  suppressMessages({
    result <- process(base_path = temp_dir, verbose = FALSE)
  })
  
  # Use existing validation helpers
  expect_true(check_rda_files(temp_dir))
  expect_true(check_csv_files(temp_dir))
  
  # Full validation should pass
  validation <- validate_processing_completed(temp_dir)
  expect_true(validation$all_required_passed)
})

test_that("collect updates DESCRIPTION", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  suppressMessages(setup(base_path = temp_dir, verbose = FALSE))
  
  # Skip collect test in non-interactive mode (requires user input)
  skip("collect() requires interactive input or mocking")
  
  # Check that DESCRIPTION was created/updated
  expect_true(check_description_exists(temp_dir))
})

test_that("generate creates dictionary", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Setup with data
  create_test_data(temp_dir)
  suppressMessages({
    setup(base_path = temp_dir, verbose = FALSE)
    process(base_path = temp_dir, verbose = FALSE)
  })
  
  # Run generate without LLM
  suppressMessages({
    result <- generate(base_path = temp_dir, verbose = FALSE)
  })
  
  # Check dictionary was created
  expect_true(check_dictionary_file(temp_dir))
  expect_true(check_dictionary_structure(temp_dir))
})

test_that("pipeline steps complete in sequence", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  create_test_data(temp_dir)
  
  # Run each step and verify with validation
  suppressMessages({
    setup(base_path = temp_dir, verbose = FALSE)
    expect_gte(validate_setup_completed(temp_dir)$passed_checks, 3)
    
    process(base_path = temp_dir, verbose = FALSE)
    expect_true(validate_processing_completed(temp_dir)$all_required_passed)
    
    # Skip collect in automated tests (needs interactive input)
    # collect(base_path = temp_dir, interactive = FALSE, verbose = FALSE)
    
    generate(base_path = temp_dir, verbose = FALSE)
    # Dictionary should at least be created, even if not fully filled
    expect_true(check_dictionary_file(temp_dir))
    expect_true(check_dictionary_structure(temp_dir))
  })
})