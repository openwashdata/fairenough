# Test data consistency through pipeline

test_that("row counts preserved through pipeline", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Create test data
  test_files <- create_test_data(temp_dir)
  
  # Run setup and process
  suppressMessages({
    setup(base_path = temp_dir, verbose = FALSE)
    process(base_path = temp_dir, verbose = FALSE)
  })
  
  # Check row counts match
  iris_rda_path <- file.path(temp_dir, "data", "iris.rda")
  iris_csv_path <- file.path(temp_dir, "inst", "extdata", "iris.csv")
  
  if (file.exists(iris_rda_path) && file.exists(iris_csv_path)) {
    # Load RDA
    temp_env <- new.env()
    load(iris_rda_path, envir = temp_env)
    rda_data <- get(ls(temp_env)[1], envir = temp_env)
    
    # Load CSV
    csv_data <- read.csv(iris_csv_path)
    
    expect_equal(nrow(rda_data), nrow(csv_data))
    expect_equal(nrow(rda_data), nrow(iris)) # Original data
  }
})

test_that("janitor cleaning applied consistently", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Create messy data
  create_messy_data(temp_dir)
  
  suppressMessages({
    setup(base_path = temp_dir, verbose = FALSE)
    process(base_path = temp_dir, verbose = FALSE)
  })
  
  # Check that RDA and CSV have same cleaned names
  rda_path <- file.path(temp_dir, "data", "messy.rda")
  csv_path <- file.path(temp_dir, "inst", "extdata", "messy.csv")
  
  if (file.exists(rda_path) && file.exists(csv_path)) {
    temp_env <- new.env()
    load(rda_path, envir = temp_env)
    rda_data <- get(ls(temp_env)[1], envir = temp_env)
    
    csv_data <- read.csv(csv_path)
    
    # Names should be cleaned and identical
    expect_equal(names(rda_data), names(csv_data))
    expect_false(any(grepl("[^a-zA-Z0-9_]", names(rda_data)))) # No special chars
  }
})

test_that("file consistency check works correctly", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Create test data and process
  create_test_data(temp_dir)
  
  suppressMessages({
    setup(base_path = temp_dir, verbose = FALSE)
    process(base_path = temp_dir, verbose = FALSE)
  })
  
  # Should pass consistency check
  expect_true(check_file_consistency(temp_dir))
})