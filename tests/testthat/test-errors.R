# Test error handling

test_that("handles missing data_raw directory gracefully", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Setup basic structure but no data_raw
  suppressMessages(setup(base_path = temp_dir, verbose = FALSE))
  unlink(file.path(temp_dir, "data_raw"), recursive = TRUE)
  
  # Should handle missing data_raw
  expect_message(
    process(base_path = temp_dir, verbose = TRUE),
    "data_raw"
  )
})

test_that("handles empty directories appropriately", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  suppressMessages(setup(base_path = temp_dir, verbose = FALSE))
  
  # Should handle empty data_raw
  expect_message(
    process(base_path = temp_dir, verbose = TRUE),
    "No data files"
  )
})

test_that("handles invalid file paths", {
  expect_false(check_description_exists("/nonexistent/path"))
  expect_false(check_dirs_exist("/nonexistent/path"))
  expect_false(check_raw_data_files("/nonexistent/path"))
})

test_that("validation works with missing base_path", {
  # Should use current directory by default
  result <- validate_setup_completed()
  expect_type(result, "list")
  expect_named(result, c("all_passed", "all_required_passed", "total_checks", 
                        "passed_checks", "failed_checks", "items"))
})

test_that("file consistency handles edge cases", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Create required directories
  dir.create(file.path(temp_dir, "data_raw"))
  dir.create(file.path(temp_dir, "data"))
  dir.create(file.path(temp_dir, "inst", "extdata"), recursive = TRUE)
  
  # No files - should return TRUE (consistent empty state)
  expect_true(check_file_consistency(temp_dir))
  
  # Add dictionary.csv - should still be consistent
  write.csv(data.frame(x = 1), file.path(temp_dir, "inst", "extdata", "dictionary.csv"))
  expect_true(check_file_consistency(temp_dir))
})