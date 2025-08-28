# Test file format support

test_that("reads CSV files correctly", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(cleanup_temp_dir(temp_dir))
  
  # Create test CSV
  test_data <- data.frame(x = 1:3, y = c("a", "b", "c"))
  csv_path <- file.path(temp_dir, "test.csv")
  write.csv(test_data, csv_path, row.names = FALSE)
  
  # Test reading
  result <- read_data(csv_path)
  
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_equal(result$x, 1:3)
})

test_that("detects supported file formats", {
  expect_true(is_supported_file_type("data.csv"))
  expect_true(is_supported_file_type("data.xlsx"))
  expect_true(is_supported_file_type("data.xls"))
  expect_false(is_supported_file_type("data.txt"))
  expect_false(is_supported_file_type("data.docx"))
})

test_that("filters supported files correctly", {
  files <- c("data.csv", "readme.txt", "data.xlsx", "script.R", "data.xls")
  result <- filter_supported_files(files)
  
  expected <- c("data.csv", "data.xlsx", "data.xls")
  expect_equal(result, expected)
})

test_that("handles empty file lists", {
  expect_equal(filter_supported_files(character(0)), character(0))
  expect_equal(filter_supported_files(c("readme.txt")), character(0))
})