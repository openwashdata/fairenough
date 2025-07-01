options(usethis.quiet = TRUE)
test_that("File and directory are created", {
  create_local_package()

  rlang::local_interactive(FALSE)
  print(getwd())
  fairenough::setup_rawdata()
  expect_true(file.exists("data-raw/data_processing.R"))
})
unlink("data-raw", recursive = TRUE)
