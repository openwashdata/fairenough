# End-to-end test of fairenough() on the no-LLM path.
# Uses a tiny synthetic CSV and verifies that the orchestrator produces
# the canonical R-package layout. No real LLM provider involved.

test_that("fairenough() builds the package layout end-to-end without a chat", {
  # Heavyweight: exercises the full pipeline including build_package's
  # devtools::install and pkgdown::build_site. Skipped on CRAN so
  # reviewers don't wait for the install/knit cycle.
  testthat::skip_on_cran()

  tmp <- withr::local_tempdir()

  # Route devtools::install into a per-test temp library so the
  # generated `testpkg` does not leak into the user's R library.
  local_lib <- withr::local_tempdir()
  withr::local_libpaths(local_lib, action = "prefix")

  write.csv(
    data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE),
    file.path(tmp, "tiny.csv"),
    row.names = FALSE
  )

  suppressMessages(
    fairenough(
      base_path = tmp,
      chat = NULL,
      verbose = FALSE,
      overwrite = TRUE,
      interactive = FALSE,
      pkg_name = "testpkg",
      title = "Test Package",
      description = "A test package for end-to-end testing."
    )
  )

  expect_true(file.exists(file.path(tmp, "DESCRIPTION")))
  expect_true(dir.exists(file.path(tmp, "R")))
  expect_true(dir.exists(file.path(tmp, "data")))
  expect_true(dir.exists(file.path(tmp, "inst", "extdata")))
})
