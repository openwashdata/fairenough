# Test helpers for fairenough

create_test_data <- function(temp_dir) {
  # Create simple test datasets
  iris_path <- file.path(temp_dir, "iris.csv")
  mtcars_path <- file.path(temp_dir, "mtcars.csv")
  
  write.csv(iris, iris_path, row.names = FALSE)
  write.csv(mtcars, mtcars_path, row.names = FALSE)
  
  return(c(iris_path, mtcars_path))
}

create_messy_data <- function(temp_dir) {
  # Create data with column names that need janitor cleaning
  messy_data <- data.frame(
    `Bad Name!` = 1:3,
    `Another Bad Name` = 4:6,
    `spaces and symbols@#` = 7:9,
    check.names = FALSE
  )
  
  messy_path <- file.path(temp_dir, "messy.csv")
  write.csv(messy_data, messy_path, row.names = FALSE)
  
  return(messy_path)
}

cleanup_temp_dir <- function(temp_dir) {
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE, force = TRUE)
  }
}