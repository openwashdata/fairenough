#' Initialize data_raw directory
#' 
#' @param data_raw_dir Name of the raw data directory
#' @param gitignore Whether to add directory to .gitignore
#' @param base_path Base path for the project
#' @export
init_data_raw <- function(data_raw_dir = "data_raw", gitignore=TRUE, base_path = NULL) {
  # Use the utility function for consistent base_path handling
  base_path <- get_base_path(base_path)
  
  # Create full path for data_raw directory
  data_raw_path <- file.path(base_path, data_raw_dir)
  
  # Use utility function to ensure directory exists
  ensure_directory(data_raw_path, description = "Data raw directory")

  if (gitignore == TRUE) {
    gitignore_dir(data_raw_dir, base_path)
  }

  all_files <- list.files(path = base_path, full.names = TRUE)

  # Use utility function to filter supported files
  files_to_move <- filter_supported_files(all_files)
  files_moved_count <- 0

  if (length(files_to_move) > 0) {
    message(paste0("Found ", length(files_to_move), " data file(s) to move."))
    for (file_path in files_to_move) {
      file_name <- basename(file_path)
      new_file_path <- file.path(data_raw_path, file_name)

      if (file.rename(file_path, new_file_path)) {
        message(paste0("Moved '", file_name, "' to '", data_raw_path, "'"))
        files_moved_count <- files_moved_count + 1
      } else {
        warning(paste0("Failed to move '", file_name, "'. It might already exist in '", data_raw_path, "' or there was a permission issue."))
      }
    }
  }

  if (files_moved_count == 0) {
    supported_exts <- paste(get_supported_extensions(), collapse = ", ")
    message(paste("No", supported_exts, "files were moved."))
  } else {
    message(paste0("Operation complete. Total files moved: ", files_moved_count, "."))
  }

  invisible(NULL)
}

gitignore_dir <- function(dir_to_ignore, base_path = NULL) {
  # Use the utility function for consistent base_path handling
  base_path <- get_base_path(base_path)
  
  gitignore_path <- file.path(base_path, ".gitignore")
  gitignore_entry <- paste0(dir_to_ignore, "/")

  if (file.exists(gitignore_path)) {
    gitignore_content <- readLines(gitignore_path)
    if (!any(grepl(paste0("^", gitignore_entry, "$"), gitignore_content))) {
      write(gitignore_entry, file = gitignore_path, append = TRUE)
      message(paste0("Added '", gitignore_entry, "' to .gitignore."))
    } else {
      message(paste0("'", gitignore_entry, "' already exists in .gitignore."))
    }
  } else {
    write(gitignore_entry, file = gitignore_path, append = FALSE)
    message(paste0("Created .gitignore and added '", gitignore_entry, "'."))
  }
}