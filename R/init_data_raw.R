init_data_raw <- function(gitignore=TRUE) {
  data_raw_dir <- "data_raw"

  if (dir.create(data_raw_dir, showWarnings = FALSE)) {
    message(paste0("Directory '", data_raw_dir, "' created successfully."))
  } else {
    message(paste0("Directory '", data_raw_dir, "' already exists."))
  }

  if (gitignore = TRUE) {
    gitignore_dir(data_raw_dir)
  }

  all_files <- list.files(path = ".", full.names = TRUE)

  target_extensions <- c(".csv", ".xlsx")

  files_moved_count <- 0

  for (ext in target_extensions) {
    files_to_move <- all_files[grepl(paste0("\\", ext, "$"), all_files, ignore.case = TRUE)]

    if (length(files_to_move) > 0) {
      message(paste0("Found ", length(files_to_move), " '", ext, "' files to move."))
      for (file_path in files_to_move) {
        file_name <- basename(file_path)

        new_file_path <- file.path(data_raw_dir, file_name)

        if (file.rename(file_path, new_file_path)) {
          message(paste0("Moved '", file_name, "' to '", data_raw_dir, "'"))
          files_moved_count <- files_moved_count + 1
        } else {
          warning(paste0("Failed to move '", file_name, "'. It might already exist in '", data_raw_dir, "' or there was a permission issue."))
        }
      }
    } else {
      message(paste0("No '", ext, "' files found in the root directory to move."))
    }
  }

  if (files_moved_count == 0) {
    message("No .csv or .xlsx files were moved.")
  } else {
    message(paste0("Operation complete. Total files moved: ", files_moved_count, "."))
  }

  invisible(NULL)
}

gitignore_dir <- function(_dir) {
  gitignore_path <- ".gitignore"
  gitignore_entry <- paste0(_dir, "/")

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