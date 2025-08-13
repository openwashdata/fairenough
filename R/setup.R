#' Setup fairenough project structure
#' 
#' High-level function to initialize a data package project.
#' Creates necessary directories, organizes data files, and configures git.
#' 
#' @param data_dir Name of the raw data directory (default: "data_raw")
#' @param gitignore Whether to add data_raw to .gitignore (default: TRUE)
#' @param base_path Base path for the project (default: uses get_base_path())
#' @param verbose Whether to show detailed messages (default: TRUE)
#' @return Invisibly returns a list with base_path and number of files moved
#' @export
#' @examples
#' \dontrun{
#' # Basic setup
#' setup()
#' 
#' # Custom data directory
#' setup(data_dir = "raw_data")
#' 
#' # Quiet mode
#' setup(verbose = FALSE)
#' }
setup <- function(data_dir = "data_raw", 
                  gitignore = TRUE, 
                  base_path = NULL,
                  verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  
  if (verbose) cli::cli_h1("Setting up fairenough project")
  
  # Create all necessary directories
  dirs_created <- create_directories(
    dirs = c(data_dir, "data", "inst/extdata", "inst/templates"),
    base_path = base_path,
    verbose = verbose
  )
  
  # Move data files to data_raw
  files_moved <- move_data_files(
    data_dir = data_dir,
    base_path = base_path,
    verbose = verbose
  )
  
  # Setup gitignore if requested
  if (gitignore) {
    setup_gitignore(
      data_dir = data_dir,
      base_path = base_path,
      verbose = verbose
    )
  }
  
  if (verbose) {
    cli::cli_alert_success("Project setup complete!")
    cli::cli_alert_info("Next step: Run {.fn process} to clean and export your data")
  }
  
  invisible(list(
    base_path = base_path,
    files_moved = files_moved,
    dirs_created = dirs_created
  ))
}

#' Create project directories
#' 
#' Creates the necessary directory structure for a fairenough project.
#' 
#' @param dirs Character vector of directories to create
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Character vector of created directories
#' @export
create_directories <- function(dirs = c("data_raw", "data", "inst/extdata", "inst/templates"),
                              base_path = NULL,
                              verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  created <- character()
  
  for (dir in dirs) {
    dir_path <- file.path(base_path, dir)
    if (!fs::dir_exists(dir_path)) {
      fs::dir_create(dir_path, recurse = TRUE)
      created <- c(created, dir)
      if (verbose) cli::cli_alert_success("Created directory: {.path {dir}}")
    } else {
      if (verbose) cli::cli_alert_info("Directory already exists: {.path {dir}}")
    }
  }
  
  return(created)
}

#' Move data files to raw data directory
#' 
#' Moves CSV and Excel files from the project root to the data_raw directory.
#' 
#' @param data_dir Name of the raw data directory
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Character vector of moved files
#' @export
move_data_files <- function(data_dir = "data_raw",
                           base_path = NULL,
                           verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  data_path <- file.path(base_path, data_dir)
  
  # Find all data files in root
  all_files <- list.files(base_path, full.names = TRUE)
  data_files <- filter_supported_files(all_files)
  
  # Filter out files already in data_dir
  data_files <- data_files[!grepl(paste0("/", data_dir, "/"), data_files)]
  
  moved_files <- character()
  
  if (length(data_files) > 0) {
    if (verbose) cli::cli_alert_info("Found {length(data_files)} data file{?s} to move")
    
    for (file in data_files) {
      file_name <- basename(file)
      new_path <- file.path(data_path, file_name)
      
      if (file.rename(file, new_path)) {
        moved_files <- c(moved_files, file_name)
        if (verbose) cli::cli_alert_success("Moved {.file {file_name}} to {.path {data_dir}/}")
      } else {
        if (verbose) cli::cli_alert_warning("Could not move {.file {file_name}}")
      }
    }
  } else {
    if (verbose) cli::cli_alert_info("No data files found in project root")
  }
  
  return(moved_files)
}

#' Setup gitignore for data directory
#' 
#' Adds the data_raw directory to .gitignore to prevent committing raw data.
#' 
#' @param data_dir Name of the directory to ignore
#' @param base_path Base path for the project
#' @param verbose Whether to show messages
#' @return Logical indicating success
#' @export
setup_gitignore <- function(data_dir = "data_raw",
                           base_path = NULL,
                           verbose = TRUE) {
  
  base_path <- get_base_path(base_path)
  gitignore_path <- file.path(base_path, ".gitignore")
  gitignore_entry <- paste0(data_dir, "/")
  
  # Read existing .gitignore or create new
  if (file.exists(gitignore_path)) {
    gitignore_lines <- readLines(gitignore_path)
    entry_exists <- any(grepl(paste0("^", gitignore_entry, "$"), gitignore_lines))
  } else {
    gitignore_lines <- character()
    entry_exists <- FALSE
  }
  
  if (!entry_exists) {
    write(gitignore_entry, file = gitignore_path, append = TRUE)
    if (verbose) cli::cli_alert_success("Added {.path {gitignore_entry}} to .gitignore")
    return(TRUE)
  } else {
    if (verbose) cli::cli_alert_info("{.path {gitignore_entry}} already in .gitignore")
    return(FALSE)
  }
}