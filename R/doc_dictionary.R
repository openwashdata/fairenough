doc_dictionary <- function(base_path = NULL, overwrite = FALSE, chat = NULL, context = NULL, ...) {
  # Use the utility function for consistent base_path handling
  base_path <- get_base_path(base_path)
  
  # Check data_raw directory existence
  data_raw_path <- file.path(base_path, "data_raw")
  if(!dir.exists(data_raw_path)){
    message("Error: 'data_raw' directory not found. Please run init_data_raw() first.")
    return(invisible(NULL))
  }
  
  # Check dictionary csvfile existence
  dict_path <- file.path(data_raw_path, "dictionary.csv")
  
  if(file.exists(dict_path) && !overwrite){
    message(paste("The dictionary CSV file", dict_path, "already exists! Use overwrite = TRUE to replace it."))
    return(invisible(NULL))
  } else {
    data_dir <- file.path(base_path, "data")
    dictionary <- fill_dictionary(dict_path, data_dir, base_path, chat, context, ...)
    return(invisible(dictionary))
  }
}

fill_dictionary <- function(dict_path, data_dir, base_path, chat = NULL, context = NULL, ...){
  # Collect tidy data information
  if(dir.exists(data_dir)){
    tidydata_info <- collect_tidydata_info(data_dir)
  } else {
    # Error because tidy data is not yet available, should complete that first
    message("Error: There is no 'data' directory available. Please run init_export() to process data files first.")
    return(invisible(NULL))
  }
  
  # Check if any data was found
  if(nrow(tidydata_info) == 0) {
    message("No .rda files found in the data directory.")
    return(invisible(NULL))
  }
  
  # Fill in dictionary
  dictionary <- data.frame(directory = basename(data_dir),
                               file_name = tidydata_info$file_name,
                               variable_name = tidydata_info$var_name,
                               variable_type = tidydata_info$var_type,
                               description = NA)
  
  # If chat is provided, use gendict to generate descriptions
  if (!is.null(chat)) {
    message("Using LLM to generate variable descriptions...")
    
    # Look for CSV files in inst/extdata
    extdata_path <- file.path(base_path, "inst", "extdata")
    if (dir.exists(extdata_path)) {
      csv_files <- list.files(extdata_path, pattern = "\\.csv$", full.names = TRUE)
      
      if (length(csv_files) > 0) {
        # Process each CSV file
        all_descriptions <- list()
        
        for (csv_file in csv_files) {
          csv_basename <- tools::file_path_sans_ext(basename(csv_file))
          message(paste("Processing", csv_basename, "..."))
          
          # Generate descriptions using gendict
          gendict_result <- tryCatch({
            gendict(data = csv_file, chat = chat, context = context, ...)
          }, error = function(e) {
            message(paste("Error generating descriptions for", csv_basename, ":", e$message))
            NULL
          })
          
          if (!is.null(gendict_result)) {
            # Add file name to the result
            gendict_result$file_name <- csv_basename
            all_descriptions[[csv_basename]] <- gendict_result
          }
        }
        
        # Merge gendict descriptions with dictionary
        for (i in seq_len(nrow(dictionary))) {
          file_name <- dictionary$file_name[i]
          var_name <- dictionary$variable_name[i]
          
          if (file_name %in% names(all_descriptions)) {
            desc_df <- all_descriptions[[file_name]]
            matching_row <- which(desc_df$variable == var_name)
            
            if (length(matching_row) > 0) {
              dictionary$description[i] <- desc_df$description[matching_row[1]]
            }
          }
        }
        
        message("Variable descriptions generated successfully!")
      } else {
        message("No CSV files found in inst/extdata for description generation.")
      }
    } else {
      message("inst/extdata directory not found. Skipping description generation.")
    }
  }
  
  # Export dictionary
  utils::write.csv(x = dictionary, file = dict_path, na = "", row.names = FALSE)
  # Prompt to complete variable description if chat was not provided
  message(paste("Dictionary created at:", dict_path))
  if (is.null(chat)) {
    message("To complete the dictionary, please provide the variable descriptions.")
  }
  return(dictionary)
  }

no_dict <- function(dict_path) {
  if(file.exists(dict_path)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

collect_tidydata_info <- function(data_dir){
  # Only look for .rda files
  tidy_data_names <- list.files(data_dir, pattern = "\\.rda$", full.names = FALSE)
  
  if(length(tidy_data_names) == 0) {
    return(data.frame(file_name = character(), var_name = character(), var_type = character()))
  }
  
  file_name <- c()
  var_name <- c()
  var_type <- c()
  
  for (d in tidy_data_names){
    # Load the .rda file
    rda_path <- file.path(data_dir, d)
    
    # Create a temporary environment to load the data
    temp_env <- new.env()
    load(rda_path, envir = temp_env)
    
    # Get the name of the loaded object (should be one object per .rda file)
    obj_names <- ls(envir = temp_env)
    
    if(length(obj_names) == 1) {
      tidydata <- get(obj_names[1], envir = temp_env)
      
      # Store file name without .rda extension
      clean_filename <- tools::file_path_sans_ext(d)
      
      file_name <- c(file_name, rep(clean_filename, ncol(tidydata)))
      var_name <- c(var_name, colnames(tidydata))
      var_type <- c(var_type, sapply(tidydata, function(x) class(x)[1]))
    }
  }
  
  return(data.frame(file_name, var_name, var_type, stringsAsFactors = FALSE))
}
