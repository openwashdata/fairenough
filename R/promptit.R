#' General-purpose prompting utilities for R
#'
#' @description
#' A collection of interactive prompting functions that follow tidyverse patterns
#' and use built-in R functions (utils::menu and readline) with cli for styling.
#' All functions handle non-interactive sessions gracefully.

#' Text input with validation
#'
#' @param message The prompt message to display
#' @param value Current value - if not NULL/empty, returns it without prompting
#' @param default Default value if user presses enter
#' @param required Whether the field is required
#' @param validator Function that returns TRUE for valid input
#' @param validator_message Message to show when validation fails
#' @return User input or default value
#' @export
prompt_input <- function(
  message,
  value = NULL,
  default = NULL,
  required = FALSE,
  validator = NULL,
  validator_message = "Invalid input"
) {
  # If value already exists and is not empty, return it
  if (!is.null(value) && value != "") {
    return(value)
  }

  # Non-interactive mode handling
  if (!interactive()) {
    if (!is.null(default)) {
      cli::cli_alert_info("Non-interactive mode: using default '{default}'")
      return(default)
    } else if (required) {
      cli::cli_abort(
        "Required field '{message}' missing in non-interactive mode"
      )
    } else {
      return("")
    }
  }

  # Format prompt message
  if (!is.null(default)) {
    prompt_text <- cli::col_cyan(paste0(message, " [", default, "]"))
  } else if (required) {
    prompt_text <- cli::col_cyan(paste0(message, " (required)"))
  } else {
    prompt_text <- cli::col_cyan(message)
  }

  repeat {
    cat(prompt_text, ": ", sep = "")
    response <- readline()

    # Handle empty response
    if (response == "") {
      if (!is.null(default)) {
        return(default)
      } else if (required) {
        cli::cli_alert_warning(
          "This field is required. Please provide a value."
        )
        next
      } else {
        return("")
      }
    }

    # Validate if validator provided
    if (!is.null(validator)) {
      if (validator(response)) {
        return(response)
      } else {
        cli::cli_alert_warning(validator_message)
        next
      }
    }

    return(response)
  }
}

#' Menu selection using utils::menu()
#'
#' @param choices Character vector or named vector (names for display, values for return)
#' @param title Title to display above menu
#' @param value Current value - if not NULL, returns it without prompting
#' @param default Default choice (index or value)
#' @param allow_none Add "None of the above" option
#' @param allow_other Add "Other (specify)" option
#' @param graphics Whether to use graphical menu if available
#' @return Selected value or NULL
#' @export
prompt_menu <- function(
  choices,
  title = "Select an option",
  value = NULL,
  default = NULL,
  allow_none = FALSE,
  allow_other = FALSE,
  graphics = getOption("menu.graphics", FALSE)
) {
  # If value already exists, return it
  if (!is.null(value)) {
    return(value)
  }

  # Handle named vectors (use names for display, values for return)
  display_choices <- if (is.null(names(choices))) choices else names(choices)
  return_values <- if (is.null(names(choices))) choices else unname(choices)

  # Non-interactive mode handling
  if (!interactive()) {
    if (!is.null(default)) {
      if (is.numeric(default)) {
        cli::cli_alert_info(
          "Non-interactive mode: using default '{display_choices[default]}'"
        )
        return(return_values[default])
      } else {
        idx <- match(default, return_values)
        if (!is.na(idx)) {
          cli::cli_alert_info(
            "Non-interactive mode: using default '{display_choices[idx]}'"
          )
          return(return_values[idx])
        }
      }
    }
    cli::cli_alert_info(
      "Non-interactive mode: using first choice '{display_choices[1]}'"
    )
    return(return_values[1])
  }

  # Display title
  cli::cli_h3(title)

  # Build menu options
  menu_choices <- display_choices

  # Add special options
  if (allow_other) {
    menu_choices <- c(menu_choices, "Other (specify)")
  }
  if (allow_none) {
    menu_choices <- c(menu_choices, "None of the above")
  }

  # Use utils::menu()
  choice_index <- utils::menu(menu_choices, graphics = graphics)

  # Handle cancellation
  if (choice_index == 0) {
    return(NULL)
  }

  # Handle special options
  n_original <- length(display_choices)

  if (allow_other && choice_index == n_original + 1) {
    # "Other" was selected
    return(prompt_input("Enter custom value", required = TRUE))
  }

  if (allow_none && choice_index == length(menu_choices)) {
    # "None" was selected
    return(NULL)
  }

  # Return the corresponding value
  return(return_values[choice_index])
}

#' Multiple selection menu
#'
#' @param choices Character vector or named vector
#' @param title Title to display
#' @param values Current values - if not NULL/empty, returns them without prompting
#' @param default Default selections (vector of values)
#' @param min_choices Minimum number of choices required
#' @param max_choices Maximum number of choices allowed
#' @return Character vector of selected values
#' @export
prompt_multi_select <- function(
  choices,
  title = "Select multiple options",
  values = NULL,
  default = NULL,
  min_choices = 0,
  max_choices = NULL
) {
  # If values already exist and are not empty, return them
  if (!is.null(values) && length(values) > 0) {
    return(values)
  }

  # Handle named vectors
  display_choices <- if (is.null(names(choices))) choices else names(choices)
  return_values <- if (is.null(names(choices))) choices else unname(choices)

  # Non-interactive mode
  if (!interactive()) {
    if (!is.null(default)) {
      cli::cli_alert_info("Non-interactive mode: using defaults")
      return(default)
    } else if (min_choices > 0) {
      cli::cli_abort(
        "Multi-select requires at least {min_choices} choices in non-interactive mode"
      )
    } else {
      return(character(0))
    }
  }

  selected <- character(0)

  cli::cli_h3(title)
  if (min_choices > 0) {
    cli::cli_alert_info("Select at least {min_choices} option{?s}")
  }
  if (!is.null(max_choices)) {
    cli::cli_alert_info("Select up to {max_choices} option{?s}")
  }

  repeat {
    # Show current selections
    if (length(selected) > 0) {
      selected_display <- display_choices[match(selected, return_values)]
      cli::cli_alert_success(
        "Selected: {paste(selected_display, collapse = ', ')}"
      )
    }

    # Check if we've reached max selections
    if (!is.null(max_choices) && length(selected) >= max_choices) {
      cli::cli_alert_info("Maximum selections reached")
      break
    }

    # Filter out already selected options
    available_mask <- !(return_values %in% selected)
    if (!any(available_mask)) {
      cli::cli_alert_info("All options selected")
      break
    }

    available_choices <- display_choices[available_mask]
    available_values <- return_values[available_mask]

    # Add control options
    menu_options <- c(available_choices, "Done selecting")
    if (length(selected) > 0) {
      menu_options <- c(menu_options, "Clear selections")
    }

    # Show menu
    choice_idx <- utils::menu(menu_options, graphics = FALSE)

    # Handle choice
    if (choice_idx == 0) {
      # User cancelled
      if (length(selected) >= min_choices) {
        break
      } else {
        cli::cli_alert_warning(
          "Please select at least {min_choices} option{?s}"
        )
        next
      }
    } else if (choice_idx == length(available_choices) + 1) {
      # "Done selecting"
      if (length(selected) >= min_choices) {
        break
      } else {
        cli::cli_alert_warning(
          "Please select at least {min_choices} option{?s}"
        )
        next
      }
    } else if (choice_idx == length(menu_options) && length(selected) > 0) {
      # "Clear selections"
      selected <- character(0)
      cli::cli_alert_info("Selections cleared")
    } else {
      # Add selection
      selected <- c(selected, available_values[choice_idx])
    }
  }

  return(selected)
}

#' Yes/No confirmation
#'
#' @param message Question to ask
#' @param value Current value - if not NULL, returns it without prompting
#' @param default Default choice (TRUE for yes, FALSE for no)
#' @return Logical TRUE or FALSE
#' @export
prompt_confirm <- function(message, value = NULL, default = TRUE) {
  # If value already exists, return it
  if (!is.null(value)) {
    return(value)
  }

  # Non-interactive mode
  if (!interactive()) {
    cli::cli_alert_info(
      "Non-interactive mode: using default '{if(default) 'Yes' else 'No'}'"
    )
    return(default)
  }

  choices <- if (default) {
    c("Yes" = TRUE, "No" = FALSE)
  } else {
    c("No" = FALSE, "Yes" = TRUE)
  }

  result <- prompt_menu(
    choices = choices,
    title = message,
    graphics = FALSE,
    default = default
  )

  # If user cancelled, use default
  if (is.null(result)) {
    return(default)
  }

  return(as.logical(result))
}

#' Common validators
#'
#' @name validators
#' @return Function that returns TRUE for valid input, FALSE otherwise
NULL

#' @describeIn validators Email validator
#' @export
validate_email <- function(x) {
  grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", x)
}

#' @describeIn validators Package name validator
#' @export
validate_package_name <- function(x) {
  grepl("^[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]$", x)
}

#' @describeIn validators ORCID validator
#' @export
validate_orcid <- function(x) {
  grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", x)
}

#' @describeIn validators Date validator (YYYY or YYYY-MM-DD)
#' @export
validate_date <- function(x) {
  grepl("^\\d{4}(-\\d{2}-\\d{2})?$", x)
}

#' @describeIn validators URL validator
#' @export
validate_url <- function(x) {
  grepl("^https?://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}", x)
}
