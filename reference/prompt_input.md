# General-purpose prompting utilities for R

A collection of interactive prompting functions that follow tidyverse
patterns and use built-in R functions (utils::menu and readline) with
cli for styling. All functions handle non-interactive sessions
gracefully. Text input with validation

## Usage

``` r
prompt_input(
  message,
  value = NULL,
  default = NULL,
  required = FALSE,
  validator = NULL,
  validator_message = "Invalid input"
)
```

## Arguments

- message:

  The prompt message to display

- value:

  Current value - if not NULL/empty, returns it without prompting

- default:

  Default value if user presses enter

- required:

  Whether the field is required

- validator:

  Function that returns TRUE for valid input

- validator_message:

  Message to show when validation fails

## Value

User input or default value
