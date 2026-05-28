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
  validator_message = "Invalid input",
  interactive = base::interactive()
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

- interactive:

  Whether to prompt the user. Defaults to the current R session's
  [`interactive()`](https://rdrr.io/r/base/interactive.html) state; pass
  `FALSE` to force the non-interactive branch (use defaults / abort on
  required fields).

## Value

User input or default value
