# Validate package with modern tools

Runs comprehensive package validation including R CMD check, spell
checking, and best practices analysis.

## Usage

``` r
.validate_package(
  base_path = NULL,
  verbose = TRUE,
  spell_check = TRUE,
  good_practice = FALSE
)
```

## Arguments

- base_path:

  Base path for the project

- verbose:

  Whether to show detailed messages

- spell_check:

  Whether to run spell check (default: TRUE)

- good_practice:

  Whether to run good practice checks (default: FALSE, as it's slow)

## Value

List with validation results
