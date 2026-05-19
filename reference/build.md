# Build package components wrapper

Wrapper function that runs all build functions in sequence. Generates
roxygen docs, citation files, builds the package, creates README, and
builds the website.

## Usage

``` r
build(
  base_path = NULL,
  verbose = TRUE,
  overwrite = TRUE,
  validate = TRUE,
  preview = TRUE,
  quarto = FALSE
)
```

## Arguments

- base_path:

  Base path for the project

- verbose:

  Whether to show detailed messages (default: TRUE)

- overwrite:

  Whether to overwrite existing files (default: TRUE)

- validate:

  Whether to validate the CITATION file (default: TRUE)

- preview:

  Whether to preview the site after building (default: TRUE)

- quarto:

  Whether to use Quarto to build readme (default: FALSE)

## Value

List with results from each build step

## Examples

``` r
if (FALSE) { # \dontrun{
build(base_path = "path/to/my-data-package")
} # }
```
