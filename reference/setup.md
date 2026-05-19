# Setup wrapper

Wrapper function that initializes a fairenough data package project with
clear step messaging.

## Usage

``` r
setup(
  raw_dir = "data_raw",
  gitignore = TRUE,
  base_path = NULL,
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- raw_dir:

  Name of the raw data directory (default: "data_raw")

- gitignore:

  Whether to add data_raw to .gitignore (default: TRUE)

- base_path:

  Base path for the project

- verbose:

  Whether to show detailed messages (default: TRUE)

- overwrite:

  Whether to overwrite existing files (default: FALSE)

## Value

Invisibly returns a list with setup results

## Examples

``` r
if (FALSE) { # \dontrun{
setup(base_path = "path/to/my-data-package")
} # }
```
