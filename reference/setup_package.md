# Setup fairenough project structure

High-level function to initialize a data package project. Creates
necessary directories, organizes data files, and configures git.

## Usage

``` r
setup_package(
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

  Base path for the project (default: uses get_base_path())

- verbose:

  Whether to show detailed messages (default: TRUE)

- overwrite:

  Whether to overwrite existing files (default: FALSE)

## Value

Invisibly returns a list with base_path and number of files moved

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic setup
setup_package()

# Custom data directory
setup_package(raw_dir = "raw")

# Quiet mode
setup_package(verbose = FALSE)
} # }
```
