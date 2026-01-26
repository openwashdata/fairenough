# Collect metadata wrapper

Wrapper function that collects comprehensive metadata for R data
packages with clear step messaging.

## Usage

``` r
collect(
  extended = FALSE,
  interactive = TRUE,
  save_to_desc = TRUE,
  base_path = NULL,
  verbose = TRUE,
  overwrite = FALSE,
  ...
)
```

## Arguments

- extended:

  Whether to prompt for extended metadata fields (default: FALSE)

- interactive:

  Whether to use interactive prompts (default: TRUE)

- save_to_desc:

  Whether to save to DESCRIPTION file (default: TRUE)

- base_path:

  Base path for the project

- verbose:

  Whether to show detailed messages (default: TRUE)

- overwrite:

  Whether to overwrite existing metadata (default: FALSE)

- ...:

  Additional arguments passed to collect_metadata

## Value

List containing all metadata organized by category
