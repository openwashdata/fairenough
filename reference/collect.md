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

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive: walks you through prompts for title, authors, license, etc.
collect(base_path = "path/to/my-data-package")

# Non-interactive: supply every required field as a named argument.
collect(
  base_path = "path/to/my-data-package",
  interactive = FALSE,
  pkg_name = "mypkg",
  title = "My Package",
  description = "What this package contains."
)
} # }
```
