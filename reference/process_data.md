# Process all data files

High-level function to read, clean, and export all data files. Processes
all files in the raw data directory and exports them as .rda and CSV.

## Usage

``` r
process_data(
  raw_dir = NULL,
  auto_clean = TRUE,
  overwrite = TRUE,
  base_path = NULL,
  verbose = TRUE
)
```

## Arguments

- raw_dir:

  Directory containing raw data files (default: "data_raw")

- auto_clean:

  Whether to run clean_data (default: TRUE)

- overwrite:

  Whether to overwrite existing files (default: TRUE)

- base_path:

  Base path for the project (default: uses get_base_path())

- verbose:

  Whether to show detailed messages (default: TRUE)

## Value

Invisibly returns a list of processed files

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic processing
process_data()

# Skip auto-cleaning
process_data(auto_clean = FALSE)

# Preserve existing files
process_data(overwrite = FALSE)
} # }
```
