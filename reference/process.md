# Process data wrapper

Wrapper function that processes all data files in the raw data directory
with clear step messaging.

## Usage

``` r
process(
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

  Whether to automatically clean data (default: TRUE)

- overwrite:

  Whether to overwrite existing files (default: TRUE)

- base_path:

  Base path for the project

- verbose:

  Whether to show detailed messages (default: TRUE)

## Value

Invisibly returns a list of processed files
