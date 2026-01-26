# Export data as CSV file

Saves a data frame as a CSV file in the inst/extdata directory.

## Usage

``` r
.export_csv(data, name, base_path = NULL, overwrite = TRUE, verbose = TRUE)
```

## Arguments

- data:

  Data frame to export

- name:

  Name for the file (without extension)

- base_path:

  Base path for the project

- overwrite:

  Whether to overwrite existing file (default: TRUE)

- verbose:

  Whether to show messages (default: TRUE)

## Value

Path to the created file
