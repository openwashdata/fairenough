# Export data as RDA file

Saves a data frame as an .rda file in the data directory.

## Usage

``` r
.export_rda(data, name, base_path = NULL, overwrite = TRUE, verbose = TRUE)
```

## Arguments

- data:

  Data frame to export

- name:

  Name for the data object (without extension)

- base_path:

  Base path for the project

- overwrite:

  Whether to overwrite existing file (default: TRUE)

- verbose:

  Whether to show messages (default: TRUE)

## Value

Path to the created file
