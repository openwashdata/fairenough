# Move data files to raw data directory

Moves CSV and Excel files from the project root to the data_raw
directory.

## Usage

``` r
.move_data_files(raw_dir = "data_raw", base_path = NULL, verbose = TRUE)
```

## Arguments

- raw_dir:

  Name of the raw data directory

- base_path:

  Base path for the project

- verbose:

  Whether to show messages

## Value

Character vector of moved files
