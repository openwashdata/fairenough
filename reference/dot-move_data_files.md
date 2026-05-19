# Move data files to raw data directory

Moves CSV and Excel files from the project root to the data_raw
directory. By default the user is asked to confirm the move and the
function aborts if any destination file already exists. Pass
`overwrite = TRUE` to skip the prompt and replace existing destination
files.

## Usage

``` r
.move_data_files(
  raw_dir = "data_raw",
  base_path = NULL,
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- raw_dir:

  Name of the raw data directory

- base_path:

  Base path for the project

- verbose:

  Whether to show messages

- overwrite:

  Skip the confirmation prompt and overwrite existing destination files
  (default: FALSE)

## Value

Character vector of moved files
