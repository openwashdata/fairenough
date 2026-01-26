# Create project directories

Creates the necessary directory structure for a fairenough project.

## Usage

``` r
.create_directories(
  dirs = c("data_raw", "data", "inst/extdata"),
  base_path = NULL,
  verbose = TRUE
)
```

## Arguments

- dirs:

  Character vector of directories to create

- base_path:

  Base path for the project

- verbose:

  Whether to show messages

## Value

Character vector of created directories
