# Get raw directory path with consistent handling across all functions

This function provides a consistent way to handle raw_dir across all
fairenough functions. If raw_dir is provided, it sets the global option
and returns the normalized path. If raw_dir is NULL, it checks for the
global option, falling back to "data_raw"

## Usage

``` r
get_raw_dir(raw_dir = NULL)
```

## Arguments

- raw_dir:

  Optional base path to set

## Value

Normalized base path
