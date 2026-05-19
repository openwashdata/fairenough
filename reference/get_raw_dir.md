# Get raw directory path with consistent handling across all functions

Provides a consistent way to handle `raw_dir` across all fairenough
functions. If `raw_dir` is provided, it is cached in a package-private
environment and returned. If `raw_dir` is `NULL`, the cached value is
returned when set, otherwise `"data_raw"` is used as the default.

## Usage

``` r
get_raw_dir(raw_dir = NULL)
```

## Arguments

- raw_dir:

  Optional base path to set

## Value

Normalized base path
