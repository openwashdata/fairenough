# Get base path with consistent handling across all functions

This function provides a consistent way to handle base_path across all
fairenough functions. If base_path is provided, it sets the global
option and returns the normalized path. If base_path is NULL, it checks
for the global option, falling back to here::here() or "."

## Usage

``` r
get_base_path(base_path = NULL)
```

## Arguments

- base_path:

  Optional base path to set

## Value

Normalized base path
