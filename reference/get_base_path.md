# Get base path with consistent handling across all functions

Provides a consistent way to handle `base_path` across all fairenough
functions. If `base_path` is provided, it is normalized, cached in a
package-private environment, and returned. If `base_path` is `NULL`, the
cached value is returned when set, otherwise the function falls back to
[`here::here()`](https://here.r-lib.org/reference/here.html) or the
current directory.

## Usage

``` r
get_base_path(base_path = NULL)
```

## Arguments

- base_path:

  Optional base path to set

## Value

Normalized base path
