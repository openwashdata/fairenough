# Match License to License Config

This is the core pattern matching function used by build_license(). Uses
the centralized configuration for pattern matching.

## Usage

``` r
match_license(license)
```

## Arguments

- license:

  Character string to match

## Value

Character string matching a canonical license, or NULL if no match

## Examples

``` r
match_license("MIT")           # "MIT"
#> [1] "MIT"
match_license("gpl v3")        # "GPL-3"
#> [1] "GPL-3"
match_license("apache 2.0")    # "Apache-2.0"
#> [1] "Apache-2.0"
```
