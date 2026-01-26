# Build package structure and core files

Creates the essential R package structure using modern development
tools. Generates roxygen docs for dataset functions, LICENSE, and
validates structure. Note: This assumes usethis::create_package() has
already been run in setup().

## Usage

``` r
build_package(base_path = NULL, verbose = TRUE, overwrite = TRUE)
```

## Arguments

- base_path:

  Base path for the project

- verbose:

  Whether to show detailed messages

- overwrite:

  Whether to overwrite existing files

## Value

Logical indicating success
