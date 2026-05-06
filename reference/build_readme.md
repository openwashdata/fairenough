# Build README from inst/templates/README.qmd

Build README.md from the README.qmd template, incorporating package
metadata.

## Usage

``` r
build_readme(
  base_path = NULL,
  verbose = TRUE,
  overwrite = TRUE,
  quarto = FALSE
)
```

## Arguments

- base_path:

  Base path for the project

- verbose:

  Whether to show messages

- overwrite:

  Whether to overwrite README.qmd found at base_path

- quarto:

  Whether to use Quarto to build readme (default: FALSE)

## Value

Path to generated README
