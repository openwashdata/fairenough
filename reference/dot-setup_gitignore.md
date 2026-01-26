# Setup gitignore for data directory and R files

Adds the data_raw directory and standard R ignores to .gitignore. Uses
usethis::use_git_ignore() for consistent handling.

## Usage

``` r
.setup_gitignore(raw_dir = NULL, base_path = NULL, verbose = TRUE)
```

## Arguments

- raw_dir:

  Name of the directory to ignore (default handled by get_raw_dir())

- base_path:

  Base path for the project

- verbose:

  Whether to show messages

## Value

Logical indicating success
