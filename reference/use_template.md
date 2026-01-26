# Use a template file with data substitution

Similar to usethis::use_template but respects base_path. Reads a
template from the package, substitutes data, and writes to target
location.

## Usage

``` r
use_template(
  template,
  save_as = template,
  data = list(),
  base_path = NULL,
  package = "fairenough",
  open = FALSE,
  verbose = TRUE
)
```

## Arguments

- template:

  Name of template file in inst/templates

- save_as:

  Path to save file relative to base_path

- data:

  List of data for substitution

- base_path:

  Base path for the project

- package:

  Package containing the template

- open:

  Whether to open the file after creation

- verbose:

  Whether to show messages

## Value

Path to created file
