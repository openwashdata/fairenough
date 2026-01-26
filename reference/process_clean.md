# Clean data with standard transformations

Applies standard cleaning operations to a data frame:

- Clean column names (janitor::clean_names)

- Convert string nulls to NA

- Optimize numeric types

## Usage

``` r
process_clean(
  data,
  clean_names = TRUE,
  string_nulls = c("null", "NA", ""),
  optimize_types = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame to clean

- clean_names:

  Whether to clean column names (default: TRUE)

- string_nulls:

  Character values to convert to NA (default: c("null", "NA", ""))

- optimize_types:

  Whether to optimize numeric types (default: TRUE)

- verbose:

  Whether to show messages (default: TRUE)

## Value

Cleaned data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic cleaning
process_clean(my_data)

# Keep original column names
process_clean(my_data, clean_names = FALSE)

# Custom null values
process_clean(my_data, string_nulls = c("N/A", "missing", ""))
} # }
```
