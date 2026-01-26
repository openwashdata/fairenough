# Save metadata to DESCRIPTION file

Saves metadata structure to DESCRIPTION file using appropriate fields.
Handles both creating new metadata and updating existing metadata.

## Usage

``` r
save_metadata(
  metadata,
  base_path = NULL,
  overwrite = TRUE,
  create = TRUE,
  verbose = TRUE
)
```

## Arguments

- metadata:

  List containing metadata

- base_path:

  Base path for the project

- overwrite:

  Whether to overwrite existing metadata fields

- create:

  Whether to create DESCRIPTION file if it doesn't exist

- verbose:

  Whether to show messages

## Value

NULL (invisibly)
