# Collect comprehensive metadata interactively

Collects metadata for R data packages using interactive prompts.
Supports partial pre-filling where provided values are used and missing
required fields are prompted. Uses the general prompt.R utilities. By
default, collects only essential metadata. Set extended=TRUE to collect
additional optional metadata for publication and archiving.

## Usage

``` r
collect_metadata(
  pkg_name = NULL,
  title = NULL,
  description = NULL,
  version = "0.0.1",
  language = "en-GB",
  github_user = NULL,
  authors = NULL,
  license = NULL,
  keywords = NULL,
  funder = NULL,
  grant_id = NULL,
  temporal_start = NULL,
  temporal_end = NULL,
  spatial_description = NULL,
  spatial_coordinates = NULL,
  related_identifiers = NULL,
  communities = NULL,
  extended = FALSE,
  interactive = TRUE,
  save_to_desc = TRUE,
  base_path = NULL,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- pkg_name:

  Package name

- title:

  Package title

- description:

  Package description (one paragraph)

- version:

  Package version (default: "0.0.1")

- language:

  Language code (default: "en-GB")

- github_user:

  GitHub username or organization

- authors:

  List of author information

- license:

  License identifier (e.g., "CC-BY-4.0")

- keywords:

  Character vector of keywords (only prompted if extended=TRUE)

- funder:

  Funding organization (only prompted if extended=TRUE)

- grant_id:

  Grant identifier (only prompted if extended=TRUE)

- temporal_start:

  Start date of data coverage (only prompted if extended=TRUE)

- temporal_end:

  End date of data coverage (only prompted if extended=TRUE)

- spatial_description:

  Geographic coverage description (only prompted if extended=TRUE)

- spatial_coordinates:

  List with lat and lon (only prompted if extended=TRUE)

- related_identifiers:

  List of related publications (only prompted if extended=TRUE)

- communities:

  Character vector of Zenodo communities (only prompted if
  extended=TRUE)

- extended:

  Whether to prompt for extended metadata fields (default: FALSE)

- interactive:

  Whether to use interactive prompts (default: TRUE)

- save_to_desc:

  Whether to save to DESCRIPTION file (default: TRUE)

- base_path:

  Base path for the project

- overwrite:

  Whether to overwrite existing metadata (default: FALSE)

- verbose:

  Whether to show detailed messages (default: TRUE)

## Value

List containing all metadata organized by category
