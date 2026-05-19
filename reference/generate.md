# Generation of documentation for datasets

Wrapper function that generates a data dictionary for all data files in
the package with clear step messaging.

## Usage

``` r
generate(
  chat = NULL,
  context = NULL,
  overwrite = FALSE,
  base_path = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- chat:

  Optional chat object for LLM-based generation

- context:

  Optional context for LLM generation

- overwrite:

  Whether to overwrite existing dictionary (default: FALSE)

- base_path:

  Base path for the project

- verbose:

  Whether to show detailed messages (default: TRUE)

- ...:

  Additional arguments passed to gendict

## Value

Data frame containing the dictionary

## Examples

``` r
if (FALSE) { # \dontrun{
# Without an LLM: produces a dictionary skeleton with empty descriptions.
generate(base_path = "path/to/my-data-package")

# With an LLM: fills in descriptions via ellmer.
chat <- ellmer::chat_openai(model = "gpt-4o-mini")
generate(base_path = "path/to/my-data-package", chat = chat)
} # }
```
