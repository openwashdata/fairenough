# Generate data dictionary

Creates a data dictionary for all data files in the package. Can use LLM
for automatic description generation. Works best when package metadata
exists in DESCRIPTION as it uses the package description for context.

## Usage

``` r
generate_dictionary(
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

  Whether to overwrite existing dictionary

- base_path:

  Base path for the project

- verbose:

  Whether to show messages

- ...:

  Additional arguments passed to gendict

## Value

Data frame containing the dictionary

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate empty dictionary structure (no LLM)
generate_dictionary()

# Generate dictionary with LLM
library(ellmer)
chat <- chat_openai(model = "gpt-4")
generate_dictionary(chat = chat, overwrite = TRUE)
} # }
```
