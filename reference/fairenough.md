# Main fairenough wrapper

Complete automated R data package creation pipeline. Runs all steps from
setup to final build in sequence. For more granular control, use
individual wrapper functions: setup(), process(), collect(), generate(),
and build().

## Usage

``` r
fairenough(
  chat = NULL,
  verbose = TRUE,
  overwrite = FALSE,
  base_path = NULL,
  ...
)
```

## Arguments

- chat:

  Chat object for LLM-based generation (optional)

- verbose:

  Whether to show detailed messages (default: TRUE)

- overwrite:

  Whether to overwrite existing files (default: FALSE)

- base_path:

  Base path for the project

- ...:

  Additional arguments passed to collect() and generate() function

## Value

List containing results from all pipeline steps

## Examples

``` r
if (FALSE) { # \dontrun{
# Full pipeline in one call, prompting for metadata interactively.
fairenough(base_path = "path/to/my-data-package")

# With an LLM-backed chat for variable descriptions.
chat <- ellmer::chat_openai(model = "gpt-4o-mini")
fairenough(base_path = "path/to/my-data-package", chat = chat)
} # }
```
