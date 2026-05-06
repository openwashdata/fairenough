# Generate data dictionary using LLM

Generate data dictionary using LLM

## Usage

``` r
gendict(
  data,
  chat,
  context = NULL,
  sample_size = 5,
  method = "sequential",
  test_llm_connection = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  Data frame or path to CSV/Excel file

- chat:

  Chat object from ellmer package

- context:

  Optional context for generation

- sample_size:

  Number of sample values to show

- method:

  Processing method (sequential, sequential_fresh, parallel)

- test_llm_connection:

  Whether to test LLM connection first

- verbose:

  Whether to show detailed messages (default: FALSE)

- ...:

  Additional arguments. Currently unused; reserved for future
  extensions.

## Value

Tibble with variable names and descriptions
