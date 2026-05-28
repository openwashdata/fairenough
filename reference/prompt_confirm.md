# Yes/No confirmation

Yes/No confirmation

## Usage

``` r
prompt_confirm(
  message,
  value = NULL,
  default = TRUE,
  interactive = base::interactive()
)
```

## Arguments

- message:

  Question to ask

- value:

  Current value - if not NULL, returns it without prompting

- default:

  Default choice (TRUE for yes, FALSE for no)

- interactive:

  Whether to prompt the user. Defaults to the current R session's
  [`interactive()`](https://rdrr.io/r/base/interactive.html) state; pass
  `FALSE` to force the non-interactive branch (returns `default`).

## Value

Logical TRUE or FALSE
