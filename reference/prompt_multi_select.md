# Multiple selection menu

Multiple selection menu

## Usage

``` r
prompt_multi_select(
  choices,
  title = "Select multiple options",
  values = NULL,
  default = NULL,
  min_choices = 0,
  max_choices = NULL,
  interactive = base::interactive()
)
```

## Arguments

- choices:

  Character vector or named vector

- title:

  Title to display

- values:

  Current values - if not NULL/empty, returns them without prompting

- default:

  Default selections (vector of values)

- min_choices:

  Minimum number of choices required

- max_choices:

  Maximum number of choices allowed

- interactive:

  Whether to prompt the user. Defaults to the current R session's
  [`interactive()`](https://rdrr.io/r/base/interactive.html) state; pass
  `FALSE` to force the non-interactive branch.

## Value

Character vector of selected values
