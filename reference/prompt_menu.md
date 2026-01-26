# Menu selection using utils::menu()

Menu selection using utils::menu()

## Usage

``` r
prompt_menu(
  choices,
  title = "Select an option",
  value = NULL,
  default = NULL,
  allow_none = FALSE,
  allow_other = FALSE,
  graphics = getOption("menu.graphics", FALSE)
)
```

## Arguments

- choices:

  Character vector or named vector (names for display, values for
  return)

- title:

  Title to display above menu

- value:

  Current value - if not NULL, returns it without prompting

- default:

  Default choice (index or value)

- allow_none:

  Add "None of the above" option

- allow_other:

  Add "Other (specify)" option

- graphics:

  Whether to use graphical menu if available

## Value

Selected value or NULL
