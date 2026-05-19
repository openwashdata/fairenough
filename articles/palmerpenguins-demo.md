# fairenough demo: turning palmerpenguins into a data package

> This vignette requires the **palmerpenguins** package. Install it with
> `install.packages("palmerpenguins")` and rebuild the vignette to see
> the demo.

`fairenough` turns a tidy data file into a fully-documented R data
package. This vignette walks through the full pipeline on the well-known
Palmer penguins dataset, ending with the resulting package layout.

We do every step in a throw-away project so nothing leaks into your real
working directory.

``` r

library(fairenough)

# Disposable working directory; cleaned up automatically.
project <- withr::local_tempdir()

# Drop a CSV of penguins into the project root — exactly what a user
# would have when they start.
write.csv(
  palmerpenguins::penguins,
  file.path(project, "penguins.csv"),
  row.names = FALSE
)
```

## Step 1: `setup()`

[`setup()`](https://openwashdata.github.io/fairenough/reference/setup.md)
scaffolds the standard data-package layout (`R/`, `data/`, `data_raw/`,
`inst/extdata/`), moves the input CSV into `data_raw/`, and configures
`.gitignore`.

``` r

setup(
  base_path = project,
  verbose = FALSE,
  overwrite = TRUE
)
```

## Step 2: `process()`

[`process()`](https://openwashdata.github.io/fairenough/reference/process.md)
reads everything in `data_raw/`, applies
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
to the column headers, and writes both a binary `.rda` (for
[`data()`](https://rdrr.io/r/utils/data.html) access) and a tidy `.csv`
(for download) per dataset.

``` r

process(base_path = project, verbose = FALSE)
```

## Step 3: `collect()`

[`collect()`](https://openwashdata.github.io/fairenough/reference/collect.md)
gathers the metadata that lives in `DESCRIPTION` — title, description,
authors, license, etc. Normally it walks the user through interactive
prompts, but every field can also be supplied as a named argument, which
is what we do here.

``` r

collect(
  base_path = project,
  interactive = FALSE,
  verbose = FALSE,
  overwrite = TRUE,
  pkg_name = "penguinpkg",
  title = "Palmer Penguins as an R Data Package",
  description = "Body size measurements for three penguin species observed on islands in the Palmer Archipelago.",
  license = "mit",
  authors = list(
    list(
      given = "Demo",
      family = "Author",
      email = "demo@example.org",
      roles = c("aut", "cre")
    )
  )
)
```

## Step 4: `generate()`

[`generate()`](https://openwashdata.github.io/fairenough/reference/generate.md)
produces a per-variable dictionary at `inst/extdata/dictionary.csv`.
Pass an
[`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html)
object to fill the `description` column from an LLM; pass `NULL` (or no
`chat`) to get the same dictionary with empty descriptions, which you
can edit by hand.

For a working vignette without an API key, we use a small stand-in
object that duck-types the `chat` interface — the same trick the
package’s tests use.

``` r

# Stand-in for an ellmer chat object. In real usage you'd write:
#   chat <- ellmer::chat_openai(model = "gpt-4o-mini")
fake_chat <- local({
  i <- 0
  list(chat = function(prompt) {
    i <<- i + 1
    sprintf("Demo description for variable #%d", i)
  })
})

generate(
  base_path = project,
  chat = fake_chat,
  verbose = FALSE,
  overwrite = TRUE
)
```

## Step 5: `build()` (not run here)

[`build()`](https://openwashdata.github.io/fairenough/reference/build.md)
is the final, heaviest step: it generates roxygen docs from the
dictionary, writes `inst/CITATION` and `CITATION.cff`, runs
`R CMD build` + `R CMD INSTALL`, knits the README, and builds the
pkgdown site. We skip the call inside this vignette to keep the build
fast, but the one-liner is just:

``` r

build(base_path = project, verbose = FALSE, preview = FALSE)
```

## The result

Here is everything `fairenough` produced under `project/`:

``` r

fs::dir_tree(project, recurse = TRUE)
```

That’s a complete, installable R data package created from a single CSV
— ready for `R CMD build`, GitHub, or CRAN.
