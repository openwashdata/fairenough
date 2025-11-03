
# fairenough

<!-- Badges will go here: CI status, CRAN status, coverage, etc. -->

<!-- ![R-CMD-check](badge-placeholder) [![CRAN status](badge-placeholder)](link) [![Codecov](badge-placeholder)](link) -->

Convert tidy data into R packages with documentation websites and
intelligent AI descriptions. **fairenough** prepares your data for
publication in one click.

## Why fairenough?

Fairenough transforms this:

    my-data/
    └── penguins.csv

Into this complete R package:

- **Documentation:** AI-generated variable descriptions and data
  dictionaries

- **Website:** Professional pkgdown site ready for deployment

- **Citations:** Properly formatted citation files with DOI support

- **Validation:** Data structure checks and format consistency

- **Integration:** Full R package that others can install and use

**[Demo: Palmerpenguins published with
fairenough!](https://openwashdata.github.io/palmerpenguins/)**

## Installation

**Prerequisites:** R ≥ 4.1.0 and an API key for your preferred LLM
provider

``` r
install.packages("pak")
pak::pkg_install("openwashdata/fairenough")
```

## Quick Start

- Create a new project (or just an empty directory) containing you
  dataset(s)
- Start an R console at the project (or directory) path
- Run

``` r
library(fairenough)

fairenough()
```

**That’s it!** You’ll get a complete R package with:

- Documented datasets and variables

- Professional package website

- Citation files and README

- Proper R package structure

## Quick Start with Ellmer

``` r
# 1. Set up your LLM chat object (see options below)
chat <- ellmer::chat_openai(model = "gpt-4o-mini", api_args = list(temperature = 0.3), api_key = "")

# 2. Place your CSV/Excel files in an empty directory and run
fairenough(chat)
```

- Running fairenough with an `ellmer::chat` object will generate
  descriptions for your dataset’s variables

- [See all supported providers
  →](https://ellmer.tidyverse.org/reference/index.html)

``` r
# OpenAI
chat <- ellmer::chat_openai()

# Anthropic Claude
chat <- ellmer::chat_anthropic()

# Local models via Ollama
chat <- ellmer::chat_ollama()
```

## Getting Help

- **Detailed documentation:**
  [Reference](https://openwashdata.github.io/fairenough/reference/index.html)

- **Issues & bugs:** [GitHub
  Issues](https://github.com/openwashdata/fairenough/issues)

- **Questions:** [GitHub
  Discussions](https://github.com/openwashdata/fairenough/discussions)

## Contributing

We welcome contributions! Please see our [contributing
guidelines](https://github.com/openwashdata/fairenough/blob/main/.github/CONTRIBUTING.md)
and note that this project follows [conventional
commits](https://www.conventionalcommits.org/).

**Development workflow:**

``` r
# Test new functions
roxygen2::roxygenise(clean = TRUE)
devtools::load_all()
```

**Architecture:** Feature functions maintain consistency in supported
formats and path handling. See
[R/utils.R](https://github.com/openwashdata/fairenough/blob/main/R/utils.R)
for implementation details.

## License

[MIT](https://github.com/openwashdata/fairenough/blob/main/LICENSE.md)

------------------------------------------------------------------------

## Citation

Please cite with:

**Text:**

    Massari N, Zhong M, Götschmann M, Walder C, Schöbitz L (2025). "fairenough: fairenough."
    <https://github.com/openwashdata/fairenough>.

**BibTeX:**

``` bibtex
@Misc{massari_etall:2025,
  title = {fairenough: fairenough},
  author = {Nicolo Massari and Mian Zhong and Margaux Götschmann and Colin Walder and Lars Schöbitz},
  year = {2025},
  url = {https://github.com/openwashdata/fairenough},
  abstract = {Convert tidy data into R packages with documentation websites and intelligent AI descriptions.},
  version = {0.1.0},
}
```
