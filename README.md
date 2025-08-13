<!-- README.md is generated from README.Rmd. Please edit that file -->

# fairenough

<!-- badges: start -->

<!-- badges: end -->

Automated R data package creation with AI-powered documentation.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("openwashdata/fairenough")
```

## Usage

``` r
# Create a directory `penguins`
# Add penguins.csv

library(fairenough)

setup()
process()

# Sys.setenv(OPENAI_API_KEY = "YOUR_API_TOKEN_HERE")
chat <- ellmer::chat_openai(
model = "gpt-4.1-nano",
api_args = list(temperature=0.1,
                top_p = 0.9)
)

fairenough::document(chat)

fairenough::build()
```

{fairenough} automatically:
- Processes CSV files
- Generates data dictionaries using LLMs
- Creates documentation and website
- Prepares for publication

## License

MIT

## References

_Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago (Antarctica) penguin data. R package version 0.1.0. [https://allisonhorst.github.io/palmerpenguins/](https://allisonhorst.github.io/palmerpenguins/). doi: [10.5281/zenodo.3960218](10.5281/zenodo.3960218)._

## Contributions
Welcome!

**Philosophy**
- Conventional commits! [https://www.conventionalcommits.org/](https://www.conventionalcommits.org/).
- Feature functions must maintain architectural consistency. This means that aspects like supported formats and path handling should be uniform across all functions. For more details, refer to [R/utils.R](R/utils.R).

**Choices**
- To test new functions:
```
roxygen2::roxygenise()
devtools::load_all()
```
- Due to their utility outside of {fairenough}, `gendict.R` and `collect_metadata.R` have been kept general and not tied to the package's architecture.

## Dependencies