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

# INITIALISE: structures repo
fairenough::init_all(base_path = ".")

# DOCUMENT: add necessary information to ship your data
Nicolo <- list(given = "Nicolo", 
               family = "Massari", 
               role = c("aut", "cre"), 
               email = "nmassari@ethz.ch", 
               ORCID = "0009-0006-8421-930X", 
               affiliation="ETH Zurich")

fairenough::doc_metadata(
  title = "palmerpenguins",
  description = "palmerpenguins provides two datasets of penguin measurements from Antarctica, collected by Dr. Kristen Gorman and the Palmer Station LTER.",
  organisation = "Global-Health-Engineering",
  authors = c(Nicolo),
  license =  "cc-by-4.0")

# GENERATE: LLM assisted dictionary documentation
library(ellmer)
Sys.setenv(OPENAI_API_KEY = "YOUR_API_TOKEN_HERE")
chat <- ellmer::chat_openai(
model = "gpt-4.1-nano",
api_args = list(temperature=0.1,
                top_p = 0.9)
)

dict <- fairenough::gendict(paste0(here::here(), "/tests/penguins/inst/extdata/penguins.csv"), chat)
```

{fairenough} automatically:
- Processes CSV files
- Generates data dictionaries using LLMs
- Creates documentation and website
- Prepares for publication

## License

CC0

## References

_Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago (Antarctica) penguin data. R package version 0.1.0. [https://allisonhorst.github.io/palmerpenguins/](https://allisonhorst.github.io/palmerpenguins/). doi: [10.5281/zenodo.3960218](10.5281/zenodo.3960218)._

## Contributions
Welcome! Philosophy:
- Conventional commits! [https://www.conventionalcommits.org/](https://www.conventionalcommits.org/).
- Feature functions must maintain architectural consistency. This means that aspects like supported formats and path handling should be uniform across all functions. For more details, refer to [R/utils.R](R/utils.R).