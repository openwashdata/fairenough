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
library(fairenough)

# Create a directory `penguins`
# Add penguins.csv
fairenough::init_all(base_path = "tests/penguins")

library(ellmer)
#Sys.setenv(OPENAI_API_KEY = "YOUR_API_TOKEN_HERE") # or setup .Renviron
chat <- ellmer::chat_openai(
base_url = "https://api.helmholtz-blablador.fz-juelich.de/v1",
api_key = Sys.getenv("OPENAI_API_KEY"),
model = "alias-fast", # Or other Blablador aliases like "alias-code", "alias-large", etc.
api_args = list(temperature=0.1,
                top_p = 0.9,
                top_k = 20)
)

dict <- fairenough::gendict(paste0(here::here(),"/tests/penguins/inst/extdata/penguins.csv"), chat, context="Palmer Penguins Dataset.")
```

{fairenough} automatically:
- Processes and cleans CSV files
- Generates data dictionaries using LLMs
- Creates documentation and website
- Prepares for publication

## License

GPL-3