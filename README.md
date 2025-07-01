<!-- README.md is generated from README.Rmd. Please edit that file -->

# fairenough

<!-- badges: start -->

[![R-CMD-check](https://github.com/openwashdata/fairenough/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openwashdata/fairenough/actions/workflows/R-CMD-check.yaml)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

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

fairenough_pipeline(
  raw_data = "data-raw/",
  package_name = "mydata",
  title = "My Dataset Package",
  api_key = Sys.getenv("OPENAI_API_KEY")
)
```

This automatically:
- Processes and cleans CSV files
- Generates data dictionaries using AI
- Creates documentation and website
- Prepares for publication

## License

GPL-3