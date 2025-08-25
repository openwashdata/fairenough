

# fairenough

Convert tidy data into R packages with documentation websites and
intelligent AI descriptions.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("openwashdata/fairenough")
```

## Usage

### One-Click Pipeline

For the simplest experience, move your datasets in an empty directory,
then run the main `fairenough()` function:

``` r
library(fairenough)

# Set up your LLM chat object
Sys.setenv(OPENAI_API_KEY = "YOUR_API_TOKEN_HERE")
chat <- elmer::chat_openai(model = "gpt-4o-mini", api_args = list(temperature = 0.3))

# Run the complete pipeline with one command
fairenough(chat)
```

This automatically: - Sets up your R package structure - Processes all
CSV/Excel files in the directory - Collects package metadata through
interactive prompts - Builds documentation, citation files, README, and
website - Generates intelligent data documentation using LLMs - Prepares
your package for publication

### Granular Control

For step-by-step control, use individual wrapper functions:

``` r
library(fairenough)

# Step 1: Initialize project structure
setup()

# Step 2: Process your data files
process()

# Step 3: Collect package metadata
collect()

# Step 4: Generate LLM-powered documentation
chat <- elmer::chat_openai(model = "gpt-4o-mini", api_args = list(temperature = 0.3))
generate(chat)

# Step 5: Build all package components
build()
```

## Features

fairenough provides a complete pipeline for R data package creation,
following these logical steps:

**1. Intelligent Project Setup (`setup()`)**

- R package structure initialization with `usethis`

- Directory organization (data_raw, etc.)

- .gitignore management

**2. Automated Data Processing (`process()`)**

- Processes all raw data files automatically with `readr`, `readxl`

- Built-in data cleaning and transformation

- Validates data structure and formats

**3. Interactive Metadata Collection (`collect()`)**

- Guided prompts for package metadata (title, description, authors,
  etc.) using `cli`

- Extended metadata options for comprehensive documentation

- Saves directly to DESCRIPTION file with `desc`

**4. LLM-Powered Documentation (`generate()`)**

- Intelligent data dictionary generation using `elmer` chat/LLM
  integration

- Variable descriptions created from dataset context + actual data
  samples

- Smart documentation that understands your data’s meaning and structure

**5. Complete Package Infrastructure (`build()`)**

- Roxygen documentation generation with `roxygen2`

- Citation file creation with validation using `cffr` - README
  generation with `rmarkdown`

- Package website building ready for deployment

**6. One-Click Pipeline (`fairenough()`)**

- Complete R data package creation with a single `fairenough(chat)` call

- Automated workflow from tidy data to finished package

**7. Granular Control Options**

- Individual wrapper functions: `setup()`, `process()`, `collect()`,
  `generate()`, `build()`

- Flexible overwrite and verbosity controls

- Step-by-step execution when needed

## License

[MIT](LICENSE.md)

## References

*Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago
(Antarctica) penguin data. R package version 0.1.0.
<https://allisonhorst.github.io/palmerpenguins/>. doi:
[10.5281/zenodo.3960218](10.5281/zenodo.3960218).*

## Contributions

Welcome!

**Philosophy**

- Conventional commits! <https://www.conventionalcommits.org/>.

- Feature functions must maintain architectural consistency. This means
  that aspects like supported formats and path handling should be
  uniform across all functions. For more details, refer to
  [R/utils.R](R/utils.R).

- To test new functions:

<!-- -->

    roxygen2::roxygenise()
    devtools::load_all()

- Due to their utility outside of {fairenough},
  [`gendict.R`](R/gendict.R), [`build_license.R`](R/build_license.R) and
  [`promptit.R`](R/promptit.R) have been kept general and not tied to
  the package’s architecture.

## Dependencies

### Imports

- cffr
- cli
- desc
- devtools
- dplyr
- ellmer
- fontawesome
- fs
- glue
- goodpractice
- gt
- here
- janitor
- jsonlite
- knitr
- pkgdown
- readr
- readxl
- rmarkdown
- roxygen2
- rstudioapi
- spelling
- stringr
- tibble
- tools
- usethis
- whisker
- withr (\>= 3.0.0)
