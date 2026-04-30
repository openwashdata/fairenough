## SETUP

Clone the repository, then install development dependencies from `DESCRIPTION`:

```r
devtools::install_deps(dependencies = TRUE)
devtools::load_all()
```

R ≥ 4.1.0 is required.

## COMMITS
This project follows [conventional commits](https://www.conventionalcommits.org/)

## README
It is importand to edit the README.Rmd as the pkgdown.yml workflow is based on the .Rmd for the website.
To update the README.md, run:
```R
rmarkdown::render('README.Rmd', output_format = "github_document")
```