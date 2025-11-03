## COMMITS
This project follows [conventional commits](https://www.conventionalcommits.org/)

## README
It is importand to edit the README.Rmd as the pkgdown.yml workflow is based on the .Rmd for the website.
To update the README.md, run:
```R
rmarkdown::render('README.Rmd', output_format = "github_document")
```