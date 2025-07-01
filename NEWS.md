# fairenough 0.1.0

## Major changes

* Forked from washr 1.0.1 to create fairenough
* Added automated CSV processing with `process_csv()` function
* Enhanced README.Rmd template with dynamic content generation
* Added placeholder for AI-powered dictionary generation (`gendict()`)
* Introduced unified pipeline workflow in pipeline.qmd
* Added Zenodo JSON creation functionality

## Breaking changes

* Package renamed from washr to fairenough
* Removed `update_access()`, `update_gsheet_metadata()`, and `utils.R`
* Functions now designed to be chainable with consistent return values

## New features

* `process_csv()`: Automated data cleaning and export
* Dynamic README generation with dataset tables and dictionaries
* Planned AI integration for documentation generation
* Modern package structure with renv support

## Under development

* `gendict()`: AI-powered dictionary generation
* Unified pipeline function for complete automation
* Enhanced error handling and progress tracking