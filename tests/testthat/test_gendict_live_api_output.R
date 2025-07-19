library(testthat)
library(tibble)
library(palmerpenguins)
library(mall)
library(jsonlite)
library(glue)
library(cli)
library(desc)
library(dplyr)

# Load all package functions (assuming gendict is part of your package)
# devtools::load_all() # Uncomment if you're loading a package

test_gendict_live_api_output <- function() {
  test_that("gendict generates correct output structure with live API and prints output using llm_vec_custom", {
    # Ensure your API key is set up before running this test.
    # Sys.setenv(OPENAI_API_KEY = "YOUR_BLABLADOR_TOKEN") # Uncomment and set if not already in .Renviron

    # 1. Load mock data
    data_mock <- palmerpenguins::penguins

    # 2. Set up the mall LLM provider globally
    cli::cli_alert_info("Setting up mall LLM with OpenAI provider.")
    mall::llm_use(
      provider = "openai",
      model = "alias-fast", # Or another appropriate Blablador alias
      .openai_base_url = "https://api.helmholtz-blablador.fz-juelich.de/v1"
    )

    # 3. Call the gendict function
    context_str <- "This dataset contains measurements of penguins from the Palmer Archipelago, collected for scientific research on their ecology."

    cli::cli_alert_info("Calling gendict with live API using llm_vec_custom. This may take some time.")
    generated_dict <- gendict(data_mock, context = context_str)

    # 4. Print the resulting data dictionary
    cli::cli_h1("Generated Data Dictionary:")
    print(generated_dict)
    cli::cli_h1("End of Generated Data Dictionary")

    # 5. Perform assertions on the generated dictionary
    # Assert that the output is a tibble/data frame
    expect_s3_class(generated_dict, "tbl_df")
    expect_s3_class(generated_dict, "data.frame")

    # Assert that the output contains the expected columns
    expected_cols <- c("variable", "type", "description", "unit")
    expect_true(all(expected_cols %in% names(generated_dict)))

    # Assert that the number of rows matches the number of input columns
    expect_equal(nrow(generated_dict), ncol(data_mock))

    # Perform basic checks on content
    # Check that 'variable' column matches input names
    expect_equal(generated_dict$variable, names(data_mock))

    # Check that descriptions are not empty
    expect_true(all(nchar(generated_dict$description) > 0), info = "All descriptions should be non-empty.")

    # Check that units are character (they can be empty string)
    expect_type(generated_dict$unit, "character")

    # Check that types are among the allowed types (allowing for slight LLM variations)
    allowed_data_types <- c(
      "logical", "integer", "numeric", "text", "datetime",
      "categorical", "identifier", "spatial", "binary", "json", "array"
    )
    # This makes the test more robust against minor LLM misclassifications or default fallbacks
    expect_true(all(generated_dict$type %in% c(allowed_data_types, "character", "numeric", "factor", "integer")),
                info = "All types should be from the allowed list or common R types.")

    # Add more specific content checks
    expect_true(any(grepl("species", generated_dict$description, ignore.case = TRUE)),
                info = "Description for 'species' should mention species.")
    expect_true(any(grepl("island", generated_dict$description, ignore.case = TRUE)),
                info = "Description for 'island' should mention island.")
    expect_true(any(
      grepl("bill", generated_dict$description, ignore.case = TRUE) &
      grepl("length", generated_dict$description, ignore.case = TRUE)
    ),
    info = "Description for 'bill_length_mm' should mention bill length.")
    expect_true(any(
      grepl("body mass", generated_dict$description, ignore.case = TRUE) |
      grepl("weight", generated_dict$description, ignore.case = TRUE)
    ),
    info = "Description for 'body_mass_g' should mention body mass or weight.")

    # Check for expected units
    expect_true(generated_dict$unit[generated_dict$variable == "bill_length_mm"] %in% c("mm", "millimeters"),
                info = "Unit for 'bill_length_mm' should be mm or millimeters.")
    expect_true(generated_dict$unit[generated_dict$variable == "body_mass_g"] %in% c("g", "grams"),
                info = "Unit for 'body_mass_g' should be g or grams.")

    message("gendict live API test completed successfully and output printed!")
  })
}

# Run the test
test_gendict_live_api_output()