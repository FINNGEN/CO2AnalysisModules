

# Test cases for readAndParseYalm
test_that("readAndParseYalm replaces placeholders and parses correctly", {
  # Create a simple YAML file with placeholders
  yamlContent <- c(
    "name: <name>",
    "age: <age>",
    "city: <city>"
  )

  tempFile <- withr::local_tempfile(
    lines = yamlContent,
    fileext = ".yaml"
  )

  # Test that the placeholders are correctly replaced and parsed
  parsedYAML <- readAndParseYalm(pathToYalmFile = tempFile, name = "Alice", age = 25, city = "Wonderland")

  # Check that the parsed result matches the expected values
  expect_type(parsedYAML, "list")
  expect_equal(parsedYAML$name, "Alice")
  expect_equal(parsedYAML$age, 25)
  expect_equal(parsedYAML$city, "Wonderland")
})

test_that("readAndParseYalm throws an error when placeholders are missing", {
  # Create a simple YAML file with placeholders
  yamlContent <- c(
    "name: <name>",
    "age: <age>"
  )

  tempFile <- withr::local_tempfile(
    lines = yamlContent,
    fileext = ".yaml"
  )

  # Test that an error is thrown when a placeholder is not found
  expect_error(
    readAndParseYalm(pathToYalmFile = tempFile, name = "Alice", age = 25, city = "Wonderland"),
    "The following placeholders were not found"
  )
})
