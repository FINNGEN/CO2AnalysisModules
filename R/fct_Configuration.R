
#' Read and Parse YAML File with Placeholder Replacement
#'
#' Reads a YAML file from a given path, replaces specified placeholders with provided values,
#' and returns the parsed content. If any provided placeholders are not found in the YAML file,
#' the function throws an error.
#'
#' @param pathToYalmFile A string representing the file path to the YAML file.
#' @param ... Named arguments to replace placeholders in the format `<name>` within the YAML file.
#'
#' @return A parsed list representing the contents of the modified YAML file.
#'
#' @importFrom yaml yaml.load as.yaml
#'
#' @examples
#' # Assuming the YAML file contains placeholders like <name>:
#' # name: <name>
#' # age: <age>
#' #
#' # You can replace those placeholders by passing named arguments:
#' readAndParseYalm("path/to/yaml/file", name = "John", age = 30)
#'
#' @export
readAndParseYalm <- function(pathToYalmFile, ...) {
  # read the yaml file
  yalmString <- readLines(pathToYalmFile) |> paste(collapse = "\n")
  # get the names of the parameters
  args <- list(...)
  argsNames <- names(args)

  # check for missing placeholders
  missingParams <- argsNames[!sapply(argsNames, function(name) any(grepl(paste0("<", name, ">"), yalmString)))]

  # if any placeholders are not found, throw an error
  if (length(missingParams) > 0) {
    stop(paste("Error: The following placeholders were not found in the YAML file:", paste(missingParams, collapse = ", ")))
  }

  # replace the values in the yaml file
  for (name in argsNames) {
    yalmString <- gsub(paste0("<", name, ">"), args[[name]], yalmString)
  }

  # parse the yaml file
  yalmFile <- yaml::yaml.load(yalmString)
  return(yalmFile)
}
