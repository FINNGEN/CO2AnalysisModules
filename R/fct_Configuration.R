
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


#' Set Up Logger
#'
#' Sets up a logger with console and file appender for logging.
#'
#' @param logsFolder A string representing the folder name for the logs.
#'
#' @return A logger object.
#'
#' @export
fcr_setUpLogger  <- function(logsFolder = "logs"){

  folderWithLog <- file.path(tempdir(),  logsFolder)
  dir.create(folderWithLog, showWarnings = FALSE)
  logger <- ParallelLogger::createLogger(
    threshold = "TRACE",
    appenders = list(
      # to console for tracking
      .createConsoleAppenderForSandboxLogging(),
      # to file for showing in app
      ParallelLogger::createFileAppender(
        fileName = file.path(folderWithLog, "log.txt"),
        layout = ParallelLogger::layoutSimple
      )
    )
  )
  ParallelLogger::clearLoggers()
  #addDefaultFileLogger(file.path(folderWithLog, "log2.txt"))
  ParallelLogger::registerLogger(logger)

  shiny::addResourcePath(logsFolder, folderWithLog)

  logshref <- paste0(logsFolder, "/log.txt")
  return(logshref)

}

#' Create Console Appender for Sandbox Logging
#'
#' Creates a console appender for sandbox logging with a specified layout.
#'
#' @param layout A layout function for the logger. Defaults to `ParallelLogger::layoutParallel`.
#'
#' @return An appended object for logging.
#'
.createConsoleAppenderForSandboxLogging <- function(layout = ParallelLogger::layoutParallel) {
  appendFunction <- function(this, level, message, echoToConsole) {
    # Avoid note in check:
    missing(this)
    message <- paste0("[sandbox-co2analysismodules-log] ", message)
    writeLines(message, con = stderr())
  }
  appender <- list(appendFunction = appendFunction, layout = layout)
  class(appender) <- "Appender"
  return(appender)
}
