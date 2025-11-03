
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
