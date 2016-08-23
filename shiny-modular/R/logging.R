format.log.entry.json <- function(event) {
  jsonlite::toJSON(unclass(event))
}

setup.logging <- function(filename) {
  stop.logging() # clear previous log
  log_file(filename, .formatter=format.log.entry.json)
  log_file("console", .formatter=format.log.entry.json)
}

stop.logging <- function() {
  log_info("Ending log")
  deactivate_log()
}
