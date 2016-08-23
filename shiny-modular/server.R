library(shiny)
library(loggr)

source("R/ColourPalettes.R")
source("R/QuestionFunctions.R")
source("R/logging.R")
source("R/schedule.R")

dir.create("results", FALSE)
log.file <- file.path("results", paste0(digest::digest(Sys.time()), ".log"))
setup.logging(log.file)

## Set this to be either "map" or "ramp" to toggle between the two types.
# TYPE <- "map"
TYPE <- "ramp"

## For the map, generate random numbers on 1..24

## shiny::runApp()
shinyServer(function(input, output, session) {
  schemes <- setdiff(names(colour_schemes()), "greenscale")
  env <- new.env(parent=emptyenv())
  env$i <- 1L
  env$scheme <- make_schedule(TYPE, schemes, 12, include_demography = TRUE, include_ugly = TRUE)
  env$properties <- list()
  # create a new progress bar object
  progress <- shiny::Progress$new()

  mc <- eventReactive(input$button, {
    ## First, get the answer to the previous question.
    keep <- setdiff(names(input), "button")
    ok <- TRUE
    if (env$i > 1L) {
      dat <- env$scheme[[env$i - 1L]]
      # The field name and any answers entered into the field are assigned to the check variable
      check <- c(dat$fields, dat[["function"]])
      # The method used to validate the answer depends on the type of field
      if ("num_100" %in% check) {
        ok <- ok && validate_num_100(input$num_100)
      }

      is_demography <- grepl("^demography_", check)
      if (any(is_demography)) {
        demography_null <- sapply(check[is_demography], function(x) is.null(input[[x]]))
        ok <- ok && !any(demography_null)
      }
      ## this check requires the text field to be filled in
      if ("demography_education_studying_details" %in% check) {
        ok <- ok && !identical(input$demography_education_studying_details, "")
      }
    } else {
      check <- character(0)
    }
    # set information to log
    log_info("",
             properties=env$properties,
             values=reactiveValuesToList(input)[check],
             ok=ok)
    if (ok) {
      i <- env$i
      env$i <- min(env$i + 1L, length(env$scheme))
      # update the progress bar
        progress$set(env$i/length(env$scheme), detail = paste("Doing question", env$i,"of", TYPE))

      
    } else {
      i <- env$i - 1L
    }

    env$properties <- dat <- env$scheme[[i]]
    res <- eval(to_call(dat), .GlobalEnv)
    if (!ok) {
      if ("num_100" %in% check) {
        err <- "Please enter a value between 1 and 100"
      } else if (length(check) == 1L) {
        err <- "Please make a selection"
      } else {
        err <- "Please make selections for all fields"
      }
      res <- c(res,
               list(p(err, class="warning")))
    }
    res
  })

  output$main_content <- renderUI({
    mc()
    
  })
})
