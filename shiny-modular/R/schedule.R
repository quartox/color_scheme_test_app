MAP_WIDTH <- 800
MAP_HEIGHT <- MAP_WIDTH * 0.63

welcome <- function() {
  list(
    h5("Welcome to the data.as.colour survey tool!"),
    h6("by Kendra"),
    h6(em("With special thanks to Will, Rich, Rita and everyone else who assisted with the development of this survey by providing feedback."))
  )
}

comments <- function() {
  list(
    h5("Thank you for completing the colour survey!"),
    p("Are there any comments you would like to make about the survey?"),
    br(),
    tags$input(type="text", id="comments", value="", autofocus="autofocus")
    
  )
}

goodbye<- function() {
  list(
    h5("Thank you for completing the colour survey! Please tell the researcher that you have finished."),
    # the log will stop running when the survey reaches this page
    stop.logging()
    
  )
}


# all of the demography questions are grouped together into a single list here
demography_questions <- function() {
  list(
    list("function"="intro_demography"),
    list("function"="demography_gender"),
    list("function"="demography_age"),
    list("function"="demography_education_completed", fields="demography_education_completed_details"),
    list("function"="demography_education_studying", fields="demography_education_studying_details"),
    list("function"="demography_see_visualisations"),
    list("function"="demography_prior_study"),
    list("function"="demography_colourblind", fields="demography_colourblind_details"),
    list("function"="demography_survey_recruitment"),
    list("function"="demography_ishihara_test")
  )
}

intro_demography <- function() {
  list(
    h2("Questions about yourself"),
    p("In the following section, you will be asked to provide basic information about yourself.")
  )
}

demography_gender <- function() {
  list(
    radioButtons("demography_gender", label="What is your gender?", choices=c("Male","Female"), selected=NA)
  )
}

demography_age <- function() {
  list(
    radioButtons("demography_age", label="Which age group do you belong to?", choices=c("18-24","25-34","35-44","45-54","55-64","65 and over"="65+"), selected=NA)
  )
}

demography_education_completed <- function() {
  list(
    strong("What is the highest level of education you have completed?"),
    p("E.g. If you are currently undertaking a bachelor's degree, you would state that the highest level of education you have completed is secondary school"),
    radioButtons("demography_education_completed", label=NULL, 
                 choices=c( "No formal education" = "None",
                            "Primary school" = "Primary",
                            "Secondary school"= "Secondary", 
                            "Vocational education and training (e.g. TAFE)"="VET",
                            "Tertiary education – Bachelor"="Bachelor",
                            "Tertiary education – Postgraduate coursework"="Postgraduate",
                            "Tertiary education – Higher degree research"="Research",
                            "Other - please provide details in the text box below" = "Other"), 
                 selected=NA),
    strong("If you selected vocational, tertiary or other qualifications in the question above, please provide details about your area of study (e.g. science, biology major):"),
    textInput("demography_education_completed_details", label=NULL)
  )
}

demography_education_studying <- function() {
  list(
    radioButtons("demography_education_studying", label="Are you currently studying? Please make a selection", 
                 choices=c( "I am not currently studying" = "None",
                            "Vocational education and training (e.g. TAFE)"="VET",
                            "Tertiary education – Bachelor"="Bachelor",
                            "Tertiary education – Postgraduate coursework"="Postgraduate",
                            "Tertiary education – Higher degree research"="Research",
                            "Other"), 
                 selected=NA),
    strong("Please provide details about your current area of study (e.g. science, biology major). If you are not currently studying, please type in 'not applicable'."),
    textInput("demography_education_studying_details", label=NULL)
  )
}

demography_see_visualisations <- function() {
  list(
    p("The following images are examples of colour-coded visualisations. They use different types of colour-coding to convey information."),
    p("For example, the map on the bottom left uses blue to represent lower temperatures and red to represent higher temperatures."),
    img(src="colour-maps.png", height=500),
    radioButtons("demography_see_visualisations", label="Thinking about your everyday life, how often do you see colour-coded visualisations?", choices=c("Never","Rarely", "Occasionally", "Sometimes","Frequently"), selected=NA)
  )
}

demography_prior_study <- function() {
  list(
    p(strong("Have you ever undertaken training or study (including formal or independent study) in any of the following areas? Please select all that apply.")),
    checkboxGroupInput("demography_prior_study", choices = list("Visual perception - e.g. in psychology or biology"="Perception", "Colour theory - e.g. how colours are formed using lights or paint"="ColourTheory", "Selecting appropriate colour schemes for data visualisations"="ColourSelection", "Colour discrimination - e.g training in a psychological experiment where you learn to discriminate new colours"="Training", "None of the above"="None"), label=NULL)
  )
}


demography_colourblind <- function() {
  list(
    radioButtons("demography_colourblind", label="Are you colour-blind?", choices=c("Yes","No"), selected=NA),
    textInput("demography_colourblind_details",label="If you answered 'Yes', please describe which colours you have difficulty discriminating." )
  )
}

demography_survey_recruitment <- function() {
  list(
    radioButtons("demography_survey_recruitment", label="How did you find out about this project?", choices=c("Advertising posters on the UNSW campus","Social media posts","Other"), selected=NA)
  )
}

demography_ishihara_test <- function() {
  list(
    h4("Stop! Please ask the researcher to enter your result", style="color:red"),
    radioButtons("demography_ishihara_test", label="Ishihara test - Normal colour vision", choices=c("Yes","No"), selected=NA)
  )
}

intro_ugly_test <- function() {
  list(
    h2("Aesthetic ratings"),
    p("In the following section, you will be asked to rate the appearance of the colours used in different images.")
  )
}

ugly_test_volcano <- function(scheme) {
  list(
    h2("Aesthetic ratings"),
    renderPlot(display.plot(scheme),antialias="none", width=600),
    p(strong("Rate the appearance of the colours used in the image, on a scale from 1 (ugly) to 7 (beautiful)")),
    br(),
    span("Ugly", style="display:inline-block"),
    span(sliderInput('ugly_test_volcano', label=NULL, min=1, max=7, value=4, step=1, width="300px"), style="display:inline-block"),
    span("Beautiful", style="display:inline-block"))
}

ugly_test_heatmap <- function(scheme) {
  list(
    h2("Aesthetic ratings"),
    renderPlot(display.heat.map(scheme), width=600),
    p(strong("Rate the appearance of the colours used in the image, on a scale from 1 (ugly) to 7 (beautiful)")),
    br(),
    span("Ugly", style="display:inline-block"),
    span(sliderInput('ugly_test_heatmap', label=NULL, min=1, max=7, value=4, step=1, width="300px"), style="display:inline-block"),
    span("Beautiful", style="display:inline-block"))
}

ugly_test_choroplethmap <- function(scheme) {
  list(
    h2("Aesthetic ratings"),
    renderPlot(display.map.for.aesthetics(scheme), width=600),
    p(strong("Rate the appearance of the colours used in the image, on a scale from 1 (ugly) to 7 (beautiful)")),
    br(),
    span("Ugly", style="display:inline-block"),
    span(sliderInput('ugly_test_choroplethmap', label=NULL, min=1, max=7, value=4, step=1, width="300px"), style="display:inline-block"),
    span("Beautiful", style="display:inline-block"))
}

test_question_ramp_1 <- function(scheme) {
  list(
    h2("Colour Ramps - Sample question 1"),
    p("In the image shown below, colour is used to represent numbers along a scale from 1 to 100"),
    renderPlot(make.gradient.rectangle(scheme), width=400, height=125),
    br(),
    p("In the sample question below, you are shown three squares."),
    p("The colour of the square on the left corresponds to a value of 1, while the colour of the square on the right corresponds to a value of 100. The colour of the middle square represents a value somewhere between that of the other two squares."),
    renderPlot(make.colour.linear.question(scheme, 50, TRUE), width=500, height=300),
    h5("Question: What is the value of the middle square?"),
    p("For example, if you estimate that the colour of the middle square represents a value of 50, type ’50’ into the text box."),
    tags$input(type="number", id="num_100", value="", min="1", max="100", autofocus="autofocus"),
    h4("Please ask the researcher to check your answer", style="color:red")
    )
}

test_question_ramp_2 <- function(scheme) {
  list(
    h2("Colour Ramps - Sample question 2"),
    p("The squares on the left and right correspond to values of 1 and 100"),
    renderPlot(make.colour.linear.question(scheme, 10, TRUE), width=500, height=300),
    h5("What is the value of the middle square?"),
    tags$input(type="number", id="num_100", value="", min="1", max="100", autofocus="autofocus"),
    h4("Please ask the researcher to check your answer", style="color:red")
  )
}

test_question_map_1 <- function(scheme) {
  marker_id <- match(100, get_london()$coords$Partic_Per)
  list(
    h2("Maps - Sample question 1"),
    p("In the map below, each area is colour-coded to represent the number of cats* found in that area."),
    p("For example, we can use the legend located on the right to estimate that the area labelled 'A' contains 100 cats."),
    p("*The data used in this part of the survey is fictional.", style="color:grey"),
    renderPlot(display.single.marker.on.map("greenscale", marker_id), width=MAP_WIDTH, height=MAP_HEIGHT),
    h5("Question: How many cats are in area A?"),
    p("Type '100' into the box"),
    tags$input(type="number", id="num_100", value="", min="1", max="100", autofocus="autofocus")
  )
}

test_question_map_2 <- function(scheme) {
  marker_id <- 32
  list(
    h2("Maps - Sample question 2"),
    renderPlot(display.single.marker.on.map("greenscale", marker_id), width=MAP_WIDTH, height=MAP_HEIGHT),
    h5("Question: How many cats are in area A?"),
    tags$input(type="number", id="num_100", value="", min="1", max="100", autofocus="autofocus"),
    h4("Please ask the researcher to check your answer", style="color:red")
  )
}



intro_question_ramp <- function(scheme) {
  list(
    h2("Colour Ramps"),
    p("In the following questions, you will be shown the start and end colours of this scale. You will be given another colour which lies between these two colours and asked what value that colour corresponds to."),
    renderPlot(make.gradient.rectangle(scheme), width=400, height=125),
    p("The scale will not be shown to you again when you are answering the questions.", style="color:red"),
    p("Click 'Next' when you have familiarised yourself with the scale and are ready to start the questions. Please note that your response time for each question will be recorded.")
    )
}

real_question_ramp <- function(scheme, value) {
  list(
    h2("Colour Ramps"),
    p("The squares on the left and right correspond to values of 1 and 100"),
    renderPlot(make.colour.linear.question(scheme, value), width=400, height=125),
    h5("What is the value of the middle square?"),
    tags$input(type="number", id="num_100", value="", min="1", max="100", autofocus="autofocus"))
}

intro_question_map <- function(scheme) {
  reload_london()
  list(
    h2("Maps"),
    p("In the following questions, you will be given the map below and asked to estimate the number of cats found in a selected area."),
    renderPlot(display.single.marker.on.map(scheme, 1), width=MAP_WIDTH, height=MAP_HEIGHT),
    p("Click 'Next' when you are ready to begin. Please note that your response time for each question will be recorded.")
  )
}

real_question_map <- function(scheme, value) {
  list(
    h2("Maps"),
    renderPlot(display.single.marker.on.map(scheme, value), width=MAP_WIDTH, height=MAP_HEIGHT),
    h5("How many cats are in area A?"),
    tags$input(type="number", id="num_100", value="", min="1", max="100", autofocus="autofocus"))
}

## Also need to deal with the finishing questions.  But that should
## more or less do, I think.

## We're working with percentages, so 1..100 is valid.  Shiny automatically converts non-integer values (e.g. text) into NULLs so no
## need to check them here.
validate_num_100 <- function(x) {
  !(is.na(x) || x < 1 || x > 100)
}

make_schedule <- function(type, schemes, n, include_demography=TRUE, include_ugly=TRUE) {
  fn_test_1 <- paste0("test_question_", type, "_1")
  fn_test_2 <- paste0("test_question_", type, "_2")
  fn_real <- paste0("real_question_", type)
  fn_intro <- paste0("intro_question_", type)

  make_tests <- function(scheme) {
    if (type == "map") {
      idx <- sample.int(24, n * 2)
    } else {
      idx <- c(random.number.interval(n), random.number.interval(n))
    }
    intro <- structure(list("function"=fn_intro, scheme=scheme, fields="num_100"))
    real <- lapply(idx, function(i)
      list("function"=fn_real, scheme=scheme, value=i, fields="num_100"))
    c(list(intro), real)
  }
  # could clean up the functions for ugly_test a bit here or above
  make_ugly_volcano <- function(scheme) {
    list("function"="ugly_test_volcano", scheme=scheme)
  }
  make_ugly_heatmap <- function(scheme) {
    list("function"="ugly_test_heatmap", scheme=scheme)
  }
  make_ugly_choroplethmap <- function(scheme) {
    list("function"="ugly_test_choroplethmap", scheme=scheme)
  }

  ## The idea here is that the schedule is abstracted to a list of lists;
  ## each list has a function name e.g. (function="welcome") that indicates
  ## the function to call, and then has an arbitary number of arguments (possibly
  ## zero) that will be passed to that function.  The argument passing is done
  ## with do.call, so _all_ arguments need to be used!
  ##
  ## Be careful with the levels of listing here.  Not pretty :(
  c(list(list("function"="welcome")),
    if (include_demography) demography_questions() else NULL,
    if (include_ugly) list(list("function"="intro_ugly_test")), 
    if (include_ugly) lapply(sample(schemes), make_ugly_volcano),
    if (include_ugly) lapply(sample(schemes), make_ugly_heatmap),
    if (include_ugly) lapply(sample(schemes), make_ugly_choroplethmap),
    list(list("function"=fn_test_1, scheme="greenscale", fields="num_100"),list("function"=fn_test_2, scheme="greenscale", fields="num_100")),
    unlist(lapply(sample(schemes), make_tests), FALSE),
    list(list("function"="comments")),
    list(list("function"="goodbye")))
}

to_call <- function(x, drop="fields") {
  args <- x[setdiff(names(x), c("function", drop))]
  as.call(c(as.name(x[["function"]]), args))
}
