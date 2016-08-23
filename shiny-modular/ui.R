shinyUI(fixedPage(
  title="Colour survey- Shiny app",
  fixedRow(
    div(
      style="padding: 10px; border-bottom: 1px solid #CCC; margin-bottom:10px;",
      h4("Shiny Survey Tool v.1.0",img(src="logo.png", height="35px", align="right"))
    )
  ),

  mainPanel(
    tags$head(tags$script(src="ui.js")),
    tags$body(onkeydown="buttonresponse()"),

    uiOutput("main_content"),

    br(),
    br(),

    ## This displays the action putton Next.
    actionButton("button", "Next")
  )))
