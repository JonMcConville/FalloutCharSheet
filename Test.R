library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=VT323&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'VT323', monospace;
      }
    "))
  ),
  titlePanel("Change Font Example with Google Fonts"),
  sidebarLayout(
    sidebarPanel(
      h3("This is a Sidebar"),
      p("Some text here.")
    ),
    mainPanel(
      h2("Main Panel Text"),
      p("This text uses the 'Roboto' font.")
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)