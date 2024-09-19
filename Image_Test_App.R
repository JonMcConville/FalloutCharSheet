library(shiny)

ui <- fluidPage(
  # Add custom CSS for background image
  tags$head(
    tags$style(
      HTML("
        body {
          background-image: url('www/GS_general_store_interior.jpg');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          height: 100vh;
        }
      ")
    )
  ),
  
  # Your app UI components here
  titlePanel("Shiny App with Background Image"),
  sidebarLayout(
    sidebarPanel(
      h3("Sidebar")
    ),
    mainPanel(
      h3("Main Content")
    )
  )
)

server <- function(input, output, session) {
  # Server logic here
}

shinyApp(ui, server)