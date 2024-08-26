library(dplyr)
library(tidyverse)
library(stringdist)
library(stringr)
library(reshape2)
library(tidyr)
library(wesanderson)
library(pivottabler)
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  
  dashboardHeader(title = "Fallout TTRPG Character Sheet"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Character Sheet", tabName = "CharBoard"),
      menuItem(text = "Inventory Sheet", tabName = "InvBoard"),
      menuItem(text = "Options", tabName = "OptionBoard")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "CharBoard"
              
            ),
      
      # Lookup Cards Dashboard ----
      
      tabItem(tabName = "InvBoard"
        
      ),
      
      # Options Dashboard ----
      
      tabItem(tabName = "OptionBoard"
              
              )
      
      
    )
  ))
  

server <- function(input, output) {
  
  
  

}  
  
  
  
  shinyApp(ui, server)