# Library ----

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
library(DT)

# Processing Steps ----

CharStats <- read.csv("C:/RLibraries/FalloutTTRPG/Grizzly.csv")
SkillDesc <- read.csv("C:/RLibraries/FalloutTTRPG/SkillDesc.csv")
PerkDesc <- read.csv("C:/RLibraries/FalloutTTRPG/Perks.csv")
Ammunition <- read.csv("C:/RLibraries/FalloutTTRPG/Ammunition.csv")
Weapons <- read.csv("C:/RLibraries/FalloutTTRPG/FalloutTTRPG_Weapons.csv")

items <- c("Choose Ammo ...",Ammunition %>%
             select(AMMUNITION.TYPE))

DerivedCharStats <- data.frame(
  Attribute = c("Carry Weight", "Defence", "Initiative", "Health Points", "Melee Damage"),
  Value = c(CharStats %>%
              filter(Attribute == "Strength")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1]%>%
              `*`(10)%>%
              `+`(150),
            if(
              CharStats %>%
              filter(Attribute == "Agility")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1]>8) {
              2}
            else {
              1},
            CharStats %>%
              filter(Attribute == "Perception")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1] + 
              CharStats %>%
              filter(Attribute == "Agility")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1],
            CharStats %>%
              filter(Attribute == "Endurance")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1] + 
              CharStats %>%
              filter(Attribute == "Luck")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1],
            if(
              CharStats %>%
              filter(Attribute == "Strength")%>%
              mutate(Value = as.numeric(Value)) %>%
              .$Value %>%
              .[1]>10) {
              3}
            else {
              if(
                CharStats %>%
                filter(Attribute == "Strength")%>%
                mutate(Value = as.numeric(Value)) %>%
                .$Value %>%
                .[1]>8) {
                2}
              else {
                if(
                  CharStats %>%
                  filter(Attribute == "Strength")%>%
                  mutate(Value = as.numeric(Value)) %>%
                  .$Value %>%
                  .[1]>6) {
                  1}
                else {
                  0}
              }
            }
            
  )
)


#UI ----

ui <- dashboardPage(
  
  dashboardHeader(title = "Fallout TTRPG Character Sheet"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Character Sheet", tabName = "CharBoard"),
      menuItem(text = "Inventory Sheet", tabName = "InvBoard"),
      menuItem(text = "Options", tabName = "OptionBoard")
    )
  ),
  
  
  # Character Dashboard ----
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "CharBoard",
              
              fluidRow(
                
                titlePanel(uiOutput("CharName"))
                
              ),
              
              
              fluidRow(
                
                titlePanel(title = "SPECIAL"),
                dataTableOutput("SPECIAL")),
              
              hr(),
              
              fluidRow(
                
                column(width = 4,
                       
                       titlePanel(title = "Skills"),
                       dataTableOutput("Skills")
                       
                       
                       ),
                column(width = 3,
                       
                       titlePanel(title = "Derived Stats"),
                       dataTableOutput("DerivStats")
                       
                       
                       )
                
                
              ),
              
              fluidRow(
                
                titlePanel(title = "Weapons"),
                dataTableOutput("Weapons")
                
                
                
              )
              
              
            ),
      
      # Inventory Dashboard ----
      
      tabItem(tabName = "InvBoard",
              
              titlePanel("Select Items and Manage Quantities"),
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("item_select", "Choose an item:", choices = items),
                  numericInput("item_quantity", "Quantity:", value = 1, min = 1),
                  actionButton("add_item", "Add Item"),
                  br(),
                  br(),
                  h4("Selected Items:")
                ),
                
                mainPanel(
                  DTOutput("item_table")
                )
              )
        
      ),
      
      # Options Dashboard ----
      
      tabItem(tabName = "OptionBoard"
              
              )
      
      
    )))
  

#Server ----

server <- function(input, output) {
  
  
  output$CharName <- renderUI({
    # Filter the data frame to find the value you want to display
    selected_value <- CharStats %>%
      filter(Attribute == "Name") %>%
      .$Value  # Extract the first matching value
    
    # Create a title with the selected value
    h2(paste("Character Name:", selected_value))
    
  })
  
  output$SPECIAL <- DT::renderDT({
    datatable(
      CharStats %>%
        filter(Attributetype == "SPECIAL")%>%
        select(Attribute, Value)%>%
        pivot_wider(names_from = Attribute, values_from = Value),
      options = list(
        paging = FALSE,  # Disable pagination
        #      scrollY = "400px",  # Optional: Scroll within a fixed height window
        scrollCollapse = TRUE
      ),
      rownames = FALSE)
  })
  
  output$Skills <- DT::renderDT({
    datatable(merge(merge(
      SkillDesc %>%
        select("Skill" = SKILL,"Perk" = ATTRIBUTE),
      CharStats %>%
        filter(Attributetype == "Skills")%>%
        select("Skill" = Attribute,Value),
      by = "Skill")
      
      ,CharStats%>%
        filter(Attributetype == "Tag Skills")%>%
        select("Skill" =  Attribute, "Tagged" = Value),
      by.x = "Skill", by.y = "Skill", all = TRUE,
      #    by = "Skill",
      options = list(
        paging = FALSE,  # Disable pagination
        #      scrollY = "400px",  # Optional: Scroll within a fixed height window
        scrollCollapse = TRUE
      ),
      rownames = FALSE),
      options = list(
        paging = FALSE,  # Disable pagination
        #      scrollY = "400px",  # Optional: Scroll within a fixed height window
        scrollCollapse = TRUE
      ),
      rownames = FALSE)
  })
  
  output$Inventory <- DT::renderDT({
    CharStats %>%
    filter(Attributetype == "Inventory")%>%
    select(Attribute, Value)
  })
  
  output$Ammunition <- DT::renderDT({
    CharStats %>%
    filter(Attributetype == "Ammunition")%>%
    select(Attribute, Value)
  })
  
  output$Caps <- DT::renderDT({
    CharStats %>%
    filter(Attributetype == "Caps")%>%
    select(Value)
  })
  
  output$DerivStats <- DT::renderDT({
    datatable(
    DerivedCharStats,
    options = list(
      paging = FALSE,  # Disable pagination
      #      scrollY = "400px",  # Optional: Scroll within a fixed height window
      scrollCollapse = TRUE
    ),
    rownames = FALSE)
  })
  
  
  output$Weapons <- DT::renderDT({
    datatable(
      merge(merge(merge(merge(CharStats%>%
                                filter(Attributetype=="Inventory")%>%
                                select(Attribute),
                              Weapons,
                              by.x = "Attribute",
                              by.y = "WEAPON"
      ),
      CharStats%>%
        filter(Attributetype == "Skills")%>%
        select(Attribute, "SkillValue" = Value),
      by.x = "WEAPON.TYPE",
      by.y = "Attribute"),
      SkillDesc,
      by.x = "WEAPON.TYPE",
      by.y = "SKILL"),
      CharStats%>%
        filter(Attributetype == "SPECIAL")%>%
        mutate(Name_short = toupper(substr(Attribute, 1, 3)))%>%
        select(Name_short, Value),
      by.x ="ATTRIBUTE",
      by.y = "Name_short")%>%
        mutate(TN = as.numeric(SkillValue) + as.numeric(Value))%>%
        select("Name" = Attribute, "Skill" = WEAPON.TYPE, TN, "Damage" = DAMAGE.RATING, "Effects" = DAMAGE.EFFECTS, "Type" = DAMAGE.TYPE, "Rate" = FIRE.RATE, "Range" = RANGE, "Qualities" = QUALITIES, "Weight" = WEIGHT),
      
      options = list(
        paging = FALSE,  # Disable pagination
        #      scrollY = "400px",  # Optional: Scroll within a fixed height window
        scrollCollapse = TRUE
      ),
      rownames = FALSE)
    
  })
  
  
  
  
  
  
  
  
  #Inventory Server code ----
  
  # Reactive value to store the selected items and quantities
  selected_items <- reactiveVal(data.frame(Item = character(), Quantity = numeric(), stringsAsFactors = FALSE))
  
  # Observe when the "Add Item" button is clicked
  observeEvent(input$add_item, {
    new_item <- input$item_select
    new_quantity <- input$item_quantity
    
    # Get the current table data
    current_data <- selected_items()
    
    # Check if the item already exists in the table
    if (new_item %in% current_data$Item) {
      # If the item exists, update its quantity
      current_data <- current_data %>%
        mutate(Quantity = ifelse(Item == new_item, Quantity + new_quantity, Quantity))
    } else {
      # If the item does not exist, add it to the table
      current_data <- rbind(current_data, data.frame(Item = new_item, Quantity = new_quantity, stringsAsFactors = FALSE))
    }
    
    # Update the reactive value
    selected_items(current_data)
  })
  
  # Observe when an item needs to be removed
  observeEvent(input$item_table_rows_selected, {
    selected_row <- input$item_table_rows_selected
    
    if (length(selected_row) > 0) {
      current_data <- selected_items()
      updated_data <- current_data[-selected_row, , drop = FALSE]
      selected_items(updated_data)
    }
  })
  
  # Render the table of selected items with editable quantities
  output$item_table <- renderDT({
    datatable(
      selected_items(), 
      selection = 'single',  # Allow single row selection
      editable = list(target = "cell", disable = list(columns = c(0))),  # Allow editing only Quantity column
      options = list(dom = 't')  # Disable search, pagination, etc.
    )
  }, server = FALSE)
  
  # Observe edits to the Quantity column and update the data
  observeEvent(input$item_table_cell_edit, {
    info <- input$item_table_cell_edit
    str(info)  # Display info for debugging
    i <- info$row
    j <- info$col
    v <- info$value
    
    if (j == 2) {  # Check if the edited column is the Quantity column
      current_data <- selected_items()
      current_data[i, j] <- as.numeric(v)  # Update the value
      selected_items(current_data)
    }
  })
  

}  
  
  
  
  shinyApp(ui, server)