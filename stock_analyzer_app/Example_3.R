library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

#source(file = "00_scripts/stock_analysis_functions.R")
source(file = "stock_analysis_functions.R")

ui <- fluidPage(
  # First pickerInput
  pickerInput(
    inputId = "index_picker",
    label = "Select an index:",
    choices = c("DAX", "SP500", "DOW", "NASDAQ")
  ),
  
  # Second pickerInput (dynamic)
  uiOutput("stock_picker")
)

#---------------------------------

server <- function(input, output, session) {
  # Render the second pickerInput based on the selected continent
  output$stock_picker <- renderUI({
    selected_index <- input$index_picker
    
    if (!is.null(selected_index)) {
      # Filter the countries based on the selected continent
      
      
      # Create the second pickerInput
      pickerInput(
        inputId = "stock_picker",
        label = "Select a stock:",
        #choices = c("DAX", "SP500", "DOW", "NASDAQ")
        choices = get_stock_list(selected_index)
      )
    }
  })
}

shinyApp(ui = ui, server = server)