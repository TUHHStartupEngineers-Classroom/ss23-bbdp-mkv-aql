# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

#source(file = "00_scripts/stock_analysis_functions.R")
source(file = "stock_analysis_functions.R")

# UI ----
ui <- fluidPage(title = "Stock Analyzer")

# 1.0 HEADER ----
ui <- fluidPage(
  title = "Stock Analyzer",
  
  # 0.0 Test ----
  #"Learning Shiny... I'm building my first App"
  
  div(
    h1("Stock Analzer"),
    p("paragraph writing goes here"), 
  
  
  
    column(width = 4, "Selection",
           wellPanel(
             pickerInput(inputId = "stock_selection",
                         choices = 1:10)
             )
           ),
    
    column(width = 8, "Plot",
           div(
             
           )
           )
  )
)



# 2.0 APPLICATION UI -----

# 3.0 ANALYST COMMENTARY ----

# SERVER ----
server <- function(input, output, session) {
  
}

# RUN APP ----
shinyApp(ui = ui, server = server)