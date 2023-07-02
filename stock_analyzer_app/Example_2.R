library(shiny)
library(tibble)

# Create a tibble with country data
countries <- tibble(
  continent = c("Africa", "Africa", "Africa", "Asia", "Asia", "Asia", "Europe", "Europe", "Europe"),
  country = c("Nigeria", "South Africa", "Egypt", "China", "India", "Japan", "France", "Germany", "Italy")
)

ui <- fluidPage(
  # First pickerInput
  pickerInput(
    inputId = "continent_picker",
    label = "Select a continent:",
    choices = unique(countries$continent)
  ),
  
  # Second pickerInput (dynamic)
  uiOutput("country_picker")
)

server <- function(input, output, session) {
  # Render the second pickerInput based on the selected continent
  output$country_picker <- renderUI({
    selected_continent <- input$continent_picker
    
    if (!is.null(selected_continent)) {
      # Filter the countries based on the selected continent
      filtered_countries <- countries %>% 
        filter(continent == selected_continent) %>% 
        pull(country)
      
      # Create the second pickerInput
      pickerInput(
        inputId = "country_picker",
        label = "Select a country:",
        choices = filtered_countries
      )
    }
  })
}

shinyApp(ui = ui, server = server)
