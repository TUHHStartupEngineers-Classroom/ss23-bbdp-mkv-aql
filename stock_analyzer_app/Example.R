library(shiny)

ui <- fluidPage(
  # First pickerInput
  pickerInput(
    inputId = "continent_picker",
    label = "Select a continent:",
    choices = c("Africa", "Asia", "Europe", "North America", "South America")
  ),
  
  # Second pickerInput (dynamic)
  uiOutput("country_picker")
)

server <- function(input, output, session) {
  # Render the second pickerInput based on the selected continent
  output$country_picker <- renderUI({
    selected_continent <- input$continent_picker
    
    if (!is.null(selected_continent)) {
      # Define the choices for the second pickerInput based on the selected continent
      country_choices <- switch(selected_continent,
                                "Africa" = c("Nigeria", "South Africa", "Egypt"),
                                "Asia" = c("China", "India", "Japan"),
                                "Europe" = c("France", "Germany", "Italy"),
                                "North America" = c("USA", "Canada", "Mexico"),
                                "South America" = c("Brazil", "Argentina", "Colombia"))
      
      # Create the second pickerInput
      pickerInput(
        inputId = "country_picker",
        label = "Select a country:",
        choices = country_choices
      )
    }
  })
}

shinyApp(ui = ui, server = server)
