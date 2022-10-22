#create R shiny template
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
library(leaflet)

dengue <- read_csv('Data/dengue_final.csv')

#create shiny app
ui <- dashboardPage(
    dashboardHeader(title = "Dengue Fever in Singapore"),


)


server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet(dengue_data) %>%
            addTiles() %>%
            
    })
}


shinyApp(ui = ui, server = server)