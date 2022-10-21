#create R shiny template
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydashboard)

dengue_data <- read.csv("dengue_final.csv")

#create shiny app
ui <- function{

    

}

server <- function{

}

shinyApp(ui, server)