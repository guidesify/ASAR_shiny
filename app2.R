packages <- c("shiny",  "ggplot2",  "tidyverse",  "shinydashboard",  "leaflet",
              "magrittr", "lubridate", "reshape", "tidyverse", "DT",  "knitr", 
              "corrplot", "sf", "tmap", "rgdal", "htmlwidgets", "terra", "janitor",
              "RColorBrewer", "leaflet.extras")

for (p in packages) {
  if (!require(p,  character.only = TRUE)) {
    install.packages(p)
  }
  library(p,  character.only = TRUE)
}

#Read dengue data:
dengue <- read_csv("Data/dengue_final.csv")

#dengue <- clean_names()

names(dengue) <- gsub(" ", "", names(dengue))
dengue$Date <- as.Date(dengue$Date, format = "%d/%m/%Y")
str(dengue)

#Read planning area shapefile 

planning_area <- readOGR('Data/','MP14_PLNG_AREA_NO_SEA_PL')
ogrInfo("Data/", 'MP14_PLNG_AREA_NO_SEA_PL')
planning_area_shape <- spTransform(planning_area, CRS("+proj=longlat +datum=WGS84"))


ui <- 
  
  dashboardPage(
    dashboardHeader(),
    sidebar <- dashboardSidebar(
      sidebarMenu(
        menuItem("Map", tabName="Map")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = 'Map', 
          titlePanel('Map'),
          tabsetPanel(
          fluidRow(
                    column(6,
                      sliderInput("range", "Number Of Dengue Cases", min(dengue$NumberofCases), max(dengue$NumberofCases),
                                 value = range(dengue$NumberofCases), step = 1
                      )
                    ),
                    #selectInput("colors", "Color Scheme",
                    #            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                    #),
                    column(6,
                      sliderInput("daterange", "Date Range", min(dengue$Date),max(dengue$Date),
                                  value = range(dengue$Date), step = 1
                      )
                    )
                    #checkboxInput("legend", "Show legend", TRUE)
            ),
            fluidRow(
              tags$style(type = "text/css", "#map {height: calc(100vh - 220px) !important;}"),
              leafletOutput("map"),
            )
          )
        )
      )
    )
  )


server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    dengue <- filter(dengue, between(NumberofCases,input$range[1], input$range[2]),
                     between(Date, input$daterange[1], input$daterange[2]))
                                         
  })
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  #colorpal <- reactive({
  #  colorNumeric(input$colors, dengue$NumberofCases)
  #})
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(dengue) %>% addTiles() %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  #sort_cases <- reactive({
  #  print(sum(dengue$NumberofCases[dengue$PlanningArea==input$map_shape_click$id]))
  #  sum(dengue$NumberofCases[dengue$PlanningArea==input$map_shape_click$id])
  #})
  
  observe({
    #pal <- colorpal()
    filtered_dengue <- filteredData()
    planning_area_cases <- sum(filtered_dengue$NumberofCases[filtered_dengue$PlanningArea==rv$planning_area])
    
    leafletProxy("map", data = filtered_dengue) %>%
    clearShapes() %>% clearHeatmap()  %>% addPolygons(data=planning_area_shape, layerId=~PLN_AREA_N, 
                                                              weight=2,col = 'black',
                                                              highlight = highlightOptions(weight = 5,
                                                                color = "red",
                                                                fillOpacity = 0.7,
                                                                bringToFront = TRUE),
                                                              label=~PLN_AREA_N,
                                                              popup=~paste("Planning Area: ", PLN_AREA_N, "<br>",
                                                                           "Number of Cases:", planning_area_cases), group="Planning Area") %>%
      addHeatmap(lng = ~Longitude, lat = ~Latitude, blur=20, radius=10, minOpacity=0.5, group="Heatmap") %>%
      addLayersControl(baseGroups = c("Planning Area", "Heatmap"), position = "bottomright", 
                                      options = layersControlOptions(collapsed = FALSE))
      #addCircles(radius = ~NumberofCases/10, weight = 1, color = "#777777",
      #           fillColor = ~pal(NumberofCases), fillOpacity = 0.7, popup = ~paste(NumberofCases)
      #)
  })
  
  # Use a separate observer to recreate the legend as needed.
  #observe({
  #  proxy <- leafletProxy("map", data = dengue)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
  #  proxy %>% clearControls()
  #  if (input$legend) {
  #    pal <- colorpal()
  #    proxy %>% addLegend(position = "bottomright",
  #                        pal = pal, values = ~NumberofCases
  #    )
  #  }
  #})

  
  observeEvent(input$map_shape_click, {
    rv$planning_area <- input$map_shape_click$id
    print(rv$planning_area)
  })
  
  
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
}

shinyApp(ui, server)