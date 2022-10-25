packages <- c("shiny",  "ggplot2",  "tidyverse",  "shinydashboard",  "leaflet",
              "magrittr", "lubridate", "reshape", "tidyverse", "DT",  "knitr", 
              "corrplot", "sf", "tmap", "rgdal", "htmlwidgets", "terra", "janitor",
              "RColorBrewer")

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

planning_area <- readOGR('Data/','URA_MP19_PLNG_AREA_PL')
ogrInfo("Data/", 'URA_MP19_PLNG_AREA_PL')
planning_area_shape <- spTransform(planning_area, CRS("+proj=longlat +datum=WGS84"))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Number Of Dengue Cases", min(dengue$NumberofCases), max(dengue$NumberofCases),
                            value = range(dengue$NumberofCases), step = 1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                sliderInput("daterange", "Date Range", min(dengue$Date),max(dengue$Date),
                            value = range(dengue$Date), step = 1
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
  
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    # dengue %>%
    #   filter(NumberofCases == between(NumberofCases, input$range[1], input$range[2]),
    #          Date == between(Date, input$daterange[1], input$daterange[2]))
    
    dengue <- filter(dengue, between(NumberofCases,input$range[1], input$range[2]),
                     between(Date, input$daterange[1], input$daterange[2]))
                                         
    # dengue[dengue$NumberofCases >= input$range[1] & dengue$NumberofCases <= input$range[2],
    #        dengue$Date >= input$daterange[1] & dengue$Date <= input$daterange[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, dengue$NumberofCases)
  })
  
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
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>% addPolygons(data=planning_area_shape, layerId=~PLN_AREA_N, weight=2,col = 'black',highlight = highlightOptions(weight = 5,
                                                                                                           color = "red",
                                                                                                           fillOpacity = 0.7,
                                                                                                           bringToFront = TRUE),label=~PLN_AREA_N) %>%
      addCircles(radius = ~NumberofCases/10, weight = 1, color = "#777777",
                 fillColor = ~pal(NumberofCases), fillOpacity = 0.7, popup = ~paste(NumberofCases)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = dengue)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~NumberofCases
      )
    }
  })
  
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    p <- input$map_shape_click
    print(p)
  })
  
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
}

shinyApp(ui, server)