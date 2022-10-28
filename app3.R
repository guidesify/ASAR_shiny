packages <- c("shiny",  "ggplot2",  "tidyverse",  "shinydashboard",  "leaflet",
              "magrittr", "lubridate", "reshape", "tidyverse", "DT",  "knitr", 
              "corrplot", "sf", "tmap", "rgdal", "htmlwidgets", "terra", "janitor",
              "RColorBrewer", "leaflet.extras", "plotly", "car")

for (p in packages) {
  if (!require(p,  character.only = TRUE)) {
    install.packages(p)
  }
  library(p,  character.only = TRUE)
}

# Read dengue data:
dengue <- read_csv("Data/dengue_final.csv")

# Data Preparation

names(dengue) <- gsub(" ", "", names(dengue))
dengue$Date <- as.Date(dengue$Date, format = "%d/%m/%Y")

dengue$RecentCasesinCluster <- as.numeric(dengue$RecentCasesinCluster)
dengue$TotalCasesinCluster <- as.numeric(dengue$TotalCasesinCluster)
dengue$ClusterNumber <- as.numeric(dengue$ClusterNumber)
dengue$StreetAddress <- str_to_title(dengue$StreetAddress)

str(dengue)

#Read population data
pop <- read_csv("Data/population_2019.csv")
pop <- pop[, c("Planning Area", "total")]
names(pop) <- c("PlanningArea", "Population")
str(pop)

#Read planning area shapefile 

planning_area <- readOGR('Data/','MP14_PLNG_AREA_NO_SEA_PL')
ogrInfo("Data/", 'MP14_PLNG_AREA_NO_SEA_PL')
planning_area_shape <- spTransform(planning_area, CRS("+proj=longlat +datum=WGS84"))


ui <- 
  
  dashboardPage(
    dashboardHeader(),
    sidebar <- dashboardSidebar(
      sidebarMenu(
        menuItem("Map", tabName="Map"),
        menuItem("Graph",tabName="Graph"),
        menuItem("Annova",tabName="Annova")
      )
    ),
    dashboardBody(
      tags$head(tags$style("section.content { overflow-y: hidden; }")),
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
        ),
        # Graph Tab
        tabItem(
          tabName='Graph',
          titlePanel('Graph'),
          tabsetPanel(
            sidebarLayout(
              sidebarPanel(
                selectInput("show", "Show", 
                            choices = c("Recent Cases", "Total Cases", "Number of Active Clusters", "Anova test"),
                            selected = "Recent Cases"),
                dateRangeInput("date", "Date Range", 
                               start = "2019-01-01", end = "2019-12-31", 
                               format = "yyyy-mm-dd", 
                               min = "2015-01-01", max = "2022-12-31"),
                # We can only allow 1 select input to be selected at a time
                selectInput("filterby", "Filter by:", 
                            choices = c("Subzone", "Planning Area", "Region","Land Use","Search by Address"),
                            selected = "Subzone"),
                # Create a select input based on the filterby input
                conditionalPanel(
                  condition = "input.filterby == 'Subzone'",
                  selectInput("subzone", "Subzone", 
                              choices = c("ALL", unique(dengue$Subzone)),
                              selected = "ALL")
                ),
                conditionalPanel(
                  condition = "input.filterby == 'Planning Area'",
                  selectInput("planningarea", "Planning Area", 
                              choices = c("ALL", unique(dengue$PlanningArea)),
                              selected = "ALL")
                ),
                conditionalPanel(
                  condition = "input.filterby == 'Region'",
                  selectInput("region", "Region", 
                              choices = c("ALL", unique(dengue$Region)),
                              selected = "ALL")
                ),
                conditionalPanel(
                  condition = "input.filterby == 'Land Use'",
                  selectInput("landuse", "Land Use", 
                              choices = c("ALL", unique(dengue$LandUse)),
                              selected = "ALL")
                ),
                conditionalPanel(
                  condition = "input.filterby == 'Search by Address'",
                  textInput("location", "Location", value = "")
                )
              ),
              mainPanel(
                # Plot the graph by DateYMD
                plotlyOutput("plot1"),
              )
            )
          )
        ),
        # Annova tab
        tabItem(
          tabName='Annova',
          titlePanel('Annova'),
          tabsetPanel(
            mainPanel(
            fluidRow(
              # Plot the graph by DateYMD
              verbatimTextOutput("result1"),
              )
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
    
    all_planning_area_cases_tib <- filtered_dengue %>% group_by(PlanningArea) %>% summarise(num_cases = sum(NumberofCases))
    all_planning_area_cases <- all_planning_area_cases_tib %>% as.data.frame()
    
    print(all_planning_area_cases)
    
    #all_planning_area_cases[nrow(all_planning_area_cases) + 1,] <- c("NORTH-EASTERN ISLANDS", 0)
    
    planning_area_cases <- all_planning_area_cases$num_cases[all_planning_area_cases$PlanningArea==rv$planning_area]
    print(planning_area_cases)
    #print(all_planning_area_cases)
    
    leafletProxy("map", data = filtered_dengue) %>%
    clearShapes() %>% clearHeatmap()  %>% addPolygons(data=planning_area_shape, layerId=~PLN_AREA_N, color='black',
                                                              weight=2, fillOpacity = 0.8,
                                                              fillColor = ~colorNumeric("YlOrRd", all_planning_area_cases$num_cases)(all_planning_area_cases$num_cases[all_planning_area_cases$PlanningArea==PLN_AREA_N]),
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
  
  # Filter the data based on the date range and location if not blank
  dengue_filtered <- reactive({
    dengue %>% 
      filter(Date >= input$date[1] & Date <= input$date[2]) %>%
      # filter by filterby selection
      filter(if (input$filterby == "Subzone") {
        Subzone == input$subzone | input$subzone == "ALL"
      } else if (input$filterby == "Planning Area") {
        PlanningArea == input$planningarea | input$planningarea == "ALL"                
      } else if (input$filterby == "Region") {
        Region == input$region | input$region == "ALL"
      } else if (input$filterby == "Land Use") {
        LandUse == input$landuse | input$landuse == "ALL"
      } else if (input$filterby == "Search by Address") {
        grepl(input$location, StreetAddress, ignore.case = TRUE) | input$location == ""
      })
  })
  
  pop_filtered <- reactive({
    pop %>% 
      filter(PlanningArea == input$planningarea | input$planningarea == "ALL")
  })
  
  #Annova test
  p <- dengue %>%
    group_by(PlanningArea) %>%
    summarise(RecentCasesinCluster = mean(RecentCasesinCluster))
  
  output$result1 <- renderPrint({
    cat('---------------------Test for Homogeneity of Variances---------------------n')
    cat("\n")
    levene_Test <- leveneTest(RecentCasesinCluster ~ PlanningArea, data = dengue)
    print(levene_Test)
    cat('----------------------------Test for Normality-----------------------------n')
    cat("\n")
    res.aov <- aov(RecentCasesinCluster ~ PlanningArea, data = dengue)
    aov_residuals <- residuals(object = res.aov)
    shapiro_test <- shapiro.test(x=aov_residuals[0:5000])
    print(shapiro_test)
    nonpara.model <- kruskal.test(RecentCasesinCluster ~ PlanningArea, data = dengue)
    cat("\n")
    cat('---------------------------Non-parametric ANOVA----------------------------n')
    print(nonpara.model)
    cat('----------------------------Pairwise Comparison----------------------------n')
    cat("\n")
    wilcox <- pairwise.wilcox.test(dengue$RecentCasesinCluster, dengue$PlanningArea,
    dengue.adjust.method = "BH")
    print(wilcox)
  })
  
  
  # Plot graph  on selection and sum up the cases by Date, set tooltip to text
  output$plot1 <- renderPlotly({
    
    population_if_planning_area <-
      if (input$filterby == "Planning Area") { 
        "<br>Population: " %>% 
          paste(scales::comma(sum(pop_filtered()$Population)))
      } else {
        ""
      }
    
    if (input$show == "Recent Cases") {
      p <- dengue_filtered() %>%
        group_by(Date) %>%
        summarise(RecentCasesinCluster = sum(RecentCasesinCluster)) %>%
        ggplot(., aes(x = Date, y = RecentCasesinCluster, text = paste("Date:", Date, "<br>Recent Cases:", scales::comma(RecentCasesinCluster), population_if_planning_area))) +
        geom_bar(stat = "identity", fill = "#6e2a25", na.rm = TRUE) +
        labs(x = "Date", y = "Number of Cases", title = "Recent Dengue Cases") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (input$show == "Total Cases") {
      p <- dengue_filtered() %>%
        group_by(Date) %>%
        summarise(TotalCasesinCluster = sum(TotalCasesinCluster)) %>%
        ggplot(., aes(x = Date, y = TotalCasesinCluster, text = paste("Date:", Date, "<br>Total Cases:", scales::comma(TotalCasesinCluster), population_if_planning_area))) +
        geom_bar(stat = "identity", fill = "#2e0303", na.rm = TRUE) +
        labs(x = "Date", y = "Number of Cases", title = "Total Dengue Cases") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      p <- dengue_filtered() %>%
        group_by(Date) %>%
        summarise(ActiveClusters = n_distinct(ClusterNumber)) %>%
        ggplot(., aes(x = Date, y = ActiveClusters, text = paste("Date:", Date, "<br>No. of Active Clusters:", scales::comma(ActiveClusters), population_if_planning_area))) +
        geom_bar(stat = "identity", fill = "#ca7b04", na.rm = TRUE) +
        labs(x = "Date", y = "Number of Active Clusters", title = "Number of Active Clusters") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        theme(plot.title = element_text(hjust = 0.5))
    } 
    ggplotly(p, tooltip = c("text", "label"))
  })
  
}

shinyApp(ui, server)