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
        menuItem("ANOVA",tabName="ANOVA"),
        menuItem("Paired T-test",tabName="PairedTtest")
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
        #################### Graph tab ####################
        tabItem(
          tabName='Graph',
          titlePanel('Graph'),
          tabsetPanel(
            sidebarLayout(
              sidebarPanel(
                selectInput("show", "Show", 
                            choices = c("Recent Cases", "Total Cases", "Number of Active Clusters"),
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
        #################### ANOVA tab ####################
        tabItem(
          tabName='ANOVA',
          titlePanel('ANOVA'),
          tabsetPanel(
            sidebarLayout(
              sidebarPanel(
                      sliderInput("daterange2", "Date Range", min(dengue$Date),max(dengue$Date),
                                  value = range(dengue$Date), step = 1
                      ),     
                selectInput("filterby3", "Filter by:", 
                            choices = c("Subzone", "Planning Area"),
                            selected = "Planning Area"),
  
              ),        
              mainPanel(
                h3("ANOVA based on Selected Filter"),       
                # p("This test is used to test if there is a significant difference between the means of two groups before and after a certain date"), 
                fluidRow(
                  #display the results based on the filterby3 input
                  verbatimTextOutput("anova"),
                )
              )
            )
            )
          ),
        #################### Paired T-test tab ####################
        tabItem(
          tabName='PairedTtest',
          titlePanel('Paired T-Test'),
          tabsetPanel(
            sidebarLayout(
              sidebarPanel(
                  dateInput("date1", "Date", value = "2019-11-01", format = "yyyy-mm-dd", min = "2015-01-01", max = "2020-12-31"),
                  # Key in number of months to be added or subtracted from date1
                  numericInput("day1", "Number of Days Before/After", value = 90, min = 31, max = 730),      
                  selectInput("filterby2", "Filter by:", 
                              choices = c("Subzone", "Planning Area"),
                              selected = "Planning Area"),
                  conditionalPanel(
                    condition = "input.filterby2 == 'Subzone'",
                      selectInput("subzone2", "Subzone", 
                                  choices = c(unique(dengue$Subzone)),
                                  selected = c(unique(dengue$Subzone)[1]))
                  ),
                  conditionalPanel(
                    condition = "input.filterby2 == 'Planning Area'",
                      selectInput("planningarea2", "Planning Area", 
                                  choices = c(unique(dengue$PlanningArea)),
                                  selected = 'YISHUN')
                  )
              ),
              mainPanel(
                h3("Paired T-Test to test before and after a certain date"),
                p("This test is used to test if there is a significant difference between the means of two groups before and after a certain date"),
                verbatimTextOutput("ttest")
              )
            )
            )
          )
        #################### End of Paired T-test tab ####################
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
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(dengue) %>% addTiles() %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  observe({
    #pal <- colorpal()
    filtered_dengue <- filteredData()
    
    all_planning_area_cases_tib <- filtered_dengue %>% group_by(PlanningArea) %>% summarise(num_cases = sum(NumberofCases))
    all_planning_area_cases <- all_planning_area_cases_tib %>% as.data.frame()
    
    all_PA_info <- merge(x=pop,y=all_planning_area_cases, by = 'PlanningArea', all.x=TRUE)
    all_PA_info[is.na(all_PA_info)] = 0
    print(all_PA_info)
    
    print(all_planning_area_cases)
    
    #all_planning_area_cases[nrow(all_planning_area_cases) + 1,] <- c("NORTH-EASTERN ISLANDS", 0)
    
    planning_area_cases <- all_PA_info$num_cases[all_PA_info$PlanningArea==rv$planning_area]
    print(planning_area_cases)
    #print(all_planning_area_cases)
    
    pal <- colorNumeric('YlOrRd', all_PA_info$num_cases)
    
    leafletProxy("map", data = filtered_dengue) %>%
    clearShapes() %>% clearHeatmap()  %>% addPolygons(data=planning_area_shape, layerId=~PLN_AREA_N, color='black',
                                                              weight=2, fillOpacity = 0.8,
                                                              fillColor = ~pal(all_PA_info$num_cases[all_PA_info$PlanningArea==PLN_AREA_N]),
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

  })
  

  
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

#################### START OF T TEST ############################
  dengue_filtered2_before <- reactive({
    data <- dengue %>%
    # The date range will be start date is date1, end date is date1 minus the number of months specified in input$month1
    filter(Date >= input$date1[1] - days(input$day1) & Date <= input$date1[1]) %>%
    # filter by filterby selection
    filter(if (input$filterby2 == "Subzone") {
      Subzone == input$subzone2 | input$subzone2 == "ALL"
    } else if (input$filterby2 == "Planning Area") {
      PlanningArea == input$planningarea2 | input$planningarea2 == "ALL"                
    }) %>%
      group_by(Date) %>% 
      summarise(NumberofCases = sum(NumberofCases)) %>%
      complete(Date = seq.Date(from = input$date1[1] - days(input$day1), to = input$date1[1], by = "day")) %>%
      replace_na(list(NumberofCases = 0))
  })

  dengue_filtered2_after <- reactive({
    data <- dengue %>%
    filter(Date >= input$date1[1] & Date <= input$date1[1] + days(input$day1)) %>%
    # filter by filterby selection
    filter(if (input$filterby2 == "Subzone") {
      Subzone == input$subzone2 | input$subzone2 == "ALL"
    } else if (input$filterby2 == "Planning Area") {
      PlanningArea == input$planningarea2 | input$planningarea2 == "ALL"                
    }) %>%
      group_by(Date) %>% 
      summarise(NumberofCases = sum(NumberofCases)) %>%
      complete(Date = seq.Date(from = input$date1[1], to = input$date1[1] + days(input$day1), by = "day")) %>%
      replace_na(list(NumberofCases = 0))
  })

  output$ttest <- renderPrint({
    data1 <- dengue_filtered2_before()
    data2 <- dengue_filtered2_after()
    cat('---------------------------Paired T-Test----------------------------')
    cat("\n")
    cat("Before: ", format(input$date1[1] - days(input$day1), "%d/%m/%Y"), " to ", format(input$date1[1], "%d/%m/%Y"))
    cat("Number of cases before: ", sum(data1$NumberofCases), "\n")
    cat("After: ", format(input$date1[1], "%d/%m/%Y"), " to ", format(input$date1[1] + days(input$day1), "%d/%m/%Y"))
    cat("Number of cases after: ", sum(data2$NumberofCases), "\n")
    result <- t.test(data2$NumberofCases, data1$NumberofCases, paired = TRUE, alternative = "less")
    print(result)
    if (result$p.value < 0.05) {
      cat("The number of cases in the second period is significantly lower than the number of cases in the first period.")
    } else {
      cat("The number of cases in the second period is not significantly lower than the number of cases in the first period.")
    }
  }) 
#################### END OF T TEST ############################

  
  pop_filtered <- reactive({
    pop %>% 
      filter(PlanningArea == input$planningarea | input$planningarea == "ALL")
  })
  
  #################### ANOVA TEST ############################
  dengue_anova <- reactive({
    data <- dengue %>%
    filter(between(Date, input$daterange2[1], input$daterange2[2])) #%>%
    #group_by(Date) %>%
    # summarise(NumberofCases = sum(NumberofCases)) #%>%
    #complete(Date = seq.Date(from = input$date1[1], to = input$date1[1] + days(input$day1), by = "day")) %>%
    #replace_na(list(NumberofCases = 0))
    

    # filter by filterby selection
    # filter(if (input$filterby3 == "Subzone") {
    #   Subzone == input$subzone3 | input$subzone3 == "ALL"
    # } else if (input$filterby3 == "Planning Area") {
    #   PlanningArea == input$planningarea3 | input$planningarea3 == "ALL"
    # }) %>%
      # group_by(Date) %>% 
      # summarise(NumberofCases = sum(NumberofCases)) %>%
      # complete(Date = seq.Date(from = input$date1[1], to = input$date1[1] + days(input$day1), by = "day")) %>%
      # replace_na(list(NumberofCases = 0))
  })


  output$anova <- renderPrint({

    print(dengue_anova())
    cat('---------------------Test for Homogeneity of Variances---------------------')
    cat("\n")
    # Test for Homogeneity of Variances (Levene's Test) based on either filterby3 selection
    if (input$filterby3 == "Subzone") {
      levene_Test <- leveneTest(NumberofCases ~ Subzone, data = dengue_anova())
    } else if (input$filterby3 == "Planning Area") {
      levene_Test <- leveneTest(NumberofCases ~ PlanningArea, data = dengue_anova())
    }
    # levene_Test <- leveneTest(NumberofCases ~ input$filterby3, data = dengue_anova())
    print(levene_Test)
    cat('----------------------------Test for Normality-----------------------------')
    cat("\n")
    # Test for Normality (Shapiro-Wilk Test) based on either filterby3 selection
    if (input$filterby3 == "Subzone") {
      res.aov <- aov(NumberofCases ~ Subzone, data = dengue_anova())
    } else if (input$filterby3 == "Planning Area") {
      res.aov <- aov(NumberofCases ~ PlanningArea, data = dengue_anova())
    }    
    # res.aov <- aov(NumberofCases ~ input$filterby3, data = dengue_anova())
    aov_residuals <- residuals(object = res.aov)
    shapiro_test <- shapiro.test(x=aov_residuals[0:5000])
    print(shapiro_test)
    cat("\n")
    cat('---------------------------Non-parametric ANOVA----------------------------')
    if (input$filterby3 == "Subzone") {
      nonpara.model <- kruskal.test(NumberofCases ~ Subzone, data = dengue_anova())
    } else if (input$filterby3 == "Planning Area") {
      nonpara.model <- kruskal.test(NumberofCases ~ PlanningArea, data = dengue_anova())
    }     
    # nonpara.model <- kruskal.test(NumberofCases ~ input$filterby3, data = dengue_anova())
    print(nonpara.model)
    cat('----------------------------Pairwise Comparison----------------------------')
    cat("\n")
    if (input$filterby3 == "Subzone") {
      pairwise.comparison <- pairwise.wilcox.test(dengue_anova()$NumberofCases, dengue_anova()$Subzone, dengue_anova.adjust.method = "BH")
    } else if (input$filterby3 == "Planning Area") {
      pairwise.comparison <- pairwise.wilcox.test(dengue_anova()$NumberofCases, dengue_anova()$PlanningArea, dengue_anova.adjust.method = "BH")
    }
    # wilcox <- pairwise.wilcox.test(dengue$NumberofCases, dengue_anova$input$filterby3, p.adjust.method = "bonferroni")
    print(pairwise.comparison)
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
        group_by(Date, ClusterNumber, PlanningArea) %>%
        summarise(RecentCasesinCluster = max(RecentCasesinCluster)) %>%
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
        group_by(Date, ClusterNumber, PlanningArea) %>%
        summarise(TotalCasesinCluster = max(TotalCasesinCluster)) %>%
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
