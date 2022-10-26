# Load required packages
packages <- c("shiny",  "ggplot2",  "tidyverse",  "shinydashboard",  "leaflet",
              "magrittr", "lubridate", "reshape", "tidyverse", "DT",  "knitr", 
              "corrplot", "sf", "tmap", "rgdal", "htmlwidgets", "terra","plotly")

for (p in packages) {
    if (!require(p,  character.only = TRUE)) {
        install.packages(p)
    }
    library(p,  character.only = TRUE)
    }

#Read the data
dengue <- read_csv("Data/dengue_final.csv")

names(dengue) <- gsub(" ", "", names(dengue))
dengue$Date <- as.Date(dengue$Date, format = "%d/%m/%Y")
dengue$RecentCasesinCluster <- as.numeric(dengue$RecentCasesinCluster)
dengue$TotalCasesinCluster <- as.numeric(dengue$TotalCasesinCluster)
dengue$ClusterNumber <- as.numeric(dengue$ClusterNumber)
dengue$StreetAddress <- str_to_title(dengue$StreetAddress)

# # Make a column clusterno-date
# dengue$clusterno_date <- paste(dengue$ClusterNumber, dengue$Date, sep = "-")
# # Remove duplicates from the data
# dengue <- dengue[!duplicated(dengue$clusterno_date),]
str(dengue)

#Read population data
pop <- read_csv("Data/population_2019.csv")
pop <- pop[, c("Planning Area", "total")]
names(pop) <- c("PlanningArea", "Population")
str(pop)

#Read planning area shapefile 

planning_area <- readOGR('Data/','URA_MP19_PLNG_AREA_PL')
ogrInfo("Data/", 'URA_MP19_PLNG_AREA_PL')
planning_area_shape <- spTransform(planning_area, CRS("+proj=longlat +datum=WGS84"))

ui <- navbarPage(
    title = 'Dengue Cases in Singapore',
    fluid = TRUE,
    tabPanel(
      "Descriptive - Bar chart",
      #titlePanel('Dengue Cases in Singapore'),
      
      # Sidebar to select to show recent cases or total cases
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
    ),
    tabPanel(
      "Anova test",
      #titlePanel('Dengue Cases in Singapore'),
        mainPanel(
          textOutput("result1"),
          )
        ),
)


server <- function(input, output, session) {

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
    
    p <- dengue %>%
      group_by(PlanningArea) %>%
      summarise(RecentCasesinCluster = mean(RecentCasesinCluster))
    
    #based
    output$result1 <- renderPrint({
      nonpara.model <- kruskal.test(RecentCasesinCluster ~ PlanningArea, data = p)
      cat('---------------------Non-parametric ANOVA---------------------n')
      print(nonpara.model)
      cat("\n")
      cat('---------------------Pairwise Comparison---------------------n')
      wilcox <- pairwise.wilcox.test(p$RecentCasesinCluster, p$PlanningArea)
                                     #p.adjust.method = "BH")
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

