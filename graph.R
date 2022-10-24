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

# Make a column clusterno-date
dengue$clusterno_date <- paste(dengue$ClusterNumber, dengue$Date, sep = "-")
# Remove duplicates from the data
dengue <- dengue[!duplicated(dengue$clusterno_date),]
str(dengue)

ui <- fluidPage(

    titlePanel("Dengue Cases in Singapore"),

    # Sidebar to select to show recent cases or total cases
    sidebarLayout(
        sidebarPanel(
            selectInput("show", "Show", 
                        choices = c("Recent Cases", "Total Cases", "Number of Active Clusters"),
                        selected = "Recent Cases"),
            dateRangeInput("date", "Date Range", 
                           start = "2019-01-01", end = "2019-12-31", 
                           format = "yyyy-mm-dd", 
                           min = "2015-01-01", max = "2022-12-31"),
            textInput("location", "Location", "Yishun")
    ),
        mainPanel(
            # Plot the graph by DateYMD
            plotlyOutput("plot1"),
        )
    )
)


server <- function(input, output, session) {

    # Filter the data based on the date range and location if not blank
    dengue_filtered <- reactive({
        dengue %>% 
            filter(Date >= input$date[1] & Date <= input$date[2]) %>%
            filter(grepl(input$location, StreetAddress, ignore.case = TRUE) | input$location == "")
            })


    # Plot graph based on selection and sum up the cases by Date, set tooltip to text
    output$plot1 <- renderPlotly({
        if (input$show == "Recent Cases") {
            p <- dengue_filtered() %>%
                group_by(Date) %>%
                summarise(RecentCasesinCluster = sum(RecentCasesinCluster)) %>%
                ggplot(., aes(x = Date, y = RecentCasesinCluster, text = paste("Date:", Date, "<br>Recent Cases:", RecentCasesinCluster))) +
                geom_bar(stat = "identity", fill = "#6e2a25", na.rm = TRUE) +
                labs(x = "Date", y = "Number of Cases", title = "Recent Dengue Cases") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
                theme(plot.title = element_text(hjust = 0.5))
        } else if (input$show == "Total Cases") {
            p <- dengue_filtered() %>%
                group_by(Date) %>%
                summarise(TotalCasesinCluster = sum(TotalCasesinCluster)) %>%
                ggplot(., aes(x = Date, y = TotalCasesinCluster, text = paste("Date:", Date, "<br>Total Cases:", TotalCasesinCluster))) +
                geom_bar(stat = "identity", fill = "#2e0303", na.rm = TRUE) +
                labs(x = "Date", y = "Number of Cases", title = "Total Dengue Cases") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
                theme(plot.title = element_text(hjust = 0.5))
        } else {
            p <- dengue_filtered() %>%
                group_by(Date) %>%
                summarise(ActiveClusters = n_distinct(ClusterNumber)) %>%
                ggplot(., aes(x = Date, y = ActiveClusters, text = paste("Date:", Date, "<br>No. of Active Clusters:", ActiveClusters))) +
                geom_bar(stat = "identity", fill = "#ca7b04", na.rm = TRUE) +
                labs(x = "Date", y = "Number of Active Clusters", title = "Number of Active Clusters") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
                theme(plot.title = element_text(hjust = 0.5))
        }
        ggplotly(p, tooltip = c("text"))
    })
}

shinyApp(ui, server)