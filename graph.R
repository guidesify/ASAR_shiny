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
# dengue$DateYMD <- as.Date(dengue$Date, format = "%Y-%m-%d")
# dengue$DateYMD <- as.character(dengue$DateYMD)
# dengue$NumberofCases <- as.numeric(dengue$NumberofCases)
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
                        choices = c("Recent Cases", "Total Cases"), 
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


    # Plot graph based on selection and sum up the cases by DateYMD
    output$plot1 <- renderPlotly({
        if (input$show == "Recent Cases") {
            p <- ggplot(dengue_filtered(), aes(x = Date, y = RecentCasesinCluster)) +
                geom_bar(stat = "identity", fill = "#6e2a25", na.rm = TRUE) +
                labs(x = "Date", y = "Number of Cases", title = "Recent Dengue Cases") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
                theme(plot.title = element_text(hjust = 0.5))
        } else {
            p <- ggplot(dengue_filtered(), aes(x = Date, y = TotalCasesinCluster)) +
                geom_bar(stat = "identity", fill = "#2e0303", na.rm = TRUE) +
                labs(x = "Date", y = "Number of Cases", title = "Total Dengue Cases") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
                theme(plot.title = element_text(hjust = 0.5))
        }
        ggplotly(p)
    })

}


shinyApp(ui, server)





# ui <- fluidPage(

#     titlePanel("Dengue Dashboard"),

#     # Main panel for displaying dengue weekly cases with daterange slider
#     mainPanel(
#         sidebarLayout(
#             sidebarPanel(
#                 dateRangeInput("daterange", "Date range:",
#                                start = "2015-01-01", end = "2022-12-31",
#                                format = "yyyy-mm-dd")
#             ),
#             mainPanel(
#                 plotOutput("dengue_weekly_cases")
#             )
#         )
#     )
# )



# server <- function(input, output, session) { # nolint

#     # Create reactive expression to filter dengue weekly cases by date range
#     dengue_weekly_cases <- reactive({
#         dengue_week %>%
#             filter(Date >= input$daterange[1] & Date <= input$daterange[2])
#     })

#     # Plot dengue weekly cases
#     output$dengue_weekly_cases <- renderPlot({
#         ggplot(dengue_weekly_cases(), aes(x = Date, y = NumberofCases)) +
#             geom_line() +
#             labs(x = "Date", y = "Number of Cases") +
#             theme_minimal()
#     })
# }
    

# shinyApp(ui = ui, server = server)


# Create a data frame with the number of cases per week
# dengue_week <- dengue %>%
#     group_by(Date) %>%
#     summarise(NumberofCases = sum(NumberofCases))

            # filter(grepl(input$address, StreetAddress, ignore.case = TRUE) | input$address == "")
