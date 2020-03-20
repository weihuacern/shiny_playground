library(leaflet)
library(shinydashboard)

source("constants.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Dashboard"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Time Series", tabName = "timeseries", icon = icon("distribution")),
            menuItem("GeoMap", tabName = "geomap", icon = icon("th"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "timeseries",
                h2("Time Series on COVID-19"),
                fluidRow(
                    box(
                        plotOutput("pois", height = 250),
                        plotOutput("norm", height = 250)),
                    box(
                        title = "Controls",
                        sliderInput("slider", "Number of observations:", 1, 100, 50))
                )
            ),
            # Second tab content
            tabItem(
                tabName = "geomap",
                h2("World Map on COVID-19"),
                leafletOutput("WorldMap"),
                absolutePanel(
                    id = "input_date_control", class = "panel panel-default", draggable = T,
                    selectInput("choices", "Confirmed or Dead?", choices = c(CHOICE_CONF, CHOICE_DEAD), selected = CHOICE_CONF),
                    uiOutput("WorldMapSlider"),
                    helpText("Checking country details by single click on that country."),
                    uiOutput("WorldMapSelection"),
                    checkboxInput("legend", "Show legend", TRUE)
                )
            )
        )
    )
)