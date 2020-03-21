library(leaflet)
library(shinydashboard)

source("constants.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Dashboard"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("GeoMap", tabName = "geomap", icon = icon("th")),
            menuItem("Time Series", tabName = "timeseries", icon = icon("bar-chart-o"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "geomap",
                h2("World Map on COVID-19"),
                leafletOutput("WorldMap", height = 600),
                fluidRow(
                    box(
                        selectInput("choices", "Confirmed or Dead?", choices = c(CHOICE_CONF, CHOICE_DEAD), selected = CHOICE_CONF),
                        uiOutput("WorldMapSlider")
                    ),
                    box(
                        helpText("Checking Country/Region details by single click on the map."),
                        uiOutput("WorldMapSelection"),
                        checkboxInput("WorldMapLegend", "Show legend", TRUE)
                    )
                )
            ),
            # Second tab content
            tabItem(
                tabName = "timeseries",
                h2("Time Series on COVID-19"),
                fluidRow(
                    box(
                        plotOutput("pois", height = 250),
                        plotOutput("norm", height = 250)
                    ),
                    box(
                        title = "Controls",
                        sliderInput("slider", "Number of observations:", 1, 100, 50)
                    )
                )
            )
        )
    )
)
