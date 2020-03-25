library(leaflet)
library(shinydashboard)

source("constants.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Dashboard"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "GeoMap",
                icon = icon("th"),
                startExpended = TRUE,
                menuSubItem(
                    'World',
                    tabName = 'world-geomap',
                    icon = icon('line-chart')
                ),
                menuSubItem(
                    'China',
                    tabName = 'chn-geomap',
                    icon = icon('line-chart')
                ),
                menuSubItem(
                    'United States of America',
                    tabName = 'usa-geomap',
                    icon = icon('line-chart')
                )
            ),
            menuItem(
                "Time Series",
                tabName = "timeseries",
                icon = icon("bar-chart-o")
            )
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "world-geomap",
                h2("COVID-19: World Map"),
                leafletOutput("WorldMap", height = 600),
                fluidRow(
                    box(
                        selectInput(
                            "wmcs",
                            "Confirmed or Dead?",
                            choices = c(CHOICE_CONF, CHOICE_DEAD),
                            selected = CHOICE_CONF),
                        uiOutput("WorldMapSlider")
                    ),
                    box(
                        helpText("Checking Country/Region details by single click on the map."),
                        uiOutput("WorldMapSelection"),
                        checkboxInput("WorldMapLegend", "Show legend", TRUE)
                    )
                )
            ),
            tabItem(
                tabName = "chn-geomap",
                h2("COVID-19: China Map"),
                leafletOutput("CHNMap", height = 600),
                fluidRow(
                    box(
                        selectInput(
                            "chnmcs",
                            "Confirmed, Dead or Revovered?",
                            choices = c(CHOICE_CONF, CHOICE_DEAD),
                            selected = CHOICE_CONF),
                        uiOutput("CHNMapSlider")
                    ),
                    box(
                        helpText("Checking Provinces details by single click on the map."),
                        uiOutput("CHNMapSelection"),
                        checkboxInput("CHNMapLegend", "Show legend", TRUE)
                    )
                )
            ),
            tabItem(
                tabName = "usa-geomap",
                h2("COVID-19: United States of America Map"),
                leafletOutput("USAMap", height = 600),
                fluidRow(
                    box(
                        selectInput(
                            "usamcs",
                            "Confirmed, Dead or Revovered?",
                            choices = c(CHOICE_CONF, CHOICE_DEAD),
                            selected = CHOICE_CONF),
                        uiOutput("USAMapSlider")
                    ),
                    box(
                        helpText("Checking States details by single click on the map."),
                        uiOutput("USAMapSelection")
                        #checkboxInput("USAMapLegend", "Show legend", TRUE)
                    )
                )
            ),
            # Second tab content
            tabItem(
                tabName = "timeseries",
                h2("COVID-19: Time Series"),
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
