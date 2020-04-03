library(DT)
library(dygraphs)
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
                "Table View",
                icon = icon("th"),
                startExpended = TRUE,
                menuSubItem(
                    "World",
                    tabName = "world-table",
                    icon = icon("table")
                ),
                menuSubItem(
                    "China",
                    tabName = "chn-table",
                    icon = icon("table")
                ),
                menuSubItem(
                    "United States of America",
                    tabName = "usa-table",
                    icon = icon("table")
                )
            ),
            menuItem(
                "Time Series",
                icon = icon("th"),
                startExpended = TRUE,
                menuSubItem(
                    "World",
                    tabName = "world-ts",
                    icon = icon("line-chart")
                ),
                menuSubItem(
                    "China",
                    tabName = "chn-ts",
                    icon = icon("line-chart")
                ),
                menuSubItem(
                    "United States of America",
                    tabName = "usa-ts",
                    icon = icon("line-chart")
                )
            ),
            menuItem(
                "GeoMap",
                icon = icon("th"),
                startExpended = TRUE,
                menuSubItem(
                    "World",
                    tabName = "world-geomap",
                    icon = icon("map")
                ),
                menuSubItem(
                    "China",
                    tabName = "chn-geomap",
                    icon = icon("map")
                ),
                menuSubItem(
                    "United States of America",
                    tabName = "usa-geomap",
                    icon = icon("map")
                )
            )
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # Table, World
            tabItem(
                tabName = "world-table",
                h2("COVID-19: World Table"),
                DT::dataTableOutput(constIDTableWorld)
            ),
            # Table, CHN
            tabItem(
                tabName = "chn-table",
                h2("COVID-19: China Table"),
                DT::dataTableOutput(constIDTableCHN)
            ),
            # Table, USA
            tabItem(
                tabName = "usa-table",
                h2("COVID-19: United States of America Table"),
                DT::dataTableOutput(constIDTableUSA)
            ),
            # Time series, World
            tabItem(
                tabName = "world-ts",
                h2("COVID-19: World Time Series"),
                uiOutput(constIDTSSelWorld),
                fluidRow(
                    box(
                        h3("Confirmed Cases"),
                        dygraphOutput(constIDTSWorldConf)
                    ),
                    box(
                        h3("Dead Cases"),
                        dygraphOutput(constIDTSWorldDead)
                    )
                )
            ),
            # Geomap, World
            tabItem(
                tabName = "world-geomap",
                h2("COVID-19: World Map"),
                leafletOutput("WorldMap", height = 600),
                fluidRow(
                    box(
                        selectInput(
                            "wmcs",
                            "Confirmed or Dead?",
                            choices = c(constChoiceConf, constChoiceDead),
                            selected = constChoiceConf),
                        uiOutput("WorldMapSlider")
                    ),
                    box(
                        helpText("Checking Country/Region details by single click on the map."),
                        uiOutput("WorldMapSelection"),
                        checkboxInput("WorldMapLegend", "Show legend", TRUE)
                    )
                )
            ),
            # Geomap, CHN
            tabItem(
                tabName = "chn-geomap",
                h2("COVID-19: China Map"),
                leafletOutput("CHNMap", height = 600),
                fluidRow(
                    box(
                        selectInput(
                            "chnmcs",
                            "Confirmed, Dead or Revovered?",
                            choices = c(constChoiceConf, constChoiceDead),
                            selected = constChoiceConf),
                        uiOutput("CHNMapSlider")
                    ),
                    box(
                        helpText("Checking Provinces details by single click on the map."),
                        uiOutput("CHNMapSelection"),
                        checkboxInput("CHNMapLegend", "Show legend", TRUE)
                    )
                )
            ),
            # Geomap, USA
            tabItem(
                tabName = "usa-geomap",
                h2("COVID-19: United States of America Map"),
                leafletOutput("USAMap", height = 600),
                fluidRow(
                    box(
                        selectInput(
                            "usamcs",
                            "Confirmed, Dead or Revovered?",
                            choices = c(constChoiceConf, constChoiceDead),
                            selected = constChoiceConf),
                        uiOutput("USAMapSlider")
                    ),
                    box(
                        helpText("Checking States details by single click on the map."),
                        uiOutput("USAMapSelection"),
                        checkboxInput("USAMapLegend", "Show legend", TRUE)
                    )
                )
            )
        )
    )
)
