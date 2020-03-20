library(leaflet)
library(shiny)
library(tidyverse)

source("constants.R")
source("utils.R")

renderStatPlot <- function(statType, input) {
    set.seed(435)
    
    if (all(statType == "pois")) {
        histdata <- rpois(5000, lambda=5)
        histtitle <- "Possion Distribution"
    } else if (all(statType == "norm")) {
        histdata <- rnorm(5000)
        histtitle <- "Normal Distribution"
    }
  
    res <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data, main=histtitle)
    })
    return(res)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    statType = "pois"
    output[[statType]] <- renderStatPlot(statType, input)
    
    statType = "norm"
    output[[statType]] <- renderStatPlot(statType, input)
    
    # GeoMap: WorldMap
    output$WorldMap <- renderLeaflet({
        leaflet(data = WorldMapShape) %>%
        setView(0, 30, zoom = 3)
    })
    
    ## Get dataset
    UrlStrConf = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
    UrlStrDead = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
    dataArea <- reactive({
        if(!is.null(input$choices)){
            if(input$choices == CHOICE_CONF){
                data <- getJHUCSSEDataset(UrlStrConf)
            } else {
                data <- getJHUCSSEDataset(UrlStrDead)
            }
            return(data)
        }
    })

    ## Derive date range
    stdDataset <- getJHUCSSEDataset(UrlStrConf);
    daysStr <- names(stdDataset%>%select(contains("/")))
    daysDate <- as.Date(daysStr, "%m/%d/%y")
    daysDate <- daysDate[!is.na(daysDate)]

    ## Set World Map Slider
    output$WorldMapSlider <- renderUI({
        if(is.null(input$variable)){
        } else {
            if(input$variable %in% c("Total confirmed cases", "Total confirmed cases/Population")) {
                sliderInput(
                    "day1", "Day", min(daysDate), max(daysDate), 
                    value = c(max(daysDate)), animate = T, step = 1
                )
            } else {
                sliderInput(
                    "day2", "Day", min(daysDate), max(daysDate),
                    value = c(max(daysDate)-7, max(daysDate)), animate = T, step = 1
                )
            }
        }
    })
    
    ## Set World Map Selection
    output$WorldMapSelection <- renderUI({
        if(input$choices == CHOICE_CONF){
            radioButtons(
                "variable",
                choices = c(
                    "New confirmed cases over period",
                    "New confirmed cases over period/Population",
                    "Total confirmed cases",
                    "Total confirmed cases/Population"),
                label = "Indicator")
        } else {
            radioButtons(
                "variable",
                choices = list(
                    "New dead cases over period" = "New confirmed cases over period",
                    "New dead cases over period/Population" = "New confirmed cases over period/Population",
                    "Total dead cases" = "Total confirmed cases",
                    "Total dead cases/Population" = "Total confirmed cases/Population"),
                label = "Indicator")
        }
    })

    #maxTotal <- reactive(max(dataPays()%>%select(-Pop)%>%select_if(is.numeric), na.rm = T))
    #maxTotalPrevalence <- reactive(max(dataPays()%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/dataPays()$Pop*100000), na.rm = T))
    #Top5<-reactive(unique(c(dataPays()$Pays[order(dataPays()[,dim(dataPays())[2]]%>%unlist(),decreasing = T)][1:5], "France")))

    #print(head(WorldMapShape, 2))
    #session$onSessionEnded(stopApp)
}
