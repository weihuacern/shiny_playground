library(viridis)
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

## Load data into memory first, save network IO
UrlStrConf = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
UrlStrDead = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
UrlStrRecv = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

RawDataConf <- getJHUCSSEDataset(UrlStrConf)
RawDataDead <- getJHUCSSEDataset(UrlStrDead)
RawDataRecv <- getJHUCSSEDataset(UrlStrRecv)

arrondi <- function(x) 10^(ceiling(log10(x)))

getCountryPopup <- function(popupCtyName, popVarName, popupNum) {        
    resCountryPopup <- paste0(
        "<strong>Country/Region: </strong>",
        popupCtyName,
        "<br><strong>",
        popVarName,
        ": </strong>",
        popupNum)

    if(popVarName %in% c(VAR_TS_RAT_CONF_NEW, VAR_TS_RAT_CONF_TTL, VAR_TS_RAT_DEAD_NEW, VAR_TS_RAT_DEAD_TTL)) {
        resCountryPopup <- paste0(
            resCountryPopup,
            " /1000000")
    }

    return(resCountryPopup)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Table GeoMap: WorldMap
    output$WorldMap <- renderLeaflet({
        leaflet(data = WorldMapShape) %>%
        setView(0, 30, zoom = 3)
    })
    
    ## Get dataset
    dataArea <- reactive({
        if(!is.null(input$choices)) {
            if(input$choices == CHOICE_CONF) {
                resData <- transformToGeoMapDataset(RawDataConf, WorldMapShape)
            } else {
                resData <- transformToGeoMapDataset(RawDataDead, WorldMapShape)
            }
            #head(resData, 2)
            return(resData)
        }
    })

    ## Derive date range
    stdDataset <- RawDataConf
    daysStr <- names(stdDataset%>%select(contains("/")))
    daysDate <- as.Date(daysStr, "%m/%d/%y")
    daysDate <- daysDate[!is.na(daysDate)]

    ## Set World Map Slider
    output$WorldMapSlider <- renderUI({
        if(is.null(input$variable)) {
        } else {
            if(input$variable %in% c(VAR_TS_CNT_CONF_TTL, VAR_TS_RAT_CONF_TTL, VAR_TS_CNT_DEAD_TTL, VAR_TS_RAT_DEAD_TTL)) {
                sliderInput(
                    "day1", "Day", min(daysDate), max(daysDate), 
                    value = c(max(daysDate)), animate = T, step = 1)
            } else {
                sliderInput(
                    "day2", "Day", min(daysDate), max(daysDate),
                    value = c(max(daysDate)-14, max(daysDate)), animate = T, step = 1)
            }
        }
    })

    ## Set World Map Selection
    var1 <- VAR_TS_CNT_CONF_NEW
    var2 <- VAR_TS_RAT_CONF_NEW
    var3 <- VAR_TS_CNT_CONF_TTL
    var4 <- VAR_TS_RAT_CONF_TTL
    output$WorldMapSelection <- renderUI({
        if(input$choices == CHOICE_CONF){
            radioButtons(
                "variable",
                choices = c(
                    var1,
                    var2,
                    var3,
                    var4),
                label = "Indicator")
        } else {
            radioButtons(
                "variable",
                choices = list(
                    var1 <- VAR_TS_CNT_DEAD_NEW,
                    var2 <- VAR_TS_RAT_DEAD_NEW,
                    var3 <- VAR_TS_CNT_DEAD_TTL,
                    var4 <- VAR_TS_RAT_DEAD_TTL),
                label = "Indicator")
        }
    })

    ## Set WorldMapLegend
    maxCNT <- reactive(max(dataArea()%>%select(-Pop)%>%select_if(is.numeric), na.rm = T))
    maxRAT <- reactive(max(dataArea()%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/dataArea()$Pop*1000000), na.rm = T))
    palCNT <- reactive(colorNumeric(c("#FFFFFFFF", rev(inferno(256))), domain = c(0,log(arrondi(maxCNT())))))
    palRAT <- reactive(colorNumeric(c("#FFFFFFFF", rev(inferno(256))), domain = c(0,log(arrondi(maxRAT())))))

    observe({
        if(is.null(input$variable)){
        } else {
            proxy <- leafletProxy("WorldMap", data = WorldMapShape)
            proxy %>% clearControls()
            if (input$WorldMapLegend) {
                if(input$variable %in% c(VAR_TS_RAT_CONF_NEW, VAR_TS_RAT_CONF_TTL, VAR_TS_RAT_DEAD_NEW, VAR_TS_RAT_DEAD_TTL)) {
                    proxy %>% addLegend(
                        position = "bottomright",
                        pal = palRAT(),
                        opacity = 1,
                        bins = log(10^(seq(0, log10(arrondi(maxRAT())), 0.5))),
                        value = log(1:10^(log10(arrondi(maxRAT())))),
                        data = log(1:10^(log10(arrondi(maxRAT())))),
                        labFormat = labelFormat(transform = function(x) round(exp(x)), suffix = " /1000000")
                    )
                } else {
                    proxy %>% addLegend(
                        position = "bottomright",
                        pal = palCNT(),
                        opacity = 1,
                        bins = log(10^(0:log10(arrondi(maxCNT())))),
                        value = log(1:10^(log10(arrondi(maxCNT())))),
                        data = log(10^(0:log10(arrondi(maxCNT())))),
                        labFormat = labelFormat(transform = exp)
                    )
                }
            }
        }
    })

    ## Set Color on World Map
    observe({
        if (!is.null(input$day1)) {
            indicator1 <- format.Date(input$day1, "%m/%d/%y")
        } else {
            indicator1 = format.Date(max(daysDate), "%m/%d/%y")
        }
        if (!is.null(input$day2)) {
            indicator2 <- format.Date(input$day2-c(1,0), "%m/%d/%y")
        } else {
            indicator2 = format.Date(c(min(daysDate)-1, max(daysDate)), "%m/%d/%y")
        }
        
        if(is.null(input$variable)){
        } else {
            if(input$variable %in% c(VAR_TS_RAT_CONF_TTL, VAR_TS_RAT_DEAD_TTL)) {
                WorldMapShapeOut <- merge(
                    WorldMapShape,
                    dataArea(),
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    WorldMapShapeOut$NAME,
                    input$variable,
                    round(WorldMapShapeOut[[indicator1]]/WorldMapShapeOut$Pop*1000000,2)
                )
                leafletProxy("WorldMap", data = WorldMapShapeOut) %>%
                addPolygons(
                    fillColor = palRAT()(log((WorldMapShapeOut[[indicator1]]/WorldMapShapeOut$Pop*1000000)+1)),
                    layerId = ~NAME,
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = countryPopup)
            } else if(input$variable %in% c(VAR_TS_CNT_CONF_TTL, VAR_TS_CNT_DEAD_TTL)) {
                WorldMapShapeOut <- merge(
                    WorldMapShape,
                    dataArea(),
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    WorldMapShapeOut$NAME,
                    input$variable,
                    round(WorldMapShapeOut[[indicator1]], 2)
                )
                leafletProxy("WorldMap", data = WorldMapShapeOut) %>%
                addPolygons(
                    fillColor = palCNT()(log((WorldMapShapeOut[[indicator1]])+1)),
                    fillOpacity = 1,
                    layerId = ~NAME,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = countryPopup)
            } else if(input$variable %in% c(VAR_TS_CNT_CONF_NEW, VAR_TS_CNT_DEAD_NEW)) {
                dataAreaSel <- dataArea() %>% select(Area, Pop)
                if(indicator2[1] == format.Date(min(daysDate)-1, "%m/%d/%y")) {
                    dataAreaSel$CALCNUM <- dataArea()[, indicator2[2]]
                } else {
                    dataAreaSel$CALCNUM <- dataArea()[, indicator2[2]] - dataArea()[, indicator2[1]]
                }
                WorldMapShapeOut <- merge(
                    WorldMapShape,
                    dataAreaSel,
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    WorldMapShapeOut$NAME,
                    input$variable,
                    WorldMapShapeOut$CALCNUM
                )
                leafletProxy("WorldMap", data = WorldMapShapeOut) %>%
                addPolygons(
                    fillColor = palCNT()(log(WorldMapShapeOut$CALCNUM+1)),
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    layerId = ~NAME,
                    weight = 1,
                    popup = countryPopup)
            } else {
                dataAreaSel <- dataArea() %>% select(Area, Pop)
                if(indicator2[1] == format.Date(min(daysDate)-1, "%m/%d/%y")) {
                    dataAreaSel$CALCNUM <- dataArea()[, indicator2[2]]
                } else {
                    dataAreaSel$CALCNUM <- dataArea()[, indicator2[2]] - dataArea()[, indicator2[1]]
                }
                dataAreaSel$CALCNUM <- round(dataAreaSel$CALCNUM/dataAreaSel$Pop*1000000, 2)
                WorldMapShapeOut <- merge(
                    WorldMapShape,
                    dataAreaSel,
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    WorldMapShapeOut$NAME,
                    input$variable,
                    WorldMapShapeOut$CALCNUM
                )
                leafletProxy("WorldMap", data = WorldMapShapeOut) %>%
                addPolygons(
                    fillColor = palRAT()(log(WorldMapShapeOut$CALCNUM/WorldMapShapeOut$Pop*1000000+1)),
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    layerId = ~NAME,
                    weight = 1,
                    popup = countryPopup)
            }
        }
    })

    #Top5<-reactive(unique(c(dataArea()$Area[order(dataArea()[,dim(dataArea())[2]]%>%unlist(),decreasing = T)][1:5], "France")))
    #print(head(WorldMapShape, 2))

    # Table Time Series
    statType = "pois"
    output[[statType]] <- renderStatPlot(statType, input)
    
    statType = "norm"
    output[[statType]] <- renderStatPlot(statType, input)
    #session$onSessionEnded(stopApp)
}
