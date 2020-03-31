library(DT)
library(dygraphs)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(viridis)
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

UrlStrConf = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
UrlStrDead = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
#UrlStrRecv = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
#UrlStrConfUSA = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
#UrlStrDeadUSA = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

RawDataConf <- getJHUCSSEDataset(UrlStrConf)
RawDataDead <- getJHUCSSEDataset(UrlStrDead)

arrondi <- function(x) 10^(ceiling(log10(x)))

getGeoMapSlider <- function(sliderVarName, sliderDateArr, geoMapType) {
    day1tag <- "wmday1"
    day2tag <- "wmday2"
    if (geoMapType == WORLD_GEOMAP) {
        day1tag <- "wmday1"
        day2tag <- "wmday2"
    } else if (geoMapType == CHN_GEOMAP){
        day1tag <- "chnmday1"
        day2tag <- "chnmday2"
    } else if (geoMapType == USA_GEOMAP){
        day1tag <- "usamday1"
        day2tag <- "usamday2"
    }

    if(is.null(sliderVarName) | is.null(sliderDateArr)) {
        return(sliderInput(
            day1tag, "Day", min = as.Date("01/01/2020", "%m/%d/%y"), max = as.Date("03/01/2020", "%m/%d/%y"),
            value = c(as.Date("03/01/2020", "%m/%d/%y")), animate = T, step = 1))
    }
    if(sliderVarName %in% c(
        VAR_TS_CNT_CONF_TTL,
        VAR_TS_RAT_CONF_TTL,
        VAR_TS_CNT_DEAD_TTL,
        VAR_TS_RAT_DEAD_TTL
        )) {
        return(sliderInput(
            day1tag, "Day", min(sliderDateArr), max(sliderDateArr),
            value = c(max(sliderDateArr)), animate = T, step = 1))
    } else {
        return(sliderInput(
            day2tag, "Day", min(sliderDateArr), max(sliderDateArr),
            value = c(max(sliderDateArr)-14, max(sliderDateArr)), animate = T, step = 1))
    }
}

getGeoMapSelection <- function(selVarName, geoMapType) {
    var1 <- VAR_TS_CNT_CONF_NEW
    var2 <- VAR_TS_RAT_CONF_NEW
    var3 <- VAR_TS_CNT_CONF_TTL
    var4 <- VAR_TS_RAT_CONF_TTL

    vartag <- "wmvar"
    if(is.null(selVarName) | is.null(geoMapType)) {
        return(radioButtons(
            vartag,
            choices = c(
                var1,
                var2,
                var3,
                var4),
            label = "Indicator"))
    }
    if(geoMapType == WORLD_GEOMAP) {
        vartag <- "wmvar"
        if(selVarName == CHOICE_CONF){
            return(radioButtons(
                vartag,
                choices = c(
                    var1,
                    var2,
                    var3,
                    var4),
                label = "Indicator"))
        } else if(selVarName == CHOICE_DEAD) {
            return(radioButtons(
                vartag,
                choices = list(
                    var1 <- VAR_TS_CNT_DEAD_NEW,
                    var2 <- VAR_TS_RAT_DEAD_NEW,
                    var3 <- VAR_TS_CNT_DEAD_TTL,
                    var4 <- VAR_TS_RAT_DEAD_TTL),
                label = "Indicator"))
        }
    } else {
        if (geoMapType == CHN_GEOMAP) {
            vartag <- "chnmvar"
        } else {
            vartag <- "usamvar"
        }
        if(selVarName == CHOICE_CONF){
            return(radioButtons(
                vartag,
                choices = c(
                    var1,
                    var3),
                label = "Indicator"))
        } else if(selVarName == CHOICE_DEAD) {
            return(radioButtons(
                vartag,
                choices = list(
                    var1 <- VAR_TS_CNT_DEAD_NEW,
                    var3 <- VAR_TS_CNT_DEAD_TTL),
                label = "Indicator"))
        }
    }
}

getLegendMax <- function(mapArea, legVarName, geoMapType) {
    if(geoMapType == WORLD_GEOMAP) {
        if(legVarName %in% c(
            VAR_TS_CNT_CONF_NEW,
            VAR_TS_CNT_CONF_TTL,
            VAR_TS_CNT_DEAD_NEW,
            VAR_TS_CNT_DEAD_TTL
        )) {
            return(
                max(
                    mapArea%>%select(-Pop)%>%select_if(is.numeric),
                    na.rm = T
                )
            )
        } else {
            return(
                max(
                    mapArea%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/mapArea$Pop*1000000),
                    na.rm = T
                )
            )
        }
    } else {
        return(
            max(
                mapArea%>%select_if(is.numeric),
                na.rm = T
            )
        )
    }
}

getLegendPal <- function(maxVal) {
    return(
        colorNumeric(
            c(
                "#FFFFFFFF",
                rev(inferno(256))
            ),
            domain = c(0,log(arrondi(maxVal)))
        )
    )
}

getCountryPopup <- function(popupCtyName, popupVarName, popupNum) {
    resCountryPopup <- paste0(
        "<strong>Country/Region: </strong>",
        popupCtyName,
        "<br><strong>",
        popupVarName,
        ": </strong>",
        popupNum)

    if(popupVarName %in% c(VAR_TS_RAT_CONF_NEW, VAR_TS_RAT_CONF_TTL, VAR_TS_RAT_DEAD_NEW, VAR_TS_RAT_DEAD_TTL)) {
        resCountryPopup <- paste0(
            resCountryPopup,
            " /1000000")
    }

    return(resCountryPopup)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Table Table View
    worldTableData <- transformToWorldTableDataset(RawDataConf, RawDataDead)
    output[[WORLD_TABLE_HTML_TAG]] <- DT::renderDataTable({
        DT::datatable(worldTableData)
    })

    # Table Time Series
    ## World TS
    worldTSDataConf <- transformToWorldTSDataset(RawDataConf)
    worldTSDataDead <- transformToWorldTSDataset(RawDataDead)
    worldDefCountryList <- c("US", "Italy", "Spain", "China", "Germany", "France")
    output$WorldTSSelection <- renderUI({
        prettyCheckboxGroup(
            inputId = "wtssel",
            label = "Country/Region to select:",
            choices = names(worldTSDataConf),
            selected = worldDefCountryList,
            shape = "round", status = "info",
            fill = TRUE, inline = TRUE)
    })

    observe({
        output[[WORLD_TS_CONF_HTML_TAG]] <- renderDygraph({
            dygraph(
                worldTSDataConf[, input$wtssel]) %>%
            dyOptions(stackedGraph = FALSE) %>%
            dyRangeSelector(height = 50)
        })

        output[[WORLD_TS_DEAD_HTML_TAG]] <- renderDygraph({
            dygraph(
                worldTSDataDead[, input$wtssel]) %>%
            dyOptions(stackedGraph = FALSE) %>%
            dyRangeSelector(height = 50)
        })
    })

    # Table GeoMap
    ## Preparation for all GeoMaps
    ### Derive date range
    stdDataset <- RawDataConf
    daysStr <- names(stdDataset%>%select(contains("/")))
    daysDate <- as.Date(daysStr, "%m/%d/%y")
    daysDate <- daysDate[!is.na(daysDate)]

    ## WorldMap
    output$WorldMap <- renderLeaflet({
        leaflet(data = WorldMapShape) %>%
        setView(0, 30, zoom = 3)
    })
    
    ### Get World Map dataset
    worldMapArea <- reactive({
        print("World Map Calculation...")
        if(!is.null(input$wmcs)) {
            if(input$wmcs == CHOICE_CONF) {
                resData <- transformToWorldGeoMapDataset(RawDataConf, WorldMapShape)
            } else if(input$wmcs == CHOICE_DEAD) {
                resData <- transformToWorldGeoMapDataset(RawDataDead, WorldMapShape)
            }
            return(resData)
        }
    })

    ### Set World Map Slider
    worldMapSlider <- reactive(
        getGeoMapSlider(input$wmvar, daysDate, WORLD_GEOMAP)
    )
    output$WorldMapSlider <- renderUI(worldMapSlider())

    ### Set World Map Selection
    worldMapSelection <- reactive(
        getGeoMapSelection(input$wmcs, WORLD_GEOMAP)
    )
    output$WorldMapSelection <- renderUI(worldMapSelection())

    ### Set WorldMapLegend
    maxCNT <- reactive(getLegendMax(worldMapArea(), VAR_TS_CNT_CONF_NEW, WORLD_GEOMAP))
    maxRAT <- reactive(getLegendMax(worldMapArea(), VAR_TS_RAT_CONF_NEW, WORLD_GEOMAP))
    palCNT <- reactive(getLegendPal(maxCNT()))
    palRAT <- reactive(getLegendPal(maxRAT()))

    observe({
        if(is.null(input$wmvar)){
        } else {
            proxy <- leafletProxy("WorldMap", data = WorldMapShape)
            proxy %>% clearControls()
            if (input$WorldMapLegend) {
                if(input$wmvar %in% c(VAR_TS_RAT_CONF_NEW, VAR_TS_RAT_CONF_TTL, VAR_TS_RAT_DEAD_NEW, VAR_TS_RAT_DEAD_TTL)) {
                    proxy %>% leaflet::addLegend(
                        position = "bottomright",
                        pal = palRAT(),
                        opacity = 1,
                        bins = log(10^(seq(0, log10(arrondi(maxRAT())), 0.5))),
                        value = log(1:10^(log10(arrondi(maxRAT())))),
                        data = log(1:10^(log10(arrondi(maxRAT())))),
                        labFormat = labelFormat(transform = function(x) round(exp(x)), suffix = " /1000000")
                    )
                } else {
                    proxy %>% leaflet::addLegend(
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

    ### Set Color on World Map
    observe({
        if (!is.null(input$wmday1)) {
            indicator1 <- format.Date(input$wmday1, "%m/%d/%y")
        } else {
            indicator1 = format.Date(max(daysDate), "%m/%d/%y")
        }
        if (!is.null(input$wmday2)) {
            indicator2 <- format.Date(input$wmday2-c(1,0), "%m/%d/%y")
        } else {
            indicator2 = format.Date(c(min(daysDate)-1, max(daysDate)), "%m/%d/%y")
        }
        
        if(is.null(input$wmvar)){
        } else {
            if(input$wmvar %in% c(VAR_TS_RAT_CONF_TTL, VAR_TS_RAT_DEAD_TTL)) {
                WorldMapShapeOut <- merge(
                    WorldMapShape,
                    worldMapArea(),
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    WorldMapShapeOut$NAME,
                    input$wmvar,
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
            } else if(input$wmvar %in% c(VAR_TS_CNT_CONF_TTL, VAR_TS_CNT_DEAD_TTL)) {
                WorldMapShapeOut <- merge(
                    WorldMapShape,
                    worldMapArea(),
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    WorldMapShapeOut$NAME,
                    input$wmvar,
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
            } else if(input$wmvar %in% c(VAR_TS_CNT_CONF_NEW, VAR_TS_CNT_DEAD_NEW)) {
                worldMapAreaSel <- worldMapArea() %>% select(Area, Pop)
                if(indicator2[1] == format.Date(min(daysDate)-1, "%m/%d/%y")) {
                    worldMapAreaSel$CALCNUM <- worldMapArea()[, indicator2[2]]
                } else {
                    worldMapAreaSel$CALCNUM <- worldMapArea()[, indicator2[2]] - worldMapArea()[, indicator2[1]]
                }
                WorldMapShapeOut <- merge(
                    WorldMapShape,
                    worldMapAreaSel,
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    WorldMapShapeOut$NAME,
                    input$wmvar,
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
                worldMapAreaSel <- worldMapArea() %>% select(Area, Pop)
                if(indicator2[1] == format.Date(min(daysDate)-1, "%m/%d/%y")) {
                    worldMapAreaSel$CALCNUM <- worldMapArea()[, indicator2[2]]
                } else {
                    worldMapAreaSel$CALCNUM <- worldMapArea()[, indicator2[2]] - worldMapArea()[, indicator2[1]]
                }
                worldMapAreaSel$CALCNUM <- round(worldMapAreaSel$CALCNUM/worldMapAreaSel$Pop*1000000, 2)
                WorldMapShapeOut <- merge(
                    WorldMapShape,
                    worldMapAreaSel,
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    WorldMapShapeOut$NAME,
                    input$wmvar,
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

    ## CHNMap
    ### CHNMap frame with init coordinate
    output$CHNMap <- renderLeaflet({
        leaflet(data = CHNMapShape) %>%
        setView(105, 35, zoom = 4)
    })
    
    ### Get CHN Map dataset
    chnMapArea <- reactive({
        print("CHN Map Calculation...")
        if(!is.null(input$chnmcs)) {
            if(input$chnmcs == CHOICE_CONF) {
                resData <- transformToCHNGeoMapDataset(RawDataConf, CHNMapShape)
            } else if(input$chnmcs == CHOICE_DEAD) {
                resData <- transformToCHNGeoMapDataset(RawDataDead, CHNMapShape)
            }
            return(resData)
        }
    })

    ### Set CHN Slider
    chnMapSlider <- reactive(
        getGeoMapSlider(input$chnmvar, daysDate, CHN_GEOMAP)
    )
    output$CHNMapSlider <- renderUI(chnMapSlider())

    ### Set CHN Map Selection
    chnMapSelection <- reactive(
        getGeoMapSelection(input$chnmcs, CHN_GEOMAP)
    )
    output$CHNMapSelection <- renderUI(chnMapSelection())

    ### Set CHNMapLegend
    maxCHNMapCNT <- reactive(getLegendMax(chnMapArea(), VAR_TS_CNT_CONF_NEW, CHN_GEOMAP))
    palCHNMapCNT <- reactive(getLegendPal(maxCHNMapCNT()))

    observe({
        if(is.null(input$chnmvar)){
        } else {
            proxy <- leafletProxy("CHNMap", data = CHNMapShape)
            proxy %>% clearControls()
            if (input$CHNMapLegend) {
                proxy %>% leaflet::addLegend(
                    position = "bottomright",
                    pal = palCHNMapCNT(),
                    opacity = 1,
                    bins = log(10^(0:log10(arrondi(maxCHNMapCNT())))),
                    value = log(1:10^(log10(arrondi(maxCHNMapCNT())))),
                    data = log(10^(0:log10(arrondi(maxCHNMapCNT())))),
                    labFormat = labelFormat(transform = exp)
                )
            }
        }
    })

    ### Set Color on CHN Map
    observe({
        if (!is.null(input$chnmday1)) {
            indicator1 <- format.Date(input$chnmday1, "%m/%d/%y")
        } else {
            indicator1 = format.Date(max(daysDate), "%m/%d/%y")
        }
        if (!is.null(input$chnmday2)) {
            indicator2 <- format.Date(input$chnmday2-c(1,0), "%m/%d/%y")
        } else {
            indicator2 = format.Date(c(min(daysDate)-1, max(daysDate)), "%m/%d/%y")
        }

        if(is.null(input$chnmvar)) {
        } else {
            if(input$chnmvar %in% c(VAR_TS_CNT_CONF_NEW, VAR_TS_CNT_DEAD_NEW)) {
                chnMapAreaSel <- chnMapArea() %>% select(Area)
                if(indicator2[1] == format.Date(min(daysDate)-1, "%m/%d/%y")) {
                    chnMapAreaSel$CALCNUM <- chnMapArea()[, indicator2[2]]
                } else {
                    chnMapAreaSel$CALCNUM <- chnMapArea()[, indicator2[2]] - chnMapArea()[, indicator2[1]]
                }
                CHNMapShapeOut <- merge(
                    CHNMapShape,
                    chnMapAreaSel,
                    by.x = "NAME_1",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    CHNMapShapeOut$`NAME_1`,
                    input$chnmvar,
                    CHNMapShapeOut$CALCNUM
                )
                leafletProxy("CHNMap", data = CHNMapShapeOut) %>%
                addPolygons(
                    fillColor = palCHNMapCNT()(log(CHNMapShapeOut$CALCNUM+1)),
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    layerId = ~`NAME_1`,
                    weight = 1,
                    popup = countryPopup)
            } else {
                CHNMapShapeOut <- merge(
                    CHNMapShape,
                    chnMapArea(),
                    by.x = "NAME_1",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    CHNMapShapeOut$`NAME_1`,
                    input$chnmvar,
                    round(CHNMapShapeOut[[indicator1]], 2)
                )
                leafletProxy("CHNMap", data = CHNMapShapeOut) %>%
                addPolygons(
                    fillColor = palCHNMapCNT()(log((CHNMapShapeOut[[indicator1]])+1)),
                    fillOpacity = 1,
                    layerId = ~`NAME_1`,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = countryPopup)
            }
        }
    })

    ## USAMap
    output$USAMap <- renderLeaflet({
        leaflet(data = USAMapShape) %>%
        setView(0, 30, zoom = 3)
    })
    
    ### Get USA Map dataset
    usaMapArea <- reactive({
        print("USA Map Calculation...")
        if(!is.null(input$usamcs)) {
            if(input$usamcs == CHOICE_CONF) {
                resData <- transformToUSAGeoMapDataset(RawDataConf, USAMapShape)
            } else if(input$usamcs == CHOICE_DEAD) {
                resData <- transformToUSAGeoMapDataset(RawDataDead, USAMapShape)
            }
            return(resData)
        }
    })

    ### Set USA Slider
    usaMapSlider <- reactive(
        getGeoMapSlider(input$usamvar, daysDate, USA_GEOMAP)
    )
    output$USAMapSlider <- renderUI(usaMapSlider())

    ### Set USA Map Selection
    usaMapSelection <- reactive(
        getGeoMapSelection(input$usamcs, USA_GEOMAP)
    )
    output$USAMapSelection <- renderUI(usaMapSelection())

    #session$onSessionEnded(stopApp)
}
