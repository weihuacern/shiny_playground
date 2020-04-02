library(DT)
library(dygraphs)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(viridis)
library(tidyverse)

source("constants.R")
source("utils.R")

## Load data into memory first, save network IO
urlStrConf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
urlStrDead <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
#urlStrRecv <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
#urlStrConfUSA <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
#urlStrDeadUSA <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

rawDataConf <- getJHUCSSEDataset(urlStrConf)
rawDataDead <- getJHUCSSEDataset(urlStrDead)

arrondi <- function(x) 10^ (ceiling(log10(x)))

getGeoMapSlider <- function(sliderVarName, sliderDateArr, geoMapType) {
    day1tag <- "wmday1"
    day2tag <- "wmday2"
    if (geoMapType == constTypeWorldGeoMap) {
        day1tag <- "wmday1"
        day2tag <- "wmday2"
    } else if (geoMapType == constTypeCHNGeoMap) {
        day1tag <- "chnmday1"
        day2tag <- "chnmday2"
    } else if (geoMapType == constTypeUSAGeoMap) {
        day1tag <- "usamday1"
        day2tag <- "usamday2"
    }

    if (is.null(sliderVarName) | is.null(sliderDateArr)) {
        return(sliderInput(
            day1tag, "Day", min = as.Date("01/01/2020", "%m/%d/%y"), max = as.Date("03/01/2020", "%m/%d/%y"),
            value = c(as.Date("03/01/2020", "%m/%d/%y")), animate = T, step = 1))
    }

    if (sliderVarName %in% c(
        constVarTSCntConfTtl,
        constVarTSRatConfTtl,
        constVarTSCntDeadTtl,
        constVarTSRatDeadTtl
        )) {
        return(sliderInput(
            day1tag, "Day", min(sliderDateArr), max(sliderDateArr),
            value = c(max(sliderDateArr)), animate = T, step = 1))
    } else {
        return(sliderInput(
            day2tag, "Day", min(sliderDateArr), max(sliderDateArr),
            value = c(max(sliderDateArr) - 14, max(sliderDateArr)), animate = T, step = 1))
    }
}

getGeoMapSelection <- function(selVarName, geoMapType) {
    var1 <- constVarTSCntConfNew
    var2 <- constVarTSRatConfNew
    var3 <- constVarTSCntConfTtl
    var4 <- constVarTSRatConfTtl

    vartag <- "wmvar"
    if (is.null(selVarName) | is.null(geoMapType)) {
        return(radioButtons(
            vartag,
            choices = c(
                var1,
                var2,
                var3,
                var4),
            label = "Indicator"))
    }
    if (geoMapType == constTypeWorldGeoMap) {
        vartag <- "wmvar"
        if (selVarName == constChoiceConf) {
            return(radioButtons(
                vartag,
                choices = c(
                    var1,
                    var2,
                    var3,
                    var4),
                label = "Indicator"))
        } else if (selVarName == constChoiceDead) {
            return(radioButtons(
                vartag,
                choices = list(
                    var1 <- constVarTSCntDeadNew,
                    var2 <- constVarTSRatDeadNew,
                    var3 <- constVarTSCntDeadTtl,
                    var4 <- constVarTSRatDeadTtl),
                label = "Indicator"))
        }
    } else {
        if (geoMapType == constTypeCHNGeoMap) {
            vartag <- "chnmvar"
        } else {
            vartag <- "usamvar"
        }
        if (selVarName == constChoiceConf) {
            return(radioButtons(
                vartag,
                choices = c(
                    var1,
                    var3),
                label = "Indicator"))
        } else if (selVarName == constChoiceDead) {
            return(radioButtons(
                vartag,
                choices = list(
                    var1 <- constVarTSCntDeadNew,
                    var3 <- constVarTSCntDeadTtl),
                label = "Indicator"))
        }
    }
}

getLegendMax <- function(mapArea, legVarName, geoMapType) {
    if (geoMapType == constTypeWorldGeoMap) {
        if (legVarName %in% c(
            constVarTSCntConfNew,
            constVarTSCntConfTtl,
            constVarTSCntDeadNew,
            constVarTSCntDeadTtl
        )) {
            return(
                max(
                    mapArea %>% select(-Pop) %>% select_if(is.numeric),
                    na.rm = T
                )
            )
        } else {
            return(
                max(
                    mapArea %>%
                    select(-Pop) %>%
                    select_if(is.numeric) %>%
                    mutate_all(function(x) x / mapArea$Pop * 1000000),
                    na.rm = T
                )
            )
        }
    } else {
        return(
            max(
                mapArea %>% select_if(is.numeric),
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
            domain = c(0, log(arrondi(maxVal)))
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

    if (popupVarName %in% c(constVarTSRatConfNew, constVarTSRatConfTtl, constVarTSRatDeadNew, constVarTSRatDeadTtl)) {
        resCountryPopup <- paste0(
            resCountryPopup,
            " /1000000")
    }

    return(resCountryPopup)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Table Table View
    worldTableData <- transformToWorldTableDataset(rawDataConf, rawDataDead)
    output[[constIDTableWorld]] <- DT::renderDataTable({
        DT::datatable(worldTableData)
    })

    # Table Time Series
    ## World TS
    worldTSDataConf <- transformToWorldTSDataset(rawDataConf)
    worldTSDataDead <- transformToWorldTSDataset(rawDataDead)
    worldDefCountryList <- c("US", "Italy", "Spain", "China", "Germany", "France")
    output$WorldTSSelection <- shiny::renderUI({
        prettyCheckboxGroup(
            inputId = "wtssel",
            label = "Country/Region to select:",
            choices = names(worldTSDataConf),
            selected = worldDefCountryList,
            shape = "round", status = "info",
            fill = TRUE, inline = TRUE)
    })

    observe({
        output[[constIDTSWorldConf]] <- renderDygraph({
            dygraph(
                worldTSDataConf[, input$wtssel]) %>%
            dyOptions(stackedGraph = FALSE) %>%
            dyRangeSelector(height = 50)
        })

        output[[constIDTSWorldDead]] <- renderDygraph({
            dygraph(
                worldTSDataDead[, input$wtssel]) %>%
            dyOptions(stackedGraph = FALSE) %>%
            dyRangeSelector(height = 50)
        })
    })

    # Table GeoMap
    ## Preparation for all GeoMaps
    ### Derive date range
    stdDataset <- rawDataConf
    daysStr <- names(stdDataset %>% dplyr::select(contains("/")))
    daysDate <- as.Date(daysStr, "%m/%d/%y")
    daysDate <- daysDate[!is.na(daysDate)]

    ## WorldMap
    output$WorldMap <- renderLeaflet({
        leaflet(data = mapShapeWorld) %>%
        setView(0, 30, zoom = 3)
    })

    ### Get World Map dataset
    worldMapArea <- reactive({
        print("World Map Calculation...")
        if (!is.null(input$wmcs)) {
            if (input$wmcs == constChoiceConf) {
                resData <- transformToWorldGeoMapDataset(rawDataConf, mapShapeWorld)
            } else if (input$wmcs == constChoiceDead) {
                resData <- transformToWorldGeoMapDataset(rawDataDead, mapShapeWorld)
            }
            return(resData)
        }
    })

    ### Set World Map Slider
    worldMapSlider <- reactive(
        getGeoMapSlider(input$wmvar, daysDate, constTypeWorldGeoMap)
    )
    output$WorldMapSlider <- shiny::renderUI(worldMapSlider())

    ### Set World Map Selection
    worldMapSelection <- reactive(
        getGeoMapSelection(input$wmcs, constTypeWorldGeoMap)
    )
    output$WorldMapSelection <- shiny::renderUI(worldMapSelection())

    ### Set WorldMapLegend
    maxCNT <- shiny::reactive(getLegendMax(worldMapArea(), constVarTSCntConfNew, constTypeWorldGeoMap))
    maxRAT <- shiny::reactive(getLegendMax(worldMapArea(), constVarTSRatConfNew, constTypeWorldGeoMap))
    palCNT <- shiny::reactive(getLegendPal(maxCNT()))
    palRAT <- shiny::reactive(getLegendPal(maxRAT()))

    observe({
        if (is.null(input$wmvar)) {
        } else {
            proxy <- leaflet::leafletProxy("WorldMap", data = mapShapeWorld)
            proxy %>% leaflet::clearControls()
            if (input$WorldMapLegend) {
                if (input$wmvar %in% c(
                    constVarTSRatConfNew,
                    constVarTSRatConfTtl,
                    constVarTSRatDeadNew,
                    constVarTSRatDeadTtl)) {
                    proxy %>% leaflet::addLegend(
                        position = "bottomright",
                        pal = palRAT(),
                        opacity = 1,
                        bins = log(10^ (seq(0, log10(arrondi(maxRAT())), 0.5))),
                        value = log(1:10^ (log10(arrondi(maxRAT())))),
                        data = log(1:10^ (log10(arrondi(maxRAT())))),
                        labFormat = labelFormat(transform = function(x) round(exp(x)), suffix = " /1000000")
                    )
                } else {
                    proxy %>% leaflet::addLegend(
                        position = "bottomright",
                        pal = palCNT(),
                        opacity = 1,
                        bins = log(10^ (0:log10(arrondi(maxCNT())))),
                        value = log(1:10^ (log10(arrondi(maxCNT())))),
                        data = log(10^ (0:log10(arrondi(maxCNT())))),
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
            indicator1 <- format.Date(max(daysDate), "%m/%d/%y")
        }
        if (!is.null(input$wmday2)) {
            indicator2 <- format.Date(input$wmday2 - c(1, 0), "%m/%d/%y")
        } else {
            indicator2 <- format.Date(c(min(daysDate) - 1, max(daysDate)), "%m/%d/%y")
        }

        if (is.null(input$wmvar)) {
        } else {
            if (input$wmvar %in% c(constVarTSRatConfTtl, constVarTSRatDeadTtl)) {
                mapShapeWorldOut <- merge(
                    mapShapeWorld,
                    worldMapArea(),
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    mapShapeWorldOut$NAME,
                    input$wmvar,
                    round(mapShapeWorldOut[[indicator1]] / mapShapeWorldOut$Pop * 1000000, 2)
                )
                leafletProxy("WorldMap", data = mapShapeWorldOut) %>%
                addPolygons(
                    fillColor = palRAT()(log((mapShapeWorldOut[[indicator1]] / mapShapeWorldOut$Pop * 1000000) + 1)),
                    layerId = ~NAME,
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = countryPopup)
            } else if (input$wmvar %in% c(constVarTSCntConfTtl, constVarTSCntDeadTtl)) {
                mapShapeWorldOut <- merge(
                    mapShapeWorld,
                    worldMapArea(),
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    mapShapeWorldOut$NAME,
                    input$wmvar,
                    round(mapShapeWorldOut[[indicator1]], 2)
                )
                leafletProxy("WorldMap", data = mapShapeWorldOut) %>%
                addPolygons(
                    fillColor = palCNT()(log((mapShapeWorldOut[[indicator1]]) + 1)),
                    fillOpacity = 1,
                    layerId = ~NAME,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = countryPopup)
            } else if (input$wmvar %in% c(constVarTSCntConfNew, constVarTSCntDeadNew)) {
                worldMapAreaSel <- worldMapArea() %>% dplyr::select(Area, Pop)
                if (indicator2[1] == format.Date(min(daysDate) - 1, "%m/%d/%y")) {
                    worldMapAreaSel$CALCNUM <- worldMapArea()[, indicator2[2]]
                } else {
                    worldMapAreaSel$CALCNUM <- worldMapArea()[, indicator2[2]] - worldMapArea()[, indicator2[1]]
                }
                mapShapeWorldOut <- merge(
                    mapShapeWorld,
                    worldMapAreaSel,
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    mapShapeWorldOut$NAME,
                    input$wmvar,
                    mapShapeWorldOut$CALCNUM
                )
                leafletProxy("WorldMap", data = mapShapeWorldOut) %>%
                addPolygons(
                    fillColor = palCNT()(log(mapShapeWorldOut$CALCNUM + 1)),
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    layerId = ~NAME,
                    weight = 1,
                    popup = countryPopup)
            } else {
                worldMapAreaSel <- worldMapArea() %>% dplyr::select(Area, Pop)
                if(indicator2[1] == format.Date(min(daysDate) - 1, "%m/%d/%y")) {
                    worldMapAreaSel$CALCNUM <- worldMapArea()[, indicator2[2]]
                } else {
                    worldMapAreaSel$CALCNUM <- worldMapArea()[, indicator2[2]] - worldMapArea()[, indicator2[1]]
                }
                worldMapAreaSel$CALCNUM <- round(worldMapAreaSel$CALCNUM / worldMapAreaSel$Pop * 1000000, 2)
                mapShapeWorldOut <- merge(
                    mapShapeWorld,
                    worldMapAreaSel,
                    by.x = "NAME",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    mapShapeWorldOut$NAME,
                    input$wmvar,
                    mapShapeWorldOut$CALCNUM
                )
                leafletProxy("WorldMap", data = mapShapeWorldOut) %>%
                addPolygons(
                    fillColor = palRAT()(log(mapShapeWorldOut$CALCNUM / mapShapeWorldOut$Pop * 1000000 + 1)),
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
        leaflet(data = mapShapeCHN) %>%
        setView(105, 35, zoom = 4)
    })

    ### Get CHN Map dataset
    chnMapArea <- reactive({
        print("CHN Map Calculation...")
        if (!is.null(input$chnmcs)) {
            if (input$chnmcs == constChoiceConf) {
                resData <- transformToCHNGeoMapDataset(rawDataConf, mapShapeCHN)
            } else if(input$chnmcs == constChoiceDead) {
                resData <- transformToCHNGeoMapDataset(rawDataDead, mapShapeCHN)
            }
            return(resData)
        }
    })

    ### Set CHN Slider
    chnMapSlider <- reactive(
        getGeoMapSlider(input$chnmvar, daysDate, constTypeCHNGeoMap)
    )
    output$CHNMapSlider <- shiny::renderUI(chnMapSlider())

    ### Set CHN Map Selection
    chnMapSelection <- reactive(
        getGeoMapSelection(input$chnmcs, constTypeCHNGeoMap)
    )
    output$CHNMapSelection <- shiny::renderUI(chnMapSelection())

    ### Set CHNMapLegend
    maxCHNMapCNT <- shiny::reactive(getLegendMax(chnMapArea(), constVarTSCntConfNew, constTypeCHNGeoMap))
    palCHNMapCNT <- shiny::reactive(getLegendPal(maxCHNMapCNT()))

    observe({
        if (is.null(input$chnmvar)) {
        } else {
            proxy <- leaflet::leafletProxy("CHNMap", data = mapShapeCHN)
            proxy %>% leaflet::clearControls()
            if (input$CHNMapLegend) {
                proxy %>% leaflet::addLegend(
                    position = "bottomright",
                    pal = palCHNMapCNT(),
                    opacity = 1,
                    bins = log(10^ (0:log10(arrondi(maxCHNMapCNT())))),
                    value = log(1:10^ (log10(arrondi(maxCHNMapCNT())))),
                    data = log(10^ (0:log10(arrondi(maxCHNMapCNT())))),
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
            indicator1 <- format.Date(max(daysDate), "%m/%d/%y")
        }
        if (!is.null(input$chnmday2)) {
            indicator2 <- format.Date(input$chnmday2 - c(1, 0), "%m/%d/%y")
        } else {
            indicator2 <- format.Date(c(min(daysDate) - 1, max(daysDate)), "%m/%d/%y")
        }

        if (is.null(input$chnmvar)) {
        } else {
            if (input$chnmvar %in% c(constVarTSCntConfNew, constVarTSCntDeadNew)) {
                chnMapAreaSel <- chnMapArea() %>% dplyr::select(Area)
                if (indicator2[1] == format.Date(min(daysDate) - 1, "%m/%d/%y")) {
                    chnMapAreaSel$CALCNUM <- chnMapArea()[, indicator2[2]]
                } else {
                    chnMapAreaSel$CALCNUM <- chnMapArea()[, indicator2[2]] - chnMapArea()[, indicator2[1]]
                }
                mapShapeCHNOut <- merge(
                    mapShapeCHN,
                    chnMapAreaSel,
                    by.x = "NAME_1",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    mapShapeCHNOut$`NAME_1`,
                    input$chnmvar,
                    mapShapeCHNOut$CALCNUM
                )
                leafletProxy("CHNMap", data = mapShapeCHNOut) %>%
                addPolygons(
                    fillColor = palCHNMapCNT()(log(mapShapeCHNOut$CALCNUM + 1)),
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    layerId = ~`NAME_1`,
                    weight = 1,
                    popup = countryPopup)
            } else {
                mapShapeCHNOut <- merge(
                    mapShapeCHN,
                    chnMapArea(),
                    by.x = "NAME_1",
                    by.y = "Area",
                    sort = FALSE)
                countryPopup <- getCountryPopup(
                    mapShapeCHNOut$`NAME_1`,
                    input$chnmvar,
                    round(mapShapeCHNOut[[indicator1]], 2)
                )
                leafletProxy("CHNMap", data = mapShapeCHNOut) %>%
                addPolygons(
                    fillColor = palCHNMapCNT()(log((mapShapeCHNOut[[indicator1]]) + 1)),
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
        leaflet(data = mapShapeUSA) %>%
        setView(0, 30, zoom = 3)
    })

    ### Get USA Map dataset
    usaMapArea <- reactive({
        print("USA Map Calculation...")
        if (!is.null(input$usamcs)) {
            if (input$usamcs == constChoiceConf) {
                resData <- transformToUSAGeoMapDataset(rawDataConf, mapShapeUSA)
            } else if(input$usamcs == constChoiceDead) {
                resData <- transformToUSAGeoMapDataset(rawDataDead, mapShapeUSA)
            }
            return(resData)
        }
    })

    ### Set USA Slider
    usaMapSlider <- reactive(
        getGeoMapSlider(input$usamvar, daysDate, constTypeUSAGeoMap)
    )
    output$USAMapSlider <- shiny::renderUI(usaMapSlider())

    ### Set USA Map Selection
    usaMapSelection <- reactive(
        getGeoMapSelection(input$usamcs, constTypeUSAGeoMap)
    )
    output$USAMapSelection <- shiny::renderUI(usaMapSelection())
}
