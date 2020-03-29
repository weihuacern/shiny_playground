library(RCurl)
library(reshape)
library(tidyverse)
library(xts)

getJHUCSSEDataset <- function(urlStr) {
    # https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv
    # https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv
    download <- getURL(urlStr)
    data <- read.csv(
        textConnection(download),
        check.names = F)
    return(data)
}

transformToWorldTableDataset <- function(JHUCSSEConfDf, JHUCSSEDeadDf) {
    names(JHUCSSEConfDf)[names(JHUCSSEConfDf) == "Country/Region"] <- "Area"
    names(JHUCSSEDeadDf)[names(JHUCSSEDeadDf) == "Country/Region"] <- "Area"
    # Latest date and second latest date
    latestDate <- (rev(names(JHUCSSEConfDf))[1])
    secondLatestDate <- (rev(names(JHUCSSEConfDf))[2])

    TableConfList <- JHUCSSEConfDf%>%dplyr::select(
        -`Province/State`,
        -Lat, -Long)%>%
        group_by(Area)%>%
        summarise_each(sum)%>%
        arrange(desc(!!sym(latestDate)))
    TableConfList$Area <- as.character(TableConfList$Area)
    ConfVars <- c(Area = "Area", ConfCnt = latestDate, TmpConfCnt = secondLatestDate)
    TableConfList <- select(TableConfList, !!ConfVars)
    TableConfList <- mutate(TableConfList, NewConfCnt = ConfCnt - TmpConfCnt)
    TableConfList$Area <- as.character(TableConfList$Area)

    TableDeadList <- JHUCSSEDeadDf%>%dplyr::select(
        -`Province/State`,
        -Lat, -Long)%>%group_by(Area)%>%summarise_each(sum)%>%arrange(desc(!!sym(latestDate)))
    TableDeadList$Area <- as.character(TableDeadList$Area)
    DeadVars <- c(Area = "Area", DeadCnt = latestDate, TmpDeadCnt = secondLatestDate)
    TableDeadList <- select(TableDeadList, !!DeadVars)
    TableDeadList <- mutate(TableDeadList, NewDeadCnt = DeadCnt - TmpDeadCnt)
    TableDeadList <- TableDeadList%>%dplyr::select(Area, DeadCnt, NewDeadCnt)
    TableDeadList$Area <- as.character(TableDeadList$Area)

    TableDf <- left_join(
        data.frame(Area = TableConfList$Area, ConfCnt = TableConfList$ConfCnt, NewConfCnt = TableConfList$NewConfCnt),
        TableDeadList
    )
    
    TableDf <- TableDf%>%dplyr::rename(
        "Confirmed Cases" = ConfCnt,
        "New Confirmed Cases" = NewConfCnt,
        "Dead Cases" = DeadCnt,
        "New Dead Cases" = NewDeadCnt
    )
    return(TableDf)
}

#df1 <- getJHUCSSEDataset("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#df2 <- getJHUCSSEDataset("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
#tabdata <- transformToWorldTableDataset(df1, df2)

transformToWorldTSDataset <- function(JHUCSSEDf) {
    # Rename
    names(JHUCSSEDf)[names(JHUCSSEDf) == "Country/Region"] <- "Area"

    TSDf <- JHUCSSEDf%>%dplyr::select(
        -`Province/State`,
        -Lat, -Long)%>%group_by(Area)%>%summarise_each(sum)
    TSDf$Area <- as.character(TSDf$Area)
    TSDf <- melt(as.data.frame(TSDf), id=c("Area"))
    names(TSDf)[names(TSDf) == "variable"] <- "Time"
    names(TSDf)[names(TSDf) == "value"] <- "Value"
    TSDf$Time <- as.Date(TSDf$Time, format = "%m/%d/%y")
    TSDf <- cast(TSDf, Time ~ Area, value = "Value")
    TSData <- xts(TSDf %>% select(-Time), order.by = TSDf$Time)
    return(TSData)
}

#df1 <- getJHUCSSEDataset("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#tsdata <- transformToWorldTSDataset(df1)
#print(names(tsdata))
#print(typeof(names(tsdata)))
#print(as.matrix(xts::last(tsdata)))

# Object: WorldMapShape
load("../data/WorldMapShape.RData")

transformToWorldGeoMapDataset <- function(JHUCSSEDf, WorldMapShapeDf) {
    # Align Country/Region Name to WorldMapShapedf$NAME
    JHUCSSEDf$`Country/Region` <- as.character(JHUCSSEDf$`Country/Region`)

    # Asia
    ## TODO, need to move to this to China category
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Taiwan*"] <- "Taiwan"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Korea, South"] <- "South Korea"
    #JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="The West Bank and Gaza"] <- ""
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Burma"] <- "Myanmar"

    # Europe
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="North Macedonia"] <- "Macedonia"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Czech Republic"] <- "Czechia"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Bosnia and Herzegovina"] <- "Bosnia and Herz."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Holy See"] <- "Vatican"

    # Africa
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Congo (Kinshasa)"] <- "Congo"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Congo (Brazzaville)"] <- "Dem. Rep. Congo"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Cote d'Ivoire"] <- "Côte d'Ivoire"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Central African Republic"] <- "Central African Rep."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Equatorial Guinea"] <- "Eq. Guinea"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Eswatini"] <- "eSwatini"

    # America
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Dominican Republic"] <- "Dominican Rep."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Antigua and Barbuda"] <- "Antigua and Barb."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="US"] <- "United States of America"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Saint Kitts and Nevis"] <- "St. Kitts and Nevis"

    JHUCSSEDf$Area <- as.character(unique(WorldMapShapeDf$NAME)[charmatch(JHUCSSEDf$`Country/Region`, unique(WorldMapShapeDf$NAME))])

    print(JHUCSSEDf$`Country/Region`[is.na(JHUCSSEDf$Area)])

    GeoMapDf <- JHUCSSEDf%>%dplyr::select(
        -`Province/State`,
        -Lat, -Long, 
        -`Country/Region`)%>%group_by(Area)%>%summarise_each(sum)
    GeoMapDf$Area <- as.character(GeoMapDf$Area)
    
    days <- names(GeoMapDf%>%select(contains("/")))
    formattedDate <- as.Date(days, "%m/%d/%y")
    names(GeoMapDf)[str_detect(names(GeoMapDf), "/")] <- format.Date(formattedDate, "%m/%d/%y")

    GeoMapDf <- left_join(
        data.frame(Area = WorldMapShapeDf$NAME%>%as.character(), Pop = WorldMapShapeDf$POP_EST%>%as.character()%>%as.numeric()),
        GeoMapDf)

    GeoMapDf[is.na(GeoMapDf)] <- 0
    return(GeoMapDf)
}

#df1 <- getJHUCSSEDataset("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#head(df1, 4)
#df2 <- getWIKIPopDataset()
#head(df2, 4)
#head(WorldMapShape, 1)
#print(WorldMapShape$NAME)
#print(WorldMapShape$POP_EST)
#df <- transformToWorldGeoMapDataset(df1, WorldMapShape)
#head(df[df$`Area`=="China",], 4)

# Object: CHNMapShape.RData
load("../data/CHNMapShape.RData")

transformToCHNGeoMapDataset <- function(JHUCSSEDf, CHNMapShapeDf) {
    JHUCSSEDf$`Province/State` <- as.character(JHUCSSEDf$`Province/State`)
    JHUCSSEDf$`Country/Region` <- as.character(JHUCSSEDf$`Country/Region`)
    
    # Deal with Taiwan
    JHUCSSEDf$`Province/State`[JHUCSSEDf$`Country/Region`=="Taiwan*"] <- "Taiwan"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Taiwan*"] <- "China"

    # Select China
    JHUCSSEDf <- JHUCSSEDf[JHUCSSEDf$`Country/Region`=="China", ]

    # Align Province/State to CHNMapShape$`NAME_1`
    #JHUCSSEDf$`Province/State`[JHUCSSEDf$`Province/State`=="Hong Kong"] <- ""
    JHUCSSEDf$`Province/State`[JHUCSSEDf$`Province/State`=="Inner Mongolia"] <- "Nei Mongol"
    #JHUCSSEDf$`Province/State`[JHUCSSEDf$`Province/State`=="Macau"] <- ""
    JHUCSSEDf$`Province/State`[JHUCSSEDf$`Province/State`=="Tibet"] <- "Xizang"
    
    JHUCSSEDf$Area <- as.character(unique(CHNMapShapeDf$`NAME_1`)[charmatch(JHUCSSEDf$`Province/State`, unique(CHNMapShapeDf$`NAME_1`))])
    
    #print(JHUCSSEDf$`Province/State`[is.na(JHUCSSEDf$Area)])
    
    GeoMapDf <- JHUCSSEDf%>%dplyr::select(
        -`Province/State`,
        -Lat, -Long, 
        -`Country/Region`)%>%group_by(Area)%>%summarise_each(sum)
    GeoMapDf$Area <- as.character(GeoMapDf$Area)
    
    days <- names(GeoMapDf%>%select(contains("/")))
    formattedDate <- as.Date(days, "%m/%d/%y")
    names(GeoMapDf)[str_detect(names(GeoMapDf), "/")] <- format.Date(formattedDate, "%m/%d/%y")
    
    GeoMapDf <- left_join(
        data.frame(
            Area = CHNMapShapeDf$`NAME_1`%>%as.character()
        ),
        GeoMapDf)
    
    GeoMapDf[is.na(GeoMapDf)] <- 0
    return(GeoMapDf)
}

#df <- transformToCHNGeoMapDataset(df1, CHNMapShape)
#head(df, 4)

# Object: USAMapShape.RData
load("../data/USAMapShape.RData")

transformToUSAGeoMapDataset <- function(JHUCSSEDf, USAMapShapeDf) {
    # Remove duplication, Province/State with Kitsap, WA and WA
    JHUCSSEDf <- JHUCSSEDf[!grepl("," ,JHUCSSEDf$`Province/State`), ]

    JHUCSSEDf$`Province/State` <- as.character(JHUCSSEDf$`Province/State`)
    JHUCSSEDf$`Country/Region` <- as.character(JHUCSSEDf$`Country/Region`)
    
    # Select United States of America
    JHUCSSEDf <- JHUCSSEDf[JHUCSSEDf$`Country/Region`=="US", ]
    
    # Align Province/State to USAMapShape$`NAME_1`
    JHUCSSEDf$`Province/State`[JHUCSSEDf$`Province/State`=="Tibet"] <- "Xizang"
    
    JHUCSSEDf$Area <- as.character(unique(USAMapShapeDf$`NAME_1`)[charmatch(JHUCSSEDf$`Province/State`, unique(USAMapShapeDf$`NAME_1`))])
    
    #print(head(JHUCSSEDf, 4))
    
    GeoMapDf <- JHUCSSEDf%>%dplyr::select(
        -`Province/State`,
        -Lat, -Long, 
        -`Country/Region`)%>%group_by(Area)%>%summarise_each(sum)
    GeoMapDf$Area <- as.character(GeoMapDf$Area)
    
    days <- names(GeoMapDf%>%select(contains("/")))
    formattedDate <- as.Date(days, "%m/%d/%y")
    names(GeoMapDf)[str_detect(names(GeoMapDf), "/")] <- format.Date(formattedDate, "%m/%d/%y")
    
    GeoMapDf <- left_join(
        data.frame(
            Area = USAMapShapeDf$`NAME_1`%>%as.character()
        ),
        GeoMapDf)
    
    GeoMapDf[is.na(GeoMapDf)] <- 0
    return(GeoMapDf)
}

#df <- transformToUSAGeoMapDataset(df1, USAMapShape)
#head(df, 4)
