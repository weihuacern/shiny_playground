library(RCurl)
library(tidyverse)

getJHUCSSEDataset <- function(urlStr) {
    # https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
    # https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv
    # https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv
    download <- getURL(urlStr)
    data <- read.csv(
        textConnection(download),
        check.names = F)
    head(data, 4)
    return(data)
}

# Object: WorldMapShape
load("../data/WorldMapShape.RData")

transformToWorldGeoMapDataset <- function(JHUCSSEDf, WorldMapShapeDf) {
    # Remove duplication, Province/State with Kitsap, WA and WA
    JHUCSSEDf <- JHUCSSEDf[!grepl("," ,JHUCSSEDf$`Province/State`), ]

    # Align Country/Region Name to WorldMapShapedf$NAME
    JHUCSSEDf$`Country/Region` <- as.character(JHUCSSEDf$`Country/Region`)

    # Asia
    ## TODO, need to move to this to China category
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Taiwan*"] <- "Taiwan"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="East Timor"] <- "Timor-Leste"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Korea, South"] <- "South Korea"

    # Europe
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="North Macedonia"] <- "Macedonia"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Czech Republic"] <- "Czechia"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Bosnia and Herzegovina"] <- "Bosnia and Herz."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Holy See"] <- "Vatican"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Martinique"] <- "France"

    # Africa
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Gambia, The"] <- "Gambia"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Bahamas, The"] <- "Bahamas"
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

#df1 <- getJHUCSSEDataset("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
#head(df1, 4)
#df2 <- getWIKIPopDataset()
#head(df2, 4)
#head(WorldMapShape, 1)
#print(WorldMapShape$NAME)
#print(WorldMapShape$POP_EST)
#df <- transformToWorldGeoMapDataset(df1, WorldMapShape)
#head(df, 4)
#max(df%>%select(-Pop)%>%select_if(is.numeric), na.rm = T)

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
            Area = USAMapShapeDf$`NAME_1`%>%as.character()
        ),
        GeoMapDf)
    
    GeoMapDf[is.na(GeoMapDf)] <- 0
    return(GeoMapDf)
}

#df <- transformToUSAGeoMapDataset(df1, USAMapShape)
#head(df, 4)
