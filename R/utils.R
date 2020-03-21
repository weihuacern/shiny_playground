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

# TODO, to be removed
getWIKIPopDataset <- function() {
    # https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population
    # Path hardcoded for now
    population <- read.csv2("../data/pop.csv", stringsAsFactors = F)
    population$Area <- as.character(unique(WorldMapShape$NAME)[charmatch(population$Country,unique(WorldMapShape$NAME))])
    return(population)
}

transformToGeoMapDataset <- function(JHUCSSEDf, WorldMapShapeDf) {
    # Align Country/Region Name to WorldMapShapedf$NAME
    JHUCSSEDf$`Country/Region`<-as.character(JHUCSSEDf$`Country/Region`)
    # TODO, need to move to this to China category
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Taiwan*"] <- "Taiwan"

    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="North Macedonia"] <- "Macedonia"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Czech Republic"] <- "Czechia"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Dominican Republic"] <- "Dominican Rep."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="UK"] <- "United Kingdom"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Gibraltar"] <- "United Kingdom"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="US"] <- "United States"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Saint Barthelemy"] <- "St-Barthélemy"

    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Faroe Islands"] <- "Faeroe Is."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Bosnia and Herzegovina"] <- "Bosnia and Herz."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Vatican City"] <- "Vatican"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Korea, South"] <- "South Korea"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Republic of Ireland"] <- "Ireland"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."

    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Gambia, The"] <- "Gambia"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Bahamas, The"] <- "Bahamas"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Congo (Kinshasa)"] <- "Congo"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Cote d'Ivoire"] <- "Côte d'Ivoire"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Reunion"] <- "France"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Martinique"] <- "France"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="French Guiana"] <- "France"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Holy See"] <- "Vatican"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Cayman Islands"] <- "Cayman Is."
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Guadeloupe"] <- "France"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Antigua and Barbuda"] <- "Antigua and Barb."
    
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Curacao"] <- "Curaçao"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Guadeloupe"] <- "France"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="occupied Palestinian territory"] <- "Palestine"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Congo (Brazzaville)"] <- "Congo"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Equatorial Guinea"] <- "Guinea"
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Central African Republic"] <- "Central African Rep."  
    JHUCSSEDf$`Country/Region`[JHUCSSEDf$`Country/Region`=="Eswatini"] <- "eSwatini"

    JHUCSSEDf$Area <- as.character(unique(WorldMapShapeDf$NAME)[charmatch(JHUCSSEDf$`Country/Region`,unique(WorldMapShapeDf$NAME))])
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
#df <- transformToGeoMapDataset(df1, WorldMapShape)
#head(df, 4)
#max(df%>%select(-Pop)%>%select_if(is.numeric), na.rm = T)
