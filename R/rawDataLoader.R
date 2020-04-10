library(R6)

library(RCurl)
library(stringr)

clsNameRawDataLoader <- "rawDataLoader"

dataTypeJHUConfGlobal <- "RawDataJHUConfGlobal"
dataTypeJHUDeadGlobal <- "RawDataJHUDeadGlobal"
dataTypeJHURecvGlobal <- "RawDataJHURecvGlobal"
dataTypeJHUConfUSA <- "RawDataJHUConfUSA"
dataTypeJHUDeadUSA <- "RawDataJHUDeadUSA"
validDataType <- c(
    dataTypeJHUConfGlobal,
    dataTypeJHUDeadGlobal,
    dataTypeJHURecvGlobal,
    dataTypeJHUConfUSA,
    dataTypeJHUDeadUSA
)

urlStrConfGlobal <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
urlStrDeadGlobal <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
urlStrRecvGlobal <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
urlStrConfUSA <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
urlStrDeadUSA <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

rawDataLoader <- R6Class(
    clsNameRawDataLoader,
    private = list(
        dataType = NULL,
        url = NULL
    ),
    public = list(
        initialize = function(dataType) {
            message(paste0("Initialize with dataType: ", dataType))
            private$dataType <- dataType
            if (private$dataType == dataTypeJHUConfGlobal) {
                private$url <- urlStrConfGlobal
            } else if (private$dataType == dataTypeJHUDeadGlobal) {
                private$url <- urlStrDeadGlobal
            } else if (private$dataType == dataTypeJHURecvGlobal) {
                private$url <- urlStrRecvGlobal
            } else if (private$dataType == dataTypeJHUConfUSA) {
                private$url <- urlStrConfUSA
            } else if (private$dataType == dataTypeJHUDeadUSA) {
                private$url <- urlStrDeadUSA
            }
        },
        loadRawData = function() {
            download <- RCurl::getURL(private$url)
            data <- read.csv(
                textConnection(download),
                check.names = F)

            if (private$dataType %in% c(
                dataTypeJHUConfGlobal,
                dataTypeJHUDeadGlobal,
                dataTypeJHURecvGlobal)) {
                data$`Province/State` <- as.character(data$`Province/State`)
                data$`Country/Region` <- as.character(data$`Country/Region`)
                # normalization Country/Region
                ## Asia
                ## TODO, need to move to this to China category
                data$`Country/Region`[data$`Country/Region` == "Taiwan*"] <- "Taiwan"
                data$`Country/Region`[data$`Country/Region` == "Korea, South"] <- "South Korea"
                data$`Country/Region`[data$`Country/Region` == "Burma"] <- "Myanmar"

                ## Europe
                data$`Country/Region`[data$`Country/Region` == "North Macedonia"] <- "Macedonia"
                data$`Country/Region`[data$`Country/Region` == "Czech Republic"] <- "Czechia"
                data$`Country/Region`[data$`Country/Region` == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
                data$`Country/Region`[data$`Country/Region` == "Holy See"] <- "Vatican"

                ## Africa
                data$`Country/Region`[data$`Country/Region` == "Congo (Kinshasa)"] <- "Congo"
                data$`Country/Region`[data$`Country/Region` == "Congo (Brazzaville)"] <- "Dem. Rep. Congo"
                data$`Country/Region`[data$`Country/Region` == "Cote d'Ivoire"] <- "Côte d'Ivoire"
                data$`Country/Region`[data$`Country/Region` == "Central African Republic"] <- "Central African Rep."
                data$`Country/Region`[data$`Country/Region` == "Equatorial Guinea"] <- "Eq. Guinea"
                data$`Country/Region`[data$`Country/Region` == "Eswatini"] <- "eSwatini"
                data$`Country/Region`[data$`Country/Region` == "Western Sahara"] <- "W. Sahara"
                data$`Country/Region`[data$`Country/Region` == "South Sudan"] <- "S. Sudan"
                data$`Country/Region`[data$`Country/Region` == "Sao Tome and Principe"] <- "São Tomé and Principe"

                ## America
                data$`Country/Region`[data$`Country/Region` == "Dominican Republic"] <- "Dominican Rep."
                data$`Country/Region`[data$`Country/Region` == "Antigua and Barbuda"] <- "Antigua and Barb."
                data$`Country/Region`[data$`Country/Region` == "US"] <- "United States of America"
                data$`Country/Region`[data$`Country/Region` == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
                data$`Country/Region`[data$`Country/Region` == "Saint Kitts and Nevis"] <- "St. Kitts and Nevis"

                # normalization Province/State
                data$`Province/State`[data$`Country/Region` == "Taiwan"] <- "Taiwan"

                ## Align Province/State to mapShapeCHN$`NAME_1`
                data$`Province/State`[data$`Province/State` == "Inner Mongolia"] <- "Nei Mongol"
                data$`Province/State`[data$`Province/State` == "Tibet"] <- "Xizang"

                selCols <- stringr::str_subset(names(data), "Province|State|/")
                data <- data[selCols]
            } else if (private$dataType %in% c(
                dataTypeJHUConfUSA,
                dataTypeJHUDeadUSA
                )) {
                names(data)[names(data) == "Country_Region"] <- "Country/Region"
                names(data)[names(data) == "Province_State"] <- "Province/State"
                data$`Country/Region` <- as.character(data$`Country/Region`)
                data$`Province/State` <- as.character(data$`Province/State`)
                data$`Country/Region`[data$`Country/Region` == "US"] <- "United States of America"

                selCols <- stringr::str_subset(names(data), "Province|State|/")
                data <- data[selCols]
            }

            return(data)
        },
        finalize = function() {
            message("Class destoryed...")
        }
    )
)

#cls <- rawDataLoader$new(dataTypeJHUConfUSA)
#cls <- rawDataLoader$new(dataTypeJHURecvGlobal)
#rawData <- cls$loadRawData()
#rm(cls)
