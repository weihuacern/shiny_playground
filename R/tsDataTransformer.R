library(dplyr)
library(reshape)
library(xts)

transformToWorldTSDataset <- function(rawDataJHUGlobal) {
    names(rawDataJHUGlobal)[names(rawDataJHUGlobal) == "Country/Region"] <- "area"

    tsDf <- rawDataJHUGlobal %>% dplyr::select(
        -`Province/State`) %>%
        group_by(area) %>%
        summarise_each(sum)
    tsDf$area <- as.character(tsDf$area)
    tsDf <- reshape::melt(as.data.frame(tsDf), id = c("area"))
    names(tsDf)[names(tsDf) == "variable"] <- "time"
    names(tsDf)[names(tsDf) == "value"] <- "value"
    tsDf$time <- as.Date(tsDf$time, format = "%m/%d/%y")
    tsDf <- reshape::cast(tsDf, time ~ area, value = "value")
    tsData <- xts::xts(tsDf %>% dplyr::select(-time), order.by = tsDf$time)
    return(tsData)
}

transformToCHNTSDataset <- function(rawDataJHUGlobal) {
    rawDataJHUCHN <- rawDataJHUGlobal[rawDataJHUGlobal$`Country/Region` == "China", ]
    names(rawDataJHUCHN)[names(rawDataJHUCHN) == "Province/State"] <- "area"

    tsDf <- rawDataJHUCHN %>% dplyr::select(
        -`Country/Region`) %>%
        group_by(area) %>%
        summarise_each(sum)
    tsDf$area <- as.character(tsDf$area)
    tsDf <- reshape::melt(as.data.frame(tsDf), id = c("area"))
    names(tsDf)[names(tsDf) == "variable"] <- "time"
    names(tsDf)[names(tsDf) == "value"] <- "value"
    tsDf$time <- as.Date(tsDf$time, format = "%m/%d/%y")
    tsDf <- reshape::cast(tsDf, time ~ area, value = "value")
    tsData <- xts::xts(tsDf %>% dplyr::select(-time), order.by = tsDf$time)
    return(tsData)
}

transformToUSATSDataset <- function(rawDataJHUUSA) {
    names(rawDataJHUUSA)[names(rawDataJHUUSA) == "Province/State"] <- "area"

    tsDf <- rawDataJHUUSA %>% dplyr::select(
        -`Country/Region`) %>%
        group_by(area) %>%
        summarise_each(sum)
    tsDf$area <- as.character(tsDf$area)
    tsDf <- reshape::melt(as.data.frame(tsDf), id = c("area"))
    names(tsDf)[names(tsDf) == "variable"] <- "time"
    names(tsDf)[names(tsDf) == "value"] <- "value"
    tsDf$time <- as.Date(tsDf$time, format = "%m/%d/%y")
    tsDf <- reshape::cast(tsDf, time ~ area, value = "value")
    tsData <- xts::xts(tsDf %>% dplyr::select(-time), order.by = tsDf$time)
    return(tsData)
}
