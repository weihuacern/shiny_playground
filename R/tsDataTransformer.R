library(R6)

library(dplyr)
library(reshape)
library(xts)

clsNameTSDataTransformer <- "tsDataTransformer"

tsTransformTypeGlobal <- "tsTransformDataGlobal"
tsTransformTypeCHN <- "tsTransformDataCHN"
tsTransformTypeUSA <- "tsTransformDataUSA"
validTStransformType <- c(
    tsTransformTypeGlobal,
    tsTransformTypeCHN,
    tsTransformTypeUSA
)

tsDataTransformer <- R6Class(
    clsNameTSDataTransformer,
    private = list(
        transformType = NULL
    ),
    public = list(
        initialize = function(transformType) {
            message(paste0("Initialize with transformType: ", transformType))
            private$transformType <- transformType
        },
        transformData = function(rawData) {
            if (private$transformType == tsTransformTypeGlobal) {
                names(rawData)[names(rawData) == "Country/Region"] <- "area"
                rawData <- rawData %>% dplyr::select(
                    -`Province/State`)
            } else if (private$transformType == tsTransformTypeCHN) {
                rawData <- rawData[rawData$`Country/Region` == "China", ]
                names(rawData)[names(rawData) == "Province/State"] <- "area"
                rawData <- rawData %>% dplyr::select(
                    -`Country/Region`)
            } else if (private$transformType == tsTransformTypeUSA) {
                names(rawData)[names(rawData) == "Province/State"] <- "area"
                rawData <- rawData %>% dplyr::select(
                    -`Country/Region`)
            }

            tsDf <- rawData %>%
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
        },
        finalize = function() {
            message(paste0("Class destoryed: ", clsNameTSDataTransformer))
        }
    )
)
