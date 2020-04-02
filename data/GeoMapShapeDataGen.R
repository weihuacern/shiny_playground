library(R6)
library(rgdal)

clsNameGeoMapShape <- "geoMapShape"

mapTypeWorld <- "GeoMapWorld"
mapTypeCHN <- "GeoMapCHN"
mapTypeUSA <- "GeoMapUSA"
validMapType <- c(
    mapTypeWorld,
    mapTypeCHN,
    mapTypeUSA
)

geoMapShape <- R6Class(
    clsNameGeoMapShape,
    private = list(
        mapType = NULL,
        dsn = NULL,
        layer = NULL,
        objName = NULL
    ),
    public = list(
        initialize = function(mapType) {
            message(paste0("Initialize with mapType: ", mapType))
            private$mapType <- mapType
            if (mapType == mapTypeWorld) {
                private$dsn <- "ne_50m_admin_0_sovereignty"
                private$layer <- "ne_50m_admin_0_sovereignty"
                private$objName <- "mapShapeWorld"
            } else if (mapType == mapTypeCHN) {
                private$dsn <- "CHN_adm"
                private$layer <- "CHN_adm1"
                private$objName <- "mapShapeCHN"
            } else if (mapType == mapTypeUSA) {
                private$dsn <- "USA_adm"
                private$layer <- "USA_adm1"
                private$objName <- "mapShapeUSA"
            }
        },
        loadGeoMapObj = function() {
            rgdal::readOGR(
                dsn = private$dsn,
                layer = private$layer,
                encoding = "utf-8",
                use_iconv = T,
                verbose = TRUE
            )
        },
        saveGeoMapObj = function(obj) {
            # Patch for World map only...
            if (private$mapType == mapTypeWorld) {
                ## ISSUES in ne_50m_admin_0_sovereignty, 4.1.0, patches below.
                ### Cast NAME from integer to character
                obj@data$NAME <- as.character(obj@data$NAME)
                ### Modify S4, slot data, column NAME and POP_EST for Kazakhstan
                levels(obj@data$POP_EST)[169] <- 18448600
                obj@data[obj@data$NAME == "Baikonur", "NAME"] <- "Kazakhstan"
            }
            ofName <- paste0(private$objName, ".RData")
            assign(private$objName, get("obj"))
            save(list = private$objName, file = ofName)
        },
        finalize = function() {
            message("Class destoryed...")
        }
    )
)

#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop("At least one argument must be supplied (mapType)", call. = FALSE)
} else if (length(args) >= 1) {
    message(paste0("Running GeoMapShapeDataGen.R with mapType: ", args[1]))
    message("Valid mapTypes are: ")
    message(paste(validMapType, collapse = ", "))
    if (!(args[1] %in% validMapType)) {
        stop("Invalid mapType! Program aborted", call. = FALSE)
    }
}

mapType <- args[1]

shapeGenCls <- geoMapShape$new(mapType)
mapShape <- shapeGenCls$loadGeoMapObj()
shapeGenCls$saveGeoMapObj(mapShape)
rm(shapeGenCls)
