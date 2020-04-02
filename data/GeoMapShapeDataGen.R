library(R6)
library(rgdal)

clsNameGeoMapShape <- "geoMapShape"

mapTypeWorld <- "GeoMapWorld"
mapTypeCHN <- "GeoMapCHN"
mapTypeUSA <- "GeoMapUSA"

geoMapShape <- R6Class(
    clsNameGeoMapShape,
    private = list(
        dsn = NULL,
        layer = NULL,
        ofname = NULL
    ),
    public = list(
        initialize = function(mapType) {
            message(paste0("Initialize with mapType: ", mapType))
            if (mapType == mapTypeWorld) {
                private$dsn <- "ne_50m_admin_0_sovereignty"
                private$layer <- "ne_50m_admin_0_sovereignty"
                private$ofname <- "mapShapeWorld.RData"
            } else if (mapType == mapTypeCHN) {
                private$dsn <- "CHN_adm"
                private$layer <- "CHN_adm1"
                private$ofname <- "mapShapeCHN.RData"
            } else if (mapType == mapTypeUSA) {
                private$dsn <- "USA_adm"
                private$layer <- "USA_adm1"
                private$ofname <- "mapShapeUSA.RData"
            }
        },
        loadGeoMapObj = function() {
            print(private$dsn)
            rgdal::readOGR(
                dsn = private$dsn,
                layer = private$layer,
                encoding = "utf-8",
                use_iconv = T,
                verbose = TRUE
            )
        },
        saveGeoMapObj = function(obj, objName) {
            assign(objName, get("obj"))
            save(list = objName, file = private$ofname)
        },
        finalize = function() {
            message("Class destoryed...")
        }
    )
)

shapeGenCls <- geoMapShape$new(mapTypeWorld)
mapShapeWorld <- shapeGenCls$loadGeoMapObj()
shapeGenCls$saveGeoMapObj(mapShapeWorld, "mapShapeWorld")
rm(shapeGenCls)
