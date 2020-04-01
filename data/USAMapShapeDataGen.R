# Generate object: USAMapShape
library(rgdal)

USAMapShape <- readOGR(
    dsn = "USA_adm",
    layer = "USA_adm1",
    encoding = "utf-8",
    use_iconv = T,
    verbose = TRUE
)

# Dump to local file
save(USAMapShape, file = "USAMapShape.RData")
