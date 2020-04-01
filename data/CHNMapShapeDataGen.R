# Generate object: CHNMapShape
library(rgdal)

CHNMapShape <- readOGR(
    dsn = "CHN_adm",
    layer = "CHN_adm1",
    encoding = "utf-8",
    use_iconv = T,
    verbose = TRUE
)

# Dump to local file
save(CHNMapShape, file = "CHNMapShape.RData")
