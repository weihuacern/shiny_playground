library(rgdal)

# Generate object: USAMapShape

USAMapShape <- readOGR(dsn ="USA_adm", layer = "USA_adm1", encoding = "utf-8", use_iconv = T, verbose = TRUE)

#print(typeof(USAMapShape))
#show(USAMapShape[1,])
#show(typeof(USAMapShape[1,]))

# Dump to local file
save(USAMapShape, file="USAMapShape.RData")

