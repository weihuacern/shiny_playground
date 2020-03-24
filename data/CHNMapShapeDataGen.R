library(rgdal)

# Generate object: CHNMapShape

CHNMapShape <- readOGR(dsn ="CHN_adm", layer = "CHN_adm1", encoding = "utf-8", use_iconv = T, verbose = TRUE)

#print(typeof(CHNMapShape))
#show(CHNMapShape[1,])
#show(typeof(CHNMapShape[1,]))

# Dump to local file
save(CHNMapShape, file="CHNMapShape.RData")

