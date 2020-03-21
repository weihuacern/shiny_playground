library(rgdal)

# Generate object: WorldMapShape

WorldMapShape <- readOGR(dsn ="ne_50m_admin_0_sovereignty", layer = "ne_50m_admin_0_sovereignty", encoding = "utf-8", use_iconv = T, verbose = TRUE)

## ISSUES in ne_50m_admin_0_sovereignty, 4.1.0, patches below. (FXXK, these people cannot even make a simple thing right)

#print(WorldMapShape@data[WorldMapShape@data$NAME == "Baikonur", "NAME"])
#print(typeof(WorldMapShape@data[WorldMapShape@data$NAME == "Baikonur", "NAME"]))
#print(typeof(WorldMapShape))
#show(WorldMapShape[1,])
#show(typeof(WorldMapShape[1,]))

### Cast NAME from integer to character
WorldMapShape@data$NAME <- as.character(WorldMapShape@data$NAME)

### Modify S4, slot data, column NAME for Kazakhstan
WorldMapShape@data[WorldMapShape@data$NAME == "Baikonur", "NAME"] <- "Kazakhstan"

# Dump to local file
save(WorldMapShape, file="WorldMapShape.RData")

