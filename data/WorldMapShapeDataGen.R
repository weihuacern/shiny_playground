# Generate object: WorldMapShape
library(rgdal)

WorldMapShape <- readOGR(
    dsn = "ne_50m_admin_0_sovereignty",
    layer = "ne_50m_admin_0_sovereignty",
    encoding = "utf-8",
    use_iconv = T,
    verbose = TRUE
)

## ISSUES in ne_50m_admin_0_sovereignty, 4.1.0, patches below. (FXXK, these people cannot even make a simple thing right)
### Cast NAME from integer to character
WorldMapShape@data$NAME <- as.character(WorldMapShape@data$NAME)

### Modify S4, slot data, column NAME and POP_EST for Kazakhstan
levels(WorldMapShape@data$POP_EST)[169] <- 18448600
WorldMapShape@data[WorldMapShape@data$NAME == "Baikonur", "NAME"] <- "Kazakhstan"

# Dump to local file
save(WorldMapShape, file = "WorldMapShape.RData")
