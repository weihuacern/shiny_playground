library(rgdal)

# Generate object: WorldMapShape
WorldMapShape <- readOGR(dsn ="ne_50m_admin_0_countries", layer = "ne_50m_admin_0_countries", encoding = "utf-8",use_iconv = T, verbose = FALSE)
save(WorldMapShape, file="WorldMapShape.RData")
