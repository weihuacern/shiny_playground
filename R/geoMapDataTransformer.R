library(RCurl)
library(reshape)
library(tidyverse)

# Object: mapShapeWorld
load("../data/mapShapeWorld.RData")

transformToWorldGeoMapDataset <- function(JHUCSSEDf, mapShapeWorldDf) {
    JHUCSSEDf$Area <- as.character(
        unique(mapShapeWorldDf$NAME)[
            charmatch(JHUCSSEDf$`Country/Region`,
            unique(mapShapeWorldDf$NAME))
        ]
    )

    print(JHUCSSEDf$`Country/Region`[is.na(JHUCSSEDf$Area)])

    GeoMapDf <- JHUCSSEDf %>% dplyr::select(
        -`Province/State`,
        -Lat, -Long,
        -`Country/Region`) %>%
        group_by(Area) %>%
        summarise_each(sum)
    GeoMapDf$Area <- as.character(GeoMapDf$Area)

    days <- names(GeoMapDf %>% dplyr::select(contains("/")))
    formattedDate <- as.Date(days, "%m/%d/%y")
    names(GeoMapDf)[stringr::str_detect(names(GeoMapDf), "/")] <- format.Date(formattedDate, "%m/%d/%y")

    GeoMapDf <- left_join(
        data.frame(
            Area = mapShapeWorldDf$NAME %>% as.character(),
            Pop = mapShapeWorldDf$POP_EST %>% as.character() %>% as.numeric()
        ),
        GeoMapDf)

    GeoMapDf[is.na(GeoMapDf)] <- 0
    return(GeoMapDf)
}

# Object: mapShapeCHN.RData
load("../data/mapShapeCHN.RData")

transformToCHNGeoMapDataset <- function(JHUCSSEDf, mapShapeCHNDf) {
    # Select China
    JHUCSSEDf <- JHUCSSEDf[JHUCSSEDf$`Country/Region` == "China", ]

    JHUCSSEDf$Area <- as.character(
        unique(mapShapeCHNDf$`NAME_1`)[
            charmatch(JHUCSSEDf$`Province/State`, unique(mapShapeCHNDf$`NAME_1`))
        ]
    )

    #print(JHUCSSEDf$`Province/State`[is.na(JHUCSSEDf$Area)])

    GeoMapDf <- JHUCSSEDf %>% dplyr::select(
        -`Province/State`,
        -Lat, -Long,
        -`Country/Region`) %>%
        group_by(Area) %>%
        summarise_each(sum)
    GeoMapDf$Area <- as.character(GeoMapDf$Area)

    days <- names(GeoMapDf %>% dplyr::select(contains("/")))
    formattedDate <- as.Date(days, "%m/%d/%y")
    names(GeoMapDf)[stringr::str_detect(names(GeoMapDf), "/")] <- format.Date(formattedDate, "%m/%d/%y")

    GeoMapDf <- left_join(
        data.frame(
            Area = mapShapeCHNDf$`NAME_1` %>% as.character()
        ),
        GeoMapDf)

    GeoMapDf[is.na(GeoMapDf)] <- 0
    return(GeoMapDf)
}

# Object: mapShapeUSA.RData
load("../data/mapShapeUSA.RData")

transformToUSAGeoMapDataset <- function(JHUCSSEDf, mapShapeUSADf) {
    # Remove duplication, Province/State with Kitsap, WA and WA
    JHUCSSEDf <- JHUCSSEDf[!grepl(",", JHUCSSEDf$`Province/State`), ]

    # Select United States of America
    JHUCSSEDf <- JHUCSSEDf[JHUCSSEDf$`Country/Region` == "United States of America", ]

    JHUCSSEDf$Area <- as.character(
        unique(mapShapeUSADf$`NAME_1`)[
            charmatch(
                JHUCSSEDf$`Province/State`,
                unique(mapShapeUSADf$`NAME_1`)
            )
        ]
    )

    #print(head(JHUCSSEDf, 4))

    GeoMapDf <- JHUCSSEDf %>% dplyr::select(
        -`Province/State`,
        -Lat, -Long,
        -`Country/Region`) %>%
        group_by(Area) %>%
        summarise_each(sum)
    GeoMapDf$Area <- as.character(GeoMapDf$Area)

    days <- names(GeoMapDf %>% dplyr::select(contains("/")))
    formattedDate <- as.Date(days, "%m/%d/%y")
    names(GeoMapDf)[stringr::str_detect(names(GeoMapDf), "/")] <- format.Date(formattedDate, "%m/%d/%y")

    GeoMapDf <- left_join(
        data.frame(
            Area = mapShapeUSADf$`NAME_1` %>% as.character()
        ),
        GeoMapDf)

    GeoMapDf[is.na(GeoMapDf)] <- 0
    return(GeoMapDf)
}
