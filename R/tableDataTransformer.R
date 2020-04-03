library(dplyr)

transformToWorldTableDataset <- function(
    rawDataJHUConfGlobal,
    rawDataJHUDeadGlobal
) {
    # Latest date and second latest date
    latestDate <- (rev(names(rawDataJHUConfGlobal))[1])
    secondLatestDate <- (rev(names(rawDataJHUConfGlobal))[2])

    names(rawDataJHUConfGlobal)[names(rawDataJHUConfGlobal) == "Country/Region"] <- "area"
    tableConfList <- rawDataJHUConfGlobal %>% dplyr::select(
        -`Province/State`,
        -Lat, -Long) %>%
        group_by(area) %>%
        summarise_each(sum) %>%
        arrange(desc(!!sym(latestDate)))
    tableConfList$area <- as.character(tableConfList$area)
    confVars <- c(area = "area", confCnt = latestDate, tmpCnt = secondLatestDate)
    tableConfList <- dplyr::select(tableConfList, !!confVars)
    tableConfList <- dplyr::mutate(tableConfList, newConfCnt = confCnt - tmpCnt)
    tableConfList <- tableConfList %>%
        dplyr::select(area, confCnt, newConfCnt)
    tableConfList$area <- as.character(tableConfList$area)

    names(rawDataJHUDeadGlobal)[names(rawDataJHUDeadGlobal) == "Country/Region"] <- "area"
    tableDeadList <- rawDataJHUDeadGlobal %>% dplyr::select(
        -`Province/State`,
        -Lat, -Long) %>%
        group_by(area) %>%
        summarise_each(sum) %>%
        arrange(desc(!!sym(latestDate)))
    tableDeadList$area <- as.character(tableDeadList$area)
    deadVars <- c(area = "area", deadCnt = latestDate, tmpCnt = secondLatestDate)
    tableDeadList <- dplyr::select(tableDeadList, !!deadVars)
    tableDeadList <- dplyr::mutate(tableDeadList, newDeadCnt = deadCnt - tmpCnt)
    tableDeadList <- tableDeadList %>%
        dplyr::select(area, deadCnt, newDeadCnt)
    tableDeadList$area <- as.character(tableDeadList$area)

    tableDf <- left_join(
        tableConfList, tableDeadList
    )

    tableDf <- tableDf %>% dplyr::rename(
        "Country/Region" = area,
        "Confirmed Cases" = confCnt,
        "New Confirmed Cases" = newConfCnt,
        "Dead Cases" = deadCnt,
        "New Dead Cases" = newDeadCnt
    )
    return(tableDf)
}

transformToCHNTableDataset <- function(
    rawDataJHUConfGlobal,
    rawDataJHUDeadGlobal
) {
    # Latest date and second latest date
    latestDate <- (rev(names(rawDataJHUConfGlobal))[1])
    secondLatestDate <- (rev(names(rawDataJHUConfGlobal))[2])

    # Select China
    rawDataJHUConfCHN <- rawDataJHUConfGlobal[rawDataJHUConfGlobal$`Country/Region` == "China", ]
    rawDataJHUDeadCHN <- rawDataJHUDeadGlobal[rawDataJHUDeadGlobal$`Country/Region` == "China", ]

    names(rawDataJHUConfCHN)[names(rawDataJHUConfCHN) == "Province/State"] <- "area"
    tableConfList <- rawDataJHUConfCHN %>% dplyr::select(
        -`Country/Region`,
        -Lat, -Long) %>%
        group_by(area) %>%
        summarise_each(sum) %>%
        arrange(desc(!!sym(latestDate)))
    tableConfList$area <- as.character(tableConfList$area)
    confVars <- c(area = "area", confCnt = latestDate, tmpCnt = secondLatestDate)
    tableConfList <- dplyr::select(tableConfList, !!confVars)
    tableConfList <- dplyr::mutate(tableConfList, newConfCnt = confCnt - tmpCnt)
    tableConfList <- tableConfList %>%
        dplyr::select(area, confCnt, newConfCnt)
    tableConfList$area <- as.character(tableConfList$area)

    names(rawDataJHUDeadCHN)[names(rawDataJHUDeadCHN) == "Province/State"] <- "area"
    tableDeadList <- rawDataJHUDeadCHN %>% dplyr::select(
        -`Country/Region`,
        -Lat, -Long) %>%
        group_by(area) %>%
        summarise_each(sum) %>%
        arrange(desc(!!sym(latestDate)))
    tableDeadList$area <- as.character(tableDeadList$area)
    deadVars <- c(area = "area", deadCnt = latestDate, tmpCnt = secondLatestDate)
    tableDeadList <- dplyr::select(tableDeadList, !!deadVars)
    tableDeadList <- dplyr::mutate(tableDeadList, newDeadCnt = deadCnt - tmpCnt)
    tableDeadList <- tableDeadList %>%
        dplyr::select(area, deadCnt, newDeadCnt)
    tableDeadList$area <- as.character(tableDeadList$area)

    tableDf <- left_join(
        tableConfList, tableDeadList
    )

    tableDf <- tableDf %>% dplyr::rename(
        "Province/State" = area,
        "Confirmed Cases" = confCnt,
        "New Confirmed Cases" = newConfCnt,
        "Dead Cases" = deadCnt,
        "New Dead Cases" = newDeadCnt
    )
    return(tableDf)
}
