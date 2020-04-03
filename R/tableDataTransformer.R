library(dplyr)

transformToWorldTableDataset <- function(
    rawDataJHUConfGlobal,
    rawDataJHUDeadGlobal
) {
    names(rawDataJHUConfGlobal)[names(rawDataJHUConfGlobal) == "Country/Region"] <- "area"
    names(rawDataJHUDeadGlobal)[names(rawDataJHUDeadGlobal) == "Country/Region"] <- "area"
    # Latest date and second latest date
    latestDate <- (rev(names(rawDataJHUConfGlobal))[1])
    secondLatestDate <- (rev(names(rawDataJHUConfGlobal))[2])

    tableConfList <- rawDataJHUConfGlobal %>% dplyr::select(
        -`Province/State`,
        -Lat, -Long) %>%
        group_by(area) %>%
        summarise_each(sum) %>%
        arrange(desc(!!sym(latestDate)))
    tableConfList$area <- as.character(tableConfList$area)
    confVars <- c(area = "area", confCnt = latestDate, tmpConfCnt = secondLatestDate)
    tableConfList <- dplyr::select(tableConfList, !!confVars)
    tableConfList <- dplyr::mutate(tableConfList, newConfCnt = confCnt - tmpConfCnt)
    tableConfList$area <- as.character(tableConfList$area)

    tableDeadList <- rawDataJHUDeadGlobal %>% dplyr::select(
        -`Province/State`,
        -Lat, -Long) %>%
        group_by(area) %>%
        summarise_each(sum) %>%
        arrange(desc(!!sym(latestDate)))
    tableDeadList$area <- as.character(tableDeadList$area)
    deadVars <- c(area = "area", deadCnt = latestDate, tmpDeadCnt = secondLatestDate)
    tableDeadList <- dplyr::select(tableDeadList, !!deadVars)
    tableDeadList <- dplyr::mutate(tableDeadList, newDeadCnt = deadCnt - tmpDeadCnt)
    tableDeadList <- tableDeadList %>%
        dplyr::select(area, deadCnt, newDeadCnt)
    tableDeadList$area <- as.character(tableDeadList$area)

    tableDf <- left_join(
        data.frame(area = tableConfList$area, confCnt = tableConfList$confCnt, newConfCnt = tableConfList$newConfCnt),
        tableDeadList
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
