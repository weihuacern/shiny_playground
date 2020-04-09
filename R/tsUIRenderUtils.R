library(dygraphs)
library(shiny)
library(shinyWidgets)

inputIDTSSelWorld <- "tsselworld"
inputIDTSSelCHN <- "tsselchn"
inputIDTSSelUSA <- "tsselusa"

defTSSelListWorld <- c(
    "United States of America",
    "Spain", "Italy", "Germany",
    "France", "China"
)

defTSSelListCHN <- c(
    "Hubei", "Guangdong", "Henan",
    "Zhejiang", "Hunan", "Beijing"
)

defTSSelListUSA <- c(
    "New York"
)

tsSelectionUIRender <- function(id, cList) {
    if (id == inputIDTSSelWorld) {
        labelStr <- "Country/Region to select:"
        sList <- defTSSelListWorld
    } else if (id == inputIDTSSelCHN) {
        labelStr <- "Province to select:"
        sList <- defTSSelListCHN
    } else if (id == inputIDTSSelUSA) {
        labelStr <- "State to select:"
        sList <- defTSSelListUSA
    } else {
        labelStr <- "Country/Region to select:"
        sList <- defTSSelListWorld
    }
    return(
        shiny::renderUI({
            shinyWidgets::prettyCheckboxGroup(
                inputId = id,
                label = labelStr,
                choices = cList,
                selected = sList,
                shape = "round", status = "info",
                fill = TRUE, inline = TRUE
            )
        })
    )
}

tsGraphUIRender <- function(tsData, sList, ifLogy) {
    return(
        dygraphs::renderDygraph({
            dygraphs::dygraph(
                tsData[, sList]) %>%
            dygraphs::dyLegend(show = "follow") %>%
            dygraphs::dyOptions(stackedGraph = FALSE, logscale = ifLogy) %>%
            dygraphs::dyRangeSelector(height = 50)
        })
    )
}
