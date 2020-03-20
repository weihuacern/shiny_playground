library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(RCurl)
library(plotly)
library(viridis)
library(tidyverse)

variable <-F

url <- "https://twitter.com/intent/tweet?url=https://thibautfabacher.shinyapps.io/covid-19"

load("../data/WorldMapShape.RData")

dataCook<- function(data, pop, WorldMapShape){  
  data$`Country/Region`<-as.character(data$`Country/Region`)
  data$`Country/Region`[data$`Country/Region`=="Macau"]<- "Macao"
  data$`Country/Region`[data$`Country/Region`=="Mainland China"]<- "China"
  data$`Country/Region`[data$`Country/Region`=="South Korea"]<- "South Korea"
  data$`Country/Region`[data$`Country/Region`=="North Macedonia"]<- "Macedonia"
  data$`Country/Region`[data$`Country/Region`=="Czech Republic"]<- "Czechia"
  data$`Country/Region`[data$`Country/Region`=="Dominican Republic"]<- "Dominican Rep."
  data$`Country/Region`[data$`Country/Region`=="UK"]<- "United Kingdom"
  data$`Country/Region`[data$`Country/Region`=="Gibraltar"]<- "United Kingdom"
  data$`Country/Region`[data$`Country/Region`=="US"]<- "United States"
  data$`Country/Region`[data$`Country/Region`=="Saint Barthelemy"]<- "St-Barthélemy"
  
  data$`Country/Region`[data$`Country/Region`=="Faroe Islands"]<- "Faeroe Is."
  data$`Country/Region`[data$`Country/Region`=="Bosnia and Herzegovina"]<- "Bosnia and Herz."
  data$`Country/Region`[data$`Country/Region`=="Vatican City"]<- "Vatican"
  data$`Country/Region`[data$`Country/Region`=="Korea, South"]<- "South Korea"
  data$`Country/Region`[data$`Country/Region`=="Republic of Ireland"]<- "Ireland"
  data$`Country/Region`[data$`Country/Region`=="Taiwan*"]<-"Taiwan"
  
  data$`Country/Region`[data$`Country/Region`=="Congo (Kinshasa)"]<-"Congo"
  data$`Country/Region`[data$`Country/Region`=="Cote d'Ivoire"]<-"Côte d'Ivoire"
  data$`Country/Region`[data$`Country/Region`=="Reunion"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Martinique"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="French Guiana"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Holy See"]<-"Vatican"
  data$`Country/Region`[data$`Country/Region`=="Cayman Islands"]<-"Cayman Is."
  data$`Country/Region`[data$`Country/Region`=="Guadeloupe"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Antigua and Barbuda"]<-"Antigua and Barb."
  
  data$`Country/Region`[data$`Country/Region`=="Curacao"]<-"Curaçao"
  data$`Country/Region`[data$`Country/Region`=="Guadeloupe"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="occupied Palestinian territory"]<-"Palestine"
  data$`Country/Region`[data$`Country/Region`=="Congo (Brazzaville)"]<-"Congo"
  data$`Country/Region`[data$`Country/Region`=="Equatorial Guinea"]<-"Guinea"
  data$`Country/Region`[data$`Country/Region`=="Central African Republic"]<-"Central African Rep."  
  data$`Country/Region`[data$`Country/Region`=="Eswatini"]<-"eSwatini"

  data$Pays<-as.character(unique(WorldMapShape$NAME)[charmatch(data$`Country/Region`,unique(WorldMapShape$NAME))])
  print(data$`Country/Region`[is.na(data$Pays)])
  dataPays<- data%>%dplyr::select(-`Province/State`, -Lat, -Long,-`Country/Region`)%>%group_by(Pays)%>%summarise_each(sum)
  dataPays$Pays<-as.character(dataPays$Pays)

  return(dataPays)
}

# https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population
population<- read.csv2("../data/pop.csv", stringsAsFactors = F)

population$pays<-as.character(unique(WorldMapShape$NAME)[charmatch(population$Country,unique(WorldMapShape$NAME))])

URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
data <- read.csv(text = URL, check.names = F)
dataCases <- dataCook(data, pop, WorldMapShape)

URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
data <- read.csv(text = URL, check.names = F)
dataDeaths <- dataCook(data, pop, WorldMapShape)

dataPays<-function(data=dataCases) return(data)
jour<-names(dataCases%>%select(contains("/")))
jourDate<- as.Date(jour, "%m/%d/%y")
names(dataCases)[str_detect(names(dataCases), "/")]<-format.Date(jourDate, "%m/%d/%y")
names(dataDeaths)[str_detect(names(dataDeaths), "/")]<-format.Date(jourDate, "%m/%d/%y")

dataCases<-left_join(data.frame(Pays = WorldMapShape$NAME%>%as.character(), Pop =WorldMapShape$POP_EST%>%as.character()%>%as.numeric()),dataCases)
dataDeaths<-left_join(data.frame(Pays = WorldMapShape$NAME%>%as.character(), Pop =WorldMapShape$POP_EST%>%as.character()%>%as.numeric()),dataDeaths)

arrondi<- function(x) 10^(ceiling(log10(x)))

dataDeaths[is.na(dataDeaths)]<- 0
dataCases[is.na(dataCases)]<- 0

ui <- bootstrapPage(
  tags$style(
    type = "text/css", "html, body {width:100%;height:100%}",
    HTML(".panel-default {background-color: rgb(256, 256, 256,0.5);
      padding : 10px;;}
      .panel-title {background-color: rgb(256, 256, 256,0.8);
      padding : 10px;
      border-style: solid;
      border-color: grey;}
      .panel-credits {background-color: rgb(256, 256, 256,1);
      padding : 15px;
      border-style: solid;
      border-color: black;}")
  ),
  leafletOutput("map", width = "100%", height = "93%"),
  column(
    2,br(),
    checkboxInput("plotEvolT", "Show Evolution",F)
  ),
  absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                uiOutput("Slider"),
                helpText("The detail of each country can be obtained by clicking on it."), 
                uiOutput("selection"),
                checkboxInput("legend", "Show legend", TRUE)            
  ),
  uiOutput("plotEvol"),
  absolutePanel(id = "name", class = "panel panel-title", top = 10, left = 100, HTML("<h1>COVID-19 outbreak</h1>"), draggable = T)
)

server <- function(input, output, session) {
  dataPays<- reactive({
    if(!is.null(input$choices)){
      if(input$choices == "Cases"){
        return(dataCases)
      } else {
        return(dataDeaths)
      }
    }
  })
  
  maxTotal <- reactive(max(dataPays()%>%select(-Pop)%>%select_if(is.numeric), na.rm = T))
  maxTotalPrevalence <- reactive(max(dataPays()%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/dataPays()$Pop*100000), na.rm = T)
  )

  Top5<-reactive(unique(c(dataPays()$Pays[order(dataPays()[,dim(dataPays())[2]]%>%unlist(),decreasing = T)][1:5], "France")))

  output$map <- renderLeaflet({
    leaflet(data = WorldMapShape) %>%
    setView(0, 30, zoom = 3)
  })
  
  pal <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotal())))))
  pal2 <- reactive(colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotalPrevalence())))))
  
  observe({
    casesDeath <- ifelse(input$choices == "Cases", "Cases", "Deaths")
    if (!is.null(input$day1)) {
      indicator<-format.Date(input$day1, "%m/%d/%y")
      
    } else {
      indicator = format.Date(max(jourDate), "%m/%d/%y")
    }
    
    if (!is.null(input$day2)) {
      indicator2<-format.Date(input$day2-c(1,0), "%m/%d/%y")
    } else {
      indicator2 =format.Date(c(min(jourDate)-1,max(jourDate)), "%m/%d/%y")
    }

    if(is.null(input$variable)){  
    } else {
      variable <- input$variable
      if(variable =="Total cases/population") {
        # nCases
        WorldMapShape2 <- merge(
          WorldMapShape,
          dataPays(),
          by.x = "NAME",
          by.y = "Pays",
          sort = FALSE)
        country_popup <- paste0(
          "<strong>Country: </strong>",
          WorldMapShape2$NAME,
          "<br><strong>",
          "Total cases/population :",
          " </strong>",
          round(WorldMapShape2[[indicator]]/WorldMapShape2$Pop*100000,2)," /100 000")
        leafletProxy("map", data = WorldMapShape2)%>%
          addPolygons(fillColor = pal2()(log((WorldMapShape2[[indicator]]/WorldMapShape2$Pop*100000)+1)),
                      layerId = ~NAME,
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = country_popup)
      } else if(variable =="Total cases") {
        WorldMapShape2 <- merge(
          WorldMapShape,
          dataPays(),
          by.x = "NAME",
          by.y = "Pays",
          sort = FALSE)
        country_popup <- paste0(
          "<strong>Country: </strong>",
          WorldMapShape2$NAME,
          "<br><strong>",
          "Total ",casesDeath," :",
          " </strong>",
          round(WorldMapShape2[[indicator]],2))
        leafletProxy("map", data = WorldMapShape2)%>%
          addPolygons(fillColor = pal()(log((WorldMapShape2[[indicator]])+1)),
                      fillOpacity = 1,
                      layerId = ~NAME,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = country_popup)
      } else if(variable =="New cases over period") {
        dataPaysSel<-dataPays()%>%select(Pays, Pop)
        if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
          dataPaysSel$ncases<-dataPays()[,indicator2[2]]
        } else {
          dataPaysSel$ncases<-dataPays()[,indicator2[2]]-dataPays()[,indicator2[1]]
        }
        
        # nCases
        WorldMapShape2 <- merge(WorldMapShape,
                            dataPaysSel,
                            by.x = "NAME",
                            by.y = "Pays",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                WorldMapShape2$NAME,
                                "<br><strong>",
                                "New ",casesDeath," over period :",
                                
                                
                                " </strong>",
                                WorldMapShape2$ncases)
        
        leafletProxy("map", data = WorldMapShape2)%>%
          addPolygons(fillColor = pal()(log(WorldMapShape2$ncases+1)),
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      layerId = ~NAME,
                      weight = 1,
                      popup = country_popup)
      } else {
        dataPaysSel<-dataPays()%>%select(Pays, Pop)
        if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
          dataPaysSel$ncases<-dataPays()[,indicator2[2]]
        } else {
          dataPaysSel$ncases<-dataPays()[,indicator2[2]]-dataPays()[,indicator2[1]]
        }

        # nCases
        WorldMapShape2 <- merge(WorldMapShape,
                            dataPaysSel,
                            by.x = "NAME",
                            by.y = "Pays",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                WorldMapShape2$NAME,
                                "<br><strong>",
                                "New ",casesDeath," over period / population :",
                                
                                
                                " </strong>",
                                round(WorldMapShape2$ncases/WorldMapShape2$Pop*100000,2)," /100 000")
        
        leafletProxy("map", data = WorldMapShape2)%>%
          addPolygons(fillColor = pal2()(log(WorldMapShape2$ncases/WorldMapShape2$Pop*100000+1)),
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      layerId = ~NAME,
                      weight = 1,
                      popup = country_popup)  
      }
    }
  })

  observe({    
    if(is.null(input$variable)){
    } else {
      variable<- input$variable
      proxy <- leafletProxy("map", data = WorldMapShape)
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        if(variable %in% c("Total cases/population","New cases over period/population")){
          proxy %>% addLegend(position = "bottomright",
                              pal = pal2(),opacity = 1,
                              bins = log(10^(seq(0,log10(arrondi(maxTotalPrevalence())),0.5))),
                              value = log(1:10^(log10(arrondi(maxTotalPrevalence())))),
                              data =log(1:10^(log10(arrondi(maxTotalPrevalence())))),
                              labFormat = labelFormat(transform = function(x) round(exp(x)) ,suffix = " /100 000")
                              
          )
          
        } else {
          proxy %>% addLegend(position = "bottomright",
                              pal = pal(),opacity = 1,
                              bins = log(10^(0:log10(arrondi(maxTotal())))),
                              value = log(1:10^(log10(arrondi(maxTotal())))),
                              data = log(10^(0:log10(arrondi(maxTotal())))),
                              labFormat = labelFormat(transform =  exp )
                              
          )
        }
      }
    }
  })
  
  output$Slider<-renderUI({
    
    if(is.null(input$variable)){
      
    }else{
      if(input$variable %in% c("Total cases", "Total cases/population")){
        sliderInput("day1", "Day", min(jourDate), max(jourDate),
                    value =  c(max(jourDate)),animate = T, step = 1
                    
                    #min(jourDate),
        )}else{
          sliderInput("day2", "Day", min(jourDate), max(jourDate),
                      value =  c(max(jourDate)-7,max(jourDate)),animate = T, step = 1
                      
                      #min(jourDate),
          )
          
        }
    }
  })
  
  output$selection <- renderUI({
    if(input$choices =="Cases"){
      radioButtons("variable", choices =  c("New cases over period",
                                            "New cases over period/population","Total cases", 'Total cases/population' ),
                   label = "Indicator")
    }else{
      radioButtons("variable", choices =  list("Deaths over period"="New cases over period",
                                               "Deaths over period/population"="New cases over period/population",
                                               "Total deaths"="Total cases",
                                               'Total deaths/population'='Total cases/population' ),
                   label = "Indicator")
      
      
    }
    
  })
  output$plotEvol<-renderUI({
    if (input$plotEvolT) {
      tagList(absolutePanel(
        id = "name",
        class = "panel panel-credits",
        top = 10,width = "700px",
        right  = 10,draggable = F,
        plotlyOutput(outputId = "evol",width = "600px"),
        actionButton("reset", "Reset Graph"),
        actionButton("clear", "Clear all traces")
      ))
    }
  })
  
  output$evol <-renderPlotly({
    
    if(input$variable %in% c("Total cases/population","Total cases")){
      df_evo<- dataPays()%>%filter(Pays%in% trace$data)%>%pivot_longer(cols = -c(Pays,Pop),
                                                                       values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      if(input$variable=="Total cases/population"){
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases/Pop*100000, color = ~Pays, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/ 100 000")))
        
      }else{
        
        
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases, color = ~Pays, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = input$choices))
        
      }
    }else{
      df_evo<- dataPays()%>%filter(Pays%in% trace$data)
      
      
      
      for(i in dim( df_evo)[2]:4)  df_evo[i]<- df_evo[i]- df_evo[i-1]
      
      
      df_evo<- df_evo%>%pivot_longer(cols = -c(Pays,Pop),
                                     values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      
      
      if( input$variable=="New cases over period/population"){
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases/Pop*100000, color = ~Pays, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/ 100 000/day")))
        
      }else{
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases, color = ~Pays, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/day")))
        
      }
      
    }
    
  })
  
  trace<- reactiveValues()
  observe({trace$data<-Top5()
  })
  
  observeEvent(input$reset, {
    
    
    
    
    
    for (i in 1: length(trace$data)){
      plotlyProxy("evol", session) %>%
        plotlyProxyInvoke("deleteTraces",list(0))
      
    }
    
    
    if(input$variable %in% c("Total cases/population","Total cases")){
      
      
      
      
      df_evo<- dataPays()%>%filter(Pays%in% Top5())%>%pivot_longer(cols = -c(Pays,Pop),
                                                                   values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      
      if(input$variable=="Total cases/population"){
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Pays == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases/df_evoi$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
      }else{
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Pays == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
      }
    }else{
      
      
      
      
      df_evo<- dataPays()%>%filter(Pays%in% Top5())

      for(i in  dim(df_evo)[2]:4) df_evo[i]<-df_evo[i]-df_evo[i-1]
      
      
      df_evo<-df_evo%>%pivot_longer(cols = -c(Pays,Pop),
                                    values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      if( input$variable=="New cases over period/population"){
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Pays == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases/df_evoi$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
        
      }else{
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Pays == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
        
      }
      
    }
    
    trace$data<-Top5()
  })
  
  
  observeEvent(input$clear, {
    for (i in 1: length(trace$data)){
      plotlyProxy("evol", session) %>%
        plotlyProxyInvoke("deleteTraces",list(0))
    }
    trace$data<- NULL
  })
  observeEvent(input$map_shape_click, {
    
    
    country_Click<- input$map_shape_click$id
    if (!country_Click%in%trace$data & input$plotEvolT){
      
      trace$data<-c(trace$data,country_Click)
      
      if(input$variable %in% c("Total cases/population","Total cases")){
        df_click<- dataPays()%>%filter(Pays%in% country_Click)%>%pivot_longer(cols = -c(Pays,Pop),
                                                                              values_to = "Cases",names_to = "Date")%>%
          mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
        
        if(input$variable=="Total cases/population"){
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases/df_click$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }else{
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
      }else{
        
        df_click<- dataPays()%>%filter(Pays%in% country_Click)
        
        
        

        for(i in  dim( df_click)[2]:4)  df_click[i]<- df_click[i]- df_click[i-1]
        
        
        df_click<- df_click%>%pivot_longer(cols = -c(Pays,Pop),
                                           values_to = "Cases",names_to = "Date")%>%
          mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
        
        
        
        if( input$variable=="New cases over period/population"){
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases/df_click$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }else{
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))  
        }
        
      }
      
      
      
      }
  })
}

shinyApp(ui, server)
