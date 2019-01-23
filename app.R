library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(shinythemes)
library(RSQLite)
library(leaflet)
library(leaflet.extras)
library(data.table)
library(htmltools)

source('./queryfunc.R')

dbname = './collision.sqlite'
tblname = 'collisionSQL'

#function for adding dependencies
addDeps <- function(x) {
  if (getOption("shiny.minified", TRUE)) {
    adminLTE_js <- "app.min.js"
    adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
  } else {
    adminLTE_js <- "app.js"
    adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
  }
  
  dashboardDeps <- list(
    htmlDependency("AdminLTE", "2.3.11",
                   c(file = system.file("AdminLTE", package = "shinydashboard")),
                   script = adminLTE_js,
                   stylesheet = adminLTE_css
    ),
    htmlDependency("shinydashboard",
                   as.character(utils::packageVersion("shinydashboard")),
                   c(file = system.file(package = "shinydashboard")),
                   script = "shinydashboard.js",
                   stylesheet = "shinydashboard.css"
    )
  )
  shinydashboard:::appendDependencies(x, dashboardDeps)
}

#global variables

boro<- c('Bronx' = 'BRONX', 'Brooklyn' = 'BROOKLYN',
         'Manhattan' = 'MANHATTAN', 'Queens' = 'QUEENS', 'Staten Island' = 'STATEN ISLAND')
yer<-as.character(2013:2018)
mons<-c('Jan' = '01', 'Feb' = '02', 'Mar' = '03', 'Apr' = '04', 'May' = '05',
        'Jun' = '06', 'Jul' = '07', 'Aug' = '08', 'Sep' = '09', 'Oct' = '10',
        'Nov' = '11', 'Dec' = '12')
hour<-c('All Day', 'Early Morning', 'Morning Rush',
        'Midday', 'Late Rush', 'Late Night')
focus<-c('All', 'Injury', 'Death', 'Cyclists', 'Pedestrians')

#shiny ui
ui <- navbarPage(title = 'NYPD Vehicle Collision Report',
                 id = 'navbar',
                 footer = 'Data from July 2012 through July 2018',
                 theme = shinytheme('darkly'),
                  
                 # #First tab to display statistics of the data set filtered by burrough
                 tabPanel('Statistics',  icon = icon('percent'),

                          #Sidebar for parameter selection
                          sidebarPanel(
                            selectInput('statfocus', 'Category', focus, selected = 'All'),
                            selectInput('borslct', 'Borough', boro, selected = 'BRONX'),
                            radioButtons('timrad', label = 'Select range',
                                         choices = c('Annual' = 1, 'Monthly' = 2)),
                            conditionalPanel(
                              condition = "input.timrad == 2",
                              selectInput('timem', 'Month', mons, selected = '01')),
                            selectInput('timed', 'Time of Day', hour, selected = 'All Day'),
                            textOutput('hours'),
                            width = 2),

                          #Main panel display for statistics tab
                          mainPanel(
                            fluidPage(
                              fluidRow(
                                plotOutput('time', width ='100%', height = '550px')),
                              br(),
                              fluidRow(
                                infoBoxOutput('tmax', width = 3),
                                infoBoxOutput('tmin', width = 3),
                                infoBoxOutput('tmean', width = 3),
                                infoBoxOutput('tvar', width = 3))
                              ), width = 10))#,

### This section is a map of accidents in each borough by month and year and time of day.          ###
### It currently works well on its own, but doesn't work when integrated into the larger app.      ###
### In the interest of time, I am going to publish the app and wait to fix this in a later update. ###

                  #Map tab
                 #  tabPanel('Map', icon = icon('map'),
                 # 
                 #            #Map output
                 #            tags$style(type  = 'text/css', '#map {height: calc(100vh - 80px) !important;'),
                 #            leafletOutput('map', width='100%', height = '100%'),
                 # 
                 #            #floating panel to select map criteria
                 #            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                 #                          draggable = TRUE, top = 180, bottom = "auto", left = 20,
                 #                          right = "auto", width = 140, height = "auto",
                 #                          selectInput('borslct', 'Borough', boro, selected = 'BRONX'),
                 #                          selectInput('timey', 'Year', yer),
                 #                          selectInput('timem', 'Month', mons),
                 #                          selectInput('timed', 'Time of Day', hour, selected = 'ALL DAY'),
                 #                          textOutput('hours'),
                 #                          checkboxInput('heatmap', label = 'heatmap', value = FALSE))
                 # 
                 # )
)







#add dependencies
ui <- addDeps(
  tags$body(shiny::fluidPage(ui))
)


server<-function(input, output, session) {
  
  #setup database connection
  conn <- dbConnector(session, dbname = dbname)  
  
  hours <- reactive(switch(input$timed,'All Day' = c(0, 24), 'Early Morning' = c(0, 6), 'Morning Rush' = c(6, 10),
                           'Midday' = c(10, 15), 'Late Rush' = c(15, 20), 'Late Night' = c(20, 24)))
  
  output$hours <- reactive(paste0(hours(),':00', collapse = '-'))
  
  #create select variable for query
  slect <- reactive(
    if(input$navbar == 'Statistics'){
      switch(input$statfocus, All = 'DATE, TIME',
             Injury = 'DATE, TIME, "NUMBER OF PERSONS INJURED" AS FeatStat',
             Death = 'DATE, TIME, "NUMBER OF PERSONS KILLED" AS FeatStat',
             Pedestrians = 'DATE, TIME, ("NUMBER OF PEDESTRIANS INJURED"||"NUMBER OF PEDESTRIANS KILLED") AS FeatStat',
             Cyclists = 'DATE, TIME, ("NUMBER OF CYCLIST INJURED"||"NUMBER OF CYCLIST KILLED") AS FeatStat'
      )
    }else{
      return('SUBSTR(DATE,1,2) "Month", SUBSTR(DATE,-4,4) "Year", TIME, LONGITUDE, LATITUDE')
    }
  ) 
  
  #create conditions for query
  params <- eventReactive(slect, {
    if(input$navbar == 'Statistics'){
      switch(input$statfocus, All = '',
             Injury = '"NUMBER OF PERSONS INJURED" >= 1',
             Death  = '"NUMBER OF PERSONS KILLED" >=1',
             Pedestrians = '("NUMBER OF PEDESTRIANS INJURED" >= 1 OR "NUMBER OF PEDESTRIANS KILLED" >= 1)',
             Cyclists = '("NUMBER OF CYCLIST INJURED" >= 1 OR "NUMBER OF CYCLIST KILLED" >= 1)')
    }else{
      return('(LONGITUDE!="")')
    }
    
  })
  
  #query database
  getDB <- reactive(dbGetData(conn = conn,
                              tblname = tblname,
                              slect = slect(),
                              borough = input$borslct,
                              params = params()))

  pltLabs <- eventReactive(input$statfocus, {
      switch(input$statfocus, All = list(title = 'Collisions per Day', yLab = 'Number of Collisions'),
             Injury = list(title = 'Injuries per Day', yLab = 'Number of Injuries'),
             Death = list(title = 'Fatalities per Day', yLab = 'Number of Fatalities'),
             Pedestrians = list(title = 'Collisions Involving Pedestrians', yLab = 'Number of Pedestrians'),
             Cyclists = list(title = 'Collisions Involving Cyclists', yLab = 'Number of Cyclists'))
  })
             

    
  #Time tab
  
  #Time plot
  output$time <- renderPlot({   
    getDB() %>% mutate(., hour = as.numeric(mapply(function(L) L[1], strsplit(TIME, split = ':')))) %>%
      filter(between(hour, hours()[1], hours()[2]-1)) %>% 
      {if(input$timrad==2) {mutate(.,month = substr(DATE, 1, 2))%>%filter(month == input$timem)} else .} %>%
      group_by(DATE) %>%
      {if(input$statfocus=='All') summarise(., sumTot = n()) else summarise(., sumTot= sum(as.numeric(FeatStat)))} %>%
      mutate(year = substr(DATE, 7, 10), day = substr(DATE, 1, 5)) %>%
      ggplot(aes(x = day, y = sumTot, color = year, group = year))+
      geom_line(size=.5)+ggtitle(pltLabs()$title)+labs(x=NULL, y=pltLabs()$yLab)+
      scale_x_discrete(breaks = paste(c(paste0("0", as.character(1:9)), (10:12)), "01", sep = "/"))+theme_dark()+
      theme(legend.justification = c(1,1), legend.position = c(1,1), legend.background = element_blank())      
  })
  
  #Time info boxes
  output$tmax <- renderInfoBox({
    infoBox(
      "Max", (getDB() %>% mutate(., hour = as.numeric(mapply(function(L) L[1], strsplit(TIME, split = ':')))) %>%
                filter(between(hour, hours()[1], hours()[2]-1)) %>%
                {if(input$timrad==2) {mutate(.,month = substr(DATE, 1, 2))%>%filter(month == input$timem)} else .}  %>%
                group_by(DATE) %>% summarise(total = {if(input$statfocus=='All')  n() else sum(as.numeric(FeatStat))}) %>% summarise(max(total)))[[1]],
      icon = icon("arrow-up"), color = "red", fill = TRUE
    )
  })

  output$tmin <- renderInfoBox({
    infoBox(
      "Min", (getDB() %>% mutate(., hour = as.numeric(mapply(function(L) L[1], strsplit(TIME, split = ':')))) %>%
                filter(between(hour, hours()[1], hours()[2]-1)) %>%
                {if(input$timrad==2) {mutate(.,month = substr(DATE, 1, 2))%>%filter(month == input$timem)} else .}  %>%
                group_by(DATE) %>% summarise(total = {if(input$statfocus=='All')  n() else sum(as.numeric(FeatStat))}) %>% summarise(min(total)))[[1]],
      icon = icon('arrow-down'), color = "blue", fill = TRUE
    )
  })

  output$tmean <- renderInfoBox({
    infoBox(
      "Mean", signif( (getDB() %>% mutate(., hour = as.numeric(mapply(function(L) L[1], strsplit(TIME, split = ':')))) %>%
                filter(between(hour, hours()[1], hours()[2]-1)) %>%
                {if(input$timrad==2) {mutate(.,month = substr(DATE, 1, 2))%>%filter(month == input$timem)} else .}  %>%
                group_by(DATE) %>% summarise(total = {if(input$statfocus=='All')  n() else sum(as.numeric(FeatStat))}) %>% summarise(mean(total)))[[1]], 4),
      icon = icon("minus"), color = "green", fill = TRUE
    )
  })

  output$tvar <- renderInfoBox({
    infoBox(
      "Variance", signif( (getDB() %>% mutate(., hour = as.numeric(mapply(function(L) L[1], strsplit(TIME, split = ':')))) %>%
                filter(between(hour, hours()[1], hours()[2]-1)) %>%
                {if(input$timrad==2) {mutate(.,month = substr(DATE, 1, 2))%>%filter(month == input$timem)} else .}  %>%
                group_by(DATE) %>% summarise(total = {if(input$statfocus=='All')  n() else sum(as.numeric(FeatStat))}) %>% summarise(var(total)))[[1]], 4),
      icon = icon("arrows-h"), color = "purple", fill = TRUE
    )
  })
  
  
  #Map
  output$map <- renderLeaflet({
    {if(input$navbar=='Map'){
        getDB() %>% mutate(hour = as.numeric(mapply(function(L) L[1], strsplit(TIME, split = ':')))) %>%
        filter(Year == input$timey , Month == input$timem , between(hour, hours()[1], hours()[2]-1)) %>% 
        leaflet() %>% addProviderTiles('Esri.WorldStreetMap') %>%
        {if(input$heatmap == TRUE) addHeatmap(., lng = ~LONGITUDE, lat = ~LATITUDE) else addMarkers(., lng = ~LONGITUDE, lat = ~LATITUDE)}
    }}
  
  })
  
}                                        

# Run the application 
shinyApp(ui = ui, server = server)

