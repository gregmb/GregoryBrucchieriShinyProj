library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(data.table)
library(reshape2)
library(leaflet)

collision<-fread('./cleancoll.csv',
                 sep = ',', stringsAsFactors = F)

boro<-c('Bronx' = 'BRONX', 'Brooklyn' = 'BROOKLYN', 'Manhattan' = 'MANHATTAN',
        'Queens' = 'QUEENS', 'Staten Island' = 'STATEN ISLAND')
mons<-c('Jan' = '01', 'Feb' = '02', 'Mar' = '03', 'Apr' = '04', 'May' = '05',
        'Jun' = '06', 'Jul' = '07', 'Aug' = '08', 'Sep' = '09', 'Oct' = '10',
        'Nov' = '11', 'Dec' = '12')


ui <- dashboardPage(
                    
  dashboardHeader(title = 'Vehicle Collision Visualization', titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Time Analysis', tabName = 'time', icon = icon('line-chart')),
      menuItem('Casualties', tabName = 'cas', icon = icon('bar-chart')),
      menuItem('Map', tabName = 'map', icon = icon('map')),
      selectInput('borslct', 'Borough', boro, selected = 'BRONX')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'time',
              fluidRow(box(title = 'Time Range', background = 'black',
                           radioButtons('timrad', label = 'Select range',
                                        choices = list('Annual' = 1, 'Monthly' = 2)),
                           selectInput('timem', 'Month', mons))),
              fluidRow(box(plotOutput('time'), width = 12, height = 500))
              ),
      tabItem(tabName = 'cas', 
              fluidRow(box(plotOutput('injury'), width = 12)),
              fluidRow(box(plotOutput('death'), width = 12))
              ),
      tabItem(tabName = 'map',
              fluidRow(box(title = NULL, background = 'black',
                           selectInput('Y', 'Year', choice = c('2016', '2017'), selected = '2016')),
                       box(title = NULL, background = 'black',
                           selectInput('m', 'Month', mons, selected = '01')
                       )),
              fluidRow(box(leafletOutput('map', height=600), width=12, height = 600))
              
      )
      
    )
  )
)


server <- function(input, output) {
  
  
  #build data frame for map
  borocoll_db <- reactive(collision%>%filter(borough==input$borslct,
                                             date<=paste(input$Y,input$m,'31', sep = '-'),
                                             date>=paste(input$Y,input$m,'01', sep = '-')))
  
  #build data frame for time analysis plots
  timecoll_db <- reactive(if (input$timrad == 1){
                              collision%>%filter(borough==input$borslct)
                          }else if (input$timrad == 2){
                            collision%>%select(date, borough)%>%
                              mutate(month=substr(date, 6,7))%>%
                              filter(borough==input$borslct,
                                              month==input$timem)
  
                          })
  
  #build data frame for casualty plots
  cascoll_db <- reactive(collision%>%filter(borough==input$borslct))
  

  #build map
  output$map <- renderLeaflet({
    borocoll_db()%>%
      leaflet()%>%addProviderTiles('Esri.WorldStreetMap')%>%
      addMarkers(lng = ~longitude, lat = ~latitude)
  })
  
  #build plots for time analysis page
  output$time <- renderPlot({
    timecoll_db()%>% group_by(date)%>%
      summarise(totalCollisions=n())%>% mutate(year=substr(date, 1, 4),
                                               day=substr(date,6,10))%>%
      ggplot(aes(x=day, y=totalCollisions, color=year, group = year))+geom_line(size=2) + scale_x_discrete(breaks=10)+
      ggtitle('Collisions per Day')
    
  })
  
  #build injury plot
  output$injury <- renderPlot({ 
    cascoll_db()%>%group_by(date)%>%
      mutate(month=substr(date, 6, 7), year=substr(date, 1, 4))%>%group_by(year, month)%>%
      summarise('totalInjured'=sum(number.of.persons.injured))%>%
      ggplot(aes(x=month, y=totalInjured, group=year, color=year))+geom_line(size=3)+ggtitle('Total Injuries per Month')
  })
  
  #build death plot
  output$death <- renderPlot({
    cascoll_db()%>%group_by(date)%>%
      mutate(month=substr(date, 6, 7), year=substr(date, 1, 4))%>%group_by(year, month)%>%
      summarise('totalKilled'=sum(number.of.persons.killed))%>%
      ggplot(aes(x=month, y=totalKilled, color=year, group=year))+geom_line(size=3)+ggtitle('Total Deaths per Month')      
     })
  
  
}
  
# Run the application 
shinyApp(ui = ui, server = server)

