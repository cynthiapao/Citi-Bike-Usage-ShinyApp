#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(rgdal)
library(dplyr)
library(leaflet)
library(plotly)

load("dataset.RData")

nycb.year1@data$id <- seq(1,nrow(nycb.year1@data),by=1)
nycb.year2@data$id <- seq(1,nrow(nycb.year1@data),by=1)
nycb.year3@data$id <- seq(1,nrow(nycb.year1@data),by=1)
nycb.year4@data$id <- seq(1,nrow(nycb.year1@data),by=1)
centers = coordinates(nycb)

shinyUI(fluidPage(
  h2("Change of the Number of Collisions from 2013 to 2016 of Neighbourhoods"),
  fluidRow(column(width=5,offset = 1,leafletOutput("map",width = "100%",height = 700)),
           column(width=5,offset = 0,fluidRow(plotlyOutput(outputId = "firstplot",width = "100%",height = 350)),
                  fluidRow(plotlyOutput(outputId = "secondplot",width = "100%",height = 350)))),
  p("The left interactive map shows the change of the number of collisions from 2013 to 2016. The areas in the south of central park are the places where the bicycle collisions happened most. 
    Other than this, in 2013 there were also a quite number of collisions occuring at the center of Brooklyn. The interactive plots on the right reflect the trends of the number of collisions 
    and persons injured/killed over the years within each neighbourhood. As mentioned above, the two plots overlap a lot. One thing notably is that fewer accidents happened in each year's winter 
    while summer is the season when bicycle collisions frequently happened and caused many injuries from 2013 to 2016. And this conclusion correponds to what is drawn from the previous Citi bike plots.")
  
  )
)

shinyServer(function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(nycb) %>% 
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") %>%
      setView(lat=40.736512, lng=-73.972151 , zoom=12) %>%
      addPolygons(group = "2013",
                  data=nycb.year1,
                  stroke = T,color=~colorNumeric(palette = "magma", domain = nycb.year1$Frequency)(nycb.year1$Frequency), 
                  fillOpacity = 0.5, smoothFactor = 0.5, weight = 1,opacity =1, 
                  fillColor = ~colorNumeric(palette = "magma", domain = nycb.year1$Frequency)(nycb.year1$Frequency),
                  highlightOptions = highlightOptions(weight = 3),
                  label = paste("Neighborhood: ",nycb.year1$NTAName,"\n", "Year: ",nycb.year1$year,"\n", "Bicycle Collision: ",nycb.year1$Frequency),
                  layerId = nycb.year1@data$id) %>%
      addPolygons(group = "2014",
                  data=nycb.year2,
                  stroke = T, fillOpacity = 0.5, smoothFactor = 0.5,
                  weight = 1, opacity=1, color=~colorNumeric(palette = "magma", domain = nycb.year2$Frequency)(nycb.year2$Frequency), 
                  fillColor = ~colorNumeric(palette = "magma", domain = nycb.year2$Frequency)(nycb.year2$Frequency), 
                  highlightOptions = highlightOptions(weight = 3),
                  label = paste("Neighborhood: ",nycb.year2$NTAName,"\n", "Year: ",nycb.year2$year,"\n", "Bicycle Collision: ",nycb.year2$Frequency),layerId = nycb.year2@data$id) %>%
      addPolygons(group = "2015",
                  data=nycb.year3,
                  stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, 
                  color = ~colorNumeric(palette = "magma", domain = nycb.year3$Frequency)(nycb.year3$Frequency),
                  opacity = 1,weight = 1,
                  fill = ~colorNumeric(palette = "magma", domain = nycb.year3$Frequency)(nycb.year3$Frequency),
                  highlightOptions = highlightOptions(weight = 1),
                  popup = paste("Neighborhood: ",nycb.year3$NTAName,"\n", "Year: ",nycb.year3$year,"\n", "Bicycle Collision: ",nycb.year3$Frequency),layerId = nycb.year3@data$id) %>%
      addPolygons(group = "2016",
                  data=nycb.year4,
                  stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, weight =1, opacity =1,
                  fillColor = ~colorNumeric(palette = "magma", domain = nycb.year4$Frequency)(nycb.year4$Frequency),
                  color = ~colorNumeric(palette = "magma", domain = nycb.year4$Frequency)(nycb.year4$Frequency),
                  highlightOptions = highlightOptions(weight =1 ),
                  popup = paste("Neighborhood: ",nycb.year4$NTAName,"\n", "Year: ",nycb.year4$year,"\n", "Bicycle Collision: ",nycb.year4$Frequency),layerId = nycb.year4@data$id) %>%
      addLayersControl(
        #baseGroups = c("OpenStreetMap"),
        baseGroups = c("2013","2014","2015","2016"),
        options = layersControlOptions(collapsed = TRUE)) %>% hideGroup(c("2014","2015","2016"))
    
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    name <- as.character(nycb@data$NTAName)[click$id]
    output$firstplot <- renderPlotly({plot_ly(data = filter(shiny3,NTAName==name),x=~month,y=~Frequency, type = "scatter",mode = "lines") %>% layout(title = "Traffic Collisions Involving Bicyclists",scene=list(xaxis = list(title ="Month"),yaxis=list(title = "Frequency")))})
    output$secondplot <- renderPlotly({plot_ly(data = filter(shiny3,NTAName==name),x = ~month,y=~injured_killed,type = "scatter",mode = "lines") %>% layout(title = "Traffic Collisions Involving Bicyclists",scene=list(xaxis = list(title ="Month"),yaxis=list(title = "Injured/Killed")))})
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

