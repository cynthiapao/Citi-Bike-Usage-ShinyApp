#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

load("dataset.RData")

ui <- fluidPage(
  h2("The Distribution of Number of Collisions in NYC, 2013-2016"),
  fluidRow(plotOutput(outputId = "splot1",width = "90%",height = 350)),
  p("The above map shows how the number of collisions involving bicycles distributes over the New York City. In general, the majority of points concentrate on the Manhattan Island, especially at Downtown Manhattan. 
    The hot pot showed in the density map is roughly located at East Village.
    There is another cluster appearing at the east end of Williamsburg Bridge in Brooklyn though the points are not so dense as in Manhattan."),
  hr(),
  h2("A Comparison Map between Collisions and Persons Injured/killed, 2013-2016"),
  fluidRow(plotOutput(outputId = "splot2",width = "90%",height = 350)),
  p("The second map displays a positive correlation between the number of collisions and persons injured/killed. 
    The neighbourhoods where bicycle collisions most frequently happened during 2013 and 2016 largely overlap the neighbourhoods where the number of persons injured or killed is great, except for several places in Queens. 
    The map also points out the locations where accidents often happened, that is, Midtown and Williamsburg. These areas are also the locations where Citi bikes are usually used."),
  hr(),
  h2("Change of the Number of Persons Injured/killed from 2013 to 2016"),
  fluidRow(leafletOutput(outputId = "iplot",width = "100%",height = 650)),
  p("In 2013, many collisions which caused injury or death happened around the bridges connecting Manhattan and Brooklyn. In 2014, the cluster of incidents became less obvious but still lots of incidents happened in Lower Manhattan. 
    In 2015 and 2016, the collisions evolved to the upper Manhattan. Certain accidents which caused injuries also happened beyond Manhattan. However, overall the number of injuries and deaths caused by bicycle collisions is not so great.")
)

library(cowplot)  
library(tmap)
library(ggmap)

server <- function(input, output) {
  output$splot1 <- renderPlot({
   
    plot_grid(g1, g2, ncol = 2, nrow = 1)
    
  })
  
  output$splot2 <- renderPlot({
    t1 <- tm_shape(nybb) + tm_fill("Frequency", title=c("Collisions")) + tm_text("neighbourhood", size=.6, shadow=TRUE, bg.color="white", bg.alpha=.25, remove.overlap=TRUE) 
    t2 <- tm_shape(nybb) + tm_fill("injured_killed", palette="Blues", title=c("Injured/killed Persons")) + tm_text("neighbourhood", size=.6, shadow=TRUE, bg.color="white", bg.alpha=.25, remove.overlap=TRUE)
    tmap_arrange(t1, t2, asp = 1)
  })
  
  output$iplot <- renderLeaflet({
    leaflet(nycb) %>%  
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") %>% 
      setView(lat=40.7, lng=-73.9 , zoom=11) %>%
      addPolygons(data = nycb, fill = F, weight = 1, color = "#3399FF") %>%
      addCircleMarkers(group = "2013", 
                       data=spoints1, weight=1, radius=~spoints1$injured_killed*2.5, 
                       opacity=spoints1$injured_killed/5, 
                       fillOpacity=spoints1$injured_killed/5, 
                       color=~colorNumeric(palette = "RdYlGn", domain = spoints1$injured_killed)(spoints1$injured_killed), 
                       popup=~paste("Neighborhood: ",spoints1$NTAName,"<br/>",
                                    "Persons Injured/killed: ",spoints1$injured_killed)) %>%
      addCircleMarkers(group = "2014", 
                       data=spoints2, weight=1, radius=~spoints2$injured_killed*2.5, 
                       opacity=spoints2$injured_killed/5, 
                       fillOpacity=spoints2$injured_killed/5, 
                       color=~colorNumeric(palette = "RdYlGn", domain = spoints2$injured_killed)(spoints2$injured_killed), 
                       popup=~paste("Neighborhood: ",spoints2$NTAName,"<br/>",
                                    "Persons Injured/killed:",spoints2$injured_killed)) %>%
      addCircleMarkers(group = "2015", 
                       data=spoints3, weight=1, radius=~spoints3$injured_killed*2.5,
                       opacity=spoints3$injured_killed/5, 
                       fillOpacity=spoints3$injured_killed/5, 
                       color=~colorNumeric(palette = "RdYlGn", domain = spoints3$injured_killed)(spoints3$injured_killed), 
                       popup=~paste("Neighborhood: ",spoints3$NTAName,"<br/>",
                                    "Persons Injured/killed:",spoints3$injured_killed)) %>%
      addCircleMarkers(group = "2016", 
                       data=spoints4, weight=1, radius=~spoints4$injured_killed*2.5, 
                       opacity=spoints4$injured_killed/5, 
                       fillOpacity=spoints4$injured_killed/5, 
                       color=~colorNumeric(palette = "RdYlGn", domain = spoints4$injured_killed)(spoints4$injured_killed), 
                       popup=~paste("Neighborhood: ",spoints4$NTAName,"<br/>",
                                    "Persons Injured/killed:",spoints4$injured_killed)) %>%
      addLayersControl(
        baseGroups = c("2013","2014","2015","2016"),
        options = layersControlOptions(collapsed = TRUE)) %>% hideGroup(c("2014","2015","2016")) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

