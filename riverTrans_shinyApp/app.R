#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(mapview)
library(leaflet)
library(leafpop)
library(sf)
library(data.table)
library(viridis)
library(ggplot2)
#library(mapiso)
#library(terra)
library(shiny)
sf_use_s2(FALSE)

ui <- fluidPage(
  titlePanel("River Transportation (beta)"),
  
  sidebarLayout(position='left',
                sidebarPanel('User Interface',
                             width = 3,
                            radioButtons(inputId = "varType",
                                          label = "Select Data to Plot:",
                                          choiceValues = c("Annual_Avg_pct", "Season_Avg_pct"),# "rawVal"),
                                          choiceNames = c("Percentile (annual)", "Percentile (for this time of year)")),# "Raw Value")),
                             radioButtons(inputId = "shippingLoc",
                                          label = "Select Port:",
                                          choiceValues = c(4625, 6124, 5451, 5890),
                                          choiceNames = c("Warehouse X1B", "Manufacturing 21A", 'Manufacturing 27C', 'Headquarters'))
                          ),
                mainPanel('Data Outputs',
                          width = 9,
                          fluidRow(
                            mapviewOutput("myMap")
                          )
                )
  )
)
   

server <- function(input, output) {
  Data = sf::st_read(paste0('./waterways_', "2024-01-05", '.gpkg'))
  Data$Annual_Avg_pct = Data$relVal_tot * 100
  Data$Season_Avg_pct = Data$relVal_seas * 100
  Data$Current_Flow_cfs = Data$rawVal
  plotter_dt = st_drop_geometry(Data)
  waterWaysDb_sf = sf::st_read('./waterWaysAndDistancesCONUS_QandH_sf.gpkg')#subset(sf::st_read('./waterWaysAndDistancesCONUS_QandH_sf.gpkg'),  WTWY_TYPE %in% c(6,8,9))# & !is.na(LENGTH1))
  waterWaysDb_dt = st_drop_geometry(waterWaysDb_sf)
  #  names(st_geometry(waterWaysDb_sf)) = NULL
  
 
  output$myMap <- renderLeaflet({
    thisPal <- turbo(n=10, begin=0, end=1, direction = -1)
    thisCol = input$varType   

    keepSearching = TRUE
    downStreamNodes = as.integer(input$shippingLoc)
    while(keepSearching)  {
      nextNode = which(waterWaysDb_dt$ANODE == waterWaysDb_dt$BNODE[data.table::last(downStreamNodes)])  
      
      if(length(nextNode) == 0) {
        keepSearching = FALSE
      } else  {
        downStreamNodes = c(downStreamNodes, nextNode)
     }
    }
    
    nodes_sf = waterWaysDb_sf[downStreamNodes,]
    mapCenter_sf = st_centroid(nodes_sf[1,])
    mapCenter = as.matrix(st_coordinates(mapCenter_sf))
    latCent = as.numeric(mapCenter[1,2])
    lngCent = as.numeric(mapCenter[1,1])
 

      # dt for pupup graph
    downStreamPlotter = data.table(Distance = cumsum(waterWaysDb_dt$LENGTH[downStreamNodes]) - waterWaysDb_dt$LENGTH[downStreamNodes[1]], downStreamVals = NA)
    thisColNum = which(names(plotter_dt) == thisCol)
    for(i in 1:length(downStreamNodes)) {
      if(waterWaysDb_dt$ID[downStreamNodes[i]] %in% plotter_dt$ID){
        downStreamPlotter$downStreamVals[i] = 
          #          plotter_dt[which(plotter_dt$ID == waterWaysDb_sf$ID[downStreamNodes[i]]), input$column]
          plotter_dt[which(plotter_dt$ID == waterWaysDb_dt$ID[downStreamNodes[i]]), thisColNum]
      }
    }
    
      # ggplot object for popup graph
    xMax = max(downStreamPlotter$Distance)
    lineSpline = as.data.frame(spline(downStreamPlotter$Distance, downStreamPlotter$downStreamVals))
    p2 = ggplot(data=downStreamPlotter, aes(x=Distance, y=downStreamVals), ylim=c(0,100)) +
      ggtitle("Downstream Flow Percentiles")+
      annotate(geom = "rect", xmin = 0, xmax = xMax, ymin = 98, ymax = 100,
               fill = "red3", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = xMax, ymin = 95, ymax = 98,
               fill = "red3", alpha = 0.3) +
      annotate(geom = "rect", xmin = 0, xmax = xMax, ymin = 90, ymax = 95,
               fill = "red3", alpha = 0.1) +
      annotate(geom = "rect", xmin = 0, xmax = xMax, ymin = 10, ymax = 20,
               fill = "red3", alpha = 0.1) +
      annotate(geom = "rect", xmin = 0, xmax = xMax, ymin = 5, ymax = 10,
               fill = "red3", alpha = 0.3) +
      annotate(geom = "rect", xmin = 0, xmax = xMax, ymin = 0, ymax = 5,
               fill = "red3", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = xMax, ymin = 20, ymax = 90,
               fill = "royalblue1", alpha = 0.1) +
      geom_point(shape = 25, size=7, color = 'grey10', stroke=1.4) +
      xlab("Distance Downstream") + ylab("Percentile Flow") +
#      geom_smooth(method = "loess") +
#      geom_line(data = lineSpline, aes(x = x, y = y)) +
      theme_minimal()
    
    mapviewsCombined = 
      mapview(nodes_sf, lwd=4, lty=3, color='purple', alpha= 0.1, legend=FALSE)+
      mapview(Data,
        zcol=thisCol, color=thisPal, lwd = 2, legend.opacity=0.8,layer.name="percentile",
        popup = popupTable(Data, zcol = c("Annual_Avg_pct", "Season_Avg_pct", "Current_Flow_cfs"))
      ) +
      mapview(mapCenter_sf, color='purple', col.regions="yellow2", legend=FALSE,
        popup = popupGraph(list(p2), width=400, height=200))
    mapviewsCombined@map %>% setView(lat=latCent, lng=lngCent, zoom = 5)
  })
  

}

shinyApp(ui, server)
