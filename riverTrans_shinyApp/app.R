#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(sf)
library(data.table)
library(maps)


ui <- fluidPage(
  titlePanel("River Transportation (beta)"),
  
  sidebarLayout(position='left',
                sidebarPanel('User Interface',
                             width = 3,
                             sliderInput("lonRng", "Longitude", value = c(-126,-65), min=-130, max=-55),
                             sliderInput("latRng", "Latitude", value = c(24,50), min=24, max=80),
                             radioButtons(inputId = "column",
                                          label = "Select Data to Plot:",
                                          choiceValues = c("relVal_tot", "relVal_seas"),# "rawVal"),
                                          choiceNames = c("Percentile", "Percentile for Season")),# "Raw Value")),
                             radioButtons(inputId = "shippingLoc",
                                          label = "Select Port:",
                                          choiceValues = c(4397, 4646, 4394, 4515),
                                          choiceNames = c("Warehouse X1B", "Manufacturing 21A", 'Manufacturing 27C', 'Headquarters'))
                          ),
                mainPanel('Data Outputs',
                          width = 9,
                          fluidRow(
                            plotOutput("myMap", height = 800),
#                            splitLayout(style = "border: 1px solid silver:", cellWidths = c(300,200),
                             plotOutput("myPlot", click = "plot_click"),
                             tableOutput('HitSpots')
#                                        )
                          )
                )
  )
)
                            
#  plotOutput("myMap"),
#  plotOutput("myPlot", click = "plot_click"),
#  plotOutput('myPlot', brush = brushOpts(id = 'plot_brush')),
#  ,
# verbatimTextOutput("info"))

server <- function(input, output) {
  plotter_sf = sf::st_read(paste0('./waterways_', "2023-09-18", '.gpkg'))
  plotter_dt = st_drop_geometry(plotter_sf)
  waterWaysDb_sf = sf::st_read('./waterWaysAndDistancesCONUS_QandH_sf.gpkg')
  waterWaysDb_dt = st_drop_geometry(waterWaysDb_sf)
  provinces10 = st_as_sf(st_read('./ne_10m_admin_1_states_provinces.shp'))
  

  downStreamTable = reactive({
    keepSearching = TRUE
    downStreamNodes = as.numeric(input$shippingLoc)
    
    downStreamLengths = waterWaysDb_dt$LENGTH[downStreamNodes]
    while(keepSearching)  {
      nextNode = which(waterWaysDb_dt$ANODE == waterWaysDb_dt$BNODE[data.table::last(downStreamNodes)])  
                            
      if(length(nextNode) == 0) {
        keepSearching = FALSE
      } else  {
        downStreamNodes = c(downStreamNodes, nextNode)
        downStreamLengths = c(downStreamLengths, data.table::last(downStreamLengths) + 
                                                                    waterWaysDb_dt$LENGTH[nextNode])
      }
    }
    
   return(data.frame(Node = downStreamNodes, Length = downStreamLengths))
  })
  
 
     
  output$myMap <- renderPlot({
    ggplot(data = plotter_sf) +
#      geom_sf(data = availableGages_Q_sf, size = 1, color='grey85') +
      borders('world', xlim=c(-100, -80), ylim=c(25, 51), 
              colour='gray90', size=.2, fill='grey80')	+
      geom_sf(data=subset(provinces10, geonunit=='United States of America'),
              colour='grey80', fill='grey95') +
      geom_sf(data = subset(waterWaysDb_sf, WTWY_TYPE %in% c(6,8,9) & !is.na(LENGTH1)), 
              color = 'black', linetype = '11', linewidth = 0.7) +
      #  geom_sf(data = plotter_sf, aes(colour = relVal_seas), linewidth = 1.4) +
      geom_sf(data = plotter_sf, aes_string(colour = input$column), linewidth = 1.4) +
      geom_sf(data = waterWaysDb_sf[downStreamTable()$Node, ], color='royalblue1', linetype = '12', linewidth = 1.1) +
      #  geom_sf(data = plotter_sf, aes(colour = rawVal), linewidth = 1.4) +
      scale_colour_viridis_c(
        limits = c(0,1),
        labels = scales::percent, 
        name = 'Current Percentile',
        option='plasma') +
      theme(text = element_text(size = 12), legend.position = 'bottom') +
      ylab(NULL)+
      xlab(NULL)+
      coord_sf(xlim = input$lonRng, ylim = input$latRng, expand = FALSE)
    #  scale_color_gradient(trans = 'log')
  }, res = 96 * 1.0)
  
  
  tablePlotterF = reactive({
    thisCol = which(names(plotter_dt) == input$column)
    tablePlotter = downStreamTable()
    tablePlotter$downStreamVals = NA
    for(i in 1:nrow(tablePlotter)) {
      if(waterWaysDb_dt$ID[tablePlotter$Node[i]] %in% plotter_dt$ID){
        tablePlotter$downStreamVals[i] = 
          #          plotter_dt[which(plotter_dt$ID == waterWaysDb_sf$ID[downStreamNodes[i]]), input$column]
          plotter_dt[which(plotter_dt$ID == waterWaysDb_sf$ID[tablePlotter$Node[i]]), thisCol]
      }
    }
    tablePlotter$plotValPct = tablePlotter$downStreamVals * 100
    return(tablePlotter)
  })
    

  output$myPlot = renderPlot({
     xMax = max(tablePlotterF()$Length)
    ggplot(data = tablePlotterF(), aes(x=Length, y=plotValPct)) +
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
      theme_minimal()+
      theme(text = element_text(size = 12)) +
      ylim(0,100) +
      labs(x = "Distance from Location (km)", y="Current Percentile")
  })
  
  element_text(size = 20)
   
  hit = reactive({nearPoints(tablePlotterF(), input$plot_click)})


  
  output$HitSpots = renderTable({
    hit()
  })  
  

}

shinyApp(ui, server)