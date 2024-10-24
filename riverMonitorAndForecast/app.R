#library(mapview)
library(leaflet)
library(leafpop)
library(sf)
sf_use_s2(FALSE)
library(data.table)
library(viridis)
library(ggplot2)
#library(mapiso)
#library(terra)
library(shiny)
library(dataRetrieval)
library(DT)
library(shinycssloaders)
library(shinyalert)
#source("./locationDataExtractor.R")
source("./portfolioTableMaker.R")
source("./locationPlotter.R")
source("./gageRssFeed_recentData.R")

source("./portfolioTablePlotter.R")
source("./gageRssFeed_toTable.R")
source("./quantileCalculator.R")

customerInputTable = data.table::fread("./Data/CustomerOnboardingTemplate.csv")
customerInputTable_sf = sf::st_as_sf(subset(customerInputTable, !is.na(Lat) & !is.na(Lon)),
                                     coords = c("Lon", "Lat"), crs = 4326)
print("customerInputTable_sf")

# pulling NWS data for each location
streamflowTable_ls = gageAPI_f(customerInputTable)  
print("streamflowTable_ls")

# calculating quantiles and previous year values
streamflowQuantiles_ls = quantileCalculator_f(streamflowTable_ls)
print("streamflowQuantiles_ls")


options(spinner.type = 6)

# user interface
ui <- fluidPage(
  titlePanel(title=div(img(src="./CAi2.png", height=60, width=200), "    River Monitor and Forecasts (beta)")),
  
  sidebarLayout(position='left',
                sidebarPanel('User Interface',
                             width = 2,
                             #                             radioButtons(inputId = "varType",
                             #                                          label = "Data Type for Map:",
                             #                                          choiceValues = c(1,2),
                             #                                          choiceNames = c("Percentile (annual)", "Percentile (for this time of year)")),
                             radioButtons(inputId = "shippingLoc",
                                          label = "Location to Plot:",
                                          choiceValues = 1:nrow(customerInputTable),
                                          choiceNames = customerInputTable$Location_Name),
                             actionButton("rssButton", "Pull Live Data")
                ),
                mainPanel(width = 10,
                          fluidRow(
                            plotOutput("locationPlotter"),
                            downloadButton("downloadPlot", "Download Plot"),
                            img(src="./plotLegend.png", height=30, width=100),
                            textOutput("liveDataText")
                          ),
                          hr(),
                          fluidRow(
                            column(6,
                                   withSpinner(leafletOutput("myMap", height = "500px"), type=7)
                            ),
                            column(6,
                                   DT::dataTableOutput("portfolioTable")
                            )
                          ),
                          hr(),
                          hr()
                )
  )
)


server <- function(input, output, session) {
  
  # plots for locations
  output$locationPlotter = renderPlot({
    locationPlotter_f(customerInputRow = customerInputTable[as.numeric(input$shippingLoc), ],
                      streamflowTable = streamflowQuantiles_ls[[as.numeric(input$shippingLoc)]])
  })
  print("locationPlotter")
  # pulling info for tables and location vals
  thisDayVals = data.table(
    Location = customerInputTable$Location_Name, 
    Now = -9999, Last_Year = -9999, Two_Years_Ago = -9999, Percentile = -9999, # Initialize numeric columns with placeholder value
    High = customerInputTable$High_Level_ft, Low = customerInputTable$Low_Level_ft)
  thisRow = which(streamflowQuantiles_ls[[1]]$date == Sys.Date())
  for(quantItem in 1:length(streamflowQuantiles_ls)){
    thisDayVals[quantItem, c(2:5)] = 
      streamflowQuantiles_ls[[quantItem]][thisRow, c("value", "valueLastYear", "valueLast2Year", "Q50")]
  }
  
  # generating table
  output$portfolioTable <- DT::renderDataTable({
    portfolioTablePlotter_f(portfolioTable = thisDayVals)
  })
  
  #### generating the map
  customerInputTable_sf = merge(customerInputTable_sf, thisDayVals, by.x="Location_Name",  by.y="Location")
  
  #  mapDataColNames = c("Annual_Avg_Pct", "Season_Avg_Pct")
  customerInputTableColNames = c("Percentile","unk")  #c("Current_Pct_Annual", "Current_Pct_Season")
  
  output$myMap = renderLeaflet({
    thisPal <- turbo(n=10, begin=0, end=1, direction = -1)
    thatPal <- colorNumeric(palette = thisPal, domain = 1:100)
    
    thisCol = 1  #as.numeric(input$varType)   
    
    # Create a base map
    my_map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -80, lat = 38, zoom = 5)  # Set the initial view
    
    theseDataForPointCols = as.data.frame(customerInputTable_sf[, which(names(customerInputTable_sf) == customerInputTableColNames[thisCol])])[,1]      
    # Add your layers
    my_map %>%
      addCircleMarkers(data = customerInputTable_sf,
                       fillColor = thatPal(theseDataForPointCols),
                       fillOpacity = 0.8, color = 'grey20', weight = 0 ) %>%#,
      #                   popup = customerInputTableColNames) %>%
      addLayersControl(baseGroups = c("providers$Esri.WorldImagery", "StreetMap", "Stamen"), overlayGroups = c("Markers"),
                       position = "bottomleft") %>%
      addLegend(data = customerInputTable_sf,
                position = "topright",
                pal = thatPal,
                values = ~theseDataForPointCols,
                title = "Percentile",
                opacity = 1)
  })
  
  
  
  output$downloadPlot = downloadHandler(
    filename = function(){
      paste0(customerInputTable$Location_Name[as.numeric(input$shippingLoc)], ".png")
    },
    content = function(file){
      png(file)
      locationPlotter_f(customerInputRow = customerInputTable[as.numeric(input$shippingLoc), ],
                        streamflowTable = streamflowQuantiles_ls[[as.numeric(input$shippingLoc)]])
      dev.off()
    }
  ) 
  #Download dataframe
  #  output$downloadData <- downloadHandler(
  #    filename = "waterwaysWithCurrentVals.gpkg",
  #    content = function(filename) {
  #      sf::st_write(Data, filename, row.names = FALSE)
  #    }
  #  )
  
  observeEvent(input$rssButton, {
    thisLoc = as.numeric(input$shippingLoc)
    if(customerInputTable$Use_USGS[thisLoc]) {
      whichIsCurrentConditions = data.table::last(which(!is.na(customerGageData[[thisLoc]]$thisYear)))
      currentConditions = customerGageData[[thisLoc]]$thisYear[whichIsCurrentConditions]
      units = " CFS"
    } else {
      currentConditions = gageRssFeed_recentData_f(customerInputTable = customerInputTable, thisLoc = thisLoc)
      units = " ft"
    }
    shinyalert(title = paste0(customerInputTable$Location_Name[thisLoc]),
               text = paste0("Current conditions are ", currentConditions, units),
               type="info",
               closeOnClickOutside = TRUE
    )
  })
  
  
}

shinyApp(ui, server)