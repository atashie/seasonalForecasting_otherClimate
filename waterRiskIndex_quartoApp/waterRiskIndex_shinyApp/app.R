#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("./gracePlotter.R")
source("./indexPlotter.R")
source("./climatePlotter.R")
source("./locationTableMaker.R")
source("./locationSummaryTableMaker.R")
source("./twoByTwoPlotter.R")
source("./towByTwoTables.R")
source("./twoByTwoSummaryText.R")
source("./waterIndextText.R")
source("./climateText.R")
source("./locationSpecificReanalysis.R")
source("./waterSupplyProjectionsPlotter.R")
library(mapview)
library(leaflet)
library(viridis)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(DT) # for interactive tables via datatable()
#library(plotly)
customerInputTable = data.table::fread("./Data/Customer Onboarding Information_NuveenCali_Jan2024.csv", skip=1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .myRow1 {
          height: 350px;
        }
        .myRow2 {
          height: 400px;
        }
        .myRow3 {
          height: 500px;
        }

      ")
    )
  ),
  
  
  # Application title
  titlePanel(title=div(img(src="./CAi2.png", height=60, width=200), "Water Risk Index (beta)")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
      sidebarPanel(
#        fileInput("file", "Upload CSV file"),
        width=2,
        selectInput("location", label = "Choose a Location", choices = customerInputTable$Location_Name),  
        radioButtons(inputId = "scenario",
                      label = "Climate Scenario:",
                      choiceValues = c(2,1,3),
                      choiceNames = c("Middle of the Road", "Low Emissions", "High Emissions"),
        ),
        radioButtons(inputId = "valType",
                      label = "Units:",
                      choiceValues = c(2,1),
                      choiceNames = c("mm", "ratio")
        )
     ),
#    ),
    
 
        # Structure of the page
      mainPanel(
        tabsetPanel(type="tabs",
            # First Tab
          tabPanel("Watershed View",
                  fluidRow(
                    class="myRow2",
                    column(7,
                          mapviewOutput("wsMap", height = "400px")),
                    column(5,
                           fluidRow(
                              class="myRow2",
                              h4("Recent Changes in Watershed Storage", style = "text-align: center;"),
                              plotOutput("gracePlot", height = "200px"),
                              hr(),
                              h4("Watershed Summary Information:"),
                              div(style = "height: 150px;", textOutput("summaryText")))
                    )
                 ),
                 hr(),
                 fluidRow(
                   h3("Water Risk Index", style = "text-align: center;"),
                 ),
                 fluidRow(
                          column(6,
                                 h4("Entire Watershed: Index Value Projections", style = "text-align: center;"),
                                 plotOutput("watershedIndexPlot")
                          ),
                          column(6,
                                 h4("User's Fields: Index Value Projections", style = "text-align: center;"),
                                 plotOutput("usersFieldsIndexPlot")
                          ),
                 ),
                 fluidRow(
                         column(5,
                                hr(),
                                h4("Highlights:"),
                                textOutput("waterIndexText")
                                ),
                         column(7,
                                h4('Index Values by Decade', style = "text-align: center;"),
                                tableOutput("indexTable"))
                 ), 
                 fluidRow(
                    hr(),
                    h3("Hydroclimate Projections", style = "text-align: center;"),
                    column(8, plotOutput("climatePlot")),
                    column(4, 
                           h4('Highlights:'),
                           textOutput("climateText"))
                  )
          ),
          # Second Tab
          tabPanel("Portfolio View",
                  fluidRow(
                    class="myRow2",
                    column(7,
                           selectInput(inputId = "stressClass", label = "Stress Class:",
                                        c("Local (drought)" = 2, "Local (typical)" = 1, 
                                          "L + Regional (drought)" = 4, "L + Regional (typical)" = 3)
                           ),
                           mapviewOutput("pfMap", height = "400px")),
                    column(5,
                           h4('Index Values by Decade', style = "text-align: center;"),
                           DT::dataTableOutput("portfolioSummaryTable"))
                  ),
                  hr(),
                  fluidRow(
#                    class="myrow3",
                    hr(),
                    hr(),
                    h3("2x2 Hazard Overview:"),
                    column(8,
                           plotOutput("twoByTwoPlot", height="550px")
                           ),
                    column(4,
                           sliderInput("twoByTwosToPlot", "Locations to Plot:",
                                       min=1, max=nrow(customerInputTable), c(1, min(nrow(customerInputTable), 20))),
                           h4('Highlights:'),
                           textOutput("portfolioHighlights1"),
                           hr(),
                           textOutput("portfolioHighlights2"),
                           textOutput("portfolioHighlights3")
                    )
                  ),
                  fluidRow(
                    class="myrow3",
                    column(12,
                      h4('Index Values by Decade', style = "text-align: center;"),
                      DT::dataTableOutput("portfolioTable")
                    )
                  )
          ),
          # Third Tab
          tabPanel("Location Analysis",
                  column(3,
                         selectInput("irrigationMethod", "Irrigation Method:",
                                     c("Use Current Inputs" = 999, "Sprinkler" = 0.85, "Drip" = 0.95, "Surface" = 0.65)),
                         selectInput("detentionPond", "Rainfall Detention Ponds:",
                                     c("Use Current Inputs" = 999, "No" = 0.5, "Yes" = 0))
                         ),
                  column(3,
                         sliderInput("irrigationAllotment", "Adjust Allotment for Irrigation (%):",
                                      min = -99, max = 100, 0),
                         numericInput("marAllotment", "Allotment for MAR (hectare meters):",
                                      value=0, min = 0, max = 1000)
                         ),
                  column(3,
                         selectInput("groundwaterAccess", "Groundwater Access:",
                                     c("Use Current Inputs" = 999, "Yes" = 1, "No" = 0)),
                         selectInput("soilMoistureAccess", "Crops in Ground or Potted:",
                                     c("Use Current Inputs" = 999, "In Ground" = 1, "Potted" = 0))
                         ),
                  column(3,
                         sliderInput("growArea", "Adjust Irrigated Crop Area (%):",
                                      min = -99, max = 100, 0),
                         selectInput("plantHybrid", "Plant Hybrid:",
                                     c( "User Current Inputs" = 1, "Drought Tolerant" = 0.85, "Drought Intolerant" = 1.15))
                        ),
                  fluidRow(
                    column(6,
                           h4("Index Value Projections with Updates", style = "text-align: center;"),
                           plotOutput("indexPlotUpdates")
                    ),
                    column(6,
                           h4('Index Values by Decade with Updates', style = "text-align: center;"),
                           tableOutput("indexTableUpdates"))
                  ),
                  hr(), 
                  fluidRow(
                    column(6,
                           h4('Watershed Summary:'),
                           textOutput("waterSupplyProjectionsSummary")
                    ),
                    column(6,
                           h4("Projected Change in Total Water Storage"),
                           plotOutput("waterSupplyProjections")
                    )
                  ),
                  fluidRow(
                    class="myRow2",
 #                   column(7,
  #                         mapviewOutput("pfMap", height = "400px")),
                    column(12,
                           h3('Coming Soon: Cost Analysis', style = "text-align: center;"),
                           textOutput("locationIndexText"),
                           plotOutput("locationIndexPlot"))
                  )
                  
          )
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  customerInputTable = data.table::fread("./Data/Customer Onboarding Information_NuveenCali_Jan2024.csv", skip=1)
  customerInputSf = sf::st_as_sf(customerInputTable, coords=c("Longitude", "Latitude"), crs = 4326)
  climateArray = readRDS("./Data/NuveenCali_Jan2024_regional_rawValues.rds")
  indexArray = readRDS("./Data/NuveenCali_Jan2024_local_waterIndex.rds")
  watershedIndexArray = readRDS("./Data/NuveenCali_Jan2024_regional_waterIndex.rds")
  graceHistorical = data.table::fread("./Data/NuveenCali_Jan2024_graceHistorical.csv")
  basinSummary = data.table::fread("./Data/NuveenCali_Jan2024_regional_hydroBasins_wIndex.csv")
  basinShapes = sf::st_read("./Data/NuveenCali_Jan2024_hydroBasins_shapesOnly.shp")
          
  graceSub <- reactive({
    list(subset(graceHistorical, Location == input$location), which(customerInputTable$Location_Name == input$location))
  })

################################
# outputs for watershed view tab
  output$wsMap = renderLeaflet({
    Watershed_Boundary = basinShapes[as.numeric(graceSub()[[2]]), ]
    mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap.DE","OpenTopoMap"))
    myMap =
       mapview(
        Watershed_Boundary,
        color = "green2", lwd = 3, alpha.regions = 0.0, col.regions="grey90", legend=FALSE
        ) 
     myMap@map
  })
  

    
  # grace plotter
  output$gracePlot <- renderPlot({
    plotData = graceSub()[[1]]
    gracePlotter_f(plotData)
  })
  
  # summary text
  output$summaryText = renderText({
    basinSub = basinSummary[as.numeric(graceSub()[[2]]), ]
    incOrDecGrace = ifelse(basinSub$recentHistoricSlope >= 0, "increase", "decrease") 
    myText = paste0("Recent historical storage ", incOrDecGrace, " of ", round(basinSub$recentHistoricSlope, 0), " mm per year, or ",
                    round(basinSub$rescaledRecentHistoricSlope, 0), " mm per irrigated acre per year. ",
                    "An estimated ", round(basinSub$thisFrcAreaUnderCult*100, 0), "% of the watershed is devoted to agriculture, with ",
                    round(basinSub$thisFrcCultAreaWthIrr*100, 0), "% irrigated. And the average water needs for crops in this watershed are ",
                    round(basinSub$thisWplant, 0), " mm per year.")
    myText
  })

  # water risk index plotter
  output$usersFieldsIndexPlot <- renderPlot({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    indexPlotter_f(waterIndexDataPlot = indexArray, thisLoc = thisLoc, thisScen = thisScen, indexValuesToPlot = c(4,2,8,6), thisValType = thisValType)
  })

  output$watershedIndexPlot <- renderPlot({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    indexPlotter_f(waterIndexDataPlot = watershedIndexArray, thisLoc = thisLoc, thisScen = thisScen, indexValuesToPlot = c(4,2,8,6), thisValType = thisValType)
  })
  
  
  # Index values table
  output$indexTable <- renderTable({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    myTable = data.table::data.table(
      "Decade" = factor(seq(2010, 2090, 10)), 
      "Local (drought)" =        indexArray[thisLoc, , 4, thisScen, 4, thisValType],
      "Local (average)" =        indexArray[thisLoc, , 4, thisScen, 2, thisValType],
      "L + Regional (drought)" = indexArray[thisLoc, , 4, thisScen, 8, thisValType],
      "L + Regional (average)" = indexArray[thisLoc, , 4, thisScen, 6, thisValType]
    )
    myTable
  })
  
  # water risk index text
  output$waterIndexText = renderText({
    thisLoc = as.numeric(graceSub()[[2]])
    waterIndexText_f(basinSummary = basinSummary, scenario = as.numeric(input$scenario), thisLoc = thisLoc)
  })
  
  # climate plotter
  output$climatePlot <- renderPlot({
     climatePlotter_f(climateDataPlot = climateArray, thisLoc = as.numeric(graceSub()[[2]]), thisScen = as.numeric(input$scenario))
  })
  
  # hydroclimate text
  output$climateText = renderText({
    climateText_f(basinSummary = basinSummary, scenario = as.numeric(input$scenario), thisLoc = as.numeric(graceSub()[[2]]))
  })

###################################  
    #outputs for portfolio view tab
  Watershed_Boundaries = cbind(customerInputTable$Location_Name, basinShapes, basinSummary)
  
  thisPal <- colorRampPalette(c("red3", "white", "blue3"))
  colorScale <- colorNumeric(palette = thisPal, domain = c(0, 2))
  
  output$pfMap = renderLeaflet({
    plotThisCol = c("currentRatio_A", "currentRatio_B", "currentRatio_C", "currentRatio_D")[as.numeric(input$stressClass)]
    watershed_plotter = Watershed_Boundaries[,plotThisCol]
    mapviewOptions(basemaps = c("OpenStreetMap.DE","Esri.WorldImagery", "Esri.WorldShadedRelief", "OpenTopoMap"))
    myMap =
      mapview(
        watershed_plotter, at = seq(0,2,0.2), col.regions = thisPal, 
        color = 'grey20', lwd = 1, alpha.regions = 0.5, legend=TRUE, layer.name="Current Index Value"
      ) +
      mapview(customerInputSf, alpha = 0.5, cex = 3.75, col.regions="forestgreen", legend=FALSE)
    myMap@map
  })

  output$portfolioSummaryTable <- DT::renderDataTable({
    locationSummaryTableMaker_f(customerInputTable, basinSummary, scenario = as.numeric(input$scenario))
  })
  
  output$portfolioTable <- DT::renderDataTable({
    locationTableMaker_f(customerInputTable, basinSummary, scenario = as.numeric(input$scenario), valType = as.numeric(input$valType))
  })
  
  # generating two by two table
  twoByTwoTables = reactive({
    twoByTwoTables_f(basinSummary = basinSummary, customerInputTable = customerInputTable, thisScen = as.numeric(input$scenario), thisStressClass =  as.numeric(input$stressClass))
  })
    
  # two by two plotter
  output$twoByTwoPlot <- renderPlot({
    twoByTwoPlotter_f(twoByTwoTables = twoByTwoTables(), as.numeric(input$twoByTwosToPlot))
  })
  
  # two by two highlights
  output$portfolioHighlights1 = renderText({
    twoByTwoSummaryText_f(twoByTwoTables = twoByTwoTables(), whichText=1)
  })
  output$portfolioHighlights2 = renderText({
    twoByTwoSummaryText_f(twoByTwoTables = twoByTwoTables(), whichText=2)
  })
  
##################################################################
  # outputs for location analysis tab
  
locationSpecificWRIcalcs = reactive({
  waterIndexCalculations_locationSpecific_f(
    customerTable_input = customerInputTable,
    thisRow = as.numeric(graceSub()[[2]]),
    hydroBasins = basinSummary,
    climateData = climateArray,
    irrigationMethod = as.numeric(input$irrigationMethod),
    detentionPonds = as.numeric(input$detentionPond),
    irrigatedCropAreaPctChng = as.numeric(input$growArea),
    irrigationAllotments = as.numeric(input$irrigationAllotment),
    allotmentForMar = as.numeric(input$marAllotment),
    groundwaterAccess = as.numeric(input$groundwaterAccess),
    soilMoistureAccess = as.numeric(input$soilMoistureAccess),
    plantHybrid = as.numeric(input$plantHybrid)
  )
})
 
#  output$locationIndexText = renderText({
#    paste0(locationSpecificWRIcalcs())
#  })
  
  output$indexPlotUpdates <- renderPlot({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    newIndexInfo = locationSpecificWRIcalcs()
    thisArray = newIndexInfo[[1]]
#    basinSummary = newIndexInfo[[2]]
    indexPlotter_f(waterIndexDataPlot = thisArray, 
                   indexValuesToPlot = c(4,2,8,6), thisValType = thisValType, thisLoc = thisLoc, thisScen = thisScen)
  })

  # Index values table
  output$indexTableUpdates <- renderTable({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    thisArray = locationSpecificWRIcalcs()[[1]]
    myTable = data.table::data.table(
      "Decade" = factor(seq(2010, 2090, 10)), 
      "Local (drought)" =        thisArray[thisLoc, , 4, thisScen, 4, thisValType],
      "Local (average)" =        thisArray[thisLoc, , 4, thisScen, 2, thisValType],
      "L + Regional (drought)" = thisArray[thisLoc, , 4, thisScen, 8, thisValType],
      "L + Regional (average)" = thisArray[thisLoc, , 4, thisScen, 6, thisValType]
    )
    myTable
  })

  # updated water risk index text gen
  output$waterSupplyProjectionsSummary <- renderText({
    thisLoc = as.numeric(graceSub()[[2]])
    waterIndexText_f(basinSummary = locationSpecificWRIcalcs()[[2]], scenario = as.numeric(input$scenario), thisLoc = thisLoc)
  })
  
  output$waterSupplyProjections <- renderPlot({
    adjustedOutputs = locationSpecificWRIcalcs()
    waterSupplyProjectionsPlotter_f(waterIndexDataPlot = adjustedOutputs[[1]], basinSummary = adjustedOutputs[[2]], 
        groundwaterAccess = as.numeric(input$groundwaterAccess),
        thisLoc =  as.numeric(graceSub()[[2]]),
        thisScen = as.numeric(input$scenario)
     )
  })
  
 
  
  
#  output$hoverInfo <- renderPrint({
 #   thisPoint = nearPoints(myTable, input$plotHover, allRows = FALSE)
  #  paste0("thisVal is ", thisPoint$Current_value, " and ", thisPoint$Trajectory)
#  })
  
#  paste("mpg:", hover$mpg, "<br>",
 #       "wt:", hover$wt)
  
  
####################################
    #outputs for location analysis tab
}

# Run the application 
shinyApp(ui = ui, server = server)
