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
source("./twoByTwoPlotter.R")
library(mapview)
library(leaflet)
library(viridis)
library(ggplot2)
library(gridExtra)
library(ggrepel)
#library(plotly)
customerInputTable = data.table::fread("./Data/Customer Onboarding Information_NuveenAus_Jan2024.csv", skip=1)

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
        h6(em("applies to all tabs")),
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
        ),
        hr(),
        h6(em("applies to Watershed View only")),
        radioButtons(inputId = "stressClass",
                     label = "Stress Class:",
                     choiceValues = c(2,1,4,3),
                     choiceNames = c("Local (drought)", "Local (typical)", "L + Regional (drought)", "L + Regional (typical)")
        ),
        
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
                   h4("Highlights:"),
                   textOutput("waterIndexText")
                 ),
                 fluidRow(
                          column(6,
                                 h4("Index Value Projections", style = "text-align: center;"),
                                 plotOutput("indexPlot")
                          ),
                          column(6,
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
                  helpText('Page is under development'),
                  fluidRow(
                    class="myRow2",
                    column(7,
                           mapviewOutput("pfMap", height = "400px")),
                    column(5,
                           h4('Index Values by Decade', style = "text-align: center;"),
                           tableOutput("portfolioTable"))
                  ),
                  hr(),
                  fluidRow(
                    class="myrow3",
                    column(8,
                           plotOutput("twoByTwoPlot")
                           )
                  )
          ),
          # Third Tab
          tabPanel("Location Analysis",
                  helpText('Page is under development'))
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  customerInputTable = data.table::fread("./Data/Customer Onboarding Information_NuveenAus_Jan2024.csv", skip=1)
  climateArray = readRDS("./Data/NuveenAus_Dec2023_regional_rawValues.rds")
  indexArray = readRDS("./Data/NuveenAus_Dec2023_regional_waterIndex.rds")
  graceHistorical = data.table::fread("./Data/NuveenAus_Dec2023_graceHistorical.csv")
  basinSummary = data.table::fread("./Data/NuveenAus_Dec2023_regional_hydroBasins_wIndex.csv")
  basinShapes = sf::st_read("./Data/NuveenAus_Dec2023_hydroBasins_shapesOnly.shp")
          
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
  
  # Index values table
  # dimensions: [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
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
  output$indexPlot <- renderPlot({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    indexValuesToPlot = c(4,2,8,6)   
    indexPlotter_f(waterIndexDataPlot = indexArray, thisLoc = thisLoc, thisScen = thisScen, indexValuesToPlot = indexValuesToPlot, thisValType = thisValType)
  })

  # water risk index text
  output$waterIndexText = renderText({
    basinSub = basinSummary[as.numeric(graceSub()[[2]]), ]
    thisScen = as.numeric(input$scenario)
    #thisValType = as.numeric(input$valType)
    thisIndexVal = c(which(basinSub[,c("currentRatio_B", "currentRatio_A", "currentRatio_D", "currentRatio_C")] > 0.95),5)
    thisIndexExpl = c("A (local supply is consistently sufficient to meet demand)", "B (local supply is usually but not always sufficient to meet demand)",
                      "C (local + regional supply is consistently sufficient to meet demand)", "D (local + regional supply is usually but not always sufficient to meet demand)",
                      "E (extraction of non-renewable water resources is necessary to meet current demand)")[thisIndexVal[1]]
    if(thisScen == 1){
      theseBadTrends = which(basinSub[,c("trendLow_Deficit_B", "trendLow_Deficit_A", "trendLow_Deficit_D", "trendLow_Deficit_C")] < 0)
      theseGoodTrends = which(basinSub[,c("trendLow_Deficit_B", "trendLow_Deficit_A", "trendLow_Deficit_D", "trendLow_Deficit_C")] > 0)
    }
    if(thisScen == 2){
      theseBadTrends = which(basinSub[,c("trendMed_Deficit_B", "trendMed_Deficit_A", "trendMed_Deficit_D", "trendMed_Deficit_C")] < 0)
      theseGoodTrends = which(basinSub[,c("trendMed_Deficit_B", "trendMed_Deficit_A", "trendMed_Deficit_D", "trendMed_Deficit_C")] > 0)
    }
    if(thisScen == 3){
      theseBadTrends = which(basinSub[,c("trendHigh_Deficit_B", "trendHigh_Deficit_A", "trendHigh_Deficit_D", "trendHigh_Deficit_C")] < 0)
      theseGoodTrends = which(basinSub[,c("trendHigh_Deficit_B", "trendHigh_Deficit_A", "trendHigh_Deficit_D", "trendHigh_Deficit_C")] > 0)
    }
    classExpls = c("locally during dry years", "locally during typical years", "regionally during dry years", "regionally during typical years")
    if(length(theseBadTrends) == 0)  {myBadTrends = "There are no significant projected increases in water stress locally or regionally. "}
    if(length(theseBadTrends) == 1)  {myBadTrends = paste0("There are significant projected increases in stress ", classExpls[theseBadTrends], ". ")}
    if(length(theseBadTrends) > 1)  {myBadTrends = paste0("There are significant projected increases in stress ", paste(classExpls[theseBadTrends], collapse=", and "), ". ")}
    if(length(theseGoodTrends) == 0)  {myGoodTrends = "There are no significant projected decreases in water stress locally or regionally. "}
    if(length(theseGoodTrends) == 1)  {myGoodTrends = paste0("There are significant projected decreases in stress ", classExpls[theseBadTrends], ". ")}
    if(length(theseGoodTrends) > 1)  {myGoodTrends = paste0("There are significant projected decreases in stress ", paste(classExpls[theseBadTrends], collapse=", and "), ". ")}

    myText = paste0("Watershed is classified as class ", thisIndexExpl, ". ")
    paste0(myText, myBadTrends, myGoodTrends)
  })
  
  # climate plotter
  output$climatePlot <- renderPlot({
     climatePlotter_f(climateDataPlot = climateArray, thisLoc = as.numeric(graceSub()[[2]]), thisScen = as.numeric(input$scenario))
  })
  
  # hydroclimate text
  output$climateText = renderText({
    basinSub = basinSummary[as.numeric(graceSub()[[2]]), ]
    theseCurrentVals = basinSub[, c("currentPrecip_avg", "currentPET_avg", "currentStreamflow_avg")]
    
    thisScen = as.numeric(input$scenario)
    if(thisScen == 1){
      precipTrends = 3; if(!is.na(basinSub$trendLow_Precip_avg))  {
        if(basinSub$trendLow_Precip_avg > 0){precipTrends = 1}; if(basinSub$trendLow_Precip_avg < 0){precipTrends = 2}
      }
      petTrends = 3;    if(!is.na(basinSub$trendLow_PET_avg)) {
        if(basinSub$trendLow_PET_avg > 0)   {petTrends = 1};    if(basinSub$trendLow_PET_avg < 0)   {petTrends = 2}
      }
      strmflTrends = 3; if(!is.na(basinSub$trendLow_Streamflow_avg)) {
        if(basinSub$trendLow_Streamflow_avg > 0){strmflTrends = 1}; if(basinSub$trendLow_Streamflow_avg < 0){strmflTrends = 2}
      }
    }
    if(thisScen == 2){
      precipTrends = 3; if(!is.na(basinSub$trendMed_Precip_avg)) {
        if(basinSub$trendMed_Precip_avg > 0){precipTrends = 1}; if(basinSub$trendMed_Precip_avg < 0){precipTrends = 2}
      }
      petTrends = 3;    if(!is.na(basinSub$trendMed_PET_avg)) {
        if(basinSub$trendMed_PET_avg > 0)   {petTrends = 1};    if(basinSub$trendMed_PET_avg < 0)   {petTrends = 2}
      }
      strmflTrends = 3; if(!is.na(basinSub$trendMed_Streamflow_avg)) {
        if(basinSub$trendMed_Streamflow_avg > 0){strmflTrends = 1}; if(basinSub$trendMed_Streamflow_avg < 0){strmflTrends = 2}
      }
    }
    if(thisScen == 3){
      precipTrends = 3; if(!is.na(basinSub$trendHigh_Precip_avg)) {
        if(basinSub$trendHigh_Precip_avg > 0) {precipTrends = 1}; if(basinSub$trendHigh_Precip_avg < 0){precipTrends = 2}
      }
      petTrends = 3;    if(!is.na(basinSub$trendHigh_PET_avg))  {
        if(basinSub$trendHigh_PET_avg > 0)   {petTrends = 1};    if(basinSub$trendHigh_PET_avg < 0)   {petTrends = 2}
      }
      strmflTrends = 3; if(!is.na(basinSub$trendHigh_Streamflow_avg))  {
        if(basinSub$trendHigh_Streamflow_avg > 0){strmflTrends = 1}; if(basinSub$trendHigh_Streamflow_avg < 0){strmflTrends = 2}
      }
    }
    trendText = c("to increase significantly. ", "to decrease significantly. ", "not to change significantly. ")
    myText = paste0("Current annual precipitation is around ", round(100 * theseCurrentVals[,1] / 990, 0), "% of global average and is projected ", trendText[precipTrends],
                    "Current annual potential evapotranspiration (PET) is around ", round(100 * theseCurrentVals[,2] / 1200, 0), "% of global average and is projected ", trendText[petTrends],
                    "Potential regional subsidies (e.g., streamflow) are projected ", trendText[strmflTrends]) 
    myText
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
      ) 
    myMap@map
  })
  
  output$portfolioTable <- renderTable({
    locationTableMaker_f(customerInputTable, basinSummary, scenario = as.numeric(input$scenario), valType = as.numeric(input$valType))
  })
  
  # two by two plotter
  output$twoByTwoPlot <- renderPlot({
    twoByTwoPlotter_f(basinSummary = basinSummary, customerInputTable = customerInputTable, thisScen = as.numeric(input$scenario), thisStressClass =  as.numeric(input$stressClass))
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
