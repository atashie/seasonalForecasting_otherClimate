####### user needs to update:
updateDate = "11-15-2024"

####### may need to be updated if new basins / categories
# Read in static table w/ hard-coded path
staticTable <- data.table::fread("./calculatedVals/basinLevelValues.csv")

# Hard-coded categories
categoryNames_northernAtlantic = c("Cat-0","Cat-1","Cat-2","Cat-3","Cat-4")
categoryNames_westernPacific = c("Cat-0","Cat-1","Cat-2","Cat-3", "Cat-4")
categoryNames_indianOcean = c("Cat-0","Cat-1","Cat-2")#,"Cat-3", "Cat-4")
categoryNames_indianOcean_SW = c("Cat-0","Cat-1","Cat-2")#,"Cat-3", "Cat-4")
categoryNames_indianOcean_SE = c("Cat-0","Cat-1","Cat-2")#,"Cat-3", "Cat-4")
categoryNames_southPacific = c("Cat-0","Cat-1","Cat-2")#,"Cat-3", "Cat-4")

# Hard-coded names
seasonNames_northernAtlantic = unique(subset(staticTable, basin == "north_atlantic")$seasonalFilter) # northern atlantic
seasonNames_westernPacific = unique(subset(staticTable, basin == "western_pacific")$seasonalFilter) # western pacific
seasonNames_indianOcean = unique(subset(staticTable, basin == "indian_ocean")$seasonalFilter) # indian ocean
seasonNames_indianOcean_SW = unique(subset(staticTable, basin == "South-West indian_ocean")$seasonalFilter) # SW indian ocean
seasonNames_indianOcean_SE = unique(subset(staticTable, basin == "South-East indian_ocean")$seasonalFilter) # SE indian ocean
seasonNames_southPacific = unique(subset(staticTable, basin == "South-Pacific")$seasonalFilter) # South Pacific


######## Following section shoudl not need to be updated

# Load necessary libraries
library(shiny)              # Ensure Shiny is loaded
library(dplyr)              # Load dplyr for data manipulation
library(mapview)
library(leaflet)
library(leafpop)
library(sf)
sf_use_s2(FALSE)
library(viridis)
library(shinycssloaders)
library(shinythemes)
library(raster)             # Ensure raster is loaded
# Uncomment other libraries if needed
# library(data.table)
# library(ggplot2)
# library(dataRetrieval)
# library(DT)

source("./addLegend_decreasing_f.R")

# Define UI
options(spinner.type = 6) # spinner for map loading
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(
    div(
      img(src = "./CAi2.png", height = 60, width = 200),
      paste0("           Hurricane Forecasts - Updated on ", updateDate) 
    )
  ),
  ############## User Interface
  sidebarLayout(
    position = 'left',
    sidebarPanel(
      'User Interface',
      width = 2,
      selectInput(
        inputId = "basin",
        label = "Basin:",
        choices = c("North Atlantic" = 1, "Western Pacific" = 2, 
                    "Indian Ocean" = 3, "SW Indian Ocean" = 4, 
                    "SE Indian Ocean" = 5, "South Pacific" = 6),
        selected = 1  # Optional: set default
      ),
      uiOutput("seasonalFilter"),      # Dynamic UI for Seasonal Filter
      uiOutput("hurricaneCategory"),      # Dynamic UI for Hurricane Category 
      selectInput(
        inputId = "dataType",
        label = "Data:",
        choices = c("Forecast" = 1, "Climatology" = 2, "Difference" = 3),
        selected = 1  # Optional: set default
      )
    ),
    ########################## Outputs
    mainPanel(
      headerPanel("Interactive Map"),
      fluidRow(
        withSpinner(leafletOutput("map"), type = 7)
      ),
      # Display clicked value
      verbatimTextOutput("click_info"),
      # Spacer (Optional for Better Layout)
      br(), # spacer
#      textOutput("sampleText"),
      fluidRow(
        column(12, h3("Accumulated Cyclone Energy (kt²) over Remainder of Forecast Period")
        )
      ),
      fluidRow(
        # Climatology Prediction Column
        column(
          6,
          div(
            style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px; height: 100%;",
            tags$small("Climatology", style = "color: #333333;"),
            tags$h2(textOutput("climatology_energy"), style = "margin-top: 5px; color: #333333;")
          )
        ),
        # ClimateAi Forecast Column
        column(
          6,
          div(
            style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px; height: 100%;",
            tags$small("ClimateAi Forecast", style = "color: #333333;"),
            tags$h2(textOutput("forecast_energy"), style = "margin-top: 5px; color: #333333;")
          )
        )
      ),
      br(),
      # Header for Expected Number of Storms
      fluidRow(
        column(12, h3("Expected Number of Storms Making Landfall in Basin for Remainder of Season"))
      ),
      # Subheader for Climatology
      fluidRow(column(12, h4("Climatology"))),
      fluidRow(
        column(4,
               div(
                 style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; height: 100%;",
                 tags$small("All Storms", style = "color: #333333;"),
                 tags$h2(textOutput("climatology_allStorms"), style = "margin-top: 5px; color: #333333;")
               )
        ),
        column(4,
               div(
                 style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; height: 100%;",
                 tags$small("Storms with MSW > 64 kt", style = "color: #333333;"), # previously "Hurricanes"
                 tags$h2(textOutput("climatology_hurricanes"), style = "margin-top: 5px; color: #333333;")
               )
        ),
        column(4,
               div(
                 style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; height: 100%;",
                 tags$small("Storms with MSW > 96 kt", style = "color: #333333;"), # previously "Major Hurricanes"
                 tags$h2(textOutput("climatology_majorHurricanes"), style = "margin-top: 5px; color: #333333;")
               )
        )
      ),
      # Subheader for Forecasts
      fluidRow(column(12, h4("Forecasts"))),
      fluidRow(
        column(4,
               div(
                 style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; height: 100%;",
                 tags$small("All Storms", style = "color: #333333;"),
                 tags$h2(textOutput("forecast_allStorms"), style = "margin-top: 5px; color: #333333;")
               )
        ),
        column(4,
               div(
                 style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; height: 100%;",
                 tags$small("Storms with MSW > 64 kt", style = "color: #333333;"), # previously "Hurricanes"
                 tags$h2(textOutput("forecast_hurricanes"), style = "margin-top: 5px; color: #333333;")
               )
        ),
        column(4,
               div(
                 style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; height: 100%;",
                 tags$small("Storms with MSW > 96 kt", style = "color: #333333;"), # previously "Major Hurricanes"
                 tags$h2(textOutput("forecast_majorHurricanes"), style = "margin-top: 5px; color: #333333;")
               )
        )
      )
      
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Path to raster files
  rasterPath <- "./tiffOuts/"
  hardCode_dataType = c("Forecast","Climatology","Difference")
  
  # Reactive expression to load raster data based on dataType
  mydat <- reactive({
    req(input$dataType)  # Ensure input$dataType is available
    if(as.numeric(input$dataType) == 1) {
      suffix = "_abs.tif"
    } else if(as.numeric(input$dataType) == 2) {
      suffix = "_clm.tif"
    } else if(as.numeric(input$dataType) == 3) {
      suffix = "_dif.tif"
    }
    # Load the raster data
    print(paste0(rasterPath, selectedBasin(), "_", thisSeason()))
    raster::brick(paste0(rasterPath, selectedBasin(), "_", thisSeason(), suffix))
  })  
  
  # **Reactive expression for selected raster layer based on hurricane category (Updated)**  
  selected_layer <- reactive({
    req(input$hurricaneCategory_ID, mydat())  # Updated input ID
    raster::subset(mydat(), as.numeric(input$hurricaneCategory_ID))
  })
  
  # UI for seasonal filter based on basin selection
  output$seasonalFilter <- renderUI({
    if (input$basin == 1) {
      selectInput(
        inputId = "seasonalFilter_ID",
        label = "Timeframe of Forecast:", # previously "Season"
        choices = setNames(1:length(seasonNames_northernAtlantic), seasonNames_northernAtlantic),
        selected = length(seasonNames_northernAtlantic)  # Optional: set default
      )
    } else if (input$basin == 2) {
      selectInput(
        inputId = "seasonalFilter_ID",
        label = "Timeframe of Forecast:", # previously "Season"
        choices = setNames(1:length(seasonNames_westernPacific), seasonNames_westernPacific),
        selected = length(seasonNames_westernPacific)  # Optional: set default
      )
    } else if (input$basin == 3) {
      selectInput(
        inputId = "seasonalFilter_ID",
        label = "Timeframe of Forecast:", # previously "Season"
        choices = setNames(1:length(seasonNames_indianOcean), seasonNames_indianOcean),
        selected = length(seasonNames_indianOcean)  # Optional: set default
      )
    } else if (input$basin == 4) {
      selectInput(
        inputId = "seasonalFilter_ID",
        label = "Timeframe of Forecast:", # previously "Season"
        choices = setNames(1:length(seasonNames_indianOcean_SW), seasonNames_indianOcean_SW),
        selected = length(categoryNames_indianOcean_SW)  # Optional: set default
      )
    } else if (input$basin == 5) {
      selectInput(
        inputId = "seasonalFilter_ID",
        label = "Timeframe of Forecast:", # previously "Season"
        choices = setNames(1:length(seasonNames_indianOcean_SE), seasonNames_indianOcean_SE),
        selected = length(categoryNames_indianOcean_SE)  # Optional: set default
      )
    } else if (input$basin == 6) {
      selectInput(
        inputId = "seasonalFilter_ID",
        label = "Timeframe of Forecast:", # previously "Season"
        choices = setNames(1:length(seasonNames_southPacific), seasonNames_southPacific),
        selected = length(categoryNames_southPacific)  # Optional: set default
      )
    }
    
    
  })
  
  # **Dynamic UI for hurricaneCategory based on basin selection (New)**
  output$hurricaneCategory <- renderUI({
    if (input$basin == 1) {  # Northern Atlantic
      selectInput(
        inputId = "hurricaneCategory_ID",  # Unique ID
        label = "Category:",
        choices = setNames(1:length(categoryNames_northernAtlantic), categoryNames_northernAtlantic),
        selected = 1  # Optional: set default
      )
    } else if (input$basin == 2) {  # Western Pacific
      selectInput(
        inputId = "hurricaneCategory_ID",
        label = "Category:",
        choices = setNames(1:length(categoryNames_westernPacific), categoryNames_westernPacific),
        selected = 1
      )
    } else if (input$basin == 3) {  # indian ocean
      selectInput(
        inputId = "hurricaneCategory_ID",
        label = "Category:",
        choices = setNames(1:length(categoryNames_indianOcean), categoryNames_indianOcean),
        selected = 1
      )
    } else if (input$basin == 4) {  #
      selectInput(
        inputId = "hurricaneCategory_ID",
        label = "Category:",
        choices = setNames(1:length(categoryNames_indianOcean_SW), categoryNames_indianOcean_SW),
        selected = 1
      )
    } else if (input$basin == 5) {  #
      selectInput(
        inputId = "hurricaneCategory_ID",
        label = "Category:",
        choices = setNames(1:length(categoryNames_indianOcean_SE), categoryNames_indianOcean_SE),
        selected = 1
      )
    } else if (input$basin == 6) {  # indian ocean
      selectInput(
        inputId = "hurricaneCategory_ID",
        label = "Category:",
        choices = setNames(1:length(categoryNames_southPacific), categoryNames_southPacific),
        selected = 1
      )
    }
  })
  
  selectedBasin <- reactive({
    if(input$basin == 1) {
      return("north_atlantic")
    } else if(input$basin == 2) {
      return("western_pacific")
    } else if(input$basin == 3) {
      return("indian_ocean")
    } else if(input$basin == 4) {
      return("South-West indian_ocean")
    } else if(input$basin == 5) {
      return("South-East indian_ocean")
    } else if(input$basin == 6) {
      return("South-Pacific")
    } else {
      return("unknown_basin")
    }
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    req(selected_layer())  # Ensure selected_layer is available
    
    # Access the selected raster layer
    layer <- selected_layer()
    
    # Determine the color palette based on dataType
    if (as.numeric(input$dataType) == 1) { # Forecast
      pal <- colorNumeric(
        palette = "viridis",
        domain = values(layer),
        na.color = "transparent"
      )
    } else if (as.numeric(input$dataType) == 2) { # Climatology
      pal <- colorNumeric(
        palette = "viridis",
        domain = values(layer),
        na.color = "transparent"
      )
    } else if (as.numeric(input$dataType) == 3) { # Difference
      # Calculate the maximum absolute value for symmetric color scaling
      max_abs <- max(abs(values(layer)), na.rm = TRUE)
      # Create a diverging color palette: Blue (-max_abs) -> White (0) -> Red (+max_abs)
      pal <- colorNumeric(
        palette = colorRampPalette(c("blue", "white", "red"))(256),
        domain = c(-max_abs, max_abs),
        na.color = "transparent"
      )
    }
    
    # Generate the Leaflet map
    leaflet() %>%
      addTiles() %>%
      addRasterImage(layer, colors = pal, opacity = 0.8, group = "Raster") %>%
      addLegend_decreasing_f(
        pal = pal,
        decreasing = TRUE,
        values = if (as.numeric(input$dataType) != 3) {
          values(layer)
        } else {
          c(-max(abs(values(layer)), na.rm = TRUE), max(abs(values(layer)), na.rm = TRUE))
        },
        title = "Storms"
      ) %>%
      addLayersControl(
        overlayGroups = c("Raster"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Reactive value to store click information
  clicked_value <- reactiveVal(NULL)
  
  # Observe map clicks
  observeEvent(input$map_click, {
    click <- input$map_click
    lon <- click$lng
    lat <- click$lat
    
    # Create SpatialPoints object; ensure CRS matches raster
    point <- SpatialPoints(cbind(lon, lat), proj4string = crs(selected_layer()))
    
    # Extract raster value at clicked point
    val <- raster::extract(selected_layer(), point)
    
    # Update reactive value
    clicked_value(paste0("Lon: ", round(lon, 2), 
                         ", Lat: ", round(lat, 2), 
                         ", Number of Events: ", round(val, 3)))
    
    # Add a popup to the map at clicked location
    leafletProxy("map") %>%
      clearPopups() %>%  # Remove existing popups
      addPopups(lon, lat, paste("Number of Events:", round(val, 3)), layerId = "popup")
  })
  
  # Display clicked information in the UI
  output$click_info <- renderPrint({
    clicked_value()
  })
  
  ###########
  ## Section 3: Basin Level Summaries
  # 1. Reactive Expression to Determine the Current Season
  thisSeason <- reactive({
    req(input$basin, input$seasonalFilter_ID) # Ensure inputs are available
    
    basin_value <- as.numeric(input$basin)    # Convert inputs to numeric
    seasonal_filter <- as.numeric(input$seasonalFilter_ID)
    
    # Determine the season based on basin and seasonalFilter
    if (basin_value == 1) {
      thisSeason_out <- seasonNames_northernAtlantic[seasonal_filter]
    } else if (basin_value == 2) {
      thisSeason_out <- seasonNames_westernPacific[seasonal_filter]
    } else if (basin_value == 3) {
      thisSeason_out <- seasonNames_indianOcean[seasonal_filter]
    } else if (basin_value == 4) {
      thisSeason_out <- seasonNames_indianOcean_SW[seasonal_filter]
    } else if (basin_value == 5) {
      thisSeason_out <- seasonNames_indianOcean_SE[seasonal_filter]
    } else if (basin_value == 6) {
      thisSeason_out <- seasonNames_southPacific[seasonal_filter]
    } else {
      thisSeason_out <- "Unknown Basin"
    }
    
    # Handle cases where seasonal_filter might be out of bounds
    if (is.na(thisSeason_out)) {
      thisSeason_out <- "Unknown Season"
    }
    
    # Return the season name
    return(thisSeason_out)
  })
  
  # 2. Render the Text Output to assess path; only for testing
#  output$sampleText <- renderText({
#    paste0(rasterPath, selectedBasin(), "_", thisSeason(), "_", input$dataType, ".tif")
#  })
  
  
  # **Render outputs for selected raster layer based on hurricane category (Updated)**
  # *Forecast Outputs*
  output$forecast_energy <- renderText({
    req(selectedBasin())
    subset(staticTable, 
           dataType == "Forecast" & 
             metric == "energy" & 
             seasonalFilter == thisSeason() &
             basin == selectedBasin())$value
  })
  output$forecast_allStorms <- renderText({
    req(selectedBasin())
    subset(staticTable, 
           dataType == "Forecast" & 
             metric == "all_storms" & 
             seasonalFilter == thisSeason() &
             basin == selectedBasin())$value
  })
  output$forecast_hurricanes <- renderText({
    req(selectedBasin())
    subset(staticTable, 
           dataType == "Forecast" & 
             metric == "hurricanes" & 
             seasonalFilter == thisSeason() &
             basin == selectedBasin())$value
  })
  output$forecast_majorHurricanes <- renderText({
    req(selectedBasin())
    subset(staticTable, 
           dataType == "Forecast" & 
             metric == "major_hurricanes" & 
             seasonalFilter == thisSeason() &
             basin == selectedBasin())$value
  })
  
  # Modified outputs for climatology
  output$climatology_energy <- renderText({
    req(selectedBasin())
    subset(staticTable, 
           dataType == "Climatology" & 
             metric == "energy" & 
             seasonalFilter == thisSeason() &
             basin == selectedBasin())$value
  })
  output$climatology_allStorms <- renderText({
    req(selectedBasin())
    subset(staticTable, 
           dataType == "Climatology" & 
             metric == "all_storms" & 
             seasonalFilter == thisSeason() &
             basin == selectedBasin())$value
  })
  output$climatology_hurricanes <- renderText({
    req(selectedBasin())
    subset(staticTable, 
           dataType == "Climatology" & 
             metric == "hurricanes" & 
             seasonalFilter == thisSeason() &
             basin == selectedBasin())$value
  })
  output$climatology_majorHurricanes <- renderText({
    req(selectedBasin())
    subset(staticTable, 
           dataType == "Climatology" & 
             metric == "major_hurricanes" & 
             seasonalFilter == thisSeason() &
             basin == selectedBasin())$value
  })  
}

# Run the Shiny App
shinyApp(ui, server)