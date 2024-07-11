#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(viridis)
library(httr)
library(googlesheets4)

myUrl = "https://docs.google.com/spreadsheets/d/1FAonggwzp_Zi9xp77XSv9SMJN0pkmYYL35x9GDC-oUg/edit?usp=sharing"
gs4_deauth()
liveLinkID = read_sheet("1FAonggwzp_Zi9xp77XSv9SMJN0pkmYYL35x9GDC-oUg", sheet = "Sheet1")[1,1]

# Use the file's direct download link
downloadLocText = "https://drive.google.com/uc?export=download&id="
idText = liveLinkID
file_download_link = paste0(downloadLocText, idText)

# Send a GET request to the download link
response <- GET(file_download_link)

# Check if the request was successful
if (http_status(response)$category == "Success") {
  # Read the content of the response into memory
  content <- content(response, "raw")
  
  # Write the content to a temporary file
  temp_file <- tempfile(fileext = ".gpkg")
  writeBin(content, temp_file)
  
  # Read the file
  myData <- st_read(temp_file)
} else {
  print("Failed to download file.")
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(title=div(img(src="./CAi2.png", height=60, width=200), "    Regional Data Review")),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,
           selectInput("columnToPlot",
                       "Choose Data to Plot:",
                       choices = names(myData)[-c(1,ncol(myData))])
    ),
    column(4,
           selectInput("colorScheme", "Choose Color Scheme",
                       c("Intensity" = 1, "Red to Blue" = 2, "Blue to Red" = 3))
    ),
    column(4,  
           sliderInput("thisOpacity",
                       "Map Opacity:",
                       min=0, max=1, value=0.7)
    )
  ),
  mapviewOutput("myMap", height="500px")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
#  myData = st_read("./data/indiaBasicAssessData_MAR2024.gpkg")

  
  output$myMap = renderLeaflet({
    plotThisCol = which(names(myData) == input$columnToPlot)
    mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap.DE","OpenTopoMap"))
    if(input$colorScheme == 1) { thisPal = mapviewPalette("mapviewTopoColors")}
    if(input$colorScheme == 2) { thisPal = colorRampPalette(c("red1","orange3","forestgreen","royalblue"))}
    if(input$colorScheme == 3) { thisPal = colorRampPalette(c("royalblue","forestgreen","orange3","red1"))}
    
    myMap =
      mapview(
        myData[,plotThisCol],
        col.regions = thisPal,  # Replace with your desired color palette
        #        color = "green2", lwd = 3, alpha.regions = 0.0, col.regions="grey90", 
        alpha.regions = input$thisOpacity,
        legend=TRUE
      ) 
    myMap@map
  })}

# Run the application 
shinyApp(ui = ui, server = server)
