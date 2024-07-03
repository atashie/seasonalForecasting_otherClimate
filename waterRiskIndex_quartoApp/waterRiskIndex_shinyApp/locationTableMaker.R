locationTableMaker_f = function(customerInputTable = NULL, basinSummary = NULL, scenario = NULL, valType = NULL)  {

  if(scenario == 1){
    if(valType == 1)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentRatio_B, 2),
        "Location (drought) - trend" =       round(((basinSummary$trendLow_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Local - current" =        round(basinSummary$currentRatio_A, 2),
        "Local - trend" =          round(((basinSummary$trendLow_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Regional (drought) - current" = round(basinSummary$currentRatio_D, 2),
        "Regional (drought) - trend" =   round(((basinSummary$trendLow_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Regional - current" = round(basinSummary$currentRatio_C, 2),
        "Regional - trend" =   round(((basinSummary$trendLow_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2)
      )
    }
    if(valType == 2)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentDeficit_B, 0),
        "Local (drought) - trend" =          round(basinSummary$trendLow_Deficit_B, 0),
        "Local - current" =        round(basinSummary$currentDeficit_A, 0),
        "Local - trend" =          round(basinSummary$trendLow_Deficit_A, 0),
        "Regional (drought) - current" = round(basinSummary$currentDeficit_D, 0),
        "Regional (drought) - trend" =   round(basinSummary$trendLow_Deficit_D, 0),
        "Regional - current" = round(basinSummary$currentDeficit_C, 0),
        "Regional - trend" =   round(basinSummary$trendLow_Deficit_C, 0)
      )
    }
  }

  if(scenario == 2){
    if(valType == 1)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentRatio_B, 2),
        "Location (drought) - trend" =       round(((basinSummary$trendMed_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Local - current" =        round(basinSummary$currentRatio_A, 2),
        "Local - trend" =          round(((basinSummary$trendMed_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Regional (drought) - current" = round(basinSummary$currentRatio_D, 2),
        "Regional (drought) - trend" =   round(((basinSummary$trendMed_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Regional - current" = round(basinSummary$currentRatio_C, 2),
        "Regional - trend" =   round(((basinSummary$trendMed_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2)
      )
    }
    if(valType == 2)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentDeficit_B, 0),
        "Local (drought) - trend" =          round(basinSummary$trendMed_Deficit_B, 0),
        "Local - current" =        round(basinSummary$currentDeficit_A, 0),
        "Local - trend" =          round(basinSummary$trendMed_Deficit_A, 0),
        "Regional (drought) - current" = round(basinSummary$currentDeficit_D, 0),
        "Regional (drought) - trend" =   round(basinSummary$trendMed_Deficit_D, 0),
        "Regional - current" = round(basinSummary$currentDeficit_C, 0),
        "Regional - trend" =   round(basinSummary$trendMed_Deficit_C, 0)
      )
    }
  }
      
      
  if(scenario == 3){
    if(valType == 1)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentRatio_B, 2),
        "Local (drought) - trend" =       round(((basinSummary$trendHigh_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Local - current" =        round(basinSummary$currentRatio_A, 2),
        "Local - trend" =          round(((basinSummary$trendHigh_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Regional (drought) - current" = round(basinSummary$currentRatio_D, 2),
        "Regional (drought) - trend" =   round(((basinSummary$trendHigh_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2),
        "Regional - current" = round(basinSummary$currentRatio_C, 2),
        "Regional - trend" =   round(((basinSummary$trendHigh_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant - 1), 2)
      )
    }
    if(valType == 2)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentDeficit_B, 0),
        "Local (drought) - trend" =          round(basinSummary$trendHigh_Deficit_B, 0),
        "Local - current" =        round(basinSummary$currentDeficit_A, 0),
        "Local - trend" =          round(basinSummary$trendHigh_Deficit_A, 0),
        "Regional (drought) - current" = round(basinSummary$currentDeficit_D, 0),
        "Regional (drought) - trend" =   round(basinSummary$trendHigh_Deficit_D, 0),
        "Regional - current" = round(basinSummary$currentDeficit_C, 0),
        "Regional - trend" =   round(basinSummary$trendHigh_Deficit_C, 0)
      )
    }
  }
      
  DT::datatable(myTable,
              options=list(
                pageLength=15,
                buttons = c("csv","excel"),
                dom = "Bfrtip",
                scrollX = TRUE,
                scrollY = TRUE
              ),
              extensions="Buttons",
              selection = "single",
              filter = "bottom",
              rownames = TRUE#,
#              server = FALSE # change to TRUE if the datatable function is slowing down the entire page
            )
}