locationTableMaker_f = function(customerInputTable = NULL, basinSummary = NULL, scenario = NULL, valType = NULL)  {

  if(scenario == 1){
    if(valType == 1)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentRatio_B, 2),
        "Location (drought) - trend" =       round((basinSummary$trendLow_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "Local (typical) - current" =        round(basinSummary$currentRatio_A, 2),
        "Local (typical) - trend" =          round((basinSummary$trendLow_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "L + Regional (drought) - current" = round(basinSummary$currentRatio_D, 2),
        "L + Regional (drought) - trend" =   round((basinSummary$trendLow_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "L + Regional (typical) - current" = round(basinSummary$currentRatio_C, 2),
        "L + Regional (typical) - trend" =   round((basinSummary$trendLow_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
      )
    }
    if(valType == 2)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentDeficit_B, 0),
        "Local (drought) - trend" =          round(basinSummary$trendLow_Deficit_B, 0),
        "Local (typical) - current" =        round(basinSummary$currentDeficit_A, 0),
        "Local (typical) - trend" =          round(basinSummary$trendLow_Deficit_A, 0),
        "L + Regional (drought) - current" = round(basinSummary$currentDeficit_D, 0),
        "L + Regional (drought) - trend" =   round(basinSummary$trendLow_Deficit_D, 0),
        "L + Regional (typical) - current" = round(basinSummary$currentDeficit_C, 0),
        "L + Regional (typical) - trend" =   round(basinSummary$trendLow_Deficit_C, 0)
      )
    }
  }

  if(scenario == 2){
    if(valType == 1)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentRatio_B, 2),
        "Location (drought) - trend" =       round((basinSummary$trendMed_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "Local (typical) - current" =        round(basinSummary$currentRatio_A, 2),
        "Local (typical) - trend" =          round((basinSummary$trendMed_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "L + Regional (drought) - current" = round(basinSummary$currentRatio_D, 2),
        "L + Regional (drought) - trend" =   round((basinSummary$trendMed_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "L + Regional (typical) - current" = round(basinSummary$currentRatio_C, 2),
        "L + Regional (typical) - trend" =   round((basinSummary$trendMed_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
      )
    }
    if(valType == 2)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentDeficit_B, 0),
        "Local (drought) - trend" =          round(basinSummary$trendMed_Deficit_B, 0),
        "Local (typical) - current" =        round(basinSummary$currentDeficit_A, 0),
        "Local (typical) - trend" =          round(basinSummary$trendMed_Deficit_A, 0),
        "L + Regional (drought) - current" = round(basinSummary$currentDeficit_D, 0),
        "L + Regional (drought) - trend" =   round(basinSummary$trendMed_Deficit_D, 0),
        "L + Regional (typical) - current" = round(basinSummary$currentDeficit_C, 0),
        "L + Regional (typical) - trend" =   round(basinSummary$trendMed_Deficit_C, 0)
      )
    }
  }
      
      
  if(scenario == 3){
    if(valType == 1)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentRatio_B, 2),
        "Location (drought) - trend" =       round((basinSummary$trendHigh_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "Local (typical) - current" =        round(basinSummary$currentRatio_A, 2),
        "Local (typical) - trend" =          round((basinSummary$trendHigh_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "L + Regional (drought) - current" = round(basinSummary$currentRatio_D, 2),
        "L + Regional (drought) - trend" =   round((basinSummary$trendHigh_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1,
        "L + Regional (typical) - current" = round(basinSummary$currentRatio_C, 2),
        "L + Regional (typical) - trend" =   round((basinSummary$trendHigh_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
      )
    }
    if(valType == 2)  {
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local (drought) - current" =        round(basinSummary$currentDeficit_B, 0),
        "Local (drought) - trend" =          round(basinSummary$trendHigh_Deficit_B, 0),
        "Local (typical) - current" =        round(basinSummary$currentDeficit_A, 0),
        "Local (typical) - trend" =          round(basinSummary$trendHigh_Deficit_A, 0),
        "L + Regional (drought) - current" = round(basinSummary$currentDeficit_D, 0),
        "L + Regional (drought) - trend" =   round(basinSummary$trendHigh_Deficit_D, 0),
        "L + Regional (typical) - current" = round(basinSummary$currentDeficit_C, 0),
        "L + Regional (typical) - trend" =   round(basinSummary$trendHigh_Deficit_C, 0)
      )
    }
  }
      
  myTable
}