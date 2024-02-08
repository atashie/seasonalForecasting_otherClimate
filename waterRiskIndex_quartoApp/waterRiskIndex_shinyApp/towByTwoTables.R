twoByTwoTables_f = function(basinSummary = NULL, customerInputTable = NULL, thisScen = NULL, thisStressClass = NULL)  {

  if(thisScen == 1){
    myTable2 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_B, 2),
      "Trajectory" =       round((basinSummary$trendLow_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable1 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_A, 2),
      "Trajectory" =       round((basinSummary$trendLow_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable4 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_D, 2),
      "Trajectory" =       round((basinSummary$trendLow_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable3 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_C, 2),
      "Trajectory" =       round((basinSummary$trendLow_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
  }
  
  if(thisScen == 2){
    myTable2 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_B, 2),
      "Trajectory" =       round((basinSummary$trendMed_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable1 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_A, 2),
      "Trajectory" =       round((basinSummary$trendMed_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable4 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_D, 2),
      "Trajectory" =       round((basinSummary$trendMed_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable3 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_C, 2),
      "Trajectory" =       round((basinSummary$trendMed_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
  } 
  
  if(thisScen == 3){
    myTable2 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_B, 2),
      "Trajectory" =       round((basinSummary$trendHigh_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable1 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_A, 2),
      "Trajectory" =       round((basinSummary$trendHigh_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable4 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_D, 2),
      "Trajectory" =       round((basinSummary$trendHigh_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable3 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_C, 2),
      "Trajectory" =       round((basinSummary$trendHigh_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
  }  
  
  
  myTable1$waterClass = 1
  myTable2$waterClass = 2
  myTable3$waterClass = 3
  myTable4$waterClass = 4
  twoByTwoTables = rbind(myTable1, myTable2, myTable3, myTable4)
  return(twoByTwoTables)
}