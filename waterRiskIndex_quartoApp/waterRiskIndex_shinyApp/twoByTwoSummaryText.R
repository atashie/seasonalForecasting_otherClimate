twoByTwoSummaryText_f = function(twoByTwoTables = NULL, whichText = NULL){
  totLocs = nrow(subset(twoByTwoTables, waterClass == 1))

  pasteText = "error - choose algo"
  if(whichText == 1) {
    pasteText = paste0(round(100 * nrow(subset(twoByTwoTables, waterClass == 1 & Current_Value < 1)) / totLocs, 0),
                       "% of watersheds lack sufficient local water supply (i.e., precipitation + soil moisture) to sustain current agricultural production without crops experiencing water stress most years. ",
                       "Local stress is decreasing at ", round(100 * nrow(subset(twoByTwoTables, waterClass == 1 & Trajectory > 0)) / totLocs, 0),
                      "% and increasing at ", round(100 * nrow(subset(twoByTwoTables, waterClass == 1 & Trajectory < 0)) / totLocs, 0),
                      "% of locations."
                      )
  }
  if(whichText == 2) {
    pasteText = paste0(round(100 * nrow(subset(twoByTwoTables, waterClass == 3 & Current_Value < 1)) / totLocs, 0),
                      "% of watersheds lack sufficient regional water supply (i.e., local + groundwater + streamflow) to sustain current agricultural production without extraction of non-renewable water resources most years. ",
                      "Regional stress is decreasing at ", round(100 * nrow(subset(twoByTwoTables, waterClass == 3 & Trajectory > 0)) / totLocs, 0),
                      "% of locations and increasing at ", round(100 * nrow(subset(twoByTwoTables, waterClass == 3 & Trajectory < 0)) / totLocs, 0),
                      "% of locations."
                      )
  }  

  pasteText
 
}