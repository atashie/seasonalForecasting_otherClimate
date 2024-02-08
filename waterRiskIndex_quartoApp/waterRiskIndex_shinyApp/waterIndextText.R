waterIndexText_f = function(basinSummary = NULL, scenario = NULL, thisLoc = NULL){

  basinSub = basinSummary[thisLoc, ]
  thisScen = scenario
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
}