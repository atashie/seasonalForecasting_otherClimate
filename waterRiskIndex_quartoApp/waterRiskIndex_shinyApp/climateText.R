climateText_f = function(thisLoc = NULL, scenario = NULL, basinSummary = basinSumary){

  basinSub = basinSummary[thisLoc, ]
  theseCurrentVals = basinSub[, c("currentPrecip_avg", "currentPET_avg", "currentStreamflow_avg")]
  
  thisScen = scenario
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
}