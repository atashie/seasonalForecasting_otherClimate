
###################################################################
# 3C- calculating and the water index with location specific data
waterIndexCalculations_locationSpecific_f = function(
    customerTable_input = customerInputTable,
    thisRow = thisRow,
    hydroBasins = basinSummary,
    climateData = climateArray,
    irrigationMethod = irrigationMethod,
    detentionPonds = detentionPonds,
    irrigatedCropAreaPctChng = irrigatedCropArea,
    irrigationAllotments = irrigationAllotments,
    allotmentForMar = allotmentForMar,
    groundwaterAccess = groundwaterAccess,
    soilMoistureAccess = soilMoistureAccess,
    plantHybrid = plantHybrid
#    shiftGrowSeason = shiftGrowSeason
    )
  {
  
  #hectToSqKm = 100
  #	runoffRatio = 0.08
  #	effectiveIrrigationRatio = 0.85
  #  cropPhenologyTable = "J:\\Cai_data\\WaterIndex\\crop_info\\cropWaterNeedsLookupTable_v1.csv"
  hectareMeter_to_km3 = 0.00001
  humidAI = 0.65
  petGlobAvg = 2000
  indexValues = c('Aridity Index - Avg', 'Plant Water Demand - Avg', 'Aridity Index - Drought', 'Plant Water Demand - Drought', 'Aridity Index w/ Irrigation - Avg', 'Plant Water Demand w/ Irrigation - Avg', 'Aridity Index w/ Irrigation - Drought', 'Plant Water Demand w/ Irrigation - Drought')
  halfDegInKm = 111.1 / 2
  kmToMm = 1000^2
  myMissingData = NA
  indexValueClass = c('ratio', 'difference')
  indexValueQuant = c('Q05', 'Q15', 'Q25', 'Q50', 'Q75', 'Q85', 'Q95', 'Mn')
  
  # array for holding outputs
  # input array is in format  [location, decade, valueClass,      scenario, climateVariable]
  # output array is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
  numDecades = length(climateData[1, ,1,1,1])
  indexValuesArray = array(rep(myMissingData, nrow(customerTable_input) * numDecades * length(indexValueQuant) * length(climateData[1,1,1, ,1]) * length(indexValues) * length(indexValueClass)), 
                           dim = c(nrow(customerTable_input),  numDecades,  length(indexValueQuant),  length(climateData[1,1,1, ,1]),  length(indexValues), length(indexValueClass)))
  hydroBasins$marInmm = NA
  
    # initializing static values
    # defining the growing season
    if(is.na(customerTable_input$GrowMonth_Start[thisRow]) | is.na(customerTable_input$GrowMonth_End[thisRow]))	{
      if(is.na(hydroBasins$growDaysAvg[thisRow]))	{growSeason = 1:12} else {growSeason = round(seq(1,12, length.out=min((25 + hydroBasins$growDaysAvg[thisRow]) / 30, 12)))}
    } else {
      if(customerTable_input$GrowMonth_Start[thisRow] < customerTable_input$GrowMonth_End[thisRow])	{
        growSeason = seq(customerTable_input$GrowMonth_Start[thisRow], customerTable_input$GrowMonth_End[thisRow], 1)
      }
      if(customerTable_input$GrowMonth_Start[thisRow] > customerTable_input$GrowMonth_End[thisRow])	{
        growSeason = unique(c(seq(customerTable_input$GrowMonth_Start[thisRow], 12, by=1), seq(1, customerTable_input$GrowMonth_End[thisRow], by=1)))
      }
      if(customerTable_input$GrowMonth_Start[thisRow] == customerTable_input$GrowMonth_End[thisRow])	{
        growSeason = customerTable_input$GrowMonth_Start[thisRow]
      }
    }
    
    # global average PET standardized by growing season
    petGlobAvgForGrowSeason = petGlobAvg * (length(growSeason) / 12)
    
    # water demand and cultivated area for the watershed
    cultAreaScalar = (100 + irrigatedCropAreaPctChng) / 100
    thisFrcAreaUnderCult = hydroBasins$thisFrcAreaUnderCult[thisRow]	* cultAreaScalar #defining water needs by location
    thisFrcCultAreaWthIrr = hydroBasins$thisFrcCultAreaWthIrr[thisRow] * cultAreaScalar
    
    if(is.na(customerTable_input$Annual_Water_Needs_mm[thisRow])){
      thisWplant = hydroBasins$thisWplant[thisRow] * plantHybrid
    } else {
      thisWplant = mean(customerTable_input$Annual_Water_Needs_mm[thisRow], hydroBasins$thisWplant[thisRow]) * plantHybrid
    } 
    
    # defining runoff, w/ or w/o on-site storage 
    runoffRatio = hydroBasins$runoffRatio[thisRow]
    if(customerTable_input$OnsiteReservoirs[thisRow]) {storageRescalar = 0} else {storageRescalar = 0.5}
    if(detentionPonds != 999) {storageRescalar = detentionPonds}
    
    # rescalar of recharge, since recharge is poorly constrained
    rechScalar = hydroBasins$rechScalar[thisRow]
    
    # defining irrigation losses
    effectiveIrrigationRatio = hydroBasins$effectiveIrrigationRatio[thisRow]
    if(customerTable_input$DripIrrigation[thisRow])	{effectiveIrrigationRatio = mean(0.98, effectiveIrrigationRatio)}
    if(irrigationMethod != 999) {
      effectiveIrrigationRatio = mean(irrigationMethod, effectiveIrrigationRatio)
    }
    
    # accounting for soils at field capacity at the start of the growing season
    if(length(growSeason) == 12){initialSoilMoisture = 0} else {initialSoilMoisture = hydroBasins$initialSoilMoisture[thisRow]}

    # accounting for regulated resources or greenhouse based ag
    groundwaterMask = 1
    if(!customerTable_input$Groundwater_Access[thisRow]) {groundwaterMask = 0}
    if(groundwaterAccess != 999) {groundwaterMask = groundwaterAccess}
    soilMoistureMask = 1
    if(soilMoistureAccess != 999) {soilMoistureMask = soilMoistureAccess}
    
    # quantiles are combined in some eqs (and e.g. Q05*Q05 --> Q0025), so quantiles for each coincident var must be able to be handled separately
    pptQntsNrml = climateData[thisRow, , c(14:20,13), , 1] /  climateData[thisRow, , rep(13,8), , 1]
    
    pptQntsAvg = sqrt(pptQntsNrml) * climateData[thisRow, , rep(13,8), , 1] # taking the sqrt since the var will be multiplied (and e.g. Q05*Q05 --> Q0025)
    pptQntsDrght = sqrt(pptQntsNrml) * climateData[thisRow, , rep(16,8), , 1]
    pptQntsDrghtShft = pptQntsNrml * mean(climateData[thisRow, , 16, , 1] /  climateData[thisRow, , 17, , 1])
    
    #pet = potential evapotranspiration
    petQntsNrml = climateData[thisRow, , c(20:14,13), , 2] /  climateData[thisRow, , rep(13,8), , 2]
    petQntsAvg = sqrt(petQntsNrml) * climateData[thisRow, , rep(13,8), , 2]
    #	petQntsDrght = sqrt(petQntsNrml) * climateData[thisRow, , rep(19,7), , 2]
    
    #strmfl = streamflow
    gridArea = halfDegInKm * (halfDegInKm * cos(customerTable_input$Lat[thisRow] * pi / 180))
    strmflQntsNrml = climateData[thisRow, , c(14:20,13), , 4] /  climateData[thisRow, , rep(13,8), , 4]
    strmflQntsAvg = sqrt(strmflQntsNrml) * climateData[thisRow, , rep(13,8), , 4]
    strmflQntsDrght = sqrt(strmflQntsNrml) * climateData[thisRow, , rep(16,8), , 4]
    strmflQntsDrghtShft = strmflQntsNrml * mean(climateData[thisRow, , 16, , 4] /  climateData[thisRow, , 17, , 4])
    
    
    # Aridity Index - Avg
    #		aridityIndex_rel = (pptQntsAvg / petQntsAvg) / humidAI
    #		aridityIndex_sclr = 1.5 - aridityIndex_rel; aridityIndex_sclr[which(aridityIndex_rel > 1.5)] = 0
    aridityIndex = pptQntsAvg / petQntsAvg
    indexValuesArray[thisRow, , , , 1, 1] =  aridityIndex / humidAI
    indexValuesArray[thisRow, , , , 1, 2] = (pptQntsAvg - petQntsAvg * humidAI)
    
    # using calibrated value to estimate streamflow losses to groundwater (i.e., regionally driven groundwater recharge)
    streamflowRechargeScalar =  hydroBasins$streamflowRechargeScalar[thisRow]
    
    # customer inputs defining streamflow diversions and average depth
    streamflowImports_km3 = 0	; doubleCountingStreamflow = FALSE
    if(!is.na(customerTable_input$SurfaceWaterDiversion_averageInCubicKM[thisRow]))	{
      streamflowImports_km3 = customerTable_input$SurfaceWaterDiversion_averageInCubicKM[thisRow]
      doubleCountingStreamflow = TRUE
    }
    streamflowImports_km3_adj = streamflowImports_km3 * ((irrigationAllotments + 100) / 100)
    if(!is.na(customerTable_input$TotalAreaUnderCultivation_km2[thisRow]))	{
      totLocalAreaUnderCult = customerTable_input$TotalAreaUnderCultivation_km2[thisRow]
    }	else	{ 
      totLocalAreaUnderCult = thisFrcCultAreaWthIrr * hydroBasins$SUB_AREA[thisRow]
    }
    streamflowImports_mm =  kmToMm * (streamflowImports_km3_adj / (totLocalAreaUnderCult * cultAreaScalar))
    
    # customer inputs for flood based MAR
    marVol_km3 = allotmentForMar * hectareMeter_to_km3
    marVol_mm = kmToMm * (marVol_km3 / (totLocalAreaUnderCult * cultAreaScalar))
    
    #		localFrcCultAreaWthIrr = totLocalAreaUnderCult / hydroBasins$SUB_AREA[thisRow]
    
    # Plant Water Demand - Avg
    meanToMedianRatio_ppt = 1 # mean(climateData[thisRow, , 17, , 1] / climateData[thisRow, , 13, , 1])
    meanToMedianRatio_pet = 1 # mean(climateData[thisRow, , 17, , 2] / climateData[thisRow, , 13, , 2])
    growSeasonPPTqntsAvg = sqrt(pptQntsNrml) * climateData[thisRow, , rep(growSeason[1], 8), , 1] * meanToMedianRatio_ppt# taking the sqrt since the var will be multiplied (and e.g. Q05*Q05 --> Q0025)
    growSeasonPETqntsAvgRatio = sqrt(petQntsNrml) * climateData[thisRow, , rep(growSeason[1],8), , 2] * meanToMedianRatio_pet / petGlobAvgForGrowSeason
    for(thisMonth in growSeason[-1])	{
      growSeasonPPTqntsAvg = growSeasonPPTqntsAvg + sqrt(pptQntsNrml) * climateData[thisRow, , rep(thisMonth, 8), , 1] * meanToMedianRatio_ppt
      growSeasonPETqntsAvgRatio = growSeasonPETqntsAvgRatio + sqrt(petQntsNrml) * climateData[thisRow, , rep(thisMonth, 8), , 2]  * meanToMedianRatio_pet/ petGlobAvgForGrowSeason
    }
    petScalar = growSeasonPETqntsAvgRatio; petScalar[which(petScalar < 0.7)] = 0.7; petScalar[which(petScalar > 1.5)] = 1.5
    
    
    # estimating recharge ratios
    rechRaw = rechScalar * climateData[thisRow, , 13, , 3]
    if(any(rechRaw < 0))	{rechRaw[which(rechRaw < 0)] = 0}
    rechRatio = mean(rechRaw) / mean(climateData[thisRow, , 13, , 1])
    rechAvg = climateData[thisRow, , c(14:20,13), , 3]
    if(any(rechAvg < 0))	{rechAvg[which(rechAvg < 0)] = 0}
    
    
    # estimating effective precip and recharge
    effectivePPT = soilMoistureMask * (initialSoilMoisture * min(aridityIndex, 1) + (1 - runoffRatio * storageRescalar - rechRatio) * growSeasonPPTqntsAvg)
    effectiveRech_local = effectiveIrrigationRatio * rechAvg * thisFrcAreaUnderCult
    effectiveRech_regional = 0.25 * (marVol_mm + effectiveIrrigationRatio * rechAvg / thisFrcAreaUnderCult)
    effectiveRech = groundwaterMask * (effectiveRech_local + effectiveRech_regional)
    effectiveWPlant = petScalar * thisWplant
    indexValuesArray[thisRow, , , , 2, 1] = effectivePPT / effectiveWPlant
    indexValuesArray[thisRow, , , , 2, 2] = effectivePPT - effectiveWPlant
    
    
    # Aridity Index - Drought
    indexValuesArray[thisRow, , , , 3, 1] = (pptQntsDrght / petQntsAvg) / humidAI
    indexValuesArray[thisRow, , , , 3, 2] = pptQntsDrght - petQntsAvg * humidAI
    # Plant Water Demand - Drought
    growSeasonPPTqntsdrought_local = sqrt(pptQntsDrghtShft) * climateData[thisRow, , rep(growSeason[1], 8), , 1]	
    #		if(!customerTable_input$Soil_Moisture[thisRow])	{growSeasonPPTqntsdrought_local[,,] = 0}				# zeroing soil moisture is it cannot be used
    for(thisMonth in growSeason[-1])	{
      growSeasonPPTqntsdrought_local = growSeasonPPTqntsdrought_local + sqrt(pptQntsDrghtShft) * climateData[thisRow, , rep(thisMonth, 8), , 1]	
    }
    effectivePPTdrought = soilMoistureMask * 0.5 * initialSoilMoisture * min(aridityIndex, 1) + (1 - runoffRatio * storageRescalar - rechRatio) * growSeasonPPTqntsdrought_local 
    indexValuesArray[thisRow, , , , 4, 1] = effectivePPTdrought / effectiveWPlant
    indexValuesArray[thisRow, , , , 4, 2] = effectivePPTdrought - effectiveWPlant
    
    
    # Aridity Index w/ Irrigation - Avg
    # using the Aridity Index to estimate streamflow losses to groundwater (i.e., regionally driven groundwater recharge)
    streamflowRecharge = (streamflowRechargeScalar / thisFrcCultAreaWthIrr) * (strmflQntsAvg / gridArea) * kmToMm
    streamflowRechargeDrght = (streamflowRechargeScalar / thisFrcCultAreaWthIrr) * (strmflQntsDrght / gridArea) * kmToMm
    if(doubleCountingStreamflow)	{
      streamflowRecharge = streamflowRecharge - 0.8 * streamflowImports_mm
      if(any(streamflowRecharge < 0))	{	streamflowRecharge[which(streamflowRecharge < 0)] = 0	}
      streamflowRechargeDrght = streamflowRechargeDrght - 0.27 * streamflowImports_mm
      if(any(streamflowRechargeDrght < 0))	{	streamflowRechargeDrght[which(streamflowRechargeDrght < 0)] = 0	}
    }
    effectiveStrmfl = 	effectiveIrrigationRatio * (streamflowImports_mm + groundwaterMask * streamflowRecharge)
    effectiveStrmflDrght = 	effectiveIrrigationRatio * (streamflowImports_mm * 0.37 + groundwaterMask * streamflowRechargeDrght)
    
    #		if(any(effectiveStrmfl > 1000))	{effectiveStrmfl[effectiveStrmfl > 1000] = 1000}
    #		if(any(effectiveStrmflDrght > 1000))	{effectiveStrmflDrght[effectiveStrmflDrght > 1000] = 1000}
    
    indexValuesArray[thisRow, , , , 5, 1] = ((pptQntsDrght + effectiveRech + effectiveStrmfl) / petQntsAvg) / humidAI
    indexValuesArray[thisRow, , , , 5, 2] = (pptQntsDrght + effectiveRech + effectiveStrmfl) - petQntsAvg * humidAI
    # Plant Water Demand w/ Irrigation - Avg'
    rechAvg_local = effectiveRech
    if(!customerTable_input$Groundwater_Access[thisRow])	{growSeasonPPTqntsdrought_local[,,] = 0}				# zeroing groundwater if it cannot be used
    indexValuesArray[thisRow, , , , 6, 1] = (effectivePPT + rechAvg_local + effectiveStrmfl) / effectiveWPlant
    indexValuesArray[thisRow, , , , 6, 2] = (effectivePPT + rechAvg_local + effectiveStrmfl) - effectiveWPlant
    
    # Aridity Index w/ Irrigation - Drought
    indexValuesArray[thisRow, , , , 7, 1] = ((pptQntsDrght + rechAvg_local + effectiveStrmflDrght) / petQntsAvg) / humidAI
    indexValuesArray[thisRow, , , , 7, 2] = (pptQntsDrght + rechAvg_local + effectiveStrmflDrght) - petQntsAvg * humidAI
    # Plant Water Demand w/ Irrigation - Drought
    indexValuesArray[thisRow, , , , 8, 1] = (effectivePPTdrought + rechAvg_local + effectiveStrmflDrght) / effectiveWPlant
    indexValuesArray[thisRow, , , , 8, 2] = (effectivePPTdrought + rechAvg_local + effectiveStrmflDrght) - effectiveWPlant
    
    
    
    hydroBasins$currentPrecip_avg[thisRow] = mean(climateData[thisRow, 1:2, 13, , 1])
    hydroBasins$currentPrecip_var[thisRow] = mean((climateData[thisRow, 1:2, 19, , 1] - climateData[thisRow, 1:2, 15, , 1]) / 2)
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 1, 1], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Precip_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 1, 1])$slope}
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 2, 1], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Precip_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 2, 1])$slope}
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 3, 1], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Precip_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 3, 1])$slope}
    if(cor.test(1:numDecades, climateData[thisRow, , 19, 1, 1] - climateData[thisRow, , 15, 1, 1], method="spearman")$p.value < 0.1) {
      hydroBasins$trendLow_Precip_var[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 19, 1, 1] - climateData[thisRow, , 15, 1, 1])$slope
    }
    if(cor.test(1:numDecades, climateData[thisRow, , 19, 2, 1] - climateData[thisRow, , 15, 2, 1], method="spearman")$p.value < 0.1) {
      hydroBasins$trendMed_Precip_var[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 19, 2, 1] - climateData[thisRow, , 15, 2, 1])$slope
    }
    if(cor.test(1:numDecades, climateData[thisRow, , 19, 3, 1] - climateData[thisRow, , 15, 3, 1], method="spearman")$p.value < 0.1) {
      hydroBasins$trendMed_Precip_var[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 19, 3, 1] - climateData[thisRow, , 15, 3, 1])$slope
    }
    
    hydroBasins$currentPET_avg[thisRow] = mean(climateData[thisRow, 1:2, 13, , 2])
    hydroBasins$currentPET_var[thisRow] = mean((climateData[thisRow, 1:2, 19, , 2] - climateData[thisRow, 1:2, 15, , 2]) / 2)
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 1, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_PET_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 1, 2])$slope}
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 2, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_PET_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 2, 2])$slope}
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 3, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_PET_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 3, 2])$slope}
    
    hydroBasins$currentStreamflow_avg[thisRow] = mean(climateData[thisRow, 1:2, 13, , 4])
    hydroBasins$currentStreamflow_var[thisRow] = mean((climateData[thisRow, 1:2, 19, , 4] - climateData[thisRow, 1:2, 15, , 4]) / 2)
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 1, 4], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Streamflow_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 1, 4])$slope}
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 2, 4], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Streamflow_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 2, 4])$slope}
    if(cor.test(1:numDecades, climateData[thisRow, , 13, 3, 4], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Streamflow_avg[thisRow] = robslopes::TheilSen(1:numDecades, climateData[thisRow, , 13, 3, 4])$slope}
    
    hydroBasins$currentAridityIndex_A[thisRow] = 	mean(indexValuesArray[thisRow, 1:2, 8, , 1, 1]) * humidAI
    hydroBasins$currentDeficit_A[thisRow] = 		mean(indexValuesArray[thisRow, 1:2, 8, , 2, 2])
    hydroBasins$currentRatio_A[thisRow] = 			mean(indexValuesArray[thisRow, 1:2, 8, , 2, 1])
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 1, 2, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Deficit_A[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 1, 2, 2])$slope}
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 2, 2, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Deficit_A[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 2, 2, 2])$slope}
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 3, 2, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Deficit_A[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 3, 2, 2])$slope}
    hydroBasins$currentAridityIndex_B[thisRow] =	mean(indexValuesArray[thisRow, 1:2, 8, , 3, 1]) * humidAI
    hydroBasins$currentDeficit_B[thisRow] = 		mean(indexValuesArray[thisRow, 1:2, 8, , 4, 2])
    hydroBasins$currentRatio_B[thisRow] = 			mean(indexValuesArray[thisRow, 1:2, 8, , 4, 1])
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 1, 4, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Deficit_B[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 1, 4, 2])$slope}
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 2, 4, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Deficit_B[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 2, 4, 2])$slope}
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 3, 4, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Deficit_B[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 3, 4, 2])$slope}
    hydroBasins$currentDeficit_C[thisRow] = 		mean(indexValuesArray[thisRow, 1:2, 8, , 6, 2])
    hydroBasins$currentRatio_C[thisRow] = 			mean(indexValuesArray[thisRow, 1:2, 8, , 6, 1])
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 1, 6, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Deficit_C[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 1, 6, 2])$slope}
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 2, 6, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Deficit_C[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 2, 6, 2])$slope}
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 3, 6, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Deficit_C[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 3, 6, 2])$slope}
    hydroBasins$currentDeficit_D[thisRow] = 		mean(indexValuesArray[thisRow, 1:2, 8, , 8, 2])
    hydroBasins$currentRatio_D[thisRow] = 			mean(indexValuesArray[thisRow, 1:2, 8, , 8, 2])
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 1, 8, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendLow_Deficit_D[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 1, 8, 2])$slope}
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 2, 8, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendMed_Deficit_D[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 2, 8, 2])$slope}
    if(cor.test(1:numDecades, indexValuesArray[thisRow, , 8, 3, 8, 2], method="spearman")$p.value < 0.1) {hydroBasins$trendHigh_Deficit_D[thisRow] = 		robslopes::TheilSen(1:numDecades, indexValuesArray[thisRow, , 8, 3, 8, 2])$slope}
    hydroBasins$streamflowRechargeScalar[thisRow] = streamflowRechargeScalar
    hydroBasins$thisFrcAreaUnderCult[thisRow] = thisFrcAreaUnderCult
    hydroBasins$thisFrcCultAreaWthIrr[thisRow] =thisFrcCultAreaWthIrr
    hydroBasins$initialSoilMoisture[thisRow] = initialSoilMoisture
    hydroBasins$thisWplant[thisRow] = thisWplant
    hydroBasins$runoffRatio[thisRow] = runoffRatio
    hydroBasins$effectiveIrrigationRatio[thisRow] = effectiveIrrigationRatio
    hydroBasins$rechScalar[thisRow] = rechScalar
    hydroBasins$marInmm[thisRow] = marVol_mm
    
    outputVals = list(indexValuesArray, hydroBasins)
    return(outputVals)
    #return(1)
#  saveRDS(indexValuesArray, paste0(customerFolder_input, clientName_input, '\\',  clientName_input,"_local", "_waterIndex.rds"))
#  data.table::fwrite(hydroBasins, paste0(customerFolder_input, clientName_input, '\\',  clientName_input, 'local_hydroBasins_wIndex.csv'))
}




