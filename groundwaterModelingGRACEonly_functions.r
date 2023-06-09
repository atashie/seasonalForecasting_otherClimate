##################
# Workflow
library(data.table)

#######################################################################################
## Part 1: functions supporting gw modeling
	# step 1: estimating gw recharge in non ag lands
	# 1a: infiltration function
totalInfiltration_f = function(
	PPT = NA,
	irrigationApplied = 0,					# timeseries of irrigation (daily); defaults to 0
	crn = 50,								# curve number
	Smax = 500,								# maximum rz soil moisture storage
	Ia_scalar = 0.1,
	irrigatedPctArea = 0.2,
	managedAquiferRecharge = FALSE)						
	{	
	Ia = Ia_scalar * Smax
	totWaterInput = PPT + irrigationApplied
	runoff = (totWaterInput - Ia)^2 / (totWaterInput - Ia + Smax)
	if(managedAquiferRecharge)	{runoff = runoff - irrigatedPctArea * runoff}
	totalInfiltration = totWaterInput - ifelse(totWaterInput > Ia, runoff, 0)
	return(totalInfiltration) # [mm]
	}
		
	# 1b PET, infiltration, AET, and recharge
gwRecharge_f = function(
	PPT = NA,				# from CAi 
	tmaxTS = NA,							# from CAi
	tminTS = NA,							# from CAi
	tavgTS = NA,														# from CAi
	Date = climateInput$Date,
	thisLatitudeInDegrees = NA,				# from customer location (centroid)
	totalInfiltration = NA,			# taken from function above
	rcScl_sm = 0.1,	# water retention curve scalar for soil moisture
	rcExp_sm = 1.3,	# water retention curve exponent for soil moisture
	Zr = 1000,		# root depth [mm]
	n = 0.5,		# soil porosity [-]
	smhp = 0.00,	# soil moisture at hydroscopic point [-]
	smwp = 0.10,	# soil moisture at wilting point [-]
	smfc = 0.25,	# soil moisture field capacity [-]
	s0 = .5,		# initial soil moisture  [-]
	rcScl_gw = 0.1,	# water retention scalar for gw rech
	initialBlackBucketStor = 100, # initial black bucket storage (mm)
	PETexp = 2) 	# exponent of PET decay

	{

	# PET
	petTS = EcoHydRology::PET_fromTemp(
		Jday = lubridate::yday(Date),										# 
		Tmax_C = tmaxTS,													#
		Tmin_C = tminTS,													#
		AvgT = (tmaxTS + tminTS)/2,											#
		lat_radians = min((abs(thisLatitudeInDegrees)*pi/180), 1.1), 
		albedo = 0.18,				# standard value; may revisit
		TerrestEmiss = 0.97,		# standard value; may revisit 
		aspect = 0,					# in radians
		slope = 0,					# in radians
		forest = 0,					# is effectively a 'shade cover' percentage for PET under a 'canopy.' May be used to adjust for mulching / shading
		PTconstant=1.26)			# standard value of Priestly Taylor constant; may revisit
	mmPetTS = petTS * 1000 # output in m, converting to mm

	# SM and GW routine
	nt = length(totalInfiltration)
	SM_store = c(s0, rep(NA, nt))
	aetTS = c(0, rep(NA, nt))
	drainageTS = c(0, rep(NA, nt))
	blackBucketTS = c(initialBlackBucketStor, rep(NA, nt))
	gwRechargeTS = 0
	smhp_stor = Zr * smhp
	smwp_stor = Zr * smwp
	smfc_stor = Zr * smfc
	max_stor_sm = Zr * n
	
	for(i in 1:length(totalInfiltration))	{
		thisSMstor = SM_store[i]
		thisBlackBucketStor = blackBucketTS[i]
		
		AET = ifelse(thisSMstor > smhp_stor, 
			min(thisSMstor - smhp_stor, mmPetTS[i] * (thisSMstor / max_stor_sm) ^ PETexp),
			0)
		thisSMstor = thisSMstor - AET
		
		deepDrainage = ifelse(thisSMstor > smfc_stor,
			min(thisSMstor - smfc_stor, rcScl_sm * thisSMstor * (thisSMstor / smfc_stor)^rcExp_sm),
			0)
		
		gwRecharge = thisBlackBucketStor * rcScl_gw
		
		drainageTS[i+1] = deepDrainage 
		aetTS[i+1] = AET 
		SM_store[i+1] = min(max_stor_sm, thisSMstor - min(c(thisSMstor, deepDrainage)) + totalInfiltration[i]) 
		blackBucketTS[i+1] = thisBlackBucketStor + deepDrainage - gwRecharge
		gwRechargeTS = c(gwRechargeTS, gwRecharge)
	}

	groundwaterRechTable = data.table::data.table(
		soilMoistureVol = SM_store,
		gwDrainage = drainageTS,
		blackBucketStor = blackBucketTS,
		gwRecharge = gwRechargeTS,
		PET = c(mean(mmPetTS), mmPetTS),
		AET = aetTS)
	return(groundwaterRechTable)
	}


	# step 2: estimating gw recharge and net infiltration in ag lands
cropWaterNeeds_f = function(
	petTS = petFromClimate,
	avgCropWaterNeeds = 900 / 365.25,		# for alfalfa #1010  pistachio in ther san J; c(900, 1200) # citrus
	petGlobAvg = 1500 / 365.25,
	mulching = FALSE)
	{
	if(mulching)	{avgCropWaterNeeds = avgCropWaterNeeds * 0.9}	# needs to be better justified, but there is not much quantification in the literature
	cropWaterNeeds = sqrt(petTS / petGlobAvg) * avgCropWaterNeeds
	return(cropWaterNeeds)
	}

	# step 3: estimating irrigation needs for ag lands
irrigation_f = function(
	irrigatedPctArea = .2,		# pct area that is regularly irrigated with gw
	cropWaterNeedsTS = cropWaterNeeds,	#ts of crop water needs from above function
	infiltrationTS = netInfiltration,		# ts of cropland infiltration (minus gw rech) from above function
	dripIrrigation = TRUE)
	{
	irrigationNeeded = irrigatedPctArea * (cropWaterNeedsTS - infiltrationTS)
	if(!dripIrrigation)	{irrigationNeeded = irrigationNeeded * 1.5}
	return(irrigationNeeded)
	}
	
	# step 4: estimating net groundwater flow across basins
netGwFlow_f = function(
	gwRecharge = gwRechargeTable$gwRecharge,	# a time series of mass flux
	irrigationNeeded = irrigationNeeded,	# time series of area-averaged irrigation
	regionalNetGwRtOfChange = -10,	# what is the long-term trend in gw resources, here likely from abutting GRACE / GRACE-FO tiles, given in mm / day; should be a small number
	graceSinMod = graceSinMod,		# model capturing the seasonal signal in gw head
	histDates = climateInput$Date,		
	initialStorageDifference = 500,	# what is the initial head on location relative to regional average (pos is higher regional so net inflow, neg is lower regional so net outflow)
	maxStorageBalance = 1000,		# what is the maximum head imbalance in mm
	rcScl_netGw = 0.1 / 365)			# recession curve scalar
	{
	
	newGraceRadians = sin(2 * pi * lubridate::yday(histDates) / 365.25)
	predSeasonalGW = predict(graceSinMod, newdata = (data.frame(graceRadians = newGraceRadians)))
	extraRegionalStorageTS = initialStorageDifference + regionalNetGwRtOfChange * seq(1, length(gwRecharge), 1) / 365.25 + predSeasonalGW - median(predSeasonalGW)

	regionalStorageTS = cumsum(gwRecharge) - cumsum(irrigationNeeded)
	regionalStorageDiff = diff(regionalStorageTS)

	subsidizedRegionalStorageTS = rep(NA, length(regionalStorageTS))
	subsidizedRegionalStorageTS[1] = regionalStorageTS[1]
	netGwFlow = rep(NA, length(regionalStorageTS))
	
	for(i in 2:length(regionalStorageTS))	{
		storageBalance = subsidizedRegionalStorageTS[i-1] - extraRegionalStorageTS[i-1]
		netGwFlow[i] = rcScl_netGw * storageBalance #* (abs(storageBalance) / maxStorageBalance)^rcExp
		subsidizedRegionalStorageTS[i] = subsidizedRegionalStorageTS[i-1] + regionalStorageDiff[i] - netGwFlow[i]
	}

	netGwFlowTable = data.table::data.table(
		regionalStorage = regionalStorageTS,
		netGwFlow = netGwFlow,
		subsidizedRegionalStorage = subsidizedRegionalStorageTS)

	return(netGwFlowTable)
	}


#######################################################################################
## Part 2: combining functions into a single call

runRegionalGWmodel_f = function(
	dataOut_location ='save_file_location',
	basinName = 'basin_name',
	thisLatitudeInDegrees = 35,
	variablesTableRow = variablesTableRow,
	graceSinMod = graceSinMod,				# model capturing the seasonal signal of gw head
#	histDates = c(climateInput$Date, tail(climateInput$Date, 1) + 1),
	multiClimateData = NA, 
	startYear = 2000,
	endYear = 2099)
	{		

	if(!is.data.table(multiClimateData))	{
		climateInput = data.table::as.data.table(readRDS(paste0(dataOut_location, 'ERA5_', basinName, '.RData')))
		climateInput = subset(climateInput, lubridate::year(Date) >= startYear & lubridate::year(Date) <= endYear)
	}	else	{
		climateInput = multiClimateData
		climateInput = subset(climateInput, lubridate::year(Date) >= startYear & lubridate::year(Date) <= endYear)
	}
	

	lat_radians = min((abs(thisLatitudeInDegrees)*pi/180), 1.1)				# from customer location (centroid)
		# totalInfiltration_f vars
	irrigationApplied = variablesTableRow$irrigationApplied					# timeseries of irrigation (daily); defaults to 0
	crn = variablesTableRow$crn							# curve number
	Smax = variablesTableRow$Smax								# maximum rz soil moisture storage
	Ia_scalar = variablesTableRow$Ia_scalar						# infiltration
	managedAquiferRecharge = variablesTableRow$Ia_scalar
		# gwRecharge_f vars
	rcScl_sm = variablesTableRow$rcScl_sm	# water retention curve scalar for soil moisture
	rcExp_sm = variablesTableRow$rcExp_sm	# water retention curve exponent for soil moisture
	Zr = variablesTableRow$Zr		# root depth [mm]
	n = variablesTableRow$n		# soil porosity [-]
	smhp = variablesTableRow$smhp	# soil moisture at hydroscopic point [-]
	smwp = variablesTableRow$smwp	# soil moisture at wilting point [-]
	smfc = variablesTableRow$smfc	# soil moisture field capacity [-]
	s0 = variablesTableRow$s0		# initial soil moisture  [-]
	rcScl_gw = variablesTableRow$rcScl_gw	# water retention scalar for gw rech
	initialBlackBucketStor = variablesTableRow$initialBlackBucketStor # initial black bucket storage (mm)
	PETexp = variablesTableRow$PETexp 	# exponent of PET decay
		# cropWaterNeeds_f vars
	avgCropWaterNeeds = variablesTableRow$avgCropWaterNeeds		# for alfalfa #1010  pistachio in ther san J; c(900, 1200) # citrus
	petGlobAvg = variablesTableRow$petGlobAvg
	mulching = variablesTableRow$mulching
		# irrigation_f vars
	irrigatedPctArea = variablesTableRow$irrigatedPctArea		# pct area that is regularly irrigated with gw
	dripIrrigation = variablesTableRow$dripIrrigation
		# netGwFlow_f vars
	regionalNetGwRtOfChange = variablesTableRow$regionalNetGwRtOfChange	# what is the long-term trend in gw resources, here likely from abutting GRACE / GRACE-FO tiles, given in mm / day; should be a small number
	initialStorageDifference = variablesTableRow$initialStorageDifference	# what is the initial head on location relative to regional average (pos is higher regional so net inflow, neg is lower regional so net outflow)
	maxStorageBalance = variablesTableRow$maxStorageBalance		# what is the maximum head imbalance in mm
	rcScl_netGw = variablesTableRow$rcScl_netGw					# recession curve scalar
	graceSinMod = graceSinMod		# model capturing the seasonal signal in gw head
	Date = c(climateInput$Date, data.table::last(climateInput$Date) + 1)

		
	totalInfiltration = totalInfiltration_f(
		PPT = climateInput$PPT,
		irrigationApplied = irrigationApplied,					# timeseries of irrigation (daily); defaults to 0
		crn = crn,								# curve number
		Smax = Smax,								# maximum rz soil moisture storage
		Ia_scalar = Ia_scalar,						# infiltration 
		irrigatedPctArea = irrigatedPctArea,
		managedAquiferRecharge = managedAquiferRecharge)
		
		# 1b PET, infiltration, AET, and recharge
	gwRechargeTable = gwRecharge_f(
		PPT = climateInput$PPT,				# from CAi 
		tmaxTS = climateInput$Tmax,							# from CAi
		tminTS = climateInput$Tmin,							# from CAi
		tavgTS = climateInput$Tavg,							# from CAi
		Date = climateInput$Date,
		thisLatitudeInDegrees = climateInput$PET,				# from customer location (centroid)
		totalInfiltration = totalInfiltration,			# taken from function above
		rcScl_sm = rcScl_sm,	# water retention curve scalar
		rcExp_sm = rcExp_sm,	# water retention curve exponent
		Zr = Zr,		# root depth [mm]
		n = n,		# soil porosity [-]
		smhp = smhp,	# soil moisture at hydroscopic point [-]
		smwp = smwp,	# soil moisture at wilting point [-]
		smfc = smfc,	# soil moisture field capacity [-]
		s0 = s0,		# initial soil moisture  [-]
		rcScl_gw = rcScl_gw,	# water retention scalar for gw rech
		initialBlackBucketStor = initialBlackBucketStor, # initial black bucket storage (mm)
		PETexp = PETexp) 	# exponent of PET decay
		
		
		# step 2: estimating gw recharge and net infiltration in ag lands
	cropWaterNeeds = cropWaterNeeds_f(
		petTS = gwRechargeTable$PET,
		avgCropWaterNeeds = avgCropWaterNeeds,		# for alfalfa #1010  pistachio in ther san J; c(900, 1200) # citrus
		petGlobAvg = petGlobAvg,
		mulching = mulching)

		# step 3: estimating irrigation needs for ag lands
	irrigationNeeded =	irrigation_f(
		irrigatedPctArea = irrigatedPctArea,		# pct area that is regularly irrigated with gw
		cropWaterNeedsTS = cropWaterNeeds,	#ts of crop water needs from above function
		infiltrationTS = c(totalInfiltration, mean(totalInfiltration)) - gwRechargeTable$gwRecharge,		# ts of cropland infiltration (minus gw rech) from above function
		dripIrrigation = dripIrrigation)

		# step 4: estimating net groundwater flow across basins
	netGwFlow = netGwFlow_f(
		gwRecharge = gwRechargeTable$gwRecharge,	# a time series of mass flux
		irrigationNeeded = irrigationNeeded,	# time series of area-averaged irrigation
		regionalNetGwRtOfChange = regionalNetGwRtOfChange,	# what is the long-term trend in gw resources, here likely from abutting GRACE / GRACE-FO tiles, given in mm / day; should be a small number
		graceSinMod = graceSinMod,				# model capturing the seasonal signal of gw head
		histDates = Date,		
		initialStorageDifference = initialStorageDifference,	# what is the initial head on location relative to regional average (pos is higher regional so net inflow, neg is lower regional so net outflow)
		maxStorageBalance = maxStorageBalance,		# what is the maximum head imbalance in mm
		rcScl_netGw = rcScl_netGw)					# recession curve scalar

	totalInfiltration = c(totalInfiltration, NA)
	return(cbind(Date, totalInfiltration, gwRechargeTable, cropWaterNeeds, irrigationNeeded, netGwFlow))
}




############################################################################################################
# part 3 calibration
calibrateRegionalGWmodel_f = function(
	variablesTable = variablesTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	numSamples = 1000, 
	maxNumRuns = 1000^2, 
#	targetMetric = 1, 					# 1 = KGE, 2 = NSE, 3 = MAE, 4 = RMSE, 5 = bias
	targetMetricValue = 0.45,			# threshold for considering a value good
	targetMetricRatioDecrease = 0.9,	# the ratio at which the threshold is diminished
	minGoodRuns = 200,					# number of 'good' calibrations before the routine stops		
	dataOut_location ='save_file_location',
	basinName = 'basin_name',
	thisLatitudeInDegrees = 35,
	startYear = 2000,
	endYear = 2099)
	{

		# initializaing the calibration output table
	calOut = variablesTable[1:minGoodRuns, ]
	calOut$KGE =  -99999
	calOut$NSE =  -99999
	calOut$mae =  -99999
	calOut$mse =  -99999
	calOut$rmse =  -99999
	calOut$pbias =  -99999
	calOut$rPearson =  -99999
	calOut$rSquared =  -99999
	

	thisRow = 0
	goodRuns = 0
	while(thisRow <= maxNumRuns & goodRuns < minGoodRuns)	{
		if(thisRow %% numSamples == 0)	{
			library(data.table)
			variablesTable = variablesTable[ , lapply(.SD, sample)]
			targetMetricValue = targetMetricValue * .9
			thisRow = 0
		}
		thisRow = thisRow + 1
		if(thisRow %% 100 == 0)	{print(thisRow)}

		regionalGwModel = runRegionalGWmodel_f(
			dataOut_location = dataOut_location,
			basinName = basinName, #'basin_name',
			thisLatitudeInDegrees = locationLat,
			variablesTableRow = variablesTable[thisRow,],
			graceSinMod = graceSinMod,				# model capturing the seasonal signal of gw head
			multiClimateData = NA, 
			startYear = startYear,
			endYear = endYear)
	
	
		allDataOutput = merge(graceTS, regionalGwModel, all.y=TRUE)
		allDataOutput$AnomalyInterp = zoo::na.fill(allDataOutput$Anomaly, c(NA, 'extend', NA))
		allDataOutput$UncertaintyInterp = zoo::na.fill(allDataOutput$Uncertainty, c(NA, 'extend', NA))

		allDataSubset = subset(allDataOutput, Date >= graceTS$Date[1])
		allDataSubset = allDataSubset[-nrow(allDataSubset),]

		thisKGE = hydroGOF::KGE(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
		if(thisKGE > targetMetricValue)	{
			goodRuns = goodRuns + 1
			calOut[goodRuns, ] = NA
			calOut[goodRuns, 1:ncol(variablesTable)] = variablesTable[thisRow]
			calOut$KGE[goodRuns] = thisKGE
			calOut$NSE[goodRuns] = hydroGOF::NSE(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
			calOut$mae[goodRuns] = hydroGOF::mae(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
			calOut$mse[goodRuns] = hydroGOF::mse(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
			calOut$rmse[goodRuns] = hydroGOF::rmse(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
			calOut$pbias[goodRuns] = hydroGOF::pbias(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
			calOut$rPearson[goodRuns] = hydroGOF::rPearson(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
			calOut$rSquared[goodRuns] = summary(lm(allDataSubset$AnomalyInterp ~ allDataSubset$subsidizedRegionalStorage))$adj.r.squared

			data.table::fwrite(calOut, paste0(dataOut_location, basinName, 'calOut.csv'))
			plot(allDataSubset$AnomalyInterp, type='l', lty=2, lwd = 3)
			lines(allDataSubset$subsidizedRegionalStorage, col='red', lwd=2)
			print(calOut[goodRuns])
		}
		
	}
	return(calOut)
}





############################################################################################################
# part 4 projecting with cmip6 and multimodel runs
projectRegionalGWmodel_f = function(
	variablesTable = calOut,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location ='save_file_location',
	basinName = 'basin_name',
	thisLatitudeInDegrees = 35,
	startYear = 2000,
	endYear = 2099)
	{

	variablesTableOrder = c(rev(order(variablesTable$KGE))[1:20], rev(order(variablesTable$NSE))[1:5],
		order(variablesTable$rmse)[1:2], order(variablesTable$mae)[1:2], order(variablesTable$rmse)[1:2], order(abs(variablesTable$pbias))[1:5], rev(order(variablesTable$rPearson)[1:2]))
	variablesTableSort = variablesTable[unique(variablesTableOrder), ]
	mulching = variablesTableSort$mulching[1]
	dripIrrigation = variablesTableSort$dripIrrigation[1]
	managedAquiferRecharge = variablesTableSort$managedAquiferRecharge[1]
	regionalFlatGW = variablesTableSort$regionalFlatGW[1] 

	histClimateInput = data.table::as.data.table(readRDS(paste0(dataOut_location, 'ERA5_', basinName, '.RData')))
	
	scenNames = c('ssp126', 'ssp245', 'ssp585')
	for(thisScen in 1:3)	{
		climateInputAllMods = readRDS(paste0(dataOut_location, 'CMIP65_', basinName, '_', scenNames[thisScen], '.RData'))
		numCalibrations = length(climateInputAllMods)
		
		climateDataList = list()
		thisIter = 0
		
		for(thisClimateModel in 1:length(climateInputAllMods))	{
			projClimateData = data.table::as.data.table(climateInputAllMods[[thisClimateModel]])
			multiClimateData = rbind(histClimateInput, subset(projClimateData, Date > max(histClimateInput$Date)))
	

			if(all(!is.na(multiClimateData)))	{	# not sure why but getting some ppt nas
				
				for(thisCalibration in 1:numCalibrations)	{
					variablesTableRow = variablesTableSort[thisCalibration, ]
					if(variablesTableRow$regionalFlatGW)	{variablesTableRow$regionalNetGwRtOfChange = 0}
					
					thisIter = thisIter + 1
					regionalGwModel = runRegionalGWmodel_f(
							dataOut_location = dataOut_location,
							basinName = basinName, #'basin_name',
							thisLatitudeInDegrees = locationLat,
							variablesTableRow = variablesTableRow,
							graceSinMod = graceSinMod,				# model capturing the seasonal signal of gw head
							multiClimateData = multiClimateData, 
							startYear = startYear,
							endYear = endYear)

					plot(regionalGwModel$Date, regionalGwModel$subsidizedRegionalStorage, type='l')
					print(c((tail(regionalGwModel$subsidizedRegionalStorage, 2)[1] - regionalGwModel$subsidizedRegionalStorage[1]) / 85, thisCalibration))
					climateDataList[[thisIter]] = as.data.table(regionalGwModel)
				}
			}	else	{ print(multiClimateData)}
		}
		
		saveRDS(climateDataList, paste0(dataOut_location, 'projectedOutputs_', scenNames[thisScen], '_', basinName, mulching, dripIrrigation, managedAquiferRecharge, regionalFlatGW, '.RData'))
	}
}







#######################################################################
### part 5: saving outputs as figures and summary tables


dataSaver_f = function(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames,
	calValPlots = FALSE)
	{
		
	dataNamesDT = data.frame(
		columnName = c('totalInfiltration',			 'soilMoistureVol',	 'gwRecharge', 						'PET', 			'AET', 			'cropWaterNeeds', 				'irrigationNeeded',			 'netGwFlow', 					'subsidizedRegionalStorage'),
		sclr = c(		365.25, 						1,					365.25,							365.25,			365.25,			365.25,							365.25,							365.25,							1),
		ylabName = c('Infiltration (mm / yr)', 'Soil Moisture (mm)',  'Annual Avg Gw Recharge (mm)', 'PET (mm / day)', 'AET (mm / day)', 'Crop Water Needs (mm / yr)', 'Irrigation Demand (mm / yr)', 'Regional Subsidy (mm / yr)','Change in Gw Storage (mm)'),
		thisVarName = c('_infil', 		      '_smAvg', 				'_gwRech',						'_pet', 			'_aet' ,			'_cwn',							'_irrigDmd', 					'_netGwFl', 				'gwStorageProjections'))								

	dataHighlights = data.frame(
		Scenario = NA, Location = NA, Variable = NA, Q05 = NA, Q25 = NA, Q50 = NA, Q75 = NA, Q95 = NA, Mean = NA, SD = NA, Trend = NA, Significance = NA)

	for(thisScen in 1:3)	{
		
		projectedData = readRDS(paste0(dataOut_location, 'projectedOutputs_', scenNames[thisScen], '_', basinName, 
			calibrationTable$mulching[1], calibrationTable$dripIrrigation[1], calibrationTable$managedAquiferRecharge[1], calibrationTable$regionalFlatGW[1] ,'.RData'))
		
		for(thisVar in 1:nrow(dataNamesDT))	{
			thisVarMtrx = matrix(nrow=nrow(projectedData[[1]]), ncol=length(projectedData))
			allDates = projectedData[[1]]$Date


			for(i in 1:length(projectedData))	{
				thisDF = projectedData[[i]]
				thisVarMtrx[ , i] = unlist(subset(thisDF, select = dataNamesDT$columnName[thisVar]) * dataNamesDT$sclr[thisVar])
			}

			thisYlab = dataNamesDT$ylabName[thisVar] # 'Regional Subsidy (mm / yr)' #'Irrigation Demand (mm / yr)' #'Crop Water Needs (mm / yr)' #'AET (mm / day)' #'PET (mm / day)' #'Annual Avg Gw Recharge (mm)' #'Soil Moisture (mm)' #'Annual Avg Infiltration (mm)'
			thisVarName = dataNamesDT$thisVarName[thisVar] #'_irrigDmd' #'_cwn' #'_aet' #'_pet' #'_gwRech' #'_smAvg' #'_annAvgInfl'

			for(thisLoc in 1:nrow(customerLocations))	{
				locationName = unlist(customerLocations[thisLoc, "Location (name)"])
				
				vectoredMtrx = as.vector(thisVarMtrx)
				repDates = rep(allDates, ncol(thisVarMtrx))
				lnMod = speedglm::speedlm(vectoredMtrx ~ repDates)

				dataHighlights = rbind(dataHighlights, data.frame(
				Scenario = scenNames[thisScen],
				Location = locationName,
				Variable = thisYlab,
				Q05 = quantile(vectoredMtrx, 0.05, na.rm=TRUE),
				Q25 = quantile(vectoredMtrx, 0.25, na.rm=TRUE),
				Q50 = quantile(vectoredMtrx, 0.50, na.rm=TRUE),
				Q75 = quantile(vectoredMtrx, 0.75, na.rm=TRUE),
				Q95 = quantile(vectoredMtrx, 0.95, na.rm=TRUE),
				Mean = mean(vectoredMtrx, na.rm=TRUE),
				SD = sd(vectoredMtrx, na.rm=TRUE),
				Trend = lnMod$coefficients[2] * 365.25,
				Significance = summary(lnMod)$f.pvalue)) 

				png(paste0(dataOut_location, locationName, '_', scenNames[thisScen],thisVarName,
					calibrationTable$mulching[1], calibrationTable$dripIrrigation[1], calibrationTable$managedAquiferRecharge[1],calibrationTable$regionalFlatGW [1],'.png'), width=1200, height=600)
				par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
				windowsFonts(A = windowsFont("Roboto"))
				
				kmeansSmoothQ05 = zoo::na.fill(ksmooth(allDates, apply(thisVarMtrx, 1, function(x) quantile(x, probs = 0.05, na.rm=TRUE)), bandwidth = 365, kernel = 'normal')$y, 'extend')
				kmeansSmoothQ95 = zoo::na.fill(ksmooth(allDates, apply(thisVarMtrx, 1, function(x) quantile(x, probs = 0.95, na.rm=TRUE)), bandwidth = 365, kernel = 'normal')$y, 'extend')
				plot(allDates, apply(thisVarMtrx, 1, mean),
					ylim=c(min(kmeansSmoothQ05), max(kmeansSmoothQ95)),
					type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
					main='', ylab=thisYlab, xlab='',
					col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
					family='A')
					
				polygon(x=c(allDates, rev(allDates)), 
						y=c(kmeansSmoothQ05, rev(kmeansSmoothQ95)),
						col=adjustcolor('#0098B2', offset = c(0.95,0.95,0.95,0)), border=NA)
			#			col=adjustcolor('#0098B2', alpha=1), border=NA)

				kmeansSmoothQ10 = zoo::na.fill(ksmooth(allDates, apply(thisVarMtrx, 1, function(x) quantile(x, probs = 0.10, na.rm=TRUE)), bandwidth = 365, kernel = 'normal')$y, 'extend')
				kmeansSmoothQ90 = zoo::na.fill(ksmooth(allDates, apply(thisVarMtrx, 1, function(x) quantile(x, probs = 0.90, na.rm=TRUE)), bandwidth = 365, kernel = 'normal')$y, 'extend')
				polygon(x=c(allDates, rev(allDates)), 
						y=c(kmeansSmoothQ10, rev(kmeansSmoothQ90)),
						col=adjustcolor('#0098B2', offset = c(0.9,0.9,0.9,0)), border=NA)
			#			col=adjustcolor('#0098B2', alpha=1), border=NA)

				kmeansSmoothQ25 = zoo::na.fill(ksmooth(allDates, apply(thisVarMtrx, 1, function(x) quantile(x, probs = 0.25, na.rm=TRUE)), bandwidth = 365, kernel = 'normal')$y, 'extend')
				kmeansSmoothQ75 = zoo::na.fill(ksmooth(allDates, apply(thisVarMtrx, 1, function(x) quantile(x, probs = 0.75, na.rm=TRUE)), bandwidth = 365, kernel = 'normal')$y, 'extend')
				polygon(x=c(allDates, rev(allDates)), 
						y=c(kmeansSmoothQ25, rev(kmeansSmoothQ75)),
						col=adjustcolor('#0098B2', offset = c(0.8,0.8,0.8,0)), border=NA)
			#			col=adjustcolor('#0098B2', alpha=1), border=NA)

				abline(h=0, lwd=3, lty=3, col='grey65')

				kmeansSmooth = zoo::na.fill(ksmooth(allDates, apply(thisVarMtrx, 1, function(x) mean(x, na.rm=TRUE)) , bandwidth = 365, kernel = 'box')$y, 'extend')
				lines(allDates, kmeansSmooth,
					col='#0098B2', lwd=3)
					
				usr <- par("usr")   # save old user/default/system coordinates
				par(usr = c(0, 1, 0, 1)) # new relative user coordinates
				text(x=0.02, y=0.05, scenNames[thisScen], col='grey25', cex=2.2, pos=4)
				par(usr = usr) # restore original user coordinates

				dev.off()
			}
		}
	}
	fwrite(dataHighlights, paste0(dataOut_location, locationName, '_', 'projectionsHighlights', 
		calibrationTable$mulching[1], calibrationTable$dripIrrigation[1], calibrationTable$managedAquiferRecharge[1], calibrationTable$regionalFlatGW[1],'.csv'))

	if(calValPlots)	{
			# cal val plots
		png(paste0(dataOut_location, locationName, '_', 'calVal_gwStorageProjections.png'), width=1200, height=600)
		par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))

		whichCalDates = which(projectedData[[1]]$Date < max(graceTS$Date))
		calDates = projectedData[[1]]$Date[whichCalDates]
		calMtrx = thisVarMtrx[whichCalDates, ]
		thisYlab = 'Groundwater Storage (mm)'

		kmeansSmoothQ05 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.05, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
		kmeansSmoothQ95 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.95, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
		plot(calDates, apply(calMtrx, 1, mean),
			ylim=c(min(kmeansSmoothQ05), max(kmeansSmoothQ95)),
			type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
			main='', ylab=thisYlab, xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
					
		polygon(x=c(calDates, rev(calDates)), 
				y=c(kmeansSmoothQ05, rev(kmeansSmoothQ95)),
				#col=adjustcolor('#0098B2', offset = c(0.95,0.95,0.95,0)), border=NA)
				col=adjustcolor('#0098B2', alpha=.1), border=NA)

		kmeansSmoothQ10 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.10, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
		kmeansSmoothQ90 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.90, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
		polygon(x=c(calDates, rev(calDates)), 
				y=c(kmeansSmoothQ10, rev(kmeansSmoothQ90)),
		#		col=adjustcolor('#0098B2', offset = c(0.9,0.9,0.9,0)), border=NA)
				col=adjustcolor('#0098B2', alpha=.1), border=NA)

		kmeansSmoothQ25 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.25, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
		kmeansSmoothQ75 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.75, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
		polygon(x=c(calDates, rev(calDates)), 
				y=c(kmeansSmoothQ25, rev(kmeansSmoothQ75)),
		#		col=adjustcolor('#0098B2', offset = c(0.8,0.8,0.8,0)), border=NA)
				col=adjustcolor('#0098B2', alpha=.1), border=NA)

		abline(h=0, lwd=3, lty=3, col='grey65')

		kmeansSmooth = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) mean(x, na.rm=TRUE)) , bandwidth = 1, kernel = 'box')$y, 'extend')
		lines(calDates, kmeansSmooth,
			col='#0098B2', lwd=2)
					
		points(graceTS$Date, graceTS$Anomaly, 
			col='grey25', lwd=2, pch=1)
		lines(graceTS$Date, graceTS$Anomaly, 
			col='grey25', lwd=1, lty=1)

		usr <- par("usr")   # save old user/default/system coordinates
		par(usr = c(0, 1, 0, 1)) # new relative user coordinates
		text(x=0.02, y=0.15, 'Model Ensemble', col='#0098B2', cex=2.2, pos=4)
		text(x=0.02, y=0.08, 'Benchmark Data', col='grey25', cex=2.2, pos=4)
		par(usr = usr) # restore original user coordinates

		dev.off()
	}
}
