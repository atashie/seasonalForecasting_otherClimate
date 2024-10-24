# load necessary functions
	# gw specific functions
source('C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\groundwaterModelingGRACEonly_functions.R')
source('C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\groundwaterModelingGRACEtileSelection_functions.R')
source('C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\longtermClimateReanalysis_functions.R')
	# basin delineation functions
source('C:\\Users\\arik\\Documents\\GitHub\\SurfaceWaterProjections\\surfaceWaterModeling_functions.r')


# file storage information
basinName = 'WestHillsFarms' #'BeefNW'
dataOut_location = paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\')

# locations information
customerLocations = data.table::fread(paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\', 'Customer Onboarding Information_', basinName, '.csv'), skip=1)
locationLon = customerLocations$Longitude[1]
locationLat = customerLocations$Latitude[1]
locationName = unlist(customerLocations[1, 'Location (name)'])
scenNames = c('ssp126', 'ssp245', 'ssp585')


era5DataNCDF =  paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\', basinName, '-testing-recent-era.nc')
era5StartDate =  as.Date('1998-08-01') #as.Date('1980-01-01') # + ncvar_get(ncin_era5, 'time') for calculating actual dates 
cmip6DataFile =  paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\cmip6ncs\\')
cmip6StartDate =  as.Date('1850-01-01') #as.Date('1980-01-01') # + ncvar_get(ncin_era5, 'time') for calculating actual dates 


#climateHistLoc = 'J:\\Cai_data\\Rabo\\Locations\\BeefNW\\'
#climateHistFileName = 'BeefNW-testing-recent-era.nc'
#climateHistoric_nc = ncdf4::nc_open(paste0(climateHistLoc, climateHistFileName))
#climLons = ncdf4::ncvar_get(climateHistoric_nc, 'longitude')
#climLats = ncdf4::ncvar_get(climateHistoric_nc, 'latitude')
#climHistDates = ncdf4::ncvar_get(climateHistoric_nc, 'time') + as.Date('1980-01-01')



#########################################################################################################
	# step 1
	# delineate a new basin
basinDelineation_f(
	gageLonLat = c(locationLon, locationLat),
	basinATLAS_locationAndFile = 'J:\\Cai_data\\BasinATLAS_Data_v10\\BasinATLAS_v10.gdb',
	dataOut_location = dataOut_location,
	basinName)


#########################################################################################################
	# step 2
	# import and convert historical climate data
climateInputConversion_f(
	basinName = basinName,
	climateDataNCDF = era5DataNCDF,
	tempConversionFactor = NA,
	pptConversionFactor = NA,
	avgTempGiven = FALSE, 
	startDate = era5StartDate, 	# when does the clock of the netcdf start?
	timeToDaysConversion = 1,	# convert time increments to days if necessary
	dataOut_location = dataOut_location,
	optionForPET = 1, 	# 1 = PET_fromTemp modified Pen-Mon, 
	variableOrderOption = 'era5', # # 'era5' = [longitude,latitude,time]; 'cfs' = [longitude, latitude, member, step]; 'seas5' = [longitude, latitude, member, lead_time] for tmax and tmin but [lead_time, longitude, latitude, member] for tp_sum]
	precipName = 'tp_sum')	# other options include: tp, tp_sum	
	


#########################################################################################################
	# step 3 
	# grace time series for location of interest
	# grace tile selection w/ smoothing
graceTS = graceTileSelection_f(
	graceFileLoc = 'J:\\Cai_data\\Rabo\\GRACE\\',
	graceFileName = 'GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc',
	locationLat = locationLat,
	locationLon = locationLon,
	reweightingExponent = 2)
	

	# selection of neighboring grace tiles w/ limited smoothing
graceNeighborTS = graceNeighborTileSelection_f(
	graceFileLoc = 'J:\\Cai_data\\Rabo\\GRACE\\',
	graceFileName = 'GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc',
	locationLat = locationLat,
	locationLon = locationLon,
	reweightingExponent = 1)
	



#########################################################################################################
	# step 4
	# calibration

numSamples = 10000 #*50
	# estimate average values and ranges for each variable
predicand = as.vector(graceNeighborTS$Anomaly) ; predictor = as.vector(graceNeighborTS$Date)
avgGraceTrend = mblm::mblm(predicand ~ predictor)$coefficients[2] * 365.25
graceRadians = sin(2 * pi * lubridate::yday(graceNeighborTS$Date) / 365.25)
graceSinMod = lm(predicand ~ graceRadians)


variablesTable = data.table::data.table(
		# totalInfiltration_f vars
	irrigationApplied = rep(0, numSamples),					# mean irrigation; defaults to 0
	crn = abs(rnorm(numSamples, 60, 10)),								# curve number
	Smax = sample(seq(100, 1000, 1), numSamples, replace=TRUE),						 		# maximum rz soil moisture storage
	Ia_scalar = abs(rnorm(numSamples, 0.15, 1)),						# infiltration 
	managedAquiferRecharge = FALSE,						
		# gwRecharge_f vars
	rcScl_sm = sample(seq(0.01, .5, 0.01), numSamples, replace=TRUE),	# water retention curve scalar for soil moisture
	rcExp_sm = sample(seq(1, 2, 0.01), numSamples, replace=TRUE),	# water retention curve exponent for soil moisture
	Zr = sample(seq(100, 2500, 1), numSamples, replace=TRUE),		# root depth [mm]										# seq(100, 2500, 1)
	n = sample(seq(0.15, 0.75, .01), numSamples, replace=TRUE),		# soil porosity [-]
	smhp = sample(seq(0.00, 0.05, .01), numSamples, replace=TRUE),	# soil moisture at hygroscopic point [-]
	smwp = sample(seq(0.06, 0.15, .01), numSamples, replace=TRUE),	# soil moisture at wilting point [-]
	smfc = sample(seq(0.15, 0.55, .01), numSamples, replace=TRUE),	# soil moisture field capacity [-]
	s0 = 0,		# initial soil moisture  [-]
	rcScl_gw = sample(seq(0.01, 1, 0.01), numSamples, replace=TRUE),	# water retention scalar for gw rech
	initialBlackBucketStor = 100, # initial black bucket storage (mm)
	PETexp = 2, 	# exponent of PET decay
		# cropWaterNeeds_f vars
	avgCropWaterNeeds = abs(rnorm(numSamples, 900, 100)) / 365.25,		# for alfalfa #1010  pistachio in ther san J; c(900, 1200) # citrus
	petGlobAvg =  abs(rnorm(numSamples, 1750, 250)) / 365.25,
	mulching = FALSE,
		# irrigation_f vars
	irrigatedPctArea = abs(rnorm(numSamples, .3, 0.15)),		# pct area that is regularly irrigated with gw
	dripIrrigation = TRUE,
		# netGwFlow_f vars
	regionalNetGwRtOfChange = rnorm(numSamples, avgGraceTrend, abs(avgGraceTrend) * 0.5),	# what is the long-term trend in gw resources, here likely from abutting GRACE / GRACE-FO tiles, given in mm / day; should be a small number
	initialStorageDifference = sample(seq(-2000,2000,1), numSamples, replace=TRUE),	# what is the initial head on location relative to regional average (pos is higher regional so net inflow, neg is lower regional so net outflow)
	maxStorageBalance = sample(seq(1001,10000,1), numSamples, replace=TRUE),		# what is the maximum head imbalance in mm
	rcScl_netGw = sample(seq(0.001, 0.5, 0.001), numSamples, replace=TRUE) / 365.25)				# recession curve scalar


calibrationTable = calibrateRegionalGWmodel_f(
	variablesTable = variablesTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod,
	numSamples = numSamples,
	maxNumRuns = numSamples^3, 
#	targetMetric = 1, 					# 1 = KGE, 2 = NSE, 3 = MAE, 4 = RMSE, 5 = bias
	targetMetricValue = 0.9,			# threshold for considering a value good
	targetMetricRatioDecrease = 0.99,	# the ratio at which the threshold is diminished
	minGoodRuns = 200,					# number of 'good' calibrations before the routine stops		
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

summary(calibrationTable)
#calibrationTable = data.table::fread(paste0(dataOut_location, 'WestHillsFarmsCalOut.csv'))
data.table::fwrite(calibrationTable, paste0(dataOut_location, 'calVal_', basinName, variablesTable$mulching[1], variablesTable$dripIrrigation[1], variablesTable$managedAquiferRecharge[1], '.csv'))
#calibrationTable = data.table::fread(paste0(dataOut_location, 'calVal_', basinName, variablesTable$mulching[1], variablesTable$dripIrrigation[1], variablesTable$managedAquiferRecharge[1], '.csv'))




#########################################################################################################
	# step 5
	# projections
	
	# step 5-a
	# import and convert projections climate data
climateInputConversionLongterm_f(
	basinName = basinName,
	climateDataNCDF = cmip6DataFile,
	climateDataNCDF_subName = 'pp_future_daily_works.nc',
	tempConversionFactor = NA,
	pptConversionFactor = NA,
	avgTempGiven = FALSE, 
	startDate = cmip6StartDate, 	# when does the clock of the netcdf start?
	timeToDaysConversion = 1,	# convert time increments to days if necessary
	dataOut_location = dataOut_location,
	optionForPET = 1, 	# 1 = PET_fromTemp modified Pen-Mon, 
	variableOrderOption = 'cmip6', # # 'era5' = [longitude,latitude,time]; 'cfs' = [longitude, latitude, member, step]; 'seas5' = [longitude, latitude, member, lead_time] for tmax and tmin but [lead_time, longitude, latitude, member] for tp_sum]
	precipName = 'tp', 
	limitedModels = NA)	# other options include: tp, tp_sum	



	# step 5-b
	# run each combination of climate projection and model parameterization
	# plotting 
#calibrationTable$mulching = FALSE
#calibrationTable$dripIrrigation = TRUE
#calibrationTable$managedAquiferRecharge = FALSE
calibrationTable$regionalFlatGW = FALSE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames,
	calValPlots = TRUE)
	





	#  alterations: just mar
calibrationTable$mulching = FALSE
calibrationTable$dripIrrigation = TRUE
calibrationTable$managedAquiferRecharge = TRUE
calibrationTable$regionalFlatGW = FALSE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)
	

	#  alterations: just mulch
calibrationTable$mulching = TRUE
calibrationTable$dripIrrigation = TRUE
calibrationTable$managedAquiferRecharge = FALSE
calibrationTable$regionalFlatGW = FALSE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)
	

	#  alterations: mulch and mar
calibrationTable$mulching = TRUE
calibrationTable$dripIrrigation = TRUE
calibrationTable$managedAquiferRecharge = TRUE
calibrationTable$regionalFlatGW = FALSE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)
	



	#  alterations: no drip
calibrationTable$mulching = FALSE
calibrationTable$dripIrrigation = FALSE
calibrationTable$managedAquiferRecharge = FALSE
calibrationTable$regionalFlatGW = FALSE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)
	


	#  alterations: EVERYTHING
calibrationTable$mulching = TRUE
calibrationTable$dripIrrigation = TRUE
calibrationTable$managedAquiferRecharge = TRUE
calibrationTable$regionalFlatGW = TRUE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)



#  alterations: just mar
calibrationTable$mulching = FALSE
calibrationTable$dripIrrigation = TRUE
calibrationTable$managedAquiferRecharge = TRUE
calibrationTable$regionalFlatGW = TRUE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)
	

	#  alterations: just mulch
calibrationTable$mulching = TRUE
calibrationTable$dripIrrigation = TRUE
calibrationTable$managedAquiferRecharge = FALSE
calibrationTable$regionalFlatGW = TRUE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)
	

	#  alterations: no drip
calibrationTable$mulching = FALSE
calibrationTable$dripIrrigation = FALSE
calibrationTable$managedAquiferRecharge = FALSE
calibrationTable$regionalFlatGW = TRUE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)
	


	#  alterations: yes drip and regional guidance
calibrationTable$mulching = FALSE
calibrationTable$dripIrrigation = TRUE
calibrationTable$managedAquiferRecharge = FALSE
calibrationTable$regionalFlatGW = TRUE
projectRegionalGWmodel_f(
	variablesTable = calibrationTable,
	graceTS = graceTS,
	graceSinMod = graceSinMod, 
	dataOut_location = dataOut_location,
	basinName = basinName,
	thisLatitudeInDegrees = locationLat,
	startYear = 2000,
	endYear = 2099)

dataSaver_f(
	dataOut_location = dataOut_location,
	calibrationTable = calibrationTable,
	scenNames = scenNames)
	



mmToAcreFt =(1/1000) * 8.107
mmToFt = (1/304.8)
hecToAc = (1/0.4046856)
sqmToHec = 10000
costMarPrCbm = 0.11
costMarPrCbmSmallField = 0.57
estDepthRunoffClctInM = 0.1
costWaterPerAcFt = 223.96
costMulchCbcYrd = 5
cbcYrdToCvrOneHec = 1000
fracLandLftUnlmch = 0.5
numYrsBtwMlch = 3
fracIrrLand = mean(calibrationTable$irrigatedPctArea)
costToPumpOneAcFt = 40
yearsOfMarWoUpkeep = 10

regionalAction = FALSE

currentConditions = fread(paste0(dataOut_location, locationName, '_', 'projectionsHighlights', 
		'FALSE','TRUE','FALSE',regionalAction,'.csv'))
addMulching = fread(paste0(dataOut_location, locationName, '_', 'projectionsHighlights', 
		'TRUE','TRUE','FALSE',regionalAction,'.csv'))
addMar = fread(paste0(dataOut_location, locationName, '_', 'projectionsHighlights', 
		'FALSE','TRUE','TRUE',regionalAction,'.csv'))
addMarAndMulch = fread(paste0(dataOut_location, locationName, '_', 'projectionsHighlights', 
		'TRUE','TRUE','TRUE',regionalAction,'.csv'))


summaryOutputDt = data.table(Mitigation_Strategy = NA, Reduced_Irrigation_Demand = NA, Reduced_Irrigation_Costs = NA, Change_in_Storage_by_2050s = NA, Implementation_Cost = NA)
summaryOutputDt = rbind(summaryOutputDt, 
	data.table(
		Mitigation_Strategy = 'Business as Usual',
		Reduced_Irrigation_Demand = NA,
		Reduced_Irrigation_Costs = NA,
		Change_in_Storage_by_2050s = subset(currentConditions, Scenario == 'ssp245' & Variable == 'Change in Gw Storage (mm)')$Trend * 35,
		Implementation_Cost = NA))

summaryOutputDt = rbind(summaryOutputDt, 
	data.table(
		Mitigation_Strategy = 'Mulching',
		Reduced_Irrigation_Demand = (subset(currentConditions, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50 - subset(addMulching, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50) / fracIrrLand,
		Reduced_Irrigation_Costs = costToPumpOneAcFt * mmToAcreFt * (subset(currentConditions, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50 - subset(addMulching, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50) / fracIrrLand,
		Change_in_Storage_by_2050s = subset(addMulching, Scenario == 'ssp245' & Variable == 'Change in Gw Storage (mm)')$Trend * 35,
		Implementation_Cost = costMulchCbcYrd * cbcYrdToCvrOneHec * fracLandLftUnlmch / numYrsBtwMlch))

summaryOutputDt = rbind(summaryOutputDt, 
	data.table(
		Mitigation_Strategy = 'On-Site MAR',
		Reduced_Irrigation_Demand = (subset(currentConditions, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50 - subset(addMar, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50) / fracIrrLand,
		Reduced_Irrigation_Costs = costToPumpOneAcFt * mmToAcreFt * (subset(currentConditions, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50 - subset(addMar, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50) / fracIrrLand,
		Change_in_Storage_by_2050s = subset(addMar, Scenario == 'ssp245' & Variable == 'Change in Gw Storage (mm)')$Trend * 35,
		Implementation_Cost = sqmToHec * costMarPrCbmSmallField * estDepthRunoffClctInM / yearsOfMarWoUpkeep))

summaryOutputDt = rbind(summaryOutputDt, 
	data.table(
		Mitigation_Strategy = 'Mulching + On-Site MAR',
		Reduced_Irrigation_Demand = (subset(currentConditions, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50 - subset(addMarAndMulch, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50) / fracIrrLand,
		Reduced_Irrigation_Costs = costToPumpOneAcFt * mmToAcreFt * (subset(currentConditions, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50 - subset(addMarAndMulch, Scenario == 'ssp245' & Variable == 'Irrigation Demand (mm / yr)')$Q50) / fracIrrLand,
		Change_in_Storage_by_2050s = subset(addMarAndMulch, Scenario == 'ssp245' & Variable == 'Change in Gw Storage (mm)')$Trend * 35,
		Implementation_Cost = sum(subset(summaryOutputDt, Mitigation_Strategy %in% c('Mulching', 'On-Site MAR'))$Implementation_Cost)))

summaryOutputDt = rbind(summaryOutputDt, 
	data.table(
		Mitigation_Strategy = 'Imported MAR',
		Reduced_Irrigation_Demand = NA,
		Reduced_Irrigation_Costs = NA,
		Change_in_Storage_by_2050s = 0,
		Implementation_Cost = mmToFt * hecToAc * costWaterPerAcFt * (1/fracIrrLand) * 
			abs(subset(currentConditions, Scenario == 'ssp245' & Variable == 'Change in Gw Storage (mm)')$Trend) +
			subset(summaryOutputDt, Mitigation_Strategy == 'On-Site MAR')$Implementation_Cost))
fwrite(summaryOutputDt, paste0(dataOut_location, locationName, '_', 'summaryMitigationHighlights', regionalAction,'.csv'))











######################################################
#### !!!!!!!!!!!!!
#### needs to be rewritten as a function
#### !!!!!!!!!!!!!
######################################################
dataNamesDT = data.frame(
	columnName = c('totalInfiltration',			 'soilMoistureVol',	 'gwRecharge', 						'PET', 			'AET', 			'cropWaterNeeds', 				'irrigationNeeded',			 'netGwFlow', 					'subsidizedRegionalStorage'),
	sclr = c(		365.25, 						1,					365.25,							365.25,			365.25,			365.25,							365.25,							365.25,							1),
	ylabName = c('Infiltration (mm / yr)', 'Soil Moisture (mm)',  'Annual Avg Gw Recharge (mm)', 'PET (mm / day)', 'AET (mm / day)', 'Crop Water Needs (mm / yr)', 'Irrigation Demand (mm / yr)', 'Regional Subsidy (mm / yr)','Change in Gw Storage (mm)'),
	thisVarName = c('_infil', 		      '_smAvg', 				'_gwRech',						'_pet', 			'_aet' ,			'_cwn',							'_irrigDmd', 					'_netGwFl', 				'gwStorageProjections'))								

dataHighlights = data.frame(
	Scenario = NA, Location = NA, Variable = NA, Trend = NA, Significance = NA)

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

























#########################################################################################################
	# redundant step, only used for initial analysis
	# sensitivity analysis
numRuns = 1000*1000#*50
	# estimate average values and ranges for each variable
predicand = as.vector(graceNeighborTS$Anomaly) ; predictor = as.vector(graceNeighborTS$Date)
avgGraceTrend = mblm::mblm(predicand ~ predictor)$coefficients[2] * 365.25

variablesTable = data.table::data.table(
		# totalInfiltration_f vars
	irrigationApplied = rep(0, numRuns),					# mean irrigation; defaults to 0
	crn = abs(rnorm(numRuns, 60, 10)),								# curve number
	Smax = sample(seq(100, 1000, 1), numRuns, replace=TRUE),						 		# maximum rz soil moisture storage
	Ia_scalar = abs(rnorm(numRuns, 0.15, 1)),						# infiltration 
		# gwRecharge_f vars
	rcScl_sm = sample(seq(0.01, .5, 0.01), numRuns, replace=TRUE),	# water retention curve scalar for soil moisture
	rcExp_sm = sample(seq(1, 2, 0.01), numRuns, replace=TRUE),	# water retention curve exponent for soil moisture
	Zr = sample(seq(100, 2500, 1), numRuns, replace=TRUE),		# root depth [mm]
	n = sample(seq(0.15, 0.75, .01), numRuns, replace=TRUE),		# soil porosity [-]
	smhp = sample(seq(0.00, 0.05, .01), numRuns, replace=TRUE),	# soil moisture at hygroscopic point [-]
	smwp = sample(seq(0.06, 0.15, .01), numRuns, replace=TRUE),	# soil moisture at wilting point [-]
	smfc = sample(seq(0.15, 0.55, .01), numRuns, replace=TRUE),	# soil moisture field capacity [-]
	s0 = 0,		# initial soil moisture  [-]
	rcScl_gw = sample(seq(0.01, 1, 0.01), numRuns, replace=TRUE),	# water retention scalar for gw rech
	initialBlackBucketStor = 100, # initial black bucket storage (mm)
	PETexp = 2, 	# exponent of PET decay
		# cropWaterNeeds_f vars
	avgCropWaterNeeds = abs(rnorm(numRuns, 900, 100)) / 365.25,		# for alfalfa #1010  pistachio in ther san J; c(900, 1200) # citrus
	petGlobAvg =  abs(rnorm(numRuns, 1750, 250)) / 365.25,
		# irrigation_f vars
	irrigatedPctArea = abs(rnorm(numRuns, .25, 0.15)),		# pct area that is regularly irrigated with gw
		# netGwFlow_f vars
	regionalNetGwRtOfChange = rnorm(numRuns, avgGraceTrend, abs(avgGraceTrend) * 0.5),	# what is the long-term trend in gw resources, here likely from abutting GRACE / GRACE-FO tiles, given in mm / day; should be a small number
	initialStorageDifference = sample(seq(-1000,1000,1), numRuns, replace=TRUE),	# what is the initial head on location relative to regional average (pos is higher regional so net inflow, neg is lower regional so net outflow)
	maxStorageBalance = sample(seq(1001,3000,1), numRuns, replace=TRUE),		# what is the maximum head imbalance in mm
	rcScl_netGw = sample(seq(0.001, 0.5, 0.001), numRuns, replace=TRUE) / 365.25)				# recession curve scalar

variablesTable$KGE = NA
variablesTable$NSE = NA
variablesTable$mae = NA
variablesTable$mse = NA
variablesTable$rmse = NA
variablesTable$pbias = NA
variablesTable$rPearson = NA


proc.time()
for(thisRow in 1:numRuns)	{
	regionalGwModel = runRegionalGWmodel_f(
		dataOut_location = dataOut_location,
		basinName = basinName, #'basin_name',
		thisLatitudeInDegrees = locationLat,
		variablesTableRow = variablesTable[thisRow,],
		startYear = 2000,
		endYear = 2099)
	
	allDataOutput = merge(graceTS, regionalGwModel, all.y=TRUE)
	allDataOutput$AnomalyInterp = zoo::na.fill(allDataOutput$Anomaly, 'extend')
	allDataOutput$UncertaintyInterp = zoo::na.fill(allDataOutput$Uncertainty, 'extend')

	allDataSubset = subset(allDataOutput, Date >= graceTS$Date[1])
	allDataSubset = allDataSubset[-nrow(allDataSubset),]

	
	variablesTable$KGE[thisRow] = hydroGOF::KGE(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
	variablesTable$NSE[thisRow] = hydroGOF::NSE(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
	variablesTable$mae[thisRow] = hydroGOF::mae(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
	variablesTable$mse[thisRow] = hydroGOF::mse(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
	variablesTable$rmse[thisRow] = hydroGOF::rmse(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
	variablesTable$pbias[thisRow] = hydroGOF::pbias(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)
	variablesTable$mae[thisRow] = hydroGOF::rPearson(allDataSubset$subsidizedRegionalStorage, allDataSubset$AnomalyInterp)

	if(thisRow %% 100 == 0)	{
		print(thisRow)
		data.table::fwrite(variablesTable, paste0(dataOut_location, basinName, 'cal_v2.csv'))
		plot(allDataSubset$AnomalyInterp)
		lines(allDataSubset$subsidizedRegionalStorage, col='red')
		print(variablesTable[which.max(variablesTable$KGE)])
	}
	
}
theseVars = names(variablesTable)
varsSignif = NA
par(mfrow = c(6,4))
for(thisVar in theseVars)	{
	thisPval = cor.test(variablesTable$KGE, unlist(variablesTable[, ..thisVar]))$p.value
	print(c(thisVar, thisPval))
	varsSignif = c(varsSignif, thisPval)
}

proc.time()











































##########################################################################################################
	## Calibration Function
modelCalibration_f = function(
	historicStreamflowFileLoc = 'https://someplace.gov',
	pathToWatershedsGPKG = 'file_location_and_name.gpkg',
	dataOut_location = 'save_file_location',
	dataSource = 1,											# 1 for FNF from cal.gov, 
	numberOfRuns = 100000,									# maximum number of runs
	targetMetric = 1, 										# 1 = KGE, 2 = NSE, 3 = MAE, 4 = RMSE, 5 = bias
	targetMetricValue = 0.81,								# threshold for considering a value good
	minGoodRuns = 200,										# number of 'good' calibrations before the routine stops
	sfcf = c(runif(5000, .2, 1), runif(5000, 1, 3)),			#snowfall correction factor [-]
	tr   = runif(10000, -6, 5),								#solid and liquid precipitation threshold temperature [C]
	tt   = runif(10000, -5, 6),								#melt temperature [C]
	fm   = c(runif(5000, .2, 1.5), (runif(5000, 1.5, 8))),	#snowmelt factor [mm/C]
	fi   = c(runif(5000, .2, 1.5), (runif(5000, 1.5, 10))),	#icemelt factor [mm/C]
	fic  = runif(10000, 2, 10),								#debris-covered icemelt factor [mm/C]
	fc   = c(runif(5000, 25, 150), (runif(5000, 150, 1200))),	#field capacity
	lp   = runif(10000, .2, 1),								#parameter to actual ET
	beta_soils = runif(10000, 1, 3),							#beta - exponential value for nonlinear relations between soil storage and runoff
	k0   = c(runif(5000, .05, .5), (runif(5000, .5, 0.999))),		#top bucket drainage
	k1   = c(runif(5000, .005, .09), (runif(5000, .09, .5))),	#middle bucket drainage
	k2   = c(runif(5000, .0001, .01), (runif(5000, .01, .1))),#bottom bucket drainage	
	uz1  = c(runif(5000, .22, 10), (runif(5000, 10, 40))),	#max flux rate from STZ to SUZ in mm/d
	perc = c(runif(5000, .1, .5), (runif(5000, .5, 20))))		#max flux rate from SUZ to SLZ in mm/d
	{

	if(file.exists(paste0(dataOut_location, "calibration_", basinName, ".csv")))	{
		print("This file already exists you big ole dummy!")
	}	else	{


		library(sf)
		library(data.table)
		library(lubridate)
		library(hydroGOF)		# for nse / kge calculations
		
		
			# reading in previously reanalyzed climate data
		climateInput = as.data.table(readRDS(paste0(dataOut_location, 'ERA5_', basinName, '.RData')))

			# reading in and reformatting streamflow input 
		historicStreamflow = as.data.table(read.csv(paste0(historicStreamflowFileLoc)))
		if(dataSource == 1)	{
			historicStreamflow$Date = ymd(unlist(strsplit(historicStreamflow$DATE.TIME, " "))[seq(1,nrow(historicStreamflow)*2,2)])
			historicStreamflow$historicQinOriginalUnits = as.numeric(historicStreamflow$VALUE)
				# removing negative streamflow
			if(any(historicStreamflow$historicQinOriginalUnits < 0))	{historicStreamflow$historicQinOriginalUnits[historicStreamflow$historicQinOriginalUnits < 0] = NA}
			basinArea = sum(st_read(paste0(dataOut_location, "HydroBASINSdata_", basinName, ".gpkg"))$SUB_AREA)
			flowUnitConversion = 4.08735e-13 # cubic mm / day in cfs
			areaUnitConversion = (1000000)^2     # sq mm per sq km
			historicStreamflow$historicQinmm = (historicStreamflow$historicQinOriginalUnits / flowUnitConversion) / (areaUnitConversion * basinArea)
				
		}	else	{ 
			return("we need to figure out how to read in and normalize this streamflow data")
		}
		
		
			# merging historic streamflow record onto climate inputs data
		climateAndStreamflowInput = historicStreamflow[climateInput, on='Date']
			# apportioning data for calibration and validation
		lengthOfCalibrationInput = nrow(climateAndStreamflowInput)
		calRows = c(1:(floor(lengthOfCalibrationInput / 3)), ceiling(lengthOfCalibrationInput/(3/2)):lengthOfCalibrationInput)
		valRows = c(ceiling(lengthOfCalibrationInput / 3):floor(lengthOfCalibrationInput/(3/2)))
		
			# dataframe for capturing calibration metrics
		cal_out = data.frame(
			sfcf =	rep(NA,minGoodRuns),
			tr = 	rep(NA,minGoodRuns),
			tt = 	rep(NA,minGoodRuns),
			fm = 	rep(NA,minGoodRuns),
			fi = 	rep(NA,minGoodRuns),
			fic =	rep(NA,minGoodRuns),
			fc =	rep(NA,minGoodRuns),
			lp =	rep(NA,minGoodRuns),
			beta_soils = rep(NA,minGoodRuns),
			k0 = 	rep(NA,minGoodRuns),
			k1 = 	rep(NA,minGoodRuns),
			k2 = 	rep(NA,minGoodRuns),
			uz1 = 	rep(NA,minGoodRuns),
			perc = 	rep(NA,minGoodRuns),
			kgeCalibration = rep(NA,minGoodRuns),
			nseCalibration = rep(NA,minGoodRuns),
			maeCalibration = rep(NA,minGoodRuns),
			rmseCalibration = rep(NA,minGoodRuns),
			biasCalibration = rep(NA,minGoodRuns),
			kgeValidation = rep(NA,minGoodRuns),
			nseValidation = rep(NA,minGoodRuns),
			maeValidation = rep(NA,minGoodRuns),
			rmseValidation = rep(NA,minGoodRuns),
			biasValidation = rep(NA,minGoodRuns),
			kgeAll = rep(NA,minGoodRuns),
			nseAll = rep(NA,minGoodRuns),
			maeAll = rep(NA,minGoodRuns),
			rmseAll = rep(NA,minGoodRuns),
			biasAll = rep(NA,minGoodRuns),
			mnthSumAbsBias = rep(NA,minGoodRuns),
			mnthBias_1 = rep(NA,minGoodRuns),
			mnthBias_2 = rep(NA,minGoodRuns),
			mnthBias_3 = rep(NA,minGoodRuns),
			mnthBias_4 = rep(NA,minGoodRuns),
			mnthBias_5 = rep(NA,minGoodRuns),
			mnthBias_6 = rep(NA,minGoodRuns),
			mnthBias_7 = rep(NA,minGoodRuns),
			mnthBias_8 = rep(NA,minGoodRuns),
			mnthBias_9 = rep(NA,minGoodRuns),
			mnthBias_10 = rep(NA,minGoodRuns),
			mnthBias_11 = rep(NA,minGoodRuns),
			mnthBias_12 = rep(NA,minGoodRuns)
		)

		iter = 0
		while(iter < minGoodRuns)	{
			jj = 0

				# sampling parameter values for calibration
			sfcf =	sample(sfcf, numberOfRuns, replace=TRUE)
			tr = 	sample(tr, numberOfRuns, replace=TRUE)
			tt = 	sample(tt, numberOfRuns, replace=TRUE)
			fm = 	sample(fm, numberOfRuns, replace=TRUE)
			fi = 	sample(fi, numberOfRuns, replace=TRUE)
			fic =	sample(fic, numberOfRuns, replace=TRUE)
			fc =	sample(fc, numberOfRuns, replace=TRUE)
			lp =	sample(lp, numberOfRuns, replace=TRUE)
			beta_soils = sample(beta_soils, numberOfRuns, replace=TRUE)
			k0 = 	sample(k0, numberOfRuns, replace=TRUE)
			k1 = 	sample(k1, numberOfRuns, replace=TRUE)
			k2 = 	sample(k2, numberOfRuns, replace=TRUE)
			uz1 = 	sample(uz1, numberOfRuns, replace=TRUE)
			perc = 	sample(perc, numberOfRuns, replace=TRUE)



			  # since k0>k1>k2 and uz1>perc or an error is thrown, we need a routine to ensure this is true while still allowing 'random' sampling
			if(any(k1 > k0))	{
				k1[which(k1 > k0)] = k0[which(k1 > k0)] * .99
			}
			if(any(k2 > k1))	{
				k2[which(k2 > k1)] = k1[which(k2 > k1)] * .99
			}
			if(any(uz1 < perc))	{
				uz1[which(uz1 < perc)] = perc[which(uz1 < perc)] * 1.01
			}


				# incrementally decreasing the target metric value every n runs
			targetMetricValue = targetMetricValue - 0.01
			print(targetMetricValue)
			print(iter)
			while(jj < numberOfRuns & iter < minGoodRuns) {
				jj = jj+1
					# running HBV
				HBVoutput = runHBV_f(
					climateInput = climateAndStreamflowInput,
					sfcf[jj],	#snowfall correction factor [-]
					tr[jj],	#solid and liquid precipitation threshold temperature [C]
					tt[jj],	#melt temperature [C]
					fm[jj],	#snowmelt factor [mm/C]
					fi[jj],	#icemelt factor [mm/C]
					fic[jj],	#debris-covered icemelt factor [mm/C]
					fc[jj], # field capacity 
					lp[jj], # parameter for PET --> AET
					beta_soils[jj], #soil moisture drainage exponent
					k0[jj],	# storage constant of top bucket
					k1[jj],	# storage constant of middle bucket
					k2[jj],	# storage constant of bottom bucket	
					uz1[jj], #max flux rate from STZ to SUZ in mm/d
					perc[jj])  # max flux rate from SUZ to SLZ in mm/d
			
			
					# identifying if a parameterization meets the criteria to be saved
				if(KGE(HBVoutput$Qg[calRows], HBVoutput$historicQinmm[calRows]) > targetMetricValue &
					KGE(HBVoutput$Qg[valRows], HBVoutput$historicQinmm[valRows]) > targetMetricValue)	{
					
					iter = iter + 1
					plot(HBVoutput$Date[-c(1:1000)], HBVoutput$historicQinmm[-c(1:1000)], main=paste0(jj, ' runs'))
					lines(HBVoutput$Date[-c(1:1000)], HBVoutput$Qg[-c(1:1000)], col='red')
					cal_out$sfcf[iter] = sfcf[jj]
					cal_out$tr[iter] = tr[jj]
					cal_out$tt[iter] = tt[jj]
					cal_out$fm[iter] = fm[jj]
					cal_out$fi[iter] = fi[jj]
					cal_out$fic[iter] = fic[jj]
					cal_out$fc[iter] = fc[jj]
					cal_out$lp[iter] = lp[jj]
					cal_out$beta_soils[iter] = beta_soils[jj]
					cal_out$k0[iter] = k0[jj]
					cal_out$k1[iter] = k1[jj]
					cal_out$k2[iter] = k2[jj]
					cal_out$uz1[iter] = uz1[jj]
					cal_out$perc[iter] = perc[jj]
					cal_out$kgeCalibration[iter] = KGE(HBVoutput$Qg[calRows], HBVoutput$historicQinmm[calRows])
					cal_out$nseCalibration[iter] = NSE(HBVoutput$Qg[calRows], HBVoutput$historicQinmm[calRows])
					cal_out$maeCalibration[iter] = mae(HBVoutput$Qg[calRows], HBVoutput$historicQinmm[calRows])
					cal_out$rmseCalibration[iter] = rmse(HBVoutput$Qg[calRows], HBVoutput$historicQinmm[calRows])
					cal_out$biasCalibration[iter] = pbias(HBVoutput$Qg[calRows], HBVoutput$historicQinmm[calRows])
					cal_out$kgeValidation[iter] = KGE(HBVoutput$Qg[valRows], HBVoutput$historicQinmm[valRows])
					cal_out$nseValidation[iter] = NSE(HBVoutput$Qg[valRows], HBVoutput$historicQinmm[valRows])
					cal_out$maeValidation[iter] = mae(HBVoutput$Qg[valRows], HBVoutput$historicQinmm[valRows])
					cal_out$rmseValidation[iter] = rmse(HBVoutput$Qg[valRows], HBVoutput$historicQinmm[valRows])
					cal_out$biasValidation[iter] = pbias(HBVoutput$Qg[valRows], HBVoutput$historicQinmm[valRows])
					cal_out$kgeAll[iter] = KGE(HBVoutput$Qg, HBVoutput$historicQinmm)
					cal_out$nseAll[iter] = NSE(HBVoutput$Qg, HBVoutput$historicQinmm)
					cal_out$maeAll[iter] = mae(HBVoutput$Qg, HBVoutput$historicQinmm)
					cal_out$rmseAll[iter] = rmse(HBVoutput$Qg, HBVoutput$historicQinmm)
					cal_out$biasAll[iter] = pbias(HBVoutput$Qg, HBVoutput$historicQinmm)
						# calculating monthly biases
					HBVoutput$month = month(HBVoutput$Date)
					HBVoutputMonth = subset(HBVoutput, month == 1)
					cal_out$mnthBias_1[iter] = pbias(HBVoutputMonth$Qg, HBVoutputMonth$historicQinmm)
#					cal_out$mnthSumAbsBias[iter] = abs(cal_out$mnthBias_1[iter])
					for(thisMonth in 2:12)	{
						HBVoutputMonth = subset(HBVoutput, month == thisMonth)
						cal_out[iter, paste0('mnthBias_', thisMonth)] = pbias(HBVoutputMonth$Qg, HBVoutputMonth$historicQinmm)
					}
					mnthBiasCols = which(names(cal_out) == 'mnthBias_1'):which(names(cal_out) == 'mnthBias_12')
					cal_out$mnthSumAbsBias[iter] = apply(abs(cal_out[iter, mnthBiasCols]), 1, mean)
					
					fwrite(cal_out, paste0(dataOut_location, "calibration_", basinName, ".csv"), append=FALSE)
				}
			}
		}
	}
}



