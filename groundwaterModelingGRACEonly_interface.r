# load necessary functions
	# gw specific functions
source('C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\groundwaterModelingGRACEonly_functions.R')
source('C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\groundwaterModelingGRACEtileSelection_functions.R')
	# basin delineation functions
source('C:\\Users\\arik\\Documents\\GitHub\\SurfaceWaterProjections\\surfaceWaterModeling_functions.r')


# file storage information
basinName = 'BeefNW'
dataOut_location = paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\')

# locations information
customerLocations = data.table::fread(paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\', 'Customer Onboarding Information_BNW.csv'), skip=1)
locationLon = customerLocations$Longitude[1]
locationLat = customerLocations$Latitude[1]



era5DataNCDF =  paste0('J:\\Cai_data\\Rabo\\Locations\\', basinName, '\\', basinName, '-testing-recent-era.nc')
era5StartDate =  as.Date('1980-01-01') # + ncvar_get(ncin_era5, 'time') for calculating actual dates 


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
	basinATLAS_locationAndFile = 'C:\\Users\\arik\\Documents\\PhD Research\\D4\\BasinATLAS_Data_v10\\BasinATLAS_v10.gdb',
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
	locationLon = locationLat,
	reweightingExponent = 1)
	

#########################################################################################################
	# step 4 
	# calibration
numRuns = 1000*1000#*50
	# estimate average values and ranges for each variable
predicand = as.vector(graceNeighborTS$Anomaly) ; predictor = as.vector(graceNeighborTS$Date)
avgGraceTrend = mblm::mblm(predicand ~ predictor)$coefficients[2] * 365.25

variablesTable = data.table::data.table(
		# totalInfiltration_f vars
	irrigationApplied = rep(0, numRuns),					# timeseries of irrigation (daily); defaults to 0
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
		data.table::fwrite(variablesTable, paste0(dataOut_location, basinName, 'cal.csv'))
		plot(allDataSubset$AnomalyInterp)
		lines(allDataSubset$subsidizedRegionalStorage, col='red')
		print(variablesTable[which.max(variablesTable$KGE)])
	}
	
}
proc.time()


