##################
# Workflow

#######################################################################################
## Part 1: functions supporting gw modeling
	# step 1: estimating gw recharge in non ag lands
	# 1a: infiltration function
totalInfiltration_f = function(
	PPT = NA,
	irrigationApplied = 0,					# timeseries of irrigation (daily); defaults to 0
	crn = 50,								# curve number
	Smax = 500,								# maximum rz soil moisture storage
	Ia_scalar = 0.1)						# infiltration 
	{	

	Ia = Ia_scalar * Smax
	totWaterInput = PPT + irrigationApplied
	totalInfiltration = totWaterInput - ifelse(totWaterInput > Ia, (totWaterInput - Ia)^2 / (totWaterInput - Ia + Smax), 0)
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
	petGlobAvg = 1500 / 365.25)
	{
	cropWaterNeeds = sqrt(petTS / petGlobAvg) * avgCropWaterNeeds
	return(cropWaterNeeds)
	}

	# step 3: estimating irrigation needs for ag lands
irrigation_f = function(
	irrigatedPctArea = .2,		# pct area that is regularly irrigated with gw
	cropWaterNeedsTS = cropWaterNeeds,	#ts of crop water needs from above function
	infiltrationTS = netInfiltration)		# ts of cropland infiltration (minus gw rech) from above function
	{
	irrigationNeeded = irrigatedPctArea * (cropWaterNeedsTS - infiltrationTS)
	return(irrigationNeeded)
	}
	
	# step 4: estimating net groundwater flow across basins
netGwFlow_f = function(
	gwRecharge = groundwaterRechTable$gwRecharge,	# a time series of mass flux
	irrigationNeeded = irrigationNeeded,	# time series of area-averaged irrigation
	regionalNetGwRtOfChange = -10,	# what is the long-term trend in gw resources, here likely from abutting GRACE / GRACE-FO tiles, given in mm / day; should be a small number
	initialStorageDifference = 500,	# what is the initial head on location relative to regional average (pos is higher regional so net inflow, neg is lower regional so net outflow)
	maxStorageBalance = 1000,		# what is the maximum head imbalance in mm
	rcScl_netGw = 0.1 / 365)			# recession curve scalar
	{
	extraRegionalStorageTS = initialStorageDifference + regionalNetGwRtOfChange * seq(1, length(gwRecharge), 1) / 365.25
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
	startYear = 2000,
	endYear = 2099)
	{		

	climateInput = data.table::as.data.table(readRDS(paste0(dataOut_location, 'ERA5_', basinName, '.RData')))
	climateInput = subset(climateInput, lubridate::year(Date) >= startYear & lubridate::year(Date) <= endYear)

	lat_radians = min((abs(thisLatitudeInDegrees)*pi/180), 1.1)				# from customer location (centroid)
		# totalInfiltration_f vars
	irrigationApplied = variablesTableRow$irrigationApplied					# timeseries of irrigation (daily); defaults to 0
	crn = variablesTableRow$crn							# curve number
	Smax = variablesTableRow$Smax								# maximum rz soil moisture storage
	Ia_scalar = variablesTableRow$Ia_scalar						# infiltration 
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
		# irrigation_f vars
	irrigatedPctArea = variablesTableRow$irrigatedPctArea		# pct area that is regularly irrigated with gw
		# netGwFlow_f vars
	regionalNetGwRtOfChange = variablesTableRow$regionalNetGwRtOfChange	# what is the long-term trend in gw resources, here likely from abutting GRACE / GRACE-FO tiles, given in mm / day; should be a small number
	initialStorageDifference = variablesTableRow$initialStorageDifference	# what is the initial head on location relative to regional average (pos is higher regional so net inflow, neg is lower regional so net outflow)
	maxStorageBalance = variablesTableRow$maxStorageBalance		# what is the maximum head imbalance in mm
	rcScl_netGw = variablesTableRow$rcScl_netGw					# recession curve scalar

		
	totalInfiltration = totalInfiltration_f(
		PPT = climateInput$PPT,
		irrigationApplied = irrigationApplied,					# timeseries of irrigation (daily); defaults to 0
		crn = crn,								# curve number
		Smax = Smax,								# maximum rz soil moisture storage
		Ia_scalar = Ia_scalar)						# infiltration 
		
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
		petGlobAvg = petGlobAvg)

		# step 3: estimating irrigation needs for ag lands
	irrigationNeeded =	irrigation_f(
		irrigatedPctArea = .2,		# pct area that is regularly irrigated with gw
		cropWaterNeedsTS = cropWaterNeeds,	#ts of crop water needs from above function
		infiltrationTS = c(totalInfiltration, mean(totalInfiltration)) - gwRechargeTable$gwRecharge)		# ts of cropland infiltration (minus gw rech) from above function


		# step 4: estimating net groundwater flow across basins
	netGwFlow = netGwFlow_f(
		gwRecharge = gwRechargeTable$gwRecharge,	# a time series of mass flux
		irrigationNeeded = irrigationNeeded,	# time series of area-averaged irrigation
		regionalNetGwRtOfChange = 0,	# what is the long-term trend in gw resources, here likely from abutting GRACE / GRACE-FO tiles, given in mm / day; should be a small number
		initialStorageDifference = 0,	# what is the initial head on location relative to regional average (pos is higher regional so net inflow, neg is lower regional so net outflow)
		maxStorageBalance = 1000,		# what is the maximum head imbalance in mm
		rcScl_netGw = 0.001)					# recession curve scalar

	Date = c(climateInput$Date, data.table::last(climateInput$Date) + 1)
	totalInfiltration = c(totalInfiltration, NA)
	return(cbind(Date, totalInfiltration, gwRechargeTable, cropWaterNeeds, irrigationNeeded, netGwFlow))
}




############################################################
# other functions
	# grace tile selection w/ smoothing
graceTileSelection_f = function(
	graceFileLoc = 'J:\\Cai_data\\Rabo\\GRACE\\',
	graceFileName = 'GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc',
	locationLat = NA,
	locationLon = NA,
	reweightingExponent = 1)
	{


	# identify and / or load necessary data
	grace_nc = ncdf4::nc_open(paste0(graceFileLoc, graceFileName))
	graceLons = ncdf4::ncvar_get(grace_nc, 'lon')
	graceLons[graceLons > 180] = -(360 - graceLons[graceLons > 180])
	graceLats = ncdf4::ncvar_get(grace_nc, 'lat')
	graceDates = ncdf4::ncvar_get(grace_nc, 'time') + as.Date('2002-01-01')
	graceLandMask = ncdf4::ncvar_get(grace_nc, 'land_mask')
	graceWaterTile = which(graceLandMask == 0)
	graceLWEthick = ncdf4::ncvar_get(grace_nc, 'lwe_thickness') * 10 # convert cm to mm
	graceLWEthickLand = graceLWEthick ; graceLWEthickLand[graceWaterTile] = NA
	graceUncertainty = ncdf4::ncvar_get(grace_nc, 'uncertainty') * 10 # convert cm to mm

	closestLat = which.min(abs(graceLats - locationLat))
	closestLon = which.min(abs(graceLons - locationLon))

		# smoothing / downscaling and reweighting based on distance
	closeishLats = rep(closestLat + c(-1,0,1), 3)
	closeishLons = rep(closestLon + c(-1,0,1), each = 3)
	closeishLatsVals = (graceLats[closeishLats] - locationLat)^2
	closeishLonsVals = (graceLons[closeishLons] - locationLon)^2
	distanceBox = sqrt(closeishLatsVals + closeishLonsVals)
	boxWeighting = max(distanceBox, na.rm=TRUE) - distanceBox
#	boxWeighting = 1 - distanceBox # since max distance of a 2x2 .5 deg grid is 1 deg

		# un-weighting water / ocean tiles
	for(thisIter in 1:length(closeishLats))	{
		if(is.na(graceLWEthickLand[closeishLons[thisIter], closeishLats[thisIter], 1]))	{
			boxWeighting[thisIter] = 0
			graceLWEthickLand[closeishLons[thisIter], closeishLats[thisIter], ] = 0
			graceUncertainty[closeishLons[thisIter], closeishLats[thisIter], ] = 0
		}
	}

		# normalizing weighting 
	boxWeighting = boxWeighting^reweightingExponent / sum(boxWeighting^reweightingExponent, na.rm=TRUE)

		# initializing array for holding 
	theseLweThickLand = graceLWEthickLand[closeishLons[1], closeishLats[1], ] * boxWeighting[1]
	theseUncertLand = graceUncertainty[closeishLons[1], closeishLats[1], ] * boxWeighting[1]
		# accounting for ocean tiles
	if(any(is.na(theseLweThickLand))) { theseLweThickLand[is.na(theseLweThickLand)] = 0 ;  theseUncertLand[is.na(theseUncertLand)] = 0 }
	for(thisWeight in 2:length(boxWeighting))	{
		nextLweThickLand = graceLWEthickLand[closeishLons[thisWeight], closeishLats[thisWeight], ] 
		nextUncertLand = graceUncertainty[closeishLons[thisWeight], closeishLats[thisWeight], ] 
		if(any(is.na(nextLweThickLand))) { nextLweThickLand[is.na(nextLweThickLand)] = 0 ; nextUncertLand[is.na(nextUncertLand)] = 0}
		theseLweThickLand = theseLweThickLand + nextLweThickLand * boxWeighting[thisWeight]# input nc is in format [lon, lat, decade, rcpScen, valueClass]
		theseUncertLand = theseUncertLand + nextUncertLand * boxWeighting[thisWeight]# input nc is in format [lon, lat, decade, rcpScen, valueClass]
	}
	
	return(data.table::data.table(Date = graceDates, Anomaly = theseLweThickLand, Uncertainty = theseUncertLand))
}


	# selection of neighboring grace tiles w/ limited smoothing
graceNeighborTileSelection_f = function(
	graceFileLoc = 'J:\\Cai_data\\Rabo\\GRACE\\',
	graceFileName = 'GRCTellus.JPL.200204_202211.GLO.RL06.1M.MSCNv03CRI.nc',
	locationLat = NA,
	locationLon = NA,
	reweightingExponent = 1)
	{


	# identify and / or load necessary data
	grace_nc = ncdf4::nc_open(paste0(graceFileLoc, graceFileName))
	graceLons = ncdf4::ncvar_get(grace_nc, 'lon')
	graceLons[graceLons > 180] = -(360 - graceLons[graceLons > 180])
	graceLats = ncdf4::ncvar_get(grace_nc, 'lat')
	graceDates = ncdf4::ncvar_get(grace_nc, 'time') + as.Date('2002-01-01')
	graceLandMask = ncdf4::ncvar_get(grace_nc, 'land_mask')
	graceWaterTile = which(graceLandMask == 0)
	graceLWEthick = ncdf4::ncvar_get(grace_nc, 'lwe_thickness') * 10 # convert cm to mm
	graceLWEthickLand = graceLWEthick ; graceLWEthickLand[graceWaterTile] = NA
	graceUncertainty = ncdf4::ncvar_get(grace_nc, 'uncertainty') * 10 # convert cm to mm

	closestLat = which.min(abs(graceLats - locationLat))
	closestLon = which.min(abs(graceLons - locationLon))

		# smoothing / downscaling and reweighting based on distance
	closeishLats = rep(closestLat + c(-1,0,1), 3)
	closeishLons = rep(closestLon + c(-1,0,1), each = 3)
	closeishLatsVals = (graceLats[closeishLats] - locationLat)^2
	closeishLonsVals = (graceLons[closeishLons] - locationLon)^2
	distanceBox = sqrt(closeishLatsVals + closeishLonsVals)
		# removing the closest tile
	distanceBox[which.min(distanceBox)] = max(distanceBox) * 1.1
	boxWeighting = max(distanceBox, na.rm=TRUE) - distanceBox
#	boxWeighting = 1 - distanceBox # since max distance of a 2x2 .5 deg grid is 1 deg

		# un-weighting water / ocean tiles
	for(thisIter in 1:length(closeishLats))	{
		if(is.na(graceLWEthickLand[closeishLons[thisIter], closeishLats[thisIter], 1]))	{
			boxWeighting[thisIter] = 0
			graceLWEthickLand[closeishLons[thisIter], closeishLats[thisIter], ] = 0
			graceUncertainty[closeishLons[thisIter], closeishLats[thisIter], ] = 0
		}
	}

		# normalizing weighting 
	boxWeighting = boxWeighting^reweightingExponent / sum(boxWeighting^reweightingExponent, na.rm=TRUE)

		# initializing array for holding 
	theseLweThickLand = graceLWEthickLand[closeishLons[1], closeishLats[1], ] * boxWeighting[1]
	theseUncertLand = graceUncertainty[closeishLons[1], closeishLats[1], ] * boxWeighting[1]
		# accounting for ocean tiles
	if(any(is.na(theseLweThickLand))) { theseLweThickLand[is.na(theseLweThickLand)] = 0 ;  theseUncertLand[is.na(theseUncertLand)] = 0 }
	for(thisWeight in 2:length(boxWeighting))	{
		nextLweThickLand = graceLWEthickLand[closeishLons[thisWeight], closeishLats[thisWeight], ] 
		nextUncertLand = graceUncertainty[closeishLons[thisWeight], closeishLats[thisWeight], ] 
		if(any(is.na(nextLweThickLand))) { nextLweThickLand[is.na(nextLweThickLand)] = 0 ; nextUncertLand[is.na(nextUncertLand)] = 0}
		theseLweThickLand = theseLweThickLand + nextLweThickLand * boxWeighting[thisWeight]# input nc is in format [lon, lat, decade, rcpScen, valueClass]
		theseUncertLand = theseUncertLand + nextUncertLand * boxWeighting[thisWeight]# input nc is in format [lon, lat, decade, rcpScen, valueClass]
	}
	
	return(data.table::data.table(Date = graceDates, Anomaly = theseLweThickLand, Uncertainty = theseUncertLand))
}
