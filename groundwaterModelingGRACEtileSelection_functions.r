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
		theseLweThickLand = theseLweThickLand + nextLweThickLand * boxWeighting[thisWeight]
		theseUncertLand = theseUncertLand + nextUncertLand * boxWeighting[thisWeight]
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
		theseLweThickLand = theseLweThickLand + nextLweThickLand * boxWeighting[thisWeight]
		theseUncertLand = theseUncertLand + nextUncertLand * boxWeighting[thisWeight]
	}
	
	return(data.table::data.table(Date = graceDates, Anomaly = theseLweThickLand, Uncertainty = theseUncertLand))
}
