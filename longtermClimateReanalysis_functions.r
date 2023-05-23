climateInputConversionLongterm_f = function(
	basinName = 'inster_basin_or_outlet_name',
	climateDataNCDF = 'file_location_and_name.nc',
	climateDataNCDF_subName = 'pp_future_daily_works.nc',
	tempConversionFactor = NA,
	pptConversionFactor = NA,
	avgTempGiven = FALSE, 
	startDate = as.Date("1990-01-01"), 	# when does the clock of the netcdf start?
	timeToDaysConversion = 1,	# convert time increments to days if necessary
	dataOut_location = 'save_file_location',
	optionForPET = 1, 	# 1 = PET_fromTemp modified Pen-Mon, 
	variableOrderOption = NA, # 'seas5' or 'era5' or 'seas5Multi'
	precipName = 'tp',	# other options include: tp, tp_sum	
	limitedModels = 25)	# the full seas5 multi only goes back to ~ 2017, so for hindcasting we may want to limit analysis to the first 25 models (which seem to be present for the whole record)
	{
	# load relevant libraries
	library(sf)				# for geospatial data
	sf::sf_use_s2(FALSE)	# for suppressing issue with intersecting spherical w flat
	library(ncdf4)			# for loading netcdf that contains lat-lons of climate data
	library(data.table)		# for data.frame and fread
	if(optionForPET == 1) {library(EcoHydRology)}	# for PET_fromTemp

		# creating the folder if it does not exist
	if(!file.exists(paste0(dataOut_location, "HydroBASINSdata_", basinName, ".gpkg"))) {
		print('Gotta delineate the basin first!!')
	} else {
		
			# read in the previously delineated basins
		basinWatersheds = st_read(paste0(dataOut_location, "HydroBASINSdata_", basinName, ".gpkg"))

			# identifying area of each subbasin for rescaling the 
		basinArea = sum(basinWatersheds$SUB_AREA)

		######################################################################################################################
			# routine for seas5 monthly forecast data
		if(variableOrderOption == 'seas5')	{
				# reading in the ncdfs
			ncin = nc_open(climateDataNCDF)

				# current post processing of ncdfs results in some nas, so must remove these before proceeding
					# na removal by mdeian value needs to be revisited... a linear interp would probably be better
			ncTmin = ncvar_get(ncin, 't2m_min'); if(any(is.na(ncTmin)))	{ncTmin[is.na(ncTmin)] = median(ncTmin, na.rm=TRUE)} # [longitude,latitude,lead_time,member] 
			ncTmax = ncvar_get(ncin, 't2m_max'); if(any(is.na(ncTmax)))	{ncTmax[is.na(ncTmax)] = median(ncTmax, na.rm=TRUE)} # [longitude,latitude,lead_time,member] 
			ncPPT = ncvar_get(ncin, precipName); if(any(is.na(ncPPT)))	{ncPPT[is.na(ncPPT)] = median(ncPPT, na.rm=TRUE)}	 # [longitude,latitude,lead_time,member] 
				# current CAi climate data pipeline does not include tavg, so must estimate using tmin and tmax
			if(avgTempGiven)	{ncTavg = ncvar_get(ncin, 't2m')
			}	else {ncTavg = (ncTmin + ncTmax) / 2}	

				# assuming all lat / lon structures are the same
			nc_lat = ncvar_get(ncin, 'latitude')
			nc_lon = ncvar_get(ncin, 'longitude')
			nc_date = startDate + timeToDaysConversion * ncvar_get(ncin, 'lead_time') # time is days after jan 1 1990

				# list for storing time series of climate data
			allClimateData = list()
			for(numModels in 1:length(ncvar_get(ncin, 'member')))	{
				allPPT = 0	; allTmin = 0	; allTmax = 0	; allPET = 0
				for(numberOfWatersheds in 1:nrow(basinWatersheds))	{
						#identify climate data closest to the centroid of the subbasin of interest
					thisLonLat = st_coordinates(st_centroid(basinWatersheds[numberOfWatersheds, ]))
					nearestLon = which.min(abs(thisLonLat[1] - nc_lon))
					nearestLat = which.min(abs(thisLonLat[2] - nc_lat))
				
						# Tmax is sometimes < Tmin, so need to adjust before running for PET
					theseTmin = ncTmin[nearestLon, nearestLat, , numModels]	# [longitude,latitude,lead_time,member]
					theseTmax = ncTmax[nearestLon, nearestLat, , numModels]	# [longitude,latitude,lead_time,member]
					if(any(theseTmin >= theseTmax))	{
						theseTmin[theseTmin >= theseTmax] = theseTmax[theseTmin >= theseTmax] - 0.1
					}
					
						# summing all climate variables; they normalized by basin area below
							# ultimately, subbasins should be disaggregated and the model run on subcomponents, but this will take time to implement
					allTmin = allTmin +  theseTmin *  basinWatersheds$SUB_AREA[numberOfWatersheds]
					allTmax = allTmax + theseTmax *  basinWatersheds$SUB_AREA[numberOfWatersheds]
					allPPT = allPPT + ncPPT[nearestLon, nearestLat, , numModels] *  basinWatersheds$SUB_AREA[numberOfWatersheds]	# [longitude,latitude,lead_time,member]
						# since we don't currently ingest PET data, we arecalculating from a penman monteith eq
					if(optionForPET == 1)	{
						allPET = allPET + PET_fromTemp(yday(nc_date), theseTmax , theseTmin,
							lat_radians =  min((thisLonLat[1,2]*pi/180), 1.1)) * 1000  *  basinWatersheds$SUB_AREA[numberOfWatersheds]	# output in m, convert to mm
					}	else	{
						print('need to figure this one out later')
					}
				}

				# normalizing data by basin area
				avgTmin	= if(is.na(tempConversionFactor))	{allTmin / basinArea}	else	{tempConversionFactor + allTmin / basinArea}
				avgTmax = if(is.na(tempConversionFactor)) 	{allTmax / basinArea}	else	{tempConversionFactor + allTmax / basinArea}
				avgTavg = (avgTmin + avgTmax) / 2
				avgPPT = if(is.na(pptConversionFactor)) 	{allPPT / basinArea}	else	{pptConversionFactor * allPPT / basinArea}
				avgPET = allPET / basinArea

					# all climate data to be saved
				allClimateData[[numModels]] = data.frame(Date = nc_date, Tmin = avgTmin, Tmax = avgTmax, Tavg = avgTavg, PPT = avgPPT, PET = avgPET)
				saveRDS(allClimateData, paste0(dataOut_location, 'SEAS5_', basinName, '.RData'))
			}
		}



		######################################################################################################################
			# routine for cmip6 data
		if(variableOrderOption == 'cmip6')	{
				# reading in the ncdfs
			for(thisScen in c('ssp126', 'ssp245', 'ssp585'))	{

				ncin_tmax = nc_open(paste0(climateDataNCDF, 't2m_max_', thisScen, '_', climateDataNCDF_subName)) # [lon,lat,time,model] 
				nc_lat = ncvar_get(ncin_tmax, 'lat')
				nc_lon = ncvar_get(ncin_tmax, 'lon')
				nc_date = startDate + timeToDaysConversion * ncvar_get(ncin_tmax, 'time') # time is days after jan 1 1990
				nc_models_tmax = ncvar_get(ncin_tmax, 'model')
				nc_models_unique = unique(nc_models_tmax)	# for some reason some models are repeated
				ncTmax = ncvar_get(ncin_tmax, 't2m_max')# zoo::na.fill(ncTmax, 'extend') #; if(any(is.na(ncTmax)))	{ncTmax[is.na(ncTmax)] = median(ncTmax, na.rm=TRUE)} # [longitude,latitude,lead_time,member] 
				nc_close(ncin_tmax)

				ncin_tmin = nc_open(paste0(climateDataNCDF, 't2m_min_', thisScen, '_', climateDataNCDF_subName)) # [lon,lat,time,model] 
				ncTmin = ncvar_get(ncin_tmin, 't2m_min')
				nc_models_tmin = ncvar_get(ncin_tmin, 'model')
				nc_close(ncin_tmin)

				ncin_tp =  nc_open(paste0(climateDataNCDF, 'tp_', thisScen, '_', climateDataNCDF_subName)) # [time,lon,lat,model]  
				ncPPT = ncvar_get(ncin_tp, 'tp')
				nc_models_tp = ncvar_get(ncin_tp, 'model')
				nc_close(ncin_tp)


					# current post processing of ncdfs results in some nas, so must remove these before proceeding
						# na removal by mdeian value needs to be revisited... a linear interp would probably be better
					# current CAi climate data pipeline does not include tavg, so must estimate using tmin and tmax
				if(avgTempGiven)	{
					ncin_tavg = nc_open(paste0(climateDataNCDF, 't2m_avg_', thisScen, '_', climateDataNCDF_subName)) # [lon,lat,time,model] 
					ncTavg = ncvar_get(ncin_tavg, 't2m_avg')
					nc_close(ncin_tavg)
				}	else {ncTavg = (ncTmin + ncTmax) / 2}	

					# list for storing time series of climate data
				allClimateData = list()
				listCounter = 0
				for(numModels in 1:length(nc_models_unique))	{
					if(nc_models_tmax[numModels] %in% nc_models_tmin & nc_models_tmax[numModels] %in% nc_models_tp)	{ # accounting for not all models being included for each variable
						listCounter = listCounter + 1
						allPPT = 0	; allTmin = 0	; allTmax = 0	; allPET = 0
						numModels_tmax = which(nc_models_tmax == nc_models_unique[numModels])[1]
						numModels_tmin = which(nc_models_tmin == nc_models_unique[numModels])[1]
						numModels_tp = which(nc_models_tp == nc_models_unique[numModels])[1]
						
						for(numberOfWatersheds in 1:nrow(basinWatersheds))	{
								#identify climate data closest to the centroid of the subbasin of interest
							thisLonLat = st_coordinates(st_centroid(basinWatersheds[numberOfWatersheds, ]))
							nearestLon = which.min(abs(thisLonLat[1] - nc_lon))
							nearestLat = which.min(abs(thisLonLat[2] - nc_lat))
						
								# Tmax is sometimes < Tmin, so need to adjust before running for PET
							theseTmin = zoo::na.fill(ncTmin[nearestLon, nearestLat, , numModels_tmin], 'extend')	#  [lon,lat,time,model] 
							theseTmax = zoo::na.fill(ncTmax[nearestLon, nearestLat, , numModels_tmax], 'extend')	#  [lon,lat,time,model] 
							if(any(theseTmin >= theseTmax))	{
								theseTmin[theseTmin >= theseTmax] = theseTmax[theseTmin >= theseTmax] - 0.1
							}
							
								# summing all climate variables; they normalized by basin area below
									# ultimately, subbasins should be disaggregated and the model run on subcomponents, but this will take time to implement
							allTmin = allTmin +  theseTmin *  basinWatersheds$SUB_AREA[numberOfWatersheds]
							allTmax = allTmax + theseTmax *  basinWatersheds$SUB_AREA[numberOfWatersheds]
							allPPT = allPPT + zoo::na.fill(ncPPT[, nearestLon, nearestLat, numModels_tp], 'extend') *  basinWatersheds$SUB_AREA[numberOfWatersheds]	#  [time,lon,lat,model]  
								# since we don't currently ingest PET data, we arecalculating from a penman monteith eq
							if(optionForPET == 1)	{
								allPET = allPET + PET_fromTemp(yday(nc_date), theseTmax , theseTmin,
									lat_radians =  min((thisLonLat[1,2]*pi/180), 1.1)) * 1000  *  basinWatersheds$SUB_AREA[numberOfWatersheds]	# output in m, convert to mm
							}	else	{
								print('need to figure this one out later')
							}
						}

						# normalizing data by basin area
						avgTmin	= if(is.na(tempConversionFactor))	{allTmin / basinArea}	else	{tempConversionFactor + allTmin / basinArea}
						avgTmax = if(is.na(tempConversionFactor)) 	{allTmax / basinArea}	else	{tempConversionFactor + allTmax / basinArea}
						avgTavg = (avgTmin + avgTmax) / 2
						avgPPT = if(is.na(pptConversionFactor)) 	{allPPT / basinArea}	else	{pptConversionFactor * allPPT / basinArea}
						avgPET = allPET / basinArea

							# all climate data to be saved
						allClimateData[[listCounter]] = data.frame(Date = nc_date, Tmin = avgTmin, Tmax = avgTmax, Tavg = avgTavg, PPT = avgPPT, PET = avgPET)
						saveRDS(allClimateData, paste0(dataOut_location, 'CMIP65_', basinName, '_', thisScen, '.RData'))
						print(numModels)
					}
				}
			}
		}



		######################################################################################################################
			# routine for era5 data
		if(variableOrderOption == 'era5')	{
				# reading in the ncdfs
			ncin = nc_open(climateDataNCDF)

				# current post processing of ncdfs results in some nas, so must remove these before proceeding
			ncTmin = ncvar_get(ncin, 't2m_min'); if(any(is.na(ncTmin)))	{ncTmin[is.na(ncTmin)] = median(ncTmin, na.rm=TRUE)} 	#[longitude,latitude,time] 
			ncTmax = ncvar_get(ncin, 't2m_max'); if(any(is.na(ncTmax)))	{ncTmax[is.na(ncTmax)] = median(ncTmax, na.rm=TRUE)}	#[longitude,latitude,time] 
			ncPPT = ncvar_get(ncin, precipName); if(any(is.na(ncPPT)))	{ncPPT[is.na(ncPPT)] = median(ncPPT, na.rm=TRUE)} 		#[longitude,latitude,time] 
					# current CAi climate data pipeline does not include tavg, so must estimate using tmin and tmax
			if(avgTempGiven)	{ncTavg = ncvar_get(ncin, 't2m_avg')
			}	else {ncTavg = (ncTmin + ncTmax) / 2}	

				# assuming all lat / lon structures are the same
			nc_lat = ncvar_get(ncin, 'latitude')
			nc_lon = ncvar_get(ncin, 'longitude')
			nc_date = startDate + timeToDaysConversion * ncvar_get(ncin, 'time') # time is days after jan 1 1990

			allPPT = 0	; allTmin = 0	; allTmax = 0	; allPET = 0
			for(numberOfWatersheds in 1:nrow(basinWatersheds))	{
				thisLonLat = st_coordinates(st_centroid(basinWatersheds[numberOfWatersheds, ]))
				nearestLon = which.min(abs(thisLonLat[1] - nc_lon))
				nearestLat = which.min(abs(thisLonLat[2] - nc_lat))
						
					# summing all climate variables; they normalized by basin area below
						# ultimately, subbasins should be disaggregated and the model run on subcomponents, but this will take time to implement
				allTmin = allTmin + ncTmin[nearestLon, nearestLat, ] *  basinWatersheds$SUB_AREA[numberOfWatersheds]
				allTmax = allTmax + ncTmax[nearestLon, nearestLat, ] *  basinWatersheds$SUB_AREA[numberOfWatersheds]
				allPPT = allPPT + ncPPT[nearestLon, nearestLat, ] *  basinWatersheds$SUB_AREA[numberOfWatersheds]
					# since we don't currently ingest PET data, we arecalculating from a penman monteith eq
				if(optionForPET == 1)	{
					allPET = allPET + PET_fromTemp(yday(nc_date), ncTmax[nearestLon, nearestLat, ], ncTmin[nearestLon, nearestLat, ],
						lat_radians =  min((thisLonLat[1,2]*pi/180), 1.1)) * 1000  *  basinWatersheds$SUB_AREA[numberOfWatersheds]	# output in m, convert to mm
				}	else	{
					print('need to figure this one out later')
				}
			}
			
				# normalizing data by basin area
			avgTmin	= if(is.na(tempConversionFactor))	{allTmin / basinArea}	else	{tempConversionFactor + allTmin / basinArea}
			avgTmax = if(is.na(tempConversionFactor)) 	{allTmax / basinArea}	else	{tempConversionFactor + allTmax / basinArea}
			avgTavg = (avgTmin + avgTmax) / 2
			avgPPT = if(is.na(pptConversionFactor)) 	{allPPT / basinArea}	else	{pptConversionFactor * allPPT / basinArea}
			avgPET = allPET / basinArea
			
				# save data output
			allClimateData = data.frame(Date = nc_date, Tmin = avgTmin, Tmax = avgTmax, Tavg = avgTavg, PPT = avgPPT, PET = avgPET)
			saveRDS(allClimateData, paste0(dataOut_location, 'ERA5_', basinName, '.RData'))
		}
		
		######################################################################################################################
			# for long record of seas5 data for hindcasting
		if(variableOrderOption == 'seas5Multi')	{
			# creating the folder to hold the output
				if(!file.exists(paste0(dataOut_location, 'seas5MultiOutput')))	{
				dir.create(file.path(paste0(dataOut_location, 'seas5MultiOutput')))
			}

			
			# reading in the ncdfs
			ncin = nc_open(climateDataNCDF) # for tmax and tmin [longitude,latitude,member,lead_time,init_time] 
											# for tp_sum [lead_time,longitude,latitude,member,init_time]

			ncTmin = ncvar_get(ncin, 't2m_min'); if(any(is.na(ncTmin)))	{ncTmin[is.na(ncTmin)] = median(ncTmin, na.rm=TRUE)} 
			ncTmax = ncvar_get(ncin, 't2m_max'); if(any(is.na(ncTmax)))	{ncTmax[is.na(ncTmax)] = median(ncTmax, na.rm=TRUE)} 
			ncPPT = ncvar_get(ncin, precipName); if(any(is.na(ncPPT)))	{ncPPT[is.na(ncPPT)] = median(ncPPT, na.rm=TRUE)} 
			if(avgTempGiven)	{ncTavg = ncvar_get(ncin, 't2m_avg')
			}	else {ncTavg = (ncTmin + ncTmax) / 2}	

				# assuming all lat / lon structures are the same
			nc_lat = ncvar_get(ncin, 'latitude')
			nc_lon = ncvar_get(ncin, 'longitude')

			initTimes = ncvar_get(ncin, 'init_time')
			leadTimes = ncvar_get(ncin, 'lead_time')
			for(thisInitTime in 1:length(initTimes))	{
				
				nc_date = startDate + timeToDaysConversion * leadTimes + timeToDaysConversion * initTimes[thisInitTime] # time is days after jan 1 

				allClimateData = list()
				
					# the full seas5 multi only goes back to ~ 2017, so for hindcasting we may want to limit analysis to the first 25 models (which seem to be present for the whole record) 
				if(limitedModels) 	{
					totModels = 1:limitedModels
				} else	{
					totModels = 1:length(ncvar_get(ncin, 'member'))
				}
				
				for(numModels in totModels)	{
					allPPT = 0	; allTmin = 0	; allTmax = 0	; allPET = 0
					for(numberOfWatersheds in 1:nrow(basinWatersheds))	{
							#identify climate data closest to the centroid of the subbasin of interest
						thisLonLat = st_coordinates(st_centroid(basinWatersheds[numberOfWatersheds, ]))
						nearestLon = which.min(abs(thisLonLat[1] - nc_lon))
						nearestLat = which.min(abs(thisLonLat[2] - nc_lat))
					
							# Tmax is sometimes < Tmin, so need to adjust before running for PET
							# this step can be removed when the bias correction / post processing is standardized
						theseTmin = ncTmin[nearestLon, nearestLat, numModels, , thisInitTime]
						theseTmax = ncTmax[nearestLon, nearestLat, numModels, , thisInitTime]
						if(any(theseTmin >= theseTmax))	{
							theseTmin[theseTmin >= theseTmax] = theseTmax[theseTmin >= theseTmax] - 0.1
						}
						
							# summing all climate variables; they normalized by basin area below
							# ultimately, subbasins should be disaggregated and the model run on subcomponents, but this will take time to implement
						allTmin = allTmin +  theseTmin *  basinWatersheds$SUB_AREA[numberOfWatersheds]
						allTmax = allTmax + theseTmax *  basinWatersheds$SUB_AREA[numberOfWatersheds]
						allPPT = allPPT + ncPPT[ , nearestLon, nearestLat, numModels, thisInitTime] *  basinWatersheds$SUB_AREA[numberOfWatersheds]
							# since we don't currently ingest PET data, we arecalculating from a penman monteith eq
						if(optionForPET == 1)	{
							allPET = allPET + PET_fromTemp(yday(nc_date), theseTmax , theseTmin,
								lat_radians =  min((thisLonLat[1,2]*pi/180), 1.1)) * 1000  *  basinWatersheds$SUB_AREA[numberOfWatersheds]	# output in m, convert to mm
						}	else	{
							print('need to figure this one out later')
						}
					}
					
					# normalizing data by basin area
					avgTmin	= if(is.na(tempConversionFactor))	{allTmin / basinArea}	else	{tempConversionFactor + allTmin / basinArea}
					avgTmax = if(is.na(tempConversionFactor)) 	{allTmax / basinArea}	else	{tempConversionFactor + allTmax / basinArea}
					avgTavg = (avgTmin + avgTmax) / 2
					avgPPT = if(is.na(pptConversionFactor)) 	{allPPT / basinArea}	else	{pptConversionFactor * allPPT / basinArea}
					avgPET = allPET / basinArea

						# all climate data to be saved
					allClimateData[[numModels]] = data.frame(Date = nc_date, Tmin = avgTmin, Tmax = avgTmax, Tavg = avgTavg, PPT = avgPPT, PET = avgPET)
				}
				saveRDS(allClimateData, paste0(dataOut_location, 'seas5MultiOutput\\SEAS5_startDate_', nc_date[1], '.RData'))
			}
		}
	}
}	
