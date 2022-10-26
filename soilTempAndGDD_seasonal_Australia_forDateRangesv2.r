############################################################
##	function for defining climatology GDD / temp thresholds
f_historicTempGDD = function(theseQuantiles = theseQuantiles,
	soilTempThresholds = soilTempThresholds,
	consecDaysThresholds = consecDaysThresholds,
	baseTempGDDs = baseTempGDDs,
	GDDvalueThresholds = GDDvalueThresholds, 
	growYearStartDate = growYearStartDate,
	dataPath = dataPath,
	dataName = era5ClimatologyDataName, 
	userName = userName,
	forecastDate = forecastDate)	{

	library(lubridate)
    library(ncdf4)
	save_iter = 0
	
		# read in ncdf data
	ncin = nc_open(paste0(dataPath, dataName)) # [longitude,latitude,time]    (Contiguous storage) 
	nc_tavg = (ncvar_get(ncin, 't2m_min') + ncvar_get(ncin, 't2m_max')) / 2
	nc_tsoil = (ncvar_get(ncin, 'stl1_mean'))

	nc_date = ncvar_get(ncin, 'time') + as.Date('2002-06-01')
	nc_doy = yday(nc_date)
	nc_year = year(nc_date)
	
	nc_lat = ncvar_get(ncin, 'latitude')
	nc_lon = ncvar_get(ncin, 'longitude')
	
	if(yday(growYearStartDate) == 1)	{
		growYearDOYs = 1:366
	}	else	{growYearDOYs = c(yday(growYearStartDate):366, 1:(yday(growYearStartDate)-1))}
	

	finalIter = 0	
	summaryOutput_df = data.frame(Lat = 0, Lon = 0, soilTempThresholds = 0, consecDaysThresholds = 0, baseTempGDDs = 0,
		variableName = 'string', projectionOrClimatology = 'string',
		quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)

	# generating .ncs for saving average historical values for backfilling projection data
	tempClimatologyTavg_nc = nc_tavg[ , , 1:366]
	tempClimatologySoils_nc = nc_tsoil[ , , 1:366]
	
	for(this_lon in 1:length(nc_lon))	{
		for(this_lat in 1:length(nc_lat))	{
			
			theseSoilTemp = nc_tsoil[this_lon, this_lat, ]
			if(any(!is.na(theseSoilTemp)))	{
				theseTavg = nc_tavg[this_lon, this_lat, ]
				
				for(gg in 1:366)	{
					tempClimatologyTavg_nc[this_lon, this_lat, gg] = median(theseTavg[nc_doy == gg])
					tempClimatologySoils_nc[this_lon, this_lat, gg] = median(theseSoilTemp[nc_doy == gg])
				}
							
				allYearsOutput_df = data.frame(soilTempThresholds = NA, consecDaysThresholds = NA, daysToWarmSoils = NA,
					baseTempGDDs = NA, GDDvalueThresholds = NA, daysToGDDThreshold = NA)
				iter = 0
				for(hh in 1:length(baseTempGDDs))	{
					theseGDD = ifelse(theseTavg > baseTempGDDs[hh], theseTavg - baseTempGDDs[hh], 0)
					for(ii in 1:length(soilTempThresholds))	{
						for(jj in 1:length(consecDaysThresholds))	{
							uniqueYears = unique(nc_year)
							for(thisYear in uniqueYears[-c(1,length(uniqueYears))])	{
								thisGrowYear = c(which(nc_year == thisYear - 1 & nc_doy >= yday(growYearStartDate)), 
									which(nc_year == thisYear & nc_doy < yday(growYearStartDate))) 
								exceedsSoilThresh = which(theseSoilTemp[thisGrowYear] > (soilTempThresholds[ii])) ## adjusting to soil temp
								threshDiff = diff(exceedsSoilThresh, consecDaysThresholds[jj])
								daysToWarmSoils = ifelse(any(threshDiff == consecDaysThresholds[jj][1]), 
									exceedsSoilThresh[which(threshDiff == consecDaysThresholds[jj])[1] + (consecDaysThresholds[jj] - 1)],
									364)
			
								for(kk in 1:length(GDDvalueThresholds))	{
									iter = iter + 1
									thisGDD = cumsum(theseGDD[thisGrowYear][-(1:daysToWarmSoils)])
									maxGDD = ifelse(daysToWarmSoils < 364, max(thisGDD) , 0)
									allYearsOutput_df[iter, ] = c(soilTempThresholds[ii], consecDaysThresholds[jj], daysToWarmSoils,
										baseTempGDDs[hh], GDDvalueThresholds[kk],
										ifelse(maxGDD > GDDvalueThresholds[kk], daysToWarmSoils + which(thisGDD > GDDvalueThresholds[kk])[1], 400))
								}
							}
						}
					}
				}
				

				thisLat = nc_lat[this_lat]
				thisLon = nc_lon[this_lon]
				for(thisSoilTempThresholds in soilTempThresholds)				{
					for(thisConsecDaysThresholds in consecDaysThresholds)		{
						for(thisBaseTempGDDs in baseTempGDDs)					{
							daysSinceDate = ceiling(quantile(subset(allYearsOutput_df,
									soilTempThresholds == thisSoilTempThresholds & 
									consecDaysThresholds == thisConsecDaysThresholds & 
									baseTempGDDs == thisBaseTempGDDs)$daysToWarmSoils,
									theseQuantiles))
							datesByQuantile = growYearStartDate + daysSinceDate

							finalIter = finalIter + 1
							summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
								paste0('Warm Soils'), 'climatology',
								'Q5_to_Q25', paste0(datesByQuantile[1]), paste0(datesByQuantile[2]),
								paste0(userName), paste0(forecastDate), daysSinceDate[2])

							finalIter = finalIter + 1
							summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
								paste0('Warm Soils'), 'climatology',
								'Q25_to_Q75', paste0(datesByQuantile[2]), paste0(datesByQuantile[4]),
								paste0(userName), paste0(forecastDate), daysSinceDate[3])

							finalIter = finalIter + 1
							summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
								paste0('Warm Soils'), 'climatology',
								'Q75_to_Q95', paste0(datesByQuantile[4]), paste0(datesByQuantile[5]),
								paste0(userName), paste0(forecastDate), daysSinceDate[4])
							
							for(thisGDDvalueThresholds in GDDvalueThresholds)	{
								daysSinceDate = ceiling(quantile(subset(allYearsOutput_df,
										soilTempThresholds == thisSoilTempThresholds &
										consecDaysThresholds == thisConsecDaysThresholds &
										baseTempGDDs == thisBaseTempGDDs &
										GDDvalueThresholds == thisGDDvalueThresholds)$daysToGDDThreshold,
										theseQuantiles))

								datesByQuantile = growYearStartDate + daysSinceDate
								
								finalIter = finalIter + 1
								summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
									paste0('GDD = ', thisGDDvalueThresholds), 'climatology',
									'Q5_to_Q25', paste0(datesByQuantile[1]), paste0(datesByQuantile[2]),
									paste0(userName), paste0(forecastDate), daysSinceDate[2])

								finalIter = finalIter + 1
								summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
									paste0('GDD = ', thisGDDvalueThresholds), 'climatology',
									'Q25_to_Q75', paste0(datesByQuantile[2]), paste0(datesByQuantile[4]),
									paste0(userName), paste0(forecastDate), daysSinceDate[3])

								finalIter = finalIter + 1
								summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
									paste0('GDD = ', thisGDDvalueThresholds), 'climatology',
									'Q75_to_Q95', paste0(datesByQuantile[4]), paste0(datesByQuantile[5]),
									paste0(userName), paste0(forecastDate), daysSinceDate[4])
							}
						}
					}
				}
                print(finalIter)
				if(nrow(summaryOutput_df) > 20000)	{
					save_iter = save_iter + 1
					fwrite(summaryOutput_df, paste0(dataPath, "temp_out_", save_iter, ".csv"))
					summaryOutput_df = data.frame(Lat = 0, Lon = 0, soilTempThresholds = 0, consecDaysThresholds = 0, baseTempGDDs = 0,
						variableName = 'string', projectionOrClimatology = 'string',
						quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)
					finalIter = 0
					print(c(this_lat, this_lon))
				}
			} 
		}
	}
		# saving workspace that holds climatology .ncs
	save(tempClimatologyTavg_nc, file=paste0(dataPath, 'climatologyTavg.RData'))
	save(tempClimatologySoils_nc, file=paste0(dataPath, 'climatologySoils.RData'))
		# combining saved iterations and returning the dataframe with formatted climatology
	save_iter = save_iter + 1
	fwrite(summaryOutput_df, paste0(dataPath, "temp_out_", save_iter, ".csv"))

	summaryOutput_df = fread(paste0(dataPath, "temp_out_", 1, ".csv"))
	for(files in 2:save_iter)	{
		summaryOutput_df = rbind(summaryOutput_df, fread(paste0(dataPath, "temp_out_", files, ".csv")))
	}
	return(summaryOutput_df)
}	



############################################################################################################################
# incorporating projections data

f_projectedTempGDD = function(theseQuantiles = theseQuantiles,
	soilTempThresholds = soilTempThresholds,
	consecDaysThresholds = consecDaysThresholds,
	baseTempGDDs = baseTempGDDs,
	GDDvalueThresholds = GDDvalueThresholds, 
	growYearStartDate = growYearStartDate,
	dataPath = dataPath,
	era5RecentDataName = era5RecentDataName,
	cfsDataName = cfsDataName,
	seas5DataName = seas5DataName,
	startDateEra5 = startDateEra5,
	startDateCfs = startDateCfs,
	startDateSeas5 = startDateSeas5,
	whichCfsModels = whichCfsModels,
	whichSeas5Models = whichSeas5Models,
	userName = userName,
	forecastDate = forecastDate,
	seas5Models = whichSeas5Models,
	cfsModels = whichCfsModels)	{

	library(lubridate)
    library(ncdf4)
	'%ni%' = Negate("%in%")

    save_iter = 0

		# loading climatology records of Tavg and soils by day of year
	load(paste0(dataPath, 'climatologyTavg.RData'))	# loads tempClimatologyTavg_nc
	load(paste0(dataPath, 'climatologySoils.RData'))# loads tempClimatologySoils_nc


		# read in ncdf data
	ncin_era5 = nc_open(paste0(dataPath, era5RecentDataName))						# [longitude,latitude,time] 
	ncin_cfs = nc_open(paste0(dataPath, cfsDataName))								# [longitude,latitude,lead_time,member] 
	ncin_seas5 = nc_open(paste0(dataPath, seas5DataName),  return_on_error = TRUE)	# [longitude,latitude,lead_time,member] 
	
	nc_dateEra5 = ncvar_get(ncin_era5, 'time') + as.Date(startDateEra5)
	nc_dateCfs = ncvar_get(ncin_cfs, 'lead_time') + as.Date(startDateCfs)
	nc_dateSeas5 = ncvar_get(ncin_seas5, 'lead_time') + as.Date(startDateSeas5)
	 
	nc_doyEra5 = yday(nc_dateEra5)
	nc_doyCfs = yday(nc_dateCfs)
	nc_doySeas5 = yday(nc_dateSeas5)
	
	nc_yearEra5 = year(nc_dateEra5)
	nc_yearCfs = year(nc_dateCfs)
	nc_yearSeas5 = year(nc_dateSeas5)
	
	nc_latEra5 = ncvar_get(ncin_era5, 'latitude')
	nc_latCfs = ncvar_get(ncin_cfs, 'latitude')
	nc_latSeas5 = ncvar_get(ncin_seas5, 'latitude')
	
	nc_lonEra5 = ncvar_get(ncin_era5, 'longitude')
	nc_lonCfs = ncvar_get(ncin_cfs, 'longitude')
	nc_lonSeas5 = ncvar_get(ncin_seas5, 'longitude')

		# defining the growing year
	growYear = seq(growYearStartDate, (growYearStartDate+364), 1)
	growYearDOYs = yday(growYear)
	
		# prioritizing higher to lower quality data
	whichEra5Days = which(nc_dateEra5 %in% growYear) 
	incHistData = ifelse(length(whichEra5Days >=1), TRUE, FALSE)
	if(incHistData)	{
		whichCfsDays = which(as.character(nc_dateCfs) %in% as.character(growYear) & 
			as.character(nc_dateCfs)  %ni%  as.character(nc_dateEra5[whichEra5Days]))
		whichSeas5Days = which(as.character(nc_dateSeas5) %in% as.character(growYear) & 
			as.character(nc_dateSeas5)  %ni%  as.character(nc_dateEra5[whichEra5Days]) &
			as.character(nc_dateSeas5)  %ni%  as.character(nc_dateCfs[whichCfsDays]))
		whichClimatologyDays = growYearDOYs[-c(1:(length(incHistData) + length(whichCfsDays) + length(whichSeas5Days)))]
	} else {
		whichCfsDays = which(as.character(nc_dateCfs) %in% as.character(growYear))
		whichSeas5Days = which(as.character(nc_dateSeas5) %in% as.character(growYear))[-c(1:length(whichCfsDays))]
		whichClimatologyDays = growYearDOYs[-c(1:(length(whichCfsDays) + length(whichSeas5Days)))]
	}
	

	nc_tavgEra5 =   (ncvar_get(ncin_era5, 't2m_min')[ , , whichEra5Days] +    ncvar_get(ncin_era5, 't2m_max')[ , , whichEra5Days]) / 2			# [longitude,latitude,time] 
	nc_tavgCfs =     (ncvar_get(ncin_cfs, 't2m_min')[ , , whichCfsDays, ] +   ncvar_get(ncin_cfs, 't2m_max')[ , , whichCfsDays, ]) / 2			# [longitude,latitude,lead_time,member] 
	nc_tavgSeas5 = (ncvar_get(ncin_seas5, 't2m_min')[ , , whichSeas5Days, ] + ncvar_get(ncin_seas5, 't2m_max')[ , , whichSeas5Days, ]) / 2		# [longitude,latitude,lead_time,member] 
	
		# soil 
	nc_tsoilEra5 =  ncvar_get(ncin_era5, 'stl1_mean')[ , , whichEra5Days]
	nc_tsoilCfs =   ncvar_get(ncin_cfs, 'stl1')[ , , whichCfsDays, ]
	nc_tsoilSeas5 = ncvar_get(ncin_seas5, 'stl1')[ , , whichSeas5Days, ]


	finalIter = 0	
	summaryOutput_df = data.frame(Lat = 0, Lon = 0, soilTempThresholds = 0, consecDaysThresholds = 0, baseTempGDDs = 0,
		variableName = 'string', projectionOrClimatology = 'string',
		quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)
	for(this_lon in 1:length(nc_lonEra5))	{
		for(this_lat in 1:length(nc_latEra5))	{
			theseSoilTemp = nc_tsoilEra5[this_lon, this_lat, ]
			
			if(any(!is.na(theseSoilTemp)))	{
	
				allYearsOutput_df = data.frame(soilTempThresholds = NA, consecDaysThresholds = NA, daysToWarmSoils = NA,
					baseTempGDDs = NA, GDDvalueThresholds = NA, daysToGDDThreshold = NA)
				iter = 0
				for(hh in 1:length(baseTempGDDs))	{
					for(ii in 1:length(soilTempThresholds))	{
						for(jj in 1:length(consecDaysThresholds))	{
							for(thisCfsModel in cfsModels)	{
								for(thisSeas5Model in seas5Models)	{
									if(incHistData)	{
										theseTavg = c(nc_tavgEra5[this_lon, this_lat, ],
											nc_tavgCfs[this_lon, this_lat, , thisCfsModel],
											nc_tavgSeas5[this_lon, this_lat, , thisSeas5Model],
											tempClimatologyTavg_nc[this_lon, this_lat, whichClimatologyDays])
										theseTavg[is.na(theseTavg)] = mean(theseTavg, na.rm=TRUE)

										theseTsoils = c(nc_tsoilEra5[this_lon, this_lat, ],
											nc_tsoilCfs[this_lon, this_lat, , thisCfsModel],
											nc_tsoilSeas5[this_lon, this_lat, , thisSeas5Model],
											tempClimatologySoils_nc[this_lon, this_lat, whichClimatologyDays])
										theseTsoils[is.na(theseTsoils)] = mean(theseTsoils, na.rm=TRUE)

									} else {
										theseTavg = c(nc_tavgCfs[this_lon, this_lat, , thisCfsModel],
											nc_tavgSeas5[this_lon, this_lat, , thisSeas5Model],
											tempClimatologyTavg_nc[this_lon, this_lat, whichClimatologyDays])
										theseTavg[is.na(theseTavg)] = mean(theseTavg, na.rm=TRUE)

										theseTsoils = c(nc_tsoilCfs[this_lon, this_lat, , thisCfsModel],
											nc_tsoilSeas5[this_lon, this_lat, , thisSeas5Model],
											tempClimatologySoils_nc[this_lon, this_lat, whichClimatologyDays])
										theseTsoils[is.na(theseTsoils)] = mean(theseTsoils, na.rm=TRUE)
									}

									theseGDD = ifelse(theseTavg > baseTempGDDs[hh], theseTavg - baseTempGDDs[hh], 0)
		
									exceedsSoilThresh = which(theseTsoils > soilTempThresholds[ii]) 
									threshDiff = diff(exceedsSoilThresh, consecDaysThresholds[jj])
									daysToWarmSoils = ifelse(any(threshDiff == consecDaysThresholds[jj][1]), 
										exceedsSoilThresh[which(threshDiff == consecDaysThresholds[jj])[1] + (consecDaysThresholds[jj] - 1)],
										364)
			
									for(kk in 1:length(GDDvalueThresholds))	{
										iter = iter + 1
										thisGDD = cumsum(theseGDD[-(1:daysToWarmSoils)])
										maxGDD = ifelse(daysToWarmSoils < 364, max(thisGDD) , 0)
										allYearsOutput_df[iter, ] = c(soilTempThresholds[ii], consecDaysThresholds[jj], daysToWarmSoils,
											baseTempGDDs[hh], GDDvalueThresholds[kk],
											ifelse(maxGDD > GDDvalueThresholds[kk], daysToWarmSoils + which(thisGDD > GDDvalueThresholds[kk])[1], 365))
									}
								}
							}	
						}
					}
				}
				

				thisLat = nc_latEra5[this_lat]
				thisLon = nc_lonEra5[this_lon]
				for(thisSoilTempThresholds in soilTempThresholds)				{
					for(thisConsecDaysThresholds in consecDaysThresholds)		{
						for(thisBaseTempGDDs in baseTempGDDs)					{
							daysSinceDate = ceiling(quantile(subset(allYearsOutput_df,
									soilTempThresholds == thisSoilTempThresholds & 
									consecDaysThresholds == thisConsecDaysThresholds &
									baseTempGDDs == thisBaseTempGDDs)$daysToWarmSoils,
									theseQuantiles))
							datesByQuantile = growYearStartDate + daysSinceDate

							finalIter = finalIter + 1
							summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
								paste0('Warm Soils'), 'projection',
								'Q5_to_Q25', paste0(datesByQuantile[1]), paste0(datesByQuantile[2]),
								paste0(userName), paste0(forecastDate), daysSinceDate[2])

							finalIter = finalIter + 1
							summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
								paste0('Warm Soils'), 'projection',
								'Q25_to_Q75', paste0(datesByQuantile[2]), paste0(datesByQuantile[4]),
								paste0(userName), paste0(forecastDate), daysSinceDate[3])

							finalIter = finalIter + 1
							summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
								paste0('Warm Soils'), 'projection',
								'Q75_to_Q95', paste0(datesByQuantile[4]), paste0(datesByQuantile[5]),
								paste0(userName), paste0(forecastDate), daysSinceDate[4])
								
								
							for(thisGDDvalueThresholds in GDDvalueThresholds)	{
								daysSinceDate = ceiling(quantile(subset(allYearsOutput_df,
										soilTempThresholds == thisSoilTempThresholds &
										consecDaysThresholds == thisConsecDaysThresholds &
										baseTempGDDs == thisBaseTempGDDs &
										GDDvalueThresholds == thisGDDvalueThresholds)$daysToGDDThreshold,
										theseQuantiles))
								datesByQuantile = growYearStartDate + daysSinceDate
								
								finalIter = finalIter + 1
								summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
									paste0('GDD = ', thisGDDvalueThresholds), 'projection',
									'Q5_to_Q25', paste0(datesByQuantile[1]), paste0(datesByQuantile[2]),
									paste0(userName), paste0(forecastDate), daysSinceDate[2])

								finalIter = finalIter + 1
								summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
									paste0('GDD = ', thisGDDvalueThresholds), 'projection',
									'Q25_to_Q75', paste0(datesByQuantile[2]), paste0(datesByQuantile[4]),
									paste0(userName), paste0(forecastDate), daysSinceDate[3])

								finalIter = finalIter + 1
								summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thisSoilTempThresholds, thisConsecDaysThresholds, thisBaseTempGDDs,
									paste0('GDD = ', thisGDDvalueThresholds), 'projection',
									'Q75_to_Q95', paste0(datesByQuantile[4]), paste0(datesByQuantile[5]),
									paste0(userName), paste0(forecastDate), daysSinceDate[4])
		

								}
						}
					}
                    print(finalIter)
					if(nrow(summaryOutput_df) > 20000)	{
						save_iter = save_iter + 1
						fwrite(summaryOutput_df, paste0(dataPath, "temp_out_", save_iter, ".csv"))
						summaryOutput_df = data.frame(Lat = 0, Lon = 0, soilTempThresholds = 0, consecDaysThresholds = 0, baseTempGDDs = 0,
							variableName = 'string', projectionOrClimatology = 'string',
							quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)
						finalIter = 0
						print(c(this_lat, this_lon))
					}
				} 
			}
		}
	}
	# combining saved iterations and returning the dataframe with formatted climatology
	save_iter = save_iter + 1
	fwrite(summaryOutput_df, paste0(dataPath, "temp_out_", save_iter, ".csv"))

	summaryOutput_df = fread(paste0(dataPath, "temp_out_", 1, ".csv"))
	for(files in 2:save_iter)	{
		summaryOutput_df = rbind(summaryOutput_df, fread(paste0(dataPath, "temp_out_", files, ".csv")))
	}
	return(summaryOutput_df)
}
