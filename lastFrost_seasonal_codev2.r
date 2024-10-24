	#load libraries
library(lubridate)
library(ncdf4)
library(data.table)


####################################################################################################
##	define all names, file locations, and variables
	# names and variables
growYearEndDate = as.Date('2023-08-01')
dataOrigination = c('ERA5', 'CFS', 'SEAS5')
dataPath = 'J:\\Cai_data\\Simplot\\lastFrost\\Midwest\\'			#	MountainWest, 			NorthernGreatPlains,			AllGreatPlains,			Midwest
#'J:\\Cai_data\\Nuseed\\'   , NuseedSouth   #startDateClimatology = '1990-08-01'	# NuseedSouth
storeLocations = NA		# 			'storLocs.csv' 			# either NA or a csv with store locations
era5RecentDataName = 'SimplotMidwest-testing-recent-era.nc'				#	SimplotMtnWst			SimplotNorthGrtPlns				SimplotAllGrtPlns,		SimplotMidwest
era5ClimatologyDataName = 	'SimplotMidwest-testing-climatology-era.nc'#	SimplotMtnWst			SimplotNorthGrtPlns				SimplotAllGrtPlns,		SimplotMidwest
cfsDataName = 				'SimplotMidwest-testing-cfs.nc'				#	SimplotMtnWst			SimplotNorthGrtPlns				SimplotAllGrtPlns,		SimplotMidwest
seas5DataName = 			'SimplotMidwest-testing-seas5.nc'			#	SimplotMtnWst			SimplotNorthGrtPlns				SimplotAllGrtPlns,		SimplotMidwest
userName = 'Simplot Midwest' 										#Simplot Mountain West,  	Simplot Northern Great Plains,	Simplot Great Plains,	Simplot Midwest
startDateEra5 = '2022-08-01'
startDateCfs = '2023-05-21'
startDateSeas5 = '2023-05-01'
#startDateClimatology = '2002-07-01'	# All Great Plains; MountainWest
startDateClimatology = '2000-08-01'	# Midwest; NorthernGreatPlains


cfsModels = 1:4	# number of cfs models used
seas5Models = 1:51	# number of seas5 models used

	# define the 'sortable' variables
theseQuantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)
lastFrostThresholds = c(-2, -1, 0, 1)	# in C
consecDaysThresholds = c(1, 2, 5)
forecastDate = ncvar_get(nc_open(paste0(dataPath, cfsDataName)), 'lead_time')[1] + as.Date(startDateCfs) 
	# test dates
ncvar_get(nc_open(paste0(dataPath, cfsDataName)), 'lead_time') + as.Date(startDateCfs) 
ncvar_get(nc_open(paste0(dataPath, era5RecentDataName)), 'time') + as.Date(startDateEra5)
ncvar_get(nc_open(paste0(dataPath, seas5DataName)), 'lead_time') + as.Date(startDateSeas5)
ncvar_get(nc_open(paste0(dataPath, era5ClimatologyDataName)), 'time') + as.Date(startDateClimatology)

#################################################################################################
##	historic data / climatology only needs to be run once, then data are stored for future use
#historicOutput = f_historiclastFrost(
#	theseQuantiles = theseQuantiles,
#	lastFrostThresholds = lastFrostThresholds,
#	consecDaysThresholds = consecDaysThresholds,
#	growYearEndDate = growYearEndDate,
#	dataPath = dataPath,
#	dataName = era5ClimatologyDataName, 
#	userName = userName,
#	forecastDate = forecastDate,
#	startDateClimatology = startDateClimatology)
	
#fwrite(historicOutput, paste0(dataPath, "climatologyLastFrost_21DEC2022.csv"))
##################################################################################################


##################################################################################################
##	forecast data, to be run every week
projectedOutput = f_projectedlastFrost(
	theseQuantiles = theseQuantiles,
	lastFrostThresholds = lastFrostThresholds,
	consecDaysThresholds = consecDaysThresholds,
	growYearEndDate = growYearEndDate,
	dataPath = dataPath,
	era5RecentDataName = era5RecentDataName,
	cfsDataName = cfsDataName,
	seas5DataName = seas5DataName,
	startDateEra5 = startDateEra5,
	startDateCfs = startDateCfs,
	startDateSeas5 = startDateSeas5,
	cfsModels = cfsModels,
	seas5Models = seas5Models,
	userName = userName,
	forecastDate = forecastDate)	



historicOutput = fread(paste0(dataPath, "climatologyLastFrost_21DEC2022.csv"))
#historicOutput$startDate = as.IDate(mdy(historicOutput$startDate))
#historicOutput$endDate =  as.IDate(mdy(historicOutput$endDate))
#historicOutput$forecastDate =  as.IDate(mdy(historicOutput$forecastDate))
allOutput = rbind(projectedOutput, historicOutput)
fwrite(allOutput, paste0(dataPath, 'lastFrost_', allOutput[1, 'User'], "_", forecastDate, '.csv'))
summary(projectedOutput); summary(historicOutput)


##	identifying user locations instead of heatmap, only needed for some users
if(!is.na(storeLocations))	{
	storeLocs = fread(paste0(dataPath, storeLocations))
	for(i in 1:nrow(storeLocs))	{
		closeLat = allOutput$Lat[which.min(abs(allOutput$Lat - storeLocs$Lat[i]))]
		closeLon = allOutput$Lon[which.min(abs(allOutput$Lon - storeLocs$Lon[i]))] 
		nrNgbr = subset(allOutput, Lat == closeLat & Lon == closeLon)
		nrNgbr$Lat = storeLocs$Lat[i]
		nrNgbr$Lon = storeLocs$Lon[i]
		nrNgbr$User = paste0(userName, ' - Store Location')
		allOutput = rbind(allOutput, nrNgbr)
		
	}
}

## saving output

fwrite(allOutput, paste0(dataPath, 'lastFrost_', allOutput[1, 'User'], "_", forecastDate, '.csv'))
summary(projectedOutput)
summary(historicOutput)
##################################################################################################

#allOutput = fread(paste0(dataPath, 'lastFrost_', userName , "_", forecastDate, '.csv'))
# one-off analysis for Nuseed, may delete if not needed again

#storeOutput = allOutput[1,]
#storeOutput$State = NA
#storeOutput$City = NA
##	identifying user locations instead of heatmap, only needed for some users
#storeLocs = fread(paste0(dataPath, 'storLocs.csv'))
#for(i in 1:nrow(storeLocs))	{
#	closeLat = allOutput$Lat[which.min(abs(allOutput$Lat - storeLocs$Lat[i]))]
#	closeLon = allOutput$Lon[which.min(abs(allOutput$Lon - storeLocs$Lon[i]))] 
#	nrNgbr = subset(allOutput, Lat == closeLat & Lon == closeLon)
#	nrNgbr$Lat = storeLocs$Lat[i]
#	nrNgbr$Lon = storeLocs$Lon[i]
#	nrNgbr$User = paste0(userName, ' - Store Location')
#	nrNgbr$City = storeLocs$City[i]
#	nrNgbr$State = storeLocs$State[i]
#	storeOutput = rbind(storeOutput, nrNgbr)
#}
#fwrite(storeOutput[-1,], paste0(dataPath, 'lastFrost_storeLocs_', allOutput[1, 'User'], "_", forecastDate, '.csv'))






############################################################
##	function for defining climatology last frost thresholds


f_historiclastFrost = function(theseQuantiles = theseQuantiles,
	lastFrostThresholds = lastFrostThresholds,
	consecDaysThresholds = consecDaysThresholds,
	growYearEndDate = growYearEndDate,
	dataPath = dataPath,
	dataName = era5ClimatologyDataName, 
	userName = userName,
	forecastDate = forecastDate,
	startDateClimatology = startDateClimatology)	{

	library(lubridate)
	save_iter = 0
	
		# read in ncdf data
	ncin = nc_open(paste0(dataPath, dataName)) # [longitude,latitude,time]   (Contiguous storage) 
	nc_tmin = (ncvar_get(ncin, 't2m_min'))

	nc_date = ncvar_get(ncin, 'time') + as.Date(startDateClimatology)
	nc_doy = yday(nc_date)
	nc_year = year(nc_date)
	
	nc_lat = ncvar_get(ncin, 'latitude')
	nc_lon = ncvar_get(ncin, 'longitude')
	
	if(yday(growYearEndDate) == 1)	{
		growYearDOYs = 1:366
	}	else	{growYearDOYs = c(yday(growYearEndDate):366, 1:(yday(growYearEndDate)-1))}
	

	finalIter = 0	

	# generating .ncs for saving average historical values for backfilling projection data
	tempClimatologyTmin_nc = nc_tmin[ , , 1:366]

	summaryOutput_df = data.frame(Lat = 0, Lon = 0, lastFrostThresholds = 0, consecDaysThresholds = 0, 
		variableName = 'string', projectionOrClimatology = 'string',
		quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)

	for(this_lon in 1:length(nc_lon))	{
		for(this_lat in 1:length(nc_lat))	{
			
			theseTmin = nc_tmin[this_lon, this_lat, ]
			
			for(gg in 1:366)	{
					tempClimatologyTmin_nc[this_lon, this_lat, gg] = median(theseTmin[nc_doy == gg])
				}
				
			
			allYearsOutput_df = data.frame(lastFrostThresholds = NA, consecDaysThresholds = NA, daysTolastFrost = NA)
			iter = 0

			for(ii in 1:length(lastFrostThresholds))	{
				for(jj in 1:length(consecDaysThresholds))	{
					theseConsecDays = consecDaysThresholds[jj] - 1
					uniqueYears = unique(nc_year)
					for(thisYear in uniqueYears[-c(1,length(uniqueYears))])	{
						iter = iter+1
						thisGrowYear = c(which(nc_year == thisYear - 1 & nc_doy >= yday(growYearEndDate)), 
							which(nc_year == thisYear & nc_doy < yday(growYearEndDate))) 
						
						atLastFrostTemp = which(theseTmin[thisGrowYear] <= (lastFrostThresholds[ii]))
						
						if(theseConsecDays == 0){
							daysTolastFrost = ifelse(length(atLastFrostTemp) >= 1,
								last(atLastFrostTemp),
								1)
						}	else {
							threshDiff = diff(atLastFrostTemp, theseConsecDays)
							daysTolastFrost = ifelse(any(threshDiff == theseConsecDays), 
								atLastFrostTemp[last(which(threshDiff == theseConsecDays))] + theseConsecDays,
								1)
						}
				
							
						allYearsOutput_df[iter, ] = c(lastFrostThresholds[ii], consecDaysThresholds[jj], daysTolastFrost)
					}
				}
			}
						

			thisLat = nc_lat[this_lat]
			thisLon = nc_lon[this_lon]
			for(thislastFrostThresholds in lastFrostThresholds)				{
				for(thisConsecDaysThresholds in consecDaysThresholds)		{
					daysSinceDate = ceiling(quantile(subset(allYearsOutput_df,
							lastFrostThresholds == thislastFrostThresholds & 
							consecDaysThresholds == thisConsecDaysThresholds)$daysTolastFrost,
							theseQuantiles))
					datesByQuantile = (growYearEndDate - 365)  + daysSinceDate

					finalIter = finalIter + 1
					summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thislastFrostThresholds, thisConsecDaysThresholds, 
						paste0('DaysTolastFrost'), 'climatology',
						'Q5_to_Q25', paste0(datesByQuantile[1]), paste0(datesByQuantile[2]),
						paste0(userName), paste0(forecastDate), daysSinceDate[2])

					finalIter = finalIter + 1
					summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thislastFrostThresholds, thisConsecDaysThresholds, 
						paste0('DaysTolastFrost'), 'climatology',
						'Q25_to_Q75', paste0(datesByQuantile[2]), paste0(datesByQuantile[4]),
						paste0(userName), paste0(forecastDate), daysSinceDate[3])

					finalIter = finalIter + 1
					summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thislastFrostThresholds, thisConsecDaysThresholds, 
						paste0('DaysTolastFrost'), 'climatology',
						'Q75_to_Q95', paste0(datesByQuantile[4]), paste0(datesByQuantile[5]),
						paste0(userName), paste0(forecastDate), daysSinceDate[4])
							
				}
			}
			
			if(nrow(summaryOutput_df) > 20000)	{
				save_iter = save_iter + 1
				fwrite(summaryOutput_df, paste0(dataPath, "temp_out_", save_iter, ".csv"))
				summaryOutput_df = data.frame(Lat = 0, Lon = 0, lastFrostThresholds = 0, consecDaysThresholds = 0, 
					variableName = 'string', projectionOrClimatology = 'string',
					quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)
				finalIter = 0
				print(c(this_lat, this_lon))
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

		# saving workspace that holds climatology .ncs
	save(tempClimatologyTmin_nc, file=paste0(dataPath, 'climatologyTmin_lastFrost.RData'))

	return(summaryOutput_df)
}	




										


	


############################################################################################################################
# incorporating projections data

f_projectedlastFrost = function(theseQuantiles = theseQuantiles,
	lastFrostThresholds = lastFrostThresholds,
	consecDaysThresholds = consecDaysThresholds,
	growYearEndDate = growYearEndDate,
	dataPath = dataPath,
	era5RecentDataName = era5RecentDataName,
	cfsDataName = cfsDataName,
	seas5DataName = seas5DataName,
	startDateEra5 = startDateEra5,
	startDateCfs = startDateCfs,
	startDateSeas5 = startDateSeas5,
	cfsModels = 1:4,
	seas5Models = 1:51,
	userName = userName,
	forecastDate = forecastDate)
	{



		# loading climatology records of Tmin by day of year
	load(paste0(dataPath, 'climatologyTmin_lastFrost.RData'))	# loads tempClimatologyTmin_nc

	library(lubridate)
	'%ni%' = Negate("%in%")
	save_iter = 0

		# read in ncdf data
	ncin_era5 = nc_open(paste0(dataPath, era5RecentDataName))	# [longitude,latitude,time]   (Contiguous storage) 
	ncin_cfs = nc_open(paste0(dataPath, cfsDataName))			# [longitude,latitude,lead_time,member]   (Contiguous storage) 
	ncin_seas5 = nc_open(paste0(dataPath, seas5DataName))		# [longitude,latitude,lead_time,member] 
	
	nc_dateEra5 = ncvar_get(ncin_era5, 'time') + as.Date(startDateEra5)
	nc_dateCfs = ncvar_get(ncin_cfs, 'lead_time') +  as.Date(startDateCfs)
	nc_dateSeas5 = ncvar_get(ncin_seas5, 'lead_time') + as.Date(startDateSeas5)

	nc_doyEra5 = yday(nc_dateEra5)
	nc_doyCfs = yday(nc_dateCfs)
	nc_doySeas5 = yday(nc_dateSeas5)
	
	nc_yearEra5 = year(nc_dateEra5)
	nc_yearCfs = year(nc_dateCfs)
	nc_yearSeas5 = year(nc_dateSeas5)
	
	nc_latEra5 = ncvar_get(ncin_era5, 'latitude')
	nc_latCfs = ncvar_get(ncin_cfs, 'latitude')	# backwards from ERA and SEAS
	nc_latSeas5 = ncvar_get(ncin_seas5, 'latitude')
	
	nc_lonEra5 = ncvar_get(ncin_era5, 'longitude')
	nc_lonCfs = ncvar_get(ncin_cfs, 'longitude')
	nc_lonSeas5 = ncvar_get(ncin_seas5, 'longitude')

		# defining the growing year
	growYear = seq((growYearEndDate - 365), growYearEndDate, 1)
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
		whichClimatologyDays = growYearDOYs[-c(1:(length(whichEra5Days) + length(whichCfsDays) + length(whichSeas5Days)))]
	} else {
		whichCfsDays = which(as.character(nc_dateCfs) %in% as.character(growYear))
		whichSeas5Days = which(as.character(nc_dateSeas5) %in% as.character(growYear))[-c(1:length(whichCfsDays))]
		whichClimatologyDays = growYearDOYs[-c(1:(length(whichCfsDays) + length(whichSeas5Days)))]
	}
	

	nc_tminEra5 = ncvar_get(ncin_era5, 't2m_min')[ , , whichEra5Days]
	nc_tminCfs = ncvar_get(ncin_cfs, 't2m_min')[ , , whichCfsDays, ]
	nc_tminSeas5 = ncvar_get(ncin_seas5, 't2m_min')[ , , whichSeas5Days, ]


	finalIter = 0	
	summaryOutput_df = data.frame(Lat = 0, Lon = 0, lastFrostThresholds = 0, consecDaysThresholds = 0, 
		variableName = 'string', projectionOrClimatology = 'string',
		quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)
	for(this_lon in 1:length(nc_lonEra5))	{
		for(this_lat in 1:length(nc_latEra5))	{
			allYearsOutput_df = data.frame(lastFrostThresholds = NA, consecDaysThresholds = NA, daysTolastFrost = NA)
			iter = 0
	
			for(ii in 1:length(lastFrostThresholds))	{
				for(jj in 1:length(consecDaysThresholds))	{
					theseConsecDays = consecDaysThresholds[jj] - 1
					for(thisCfsModel in cfsModels)	{
						for(thisSeas5Model in seas5Models)	{
							if(incHistData)	{
								theseTmin = c(nc_tminEra5[this_lon, this_lat, ],
									nc_tminCfs[this_lon, this_lat, , thisCfsModel],		
									nc_tminSeas5[this_lon, this_lat, , thisSeas5Model],
									tempClimatologyTmin_nc[this_lon, this_lat, whichClimatologyDays])
								theseTmin[is.na(theseTmin)] = mean(theseTmin, na.rm=TRUE)
							} else {
								theseTmin = c(nc_tminCfs[this_lon, this_lat, , thisCfsModel],
									nc_tminSeas5[this_lon, this_lat, , thisSeas5Model],
									tempClimatologyTmin_nc[this_lon, this_lat, whichClimatologyDays])
								theseTmin[is.na(theseTmin)] = mean(theseTmin, na.rm=TRUE)
							}

							atLastFrostTemp = which(theseTmin <= (lastFrostThresholds[ii])) ## adjusting to soil temp

							if(theseConsecDays == 0){
								daysTolastFrost = ifelse(length(atLastFrostTemp) >= 1,
									last(atLastFrostTemp),
									1)
							}	else {
								threshDiff = diff(atLastFrostTemp, theseConsecDays)
								daysTolastFrost = ifelse(any(threshDiff == theseConsecDays), 
									last(atLastFrostTemp[which(threshDiff == theseConsecDays)]) + theseConsecDays,
									1)
							}

							iter = iter + 1
							allYearsOutput_df[iter, ] = c(lastFrostThresholds[ii], consecDaysThresholds[jj], daysTolastFrost)
						}
					}	
				}
			}
				

			thisLat = nc_latEra5[this_lat]
			thisLon = nc_lonEra5[this_lon]
			for(thislastFrostThresholds in lastFrostThresholds)				{
				for(thisConsecDaysThresholds in consecDaysThresholds)		{
					daysSinceDate = ceiling(quantile(subset(allYearsOutput_df,
							lastFrostThresholds == thislastFrostThresholds & 
							consecDaysThresholds == thisConsecDaysThresholds)$daysTolastFrost,
							theseQuantiles))
					datesByQuantile = (growYearEndDate - 365) + daysSinceDate

					finalIter = finalIter + 1
					summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thislastFrostThresholds, thisConsecDaysThresholds, 
						paste0('DaysTolastFrost'), 'projection',
						'Q5_to_Q25', paste0(datesByQuantile[1]), paste0(datesByQuantile[2]),
						paste0(userName), paste0(forecastDate), daysSinceDate[2])

					finalIter = finalIter + 1
					summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thislastFrostThresholds, thisConsecDaysThresholds,
						paste0('DaysTolastFrost'), 'projection',
						'Q25_to_Q75', paste0(datesByQuantile[2]), paste0(datesByQuantile[4]),
						paste0(userName), paste0(forecastDate), daysSinceDate[3])

					finalIter = finalIter + 1
					summaryOutput_df[finalIter, ] = c(thisLat, thisLon, thislastFrostThresholds, thisConsecDaysThresholds,
						paste0('DaysTolastFrost'), 'projection',
						'Q75_to_Q95', paste0(datesByQuantile[4]), paste0(datesByQuantile[5]),
						paste0(userName), paste0(forecastDate), daysSinceDate[4])
				}
			}

			if(nrow(summaryOutput_df) > 20000)	{
				save_iter = save_iter + 1
				fwrite(summaryOutput_df, paste0(dataPath, "temp_out_", save_iter, ".csv"))
				summaryOutput_df = data.frame(Lat = 0, Lon = 0, lastFrostThresholds = 0, consecDaysThresholds = 0, 
					variableName = 'string', projectionOrClimatology = 'string',
					quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)
				finalIter = 0
				print(c(this_lat, this_lon))
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










