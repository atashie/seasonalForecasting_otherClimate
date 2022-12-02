
############################################################
##	function for defining climatology GDD / temp thresholds
f_historicTempGDD = function(theseQuantiles = theseQuantiles,
	baseTempGDDs = baseTempGDDs,
	kfact = kfact, 
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
baseTempGDDs = baseTempGDDs,
	kfact = kfact, 
	growYearStartDate = growYearStartDate,
	dataPath = dataPath,
	seas5DataName = seas5DataName, 
	userName = userName,
	forecastDate = forecastDate,
	userData = userData)	{

	ncin_seas5 = nc_open(paste0(dataPath, seas5DataName),  return_on_error = TRUE)	# [longitude,latitude,lead_time,member] 
	nc_dateSeas5 = ncvar_get(ncin_seas5, 'lead_time') + as.Date(startDateSeas5)
	nc_doySeas5 = yday(nc_dateSeas5)
	nc_yearSeas5 = year(nc_dateSeas5)
	nc_latSeas5 = ncvar_get(ncin_seas5, 'latitude')
	nc_lonSeas5 = ncvar_get(ncin_seas5, 'longitude')

	summaryOutput_df = data.frame(ID = 'string', 
		Lat = 0, Lon = 0, soilTempThresholds = 0, consecDaysThresholds = 0, baseTempGDDs = 0,
		variableName = 'string', projectionOrClimatology = 'string',
		quantiles = 'string', startDate = 'string', endDate = 'string', User = 'string', forecastDate = 'string', numericValue = 0)



#########################################
# reading in climai netcdf clim data
ncpath = "j:\\Cai_data\\Propane\\"

city_name = 'Detroit'#"Columbus"#'Chicago'#'Milwaukee'##'Indianapolis'#
state_name = 'Michigan'#"Ohio"#'Illinois'#'Wisconsin'# 'Indiana'#
country_name = "USA"
user_name = "Propane Demo"

	# reading in tmin 
ncname = paste0("t2m_max_bias_corrected_hist_", city_name,".nc")  
ncfname = paste0(ncpath, ncname)
ncin = nc_open(ncfname)
nc_lat = ncvar_get(ncin, 'latitude')
nc_lon = ncvar_get(ncin, 'longitude')
nc_vTime_tmax = ncvar_get(ncin, 'valid_time')
dates_df = data.frame(matrix("", nrow=nrow(nc_vTime_tmax), ncol=ncol(nc_vTime_tmax)))
for(i in 1:ncol(dates_df))	{dates_df[,i] =  as.Date("1993-01-02")}
nc_dates = (dates_df + nc_vTime_tmax)	; nc_yday = NULL
nc_yday = data.frame(matrix("", nrow=nrow(nc_vTime_tmax), ncol=ncol(nc_vTime_tmax)))
for(i in 1:ncol(nc_dates))	{nc_yday[,i] = yday(nc_dates[,i])}
nc_month = ncvar_get(ncin, 'month')
models_to_use = 1:25	# models after 25 are NaN
nc_tmax = ncvar_get(ncin, "t2m_max")[models_to_use,,]

ncname = paste0("t2m_min_bias_corrected_hist_", city_name,".nc")   
ncfname = paste0(ncpath, ncname)
ncin = nc_open(ncfname)
nc_lat = ncvar_get(ncin, 'latitude')
nc_lon = ncvar_get(ncin, 'longitude')
nc_vTime_tmin = ncvar_get(ncin, 'valid_time')
nc_month = ncvar_get(ncin, 'month')
nc_date_tmin = as.Date("1993-01-02") + nc_vTime_tmin # time is days after jan 1 1990
nc_tmin = ncvar_get(ncin, "t2m_min")[models_to_use,,]
	
	# since tmin contains NAs at random locations, first find the avearge daily difference for calculating a reasonable tavg
avg_diff = mean(nc_tmin[models_to_use,,1:100] - nc_tmax[models_to_use,,1:100])

cdd_list = list()
hdd_list = list()
for(i in models_to_use)	{
	nc_tavg = nc_tmax + avg_diff
	tav = nc_tavg[i,,]
	tav_f = tav * (9/5) + 32
	
	cdd_list[[i]] = ifelse(tav_f > 65, tav_f - 65, 0)
	hdd_list[[i]] = ifelse(tav_f < 65, 65 - tav_f, 0)
}

climatology_cdd = NULL
climatology_hdd = NULL
for(i in 1:366)	{
	this_yday = which(nc_yday == i)
	this_clim_cdd = NULL
	this_clim_hdd = NULL
	for(j in models_to_use)	{
		this_clim_cdd = c(this_clim_cdd, mean(cdd_list[[j]][this_yday]))
		this_clim_hdd = c(this_clim_hdd, mean(hdd_list[[j]][this_yday]))
	}
	climatology_cdd = c(climatology_cdd, mean(this_clim_cdd))
	climatology_hdd = c(climatology_hdd, mean(this_clim_hdd))
}
		

dd_df = data.frame('ID' = NA, 'User' = NA, "Location" = NA, 'State_or_Province' = NA, 'Country' = NA, "Lat" = NA, "Lon" = NA, 'Variable' = NA, 
	"Forecast_Date" = as.Date("1993-01-02"), "Lead_Time" = NA,
	"Q5" = NA, "Q25" = NA, "Q50" = NA, "Q75" = NA, "Q95" = NA,
	"Climatology" = NA, "Unit" = 'Farenheit', "System_of_Unit" = 'Imperial')
var_names = c("HDD_avg","HDD_rel","HDD_kfact","CDD_avg","CDD_rel","CDD_kfact")
kfactor = 6.5

date_col =349 #342 # 
num_months = 1:(floor(nrow(nc_vTime_tmin) / 30) - 1)
iter = 0
for(j in num_months)	{
	these_days = 1:30 + (j - 1) * 30
	iter = iter + 1
	which_var = 1

	this_hdd = NULL
	this_cdd = NULL
	for(k in models_to_use)	{
		this_hdd = c(this_hdd, sum(hdd_list[[k]][these_days,date_col]))	# currently only using the most recent data, the 349th column
		this_cdd = c(this_cdd, sum(cdd_list[[k]][these_days,date_col]))
	}

	these_yday = nc_yday[these_days,date_col]


		#HDD avg
	hdd_clim = mean(climatology_hdd[these_yday])
	dd_df[iter,c('ID','User', 'Location', 'State_or_Province', 'Country')] = c(iter, user_name, city_name, state_name, country_name)
	dd_df[iter,c('Lat','Lon')] = c(nc_lat, nc_lon)
	dd_df[iter,'Forecast_Date'] = as.Date(nc_vTime_tmax[1,date_col] + as.Date("1993-01-02"))
	dd_df[iter,c('Unit','System_of_Unit')] = c('Farenheit','Imperial')
	dd_df[iter,'Variable'] = var_names[which_var]
	dd_df[iter,'Lead_Time'] = paste(j, 'months')
	dd_df[iter,c('Q5','Q25','Q50','Q75','Q95')] = quantile(this_hdd, c(0.05,0.25,0.5,0.75,0.95)) / 30
	dd_df[iter,'Climatology'] = hdd_clim

		#HDD relative
	which_var = which_var + 1	; iter = iter + 1
	dd_df[iter,c('ID','User', 'Location', 'State_or_Province', 'Country')] = c(iter, user_name, city_name, state_name, country_name)
	dd_df[iter,c('Lat','Lon')] = c(nc_lat, nc_lon)
	dd_df[iter,'Forecast_Date'] = as.Date(nc_vTime_tmax[1,date_col] + as.Date("1993-01-02"))
	dd_df[iter,c('Unit','System_of_Unit')] = c('Farenheit','Imperial')
	dd_df[iter,'Variable'] = var_names[which_var]
	dd_df[iter,'Lead_Time'] = paste(j, 'months')
	if(hdd_clim > 1)	{	# catching NaNs from dividing by 0
		dd_df[iter,c('Q5','Q25','Q50','Q75','Q95')] = 100 * ((quantile(this_hdd, c(0.05,0.25,0.5,0.75,0.95)) / 30) - hdd_clim) / hdd_clim
	} else {
		dd_df[iter,c('Q5','Q25','Q50','Q75','Q95')] = rep(0,5)
	}
	dd_df[iter,'Climatology'] = 0

		#HDD with kfactor
	which_var = which_var + 1	; iter = iter + 1
	dd_df[iter,c('ID','User', 'Location', 'State_or_Province', 'Country')] = c(iter, user_name, city_name, state_name, country_name)
	dd_df[iter,c('Lat','Lon')] = c(nc_lat, nc_lon)
	dd_df[iter,'Forecast_Date'] = as.Date(nc_vTime_tmax[1,date_col] + as.Date("1993-01-02"))
	dd_df[iter,c('Unit','System_of_Unit')] = c('Farenheit','Imperial')
	dd_df[iter,'Variable'] = var_names[which_var]
	dd_df[iter,'Lead_Time'] = paste(j, 'months')
	dd_df[iter,c('Q5','Q25','Q50','Q75','Q95')] = quantile(this_hdd, c(0.05,0.25,0.5,0.75,0.95)) / (30 * kfactor)
	dd_df[iter,'Climatology'] = hdd_clim / kfactor

		#CDD avg
	which_var = which_var + 1	; iter = iter + 1
	cdd_clim = mean(climatology_cdd[these_yday])
	dd_df[iter,c('ID','User', 'Location', 'State_or_Province', 'Country')] = c(iter, user_name, city_name, state_name, country_name)
	dd_df[iter,c('Lat','Lon')] = c(nc_lat, nc_lon)
	dd_df[iter,'Forecast_Date'] = as.Date(nc_vTime_tmax[1,date_col] + as.Date("1993-01-02"))
	dd_df[iter,c('Unit','System_of_Unit')] = c('Farenheit','Imperial')
	dd_df[iter,'Variable'] = var_names[which_var]
	dd_df[iter,'Lead_Time'] = paste(j, 'months')
	dd_df[iter,c('Q5','Q25','Q50','Q75','Q95')] = quantile(this_cdd, c(0.05,0.25,0.5,0.75,0.95)) / 30
	dd_df[iter,'Climatology'] = cdd_clim

		#CDD relative
	which_var = which_var + 1	; iter = iter + 1
	dd_df[iter,c('ID','User', 'Location', 'State_or_Province', 'Country')] = c(iter, user_name, city_name, state_name, country_name)
	dd_df[iter,c('Lat','Lon')] = c(nc_lat, nc_lon)
	dd_df[iter,'Forecast_Date'] = as.Date(nc_vTime_tmax[1,date_col] + as.Date("1993-01-02"))
	dd_df[iter,c('Unit','System_of_Unit')] = c('Farenheit','Imperial')
	dd_df[iter,'Variable'] = var_names[which_var]
	dd_df[iter,'Lead_Time'] = paste(j, 'months')
	if(cdd_clim > 1)	{	# catching NaNs from dividing by 0, and preventing big swings due to low values
		dd_df[iter,c('Q5','Q25','Q50','Q75','Q95')] = 100 * ((quantile(this_cdd, c(0.05,0.25,0.5,0.75,0.95)) / 30) - cdd_clim) / cdd_clim
	} else {
		dd_df[iter,c('Q5','Q25','Q50','Q75','Q95')] = rep(0,5)
	}
	dd_df[iter,'Climatology'] = 0

		#CDD with kfactor
	which_var = which_var + 1	; iter = iter + 1
	dd_df[iter,c('ID','User', 'Location', 'State_or_Province', 'Country')] = c(iter, user_name, city_name, state_name, country_name)
	dd_df[iter,c('Lat','Lon')] = c(nc_lat, nc_lon)
	dd_df[iter,'Forecast_Date'] = as.Date(nc_vTime_tmax[1,date_col] + as.Date("1993-01-02"))
	dd_df[iter,c('Unit','System_of_Unit')] = c('Farenheit','Imperial')
	dd_df[iter,'Variable'] = var_names[which_var]
	dd_df[iter,'Lead_Time'] = paste(j, 'months')
	dd_df[iter,c('Q5','Q25','Q50','Q75','Q95')] = quantile(this_cdd, c(0.05,0.25,0.5,0.75,0.95)) / (30 * kfactor)
	dd_df[iter,'Climatology'] = cdd_clim / kfactor
}

write.csv(dd_df, paste0("j:\\Cai_data\\Propane\\", city_name, "_", state_name, "_hddccdd_2JAN2022.csv"))


