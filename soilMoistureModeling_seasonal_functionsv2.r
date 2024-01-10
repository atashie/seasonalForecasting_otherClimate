
	# rainfall infiltration function
infiltration_f = function(PPT = NA, crn = 50, Smax = 100, Ia_scalar = 0.2){
#	if(PPT == NA) { print('ppt is null') ; stop()}
	Ia = Ia_scalar * Smax
	infil = PPT - ifelse(PPT > Ia, (PPT - Ia)^2 / (PPT - Ia + Smax), 0)
	return(infil)
	}
	
#PET_f = function(	)	{
#	
#	}

	
	# soil moisture 
SM_routine_f = function(infiltration = NA, PET = NA, 
	rcScl = 0.1,	# water retention curve scalar
	rcExp = 1.3,	# water retention curve exponent
	PETexp = 2, 	# exponent of PET decay
	Zr = 1000,	# root depth
	n = 0.5,	# soil porosity
	smhp = 0.00,	# soil moisture at hydroscopic point
	smwp = 0.10,	# soil moisture at wilting point
	smfc = 0.25,	# soil moisture field capacity
	s0 = .5)	# initial soil moisture 
	{
	nt = length(infiltration)
	SM_store = c(s0, rep(NA, nt))
	smhp_stor = Zr * smhp
	smwp_stor = Zr * smwp
	smfc_stor = Zr * smfc
	max_stor = Zr * n
	
	for(i in 1:length(infiltration))	{
		thisSMstor = SM_store[i]
		AET = ifelse(thisSMstor > smhp_stor, 
			min(thisSMstor - smhp_stor, PET[i] * (thisSMstor / max_stor) ^ PETexp),
			0)
		thisSMstor = thisSMstor - AET
		
		deepDrainage = ifelse(thisSMstor > smfc_stor,
			min(thisSMstor - smfc_stor, rcScl * thisSMstor * (thisSMstor / smfc_stor)^rcExp),
			0)
		
		SM_store[i+1] = min(max_stor, thisSMstor - min(c(thisSMstor, deepDrainage)) + infiltration[i]) 
		}
	return(SM_store / n)
	}
	
	
#SM_routine_f(infiltration = infil, PET = PET)
	

#rain = 90 * RainPoisson(ndays = 365, lambda = 0.05, alpha = 0.60)








#sm_out_1 = swb_f(R = rain,
#	Rstar = 3,	# max amount that the cannopy intercepts
#	Emax = seq(4,5,length.out=365),	# max ET rate
#	Ew = 0.5,	# min ET rate
##	Ks = 2000,	# Ksat
#	b = 4.38,	# exponent of water retention curve
#	Zr = 400,	# root depth
#	n = 0.5,	# soil porosity
#	sh = 0.01,	# soil moisture at hydroscopic point
#	sw = 0.10,	# soil moisture at wilting point
#	sstar = 0.25,	# soil moisture field capacity
#	s0 = 0.10,	# initial soil moisture (else S0 = sh)
#	nsteps = 1,	# number of steps/division for the numerical colution #???
#	gr = TRUE)	# logical to show graphics of results











############################################################################################################################
# incorporating projections data

f_projectedSoilMoisture = function(
	dataPath = dataPath,
	cfsDataName = cfsDataName,
	seas5DataName = seas5DataName,
	startDateCfs = startDateCfs,
	startDateSeas5 = startDateSeas5,
	userName = userName,
#	forecastDate = forecastDate,
	seas5Models = seas5Models,
	cfsModels = cfsModels,
	rootDepth = rootDepth,
	saveDate = saveDate)	{

	library(lubridate)
	library(data.table)
	library(ncdf4)
	library(EcoHydRology)
	save_iter = 0

		# read in ncdf data
	ncin_cfs = nc_open(paste0(dataPath, cfsDataName))			# [longitude,latitude,lead_time,member]   (Contiguous storage) 
	ncin_seas5 = nc_open(paste0(dataPath, seas5DataName))		# [longitude,latitude,lead_time,member] 
	
	nc_dateCfs = ncvar_get(ncin_cfs, 'lead_time') +  as.Date(startDateCfs)
	nc_dateSeas5 = ncvar_get(ncin_seas5, 'lead_time') + as.Date(startDateSeas5)
	 
	nc_doyCfs = yday(nc_dateCfs)
	nc_doySeas5 = yday(nc_dateSeas5)
	
	nc_yearCfs = year(nc_dateCfs)
	nc_yearSeas5 = year(nc_dateSeas5)
	
	nc_latCfs = ncvar_get(ncin_cfs, 'latitude')
	nc_latSeas5 = ncvar_get(ncin_seas5, 'latitude')
	
	nc_lonCfs = ncvar_get(ncin_cfs, 'longitude')
	nc_lonSeas5 = ncvar_get(ncin_seas5, 'longitude')

	
	# reading in the soil moisture data for calibration
	# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	# note that the updated historical soil moisture database must be downloaded prior to running model

	ncin_sm23 = nc_open(paste0(dataPath, 'sm_pct_2023.nc'))
	nc_lat_sm = ncvar_get(ncin_sm23, 'latitude')
	nc_lon_sm = ncvar_get(ncin_sm23, 'longitude')
	theseSMlons = which(nc_lon_sm %in% unique(nc_lonCfs))
	theseSMlats = which(nc_lat_sm %in% unique(nc_latCfs))
	theseSMlonVals = nc_lon_sm[theseSMlons]
	theseSMlatVals = nc_lat_sm[theseSMlats]
	
	nc_sm23 = ncvar_get(ncin_sm23, 'sm_pct')[theseSMlons, theseSMlats, ]
	
	nc_sm17 = ncvar_get(nc_open(paste0(dataPath, 'sm_pct_2017.nc')), 'sm_pct')[theseSMlons, theseSMlats, ]
	nc_sm18 = ncvar_get(nc_open(paste0(dataPath, 'sm_pct_2018.nc')), 'sm_pct')[theseSMlons, theseSMlats, ]
	nc_sm19 = ncvar_get(nc_open(paste0(dataPath, 'sm_pct_2019.nc')), 'sm_pct')[theseSMlons, theseSMlats, ]
	nc_sm20 = ncvar_get(nc_open(paste0(dataPath, 'sm_pct_2020.nc')), 'sm_pct')[theseSMlons, theseSMlats, ]
	nc_sm21 = ncvar_get(nc_open(paste0(dataPath, 'sm_pct_2021.nc')), 'sm_pct')[theseSMlons, theseSMlats, ]
	nc_sm22 = ncvar_get(nc_open(paste0(dataPath, 'sm_pct_2022.nc')), 'sm_pct')[theseSMlons, theseSMlats, ]


	nc_date_sm = as.Date("1900-01-01") + ncvar_get(ncin_sm23, 'time') # time is days after jan 1 1900
	allSMdates = as.Date("1970-01-01") + (as.Date("2017-01-01") : last(nc_date_sm))
	allSMdoys = yday(allSMdates)


		# prioritizing higher to lower quality data
	whichCfsDays = which(as.character(nc_dateCfs) ==  as.character(last(nc_date_sm) + 1)):length(nc_dateCfs)
	whichSeas5Days = which(as.character(nc_dateSeas5) == as.character(last(nc_dateCfs) + 1)):length(nc_dateSeas5)
	theseDates = c(last(nc_date_sm), nc_dateCfs[whichCfsDays], nc_dateSeas5[whichSeas5Days])
	theseDoys = yday(theseDates)

	nc_tminCfs = ncvar_get(ncin_cfs, 't2m_min')[ , , whichCfsDays, ]	# [longitude,latitude,lead_time,member] 
	nc_tminSeas5 = ncvar_get(ncin_seas5, 't2m_min')[ , , whichSeas5Days, ] 

	nc_tmaxCfs = ncvar_get(ncin_cfs, 't2m_max')[ , , whichCfsDays, ]
	nc_tmaxSeas5 = ncvar_get(ncin_seas5, 't2m_max')[ , , whichSeas5Days, ] 

	nc_pptCfs = ncvar_get(ncin_cfs, 'tp')[ , , whichCfsDays, ]	# 
	nc_pptSeas5 = ncvar_get(ncin_seas5, 'tp')[, , whichSeas5Days, ] # [longitude,latitude,lead_time,member] 

	calibDF = fread("J:\\Cai_data\\Advanta\\SoilMoisture_redux_newsm2_20JUL2022.csv")

	summaryOutput_df = data.frame(Lat = 0, Lon = 0, User = 'string', rootDepth = 0, monthsOut = 0, startDate = as.Date('2000-01-01'),
		projectedQ05 = 0, projectedQ25 = 0, projectedQ50 = 0, projectedQ75 = 0, projectedQ95 = 0,
		climatologyQ05 = 0, climatologyQ25 = 0, climatologyQ50 = 0, climatologyQ75 = 0, climatologyQ95 = 0)

	iter = 0
	for(i in 1:length(nc_latSeas5))	{
			#identifying nearest lat in the SM ncdf
		closeSMLat = which.min(abs(nc_latSeas5[i] - theseSMlatVals))
		
		for(j in 1:length(nc_lonSeas5))	{
				#identify nearest lon in the SM ncdf
			closeSMLon = which.min(abs(nc_lonSeas5[j] - theseSMlonVals))
			recentSM = nc_sm23[closeSMLon, closeSMLat, ]

			if(any(!is.na(recentSM)))	{

				allSM = c(nc_sm17[closeSMLon, closeSMLat, ], nc_sm18[closeSMLon, closeSMLat, ], nc_sm19[closeSMLon, closeSMLat, ],
					nc_sm20[closeSMLon, closeSMLat, ], nc_sm21[closeSMLon, closeSMLat, ], nc_sm22[closeSMLon, closeSMLat, ])
	
				#identify the relevant row for the calibration database
				closeCalRow = subset(calibDF, Lat == nc_latSeas5[i] & Lon == nc_lonSeas5[j])[1,]
				if(!is.na(closeCalRow[1,1]))	{
					if(closeCalRow$KGE[1] > 0.4)	{
						initialSM = last(recentSM) * rootDepth * closeCalRow$n
							
		#				incPrevSM = (which(nc_date_sm == nc_dateCfs[1]) + 1):length(nc_date_sm)
		#				previousSM = recentSM[incPrevSM]
					
						smOutAll = matrix(nrow = (length(theseDates) + 1), ncol = (length(cfsModels) * length(seas5Models)))
						modelIter = 0
						for(kk in cfsModels)	{
							for(ll in seas5Models)	{
								# soil moisture routine
								this_ppt  = c(nc_pptCfs[j, i, , kk],  nc_pptSeas5[j, i, , ll])	; this_ppt[is.na(this_ppt)] = 0 ; this_ppt[this_ppt < 0] = 0 # [longitude,latitude,lead_time,member] 
								this_tmin = c(nc_tminCfs[j, i, , kk], nc_tminSeas5[j, i, , ll])	; this_tmin[is.na(this_tmin)] = mean(this_tmin)			 # [longitude,latitude,lead_time,member] 
								this_tmax = c(nc_tmaxCfs[j, i, , kk], nc_tmaxSeas5[j, i, , ll])	; this_tmax[is.na(this_tmax)] = mean(this_tmax)	 		 # [longitude,latitude,lead_time,member] 
									
								# check to see if tmin is ever greater than tmax, and correct before 
								badTmins = which(this_tmin >= this_tmax)
								if(length(badTmins) > 0)	{
									this_tmin[badTmins] = this_tmax[badTmins] - 0.001
								}
								
								PET = PET_fromTemp(theseDoys[-1],this_tmax, this_tmin,
											lat_radians =  min((nc_latSeas5[i]*pi/180), 1.1)) * 1000 # output in m, converting to mm

								infil = infiltration_f(PPT = this_ppt,
									crn = closeCalRow$crn,#50,
									Smax = rootDepth * closeCalRow$n,
									Ia_scalar = closeCalRow$Ia_scalar)#0.2)


								sm_out = SM_routine_f(infiltration = infil, PET = PET,
										rcScl = closeCalRow$rcScl,#0.1,	# water retention curve scalar
										rcExp = closeCalRow$rcExp,#1.3,	# water retention curve exponent
										PETexp = closeCalRow$PETexp, 	# max PET fraction per day
										Zr = rootDepth,	# root depth
										n = closeCalRow$n, #0.5,	# soil porosity
										smhp = closeCalRow$smhp, #0.00,	# soil moisture at hydroscopic point
										smwp = closeCalRow$smhp, #0.10,	# soil moisture at wilting point
										smfc = closeCalRow$smfc, #0.25,	# soil moisture field capacity
										s0 = initialSM)	# initial soil moisture 
									
								modelIter = modelIter + 1
								smOutAll[,modelIter] = c(initialSM, sm_out)  / rootDepth
							}
						}
						print(c(i,j))
						for(mm in 1:floor(nrow(smOutAll) / 30))	{
							thisMonthsOut = mm - 1
							thisTimeFrame = 1:30 + (thisMonthsOut * 30)
							theseSubDates = theseDates[thisTimeFrame]
							
							theseDOY = yday(theseSubDates)
							theseSMhist = allSM[allSMdoys %in% theseDOY]
								
							iter = iter + 1
							projQuantiles = quantile(smOutAll[thisTimeFrame, ], c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm=T) * 100
							climQuantiles = quantile(theseSMhist, c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm=T) * 100
							summaryOutput_df[iter, ] = data.frame(nc_latSeas5[i], nc_lonSeas5[j], userName,
								rootDepth, thisMonthsOut, theseDates[1],
								projQuantiles[1], projQuantiles[2], projQuantiles[3], projQuantiles[4], projQuantiles[5],
								climQuantiles[1], climQuantiles[2], climQuantiles[3], climQuantiles[4], climQuantiles[5])
									
								
						}
					}
				}
			}
		}
	}
	fwrite(summaryOutput_df, paste0(dataPath, "SoilMoisture_projectionOutput_", saveDate, ".csv"))
}
