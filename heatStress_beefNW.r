##########################################################
### loading needed libraries
library(ncdf4)
library(data.table)

##########################################################
# loading data
dataPath = 'C:\\Users\\18033\\Documents\\CaiData\\temp_locationsForRabo\\Locations\\BeefNW\\' #'J:\\Cai_data\\Rabo\\Locations\\BeefNW\\'
cmipDataName = 'pp_future_daily.nc'
startDateCmip = '2015-01-01'
nc_allDat = nc_open(paste0(dataPath, cmipDataName))

nc_lat = ncvar_get(nc_allDat, 'lat')
nc_lon = ncvar_get(nc_allDat, 'lon')
nc_date = ncvar_get(nc_allDat, 'time') + as.Date(startDateCmip)
nc_year = lubridate::year(nc_date)
nc_location = ncvar_get(nc_allDat, 'location')
nc_model = ncvar_get(nc_allDat, 'model')
	#[time,model,scenario,location] 
nc_rh = ncvar_get(nc_allDat, 'rh')
nc_ssrd = ncvar_get(nc_allDat, 'ssrd')
nc_tmax = ncvar_get(nc_allDat, 't2m_max')
nc_tmin = ncvar_get(nc_allDat, 't2m_min')
nc_ws = ncvar_get(nc_allDat, 'ws')
nc_wsmax = ncvar_get(nc_allDat, 'ws_max')


hliThresholds = seq(86,96,2)

countArray = array(NA, dim = c(length(unique(nc_year)), length(hliThresholds), length(nc_location), length(nc_model)))


for(thisLoc in 1:length(nc_location))	{
	for(thisModel in 1:length(thisModel))	{
		

	# calculating balck globe temperture
	# wet bulb temp (Twb) from https://journals.physiology.org/doi/full/10.1152/japplphysiol.00738.2021
		# rh in %
Twb = T2m * arctan (0.151977 * (RH_pct + 8.313659)^(1/2)) + 
		arctan (T2m + RH_pct) - arctan (RH_pct - 1.676331) + 
		0.00391838 * (RH_pct^(3/2)) * arctan (0.023101 * RH_pct) -
		4.686035

	# black globe temp (Tbg) from https://www.weather.gov/media/tsa/pdf/WBGTpaper2.pdf
Tbg = 0.7 * T2m + 0.3 * Twb 

	# HLI from https://pubmed.ncbi.nlm.nih.gov/17911236/
		# windspeed in m/s; temps in C; rh in decimal
		# HLI(BG>25) = 8.62 + (0.38 x RH) + (1.55 x BG) - (0.5 x WS) + e((2.4-WS))
	HLI_high = 8.62 + (0.38 * RH_dec) + (1.55 * Tbg) - (0.5 * winspeed) + exp(2.4 - winspeed)
		# HLI(BG<25) = 10.66 + (0.28 x RH) + (1.3 x BG) - WS
	HLI_low = 10.66 + (0.28 * RH_dec) + (1.3 * Tbg) - windspeed

#A threshold HLI above which cattle of different genotypes gain body heat was developed for 7 genotypes.
#The threshold for unshaded black B. taurus steers was 86, and for 
#unshaded B. indicus (100%) the threshold was 96.
#Threshold adjustments were developed for factors such as 
#	coat color, health status, access to shade, drinking water temperature, and manure management.
#Upward and downward adjustments are possible
#	upward adjustments occur when cattle have access to shade (+3 to +7) and 
#	downward adjustments occur when cattle are showing clinical signs of disease (-5). 






## old: historic heat stress analysis

####################################################################################################
##	define all names, file locations, and variables
	# names and variables
dataOrigination = c('ERA5', 'CFS', 'SEAS5')
dataPath = 'J:\\Cai_data\\Rabo\\Locations\\BeefNW\\'
era5RecentDataName = 'BeefNW-testing-recent-era.nc'
startDateEra5 = '1980-01-01'
nc_allDat = nc_open(paste0(dataPath, era5RecentDataName))

nc_lat = ncvar_get(nc_allDat, 'latitude')
nc_lon = ncvar_get(nc_allDat, 'longitude')
nc_date = ncvar_get(nc_allDat, 'time') + as.Date(startDateEra5)
nc_year = lubridate::year(nc_date)
nc_maxTemp = ncvar_get(nc_allDat, 't2m_max') * 9/5 + 32
nc_humidity = ncvar_get(nc_allDat, 'rh_mean')



# reading in customer data
userName = 'Rabo'	
customerFolder = 'J:\\Cai_data\\Rabo\\Locations\\'
clientName = 'BeefNW'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, clientName, '\\', 'Customer Onboarding Information_BNW.csv'), 
	skip = 1) #'Customer_Hazards_and_Locations-Rabobank_grid - Sheet1.csv'


# defining thresholds
thiThresholds = c(68, 72, 80, 90, 100)
mildStress = 68
mildToModStress = 72
modToSevStress = 80
sevStress = 90
overSevStress = 100

######!!!!!!!!!!!!!!!! there are some small adjustments that need to be made if actaully used
heatIndex_f = function(T, RH)	{ # from https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
	heatIndex = weathermetrics::heat.index(t = T, rh = RH)
#	heatIndex =  -42.379 + 2.04901523*T + 10.14333127*RH - .22475541*T*RH - .00683783*T*T - .05481717*RH*RH + .00122874*T*T*RH + .00085282*T*RH*RH - .00000199*T*T*RH*RH
#((13-RH)/4)*sqrt((17-abs(T-95))/17)
	return(heatIndex)
}
######!!!!!!!!!!!!!!!! there are some small adjustments that need to be made if actaully used

tempTable = data.frame(Date = nc_date)
humidTable = data.frame(Date = nc_date) 
heatIndexTable = data.frame(Date = nc_date)
countArray = array(NA, dim = c(length(unique(nc_year)), length(thiThresholds), nrow(customerTable)))

for(thisLoc in 1:nrow(customerTable))	{
	closestLat = which.min(abs(nc_lat - customerTable$Lat[thisLoc]))
	closestLon = which.min(abs(nc_lon - customerTable$Lon[thisLoc]))

	thisMaxTemp = zoo::na.spline(nc_maxTemp[closestLon, closestLat, ], na.rm=FALSE)
	thisHumidity = zoo::na.spline(nc_humidity[closestLon, closestLat, ], na.rm=FALSE)	; thisHumidity[thisHumidity > 100] = 100	; thisHumidity[thisHumidity < 0] = 0
	thisHeatIndex = heatIndex_f(thisMaxTemp, thisHumidity)
	
	tempTable = cbind(tempTable, thisMaxTemp)
	humidTable = cbind(humidTable, thisHumidity)
	heatIndexTable = cbind(heatIndexTable, thisHeatIndex)

	yearIter = 0
	for(thisYear in unique(nc_year))	{
	yearIter = yearIter + 1
		
	thresholdIter = 0
		for(thisThreshold in thiThresholds)	{
			thresholdIter = thresholdIter + 1
			
			countArray[yearIter, thresholdIter, thisLoc] = length(which(thisHeatIndex[nc_year == thisYear] > thisThreshold))
		}
	}
}
	

	# heat index
uniqYears = unique(nc_year)
for(thisLoc in 1:nrow(customerTable))	{
	png(paste0(customerFolder, clientName, '\\',  customerTable[thisLoc, ..locationHeader], '_historicalHeatIndex.png'), width=1200, height=600)
	par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
	windowsFonts(A = windowsFont("Roboto"))

#	currentAverage = mean(climDataPlot[thisLoc, 1:2, 17, 1:3, thisClimVar])
#	ylabPctVals = c(seq(-5,5,0.1))
#	ylabPctValLocs = currentAverage + currentAverage * ylabPctVals
		
	yMin = 0
	yMax = 150 #max(heatIndexTable[ , -1], na.rm=TRUE) * 1.025
	
	nDaysMin = 0
	nDaysMax = max(countArray, na.rm=TRUE) * 1.025
	
	countArrayScaled = countArray * yMax / nDaysMax
	
	plot(nc_date, heatIndexTable[ , thisLoc + 1],
		ylim = c(yMin,yMax) ,
		type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
		main='', ylab='Daily Max Heat Index (F)', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	abline(h=mean(heatIndexTable[ , thisLoc + 1][nc_year < 2000], na.rm=TRUE), lwd=2, lty =2, col='#1A232F')
	abline(h=seq(yMin, yMax, 50), col='grey90', lwd=1.1)

	lines(nc_date, heatIndexTable[ , thisLoc + 1], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
	kmeansSmooth = ksmooth(nc_date, heatIndexTable[ , thisLoc + 1] , bandwidth = 365, kernel = 'box')
	lines(nc_date[nc_year < last(nc_year)], kmeansSmooth$y[nc_year < last(nc_year)],
		col='#0098B2', lwd=3)

	loessSmooth = loess(countArrayScaled[-length(uniqYears) , 1, thisLoc] ~ seq(1,length(uniqYears)-1,1))
	lines(ymd(uniqYears[-length(uniqYears)] + 1, truncated=2L),   predict(loessSmooth),
		col=adjustcolor('#EE6222', alpha=0.4), lwd=2)
	loessSmooth = loess(countArrayScaled[-length(uniqYears) , 2, thisLoc] ~ seq(1,length(uniqYears)-1,1))
	lines(ymd(uniqYears[-length(uniqYears)] + 1, truncated=2L),   predict(loessSmooth),
		col=adjustcolor('#EE6222', alpha=0.55), lwd=2)
	loessSmooth = loess(countArrayScaled[-length(uniqYears) , 3, thisLoc] ~ seq(1,length(uniqYears)-1,1))
	lines(ymd(uniqYears[-length(uniqYears)] + 1, truncated=2L),   predict(loessSmooth),
		col=adjustcolor('#EE6222', alpha=0.7), lwd=2)
	loessSmooth = loess(countArrayScaled[-length(uniqYears) , 4, thisLoc] ~ seq(1,length(uniqYears)-1,1))
	lines(ymd(uniqYears[-length(uniqYears)] + 1, truncated=2L),   predict(loessSmooth),
		col=adjustcolor('#EE6222', alpha=0.85), lwd=2)
	loessSmooth = loess(countArrayScaled[-length(uniqYears) , 5, thisLoc] ~ seq(1,length(uniqYears)-1,1))
	lines(ymd(uniqYears[-length(uniqYears)] + 1, truncated=2L),   predict(loessSmooth),
		col=adjustcolor('#EE6222', alpha=1.0), lwd=2)

	newYlab = seq(0,yMax,by=50)
	axis(4, at=newYlab, col='#EE6222', lwd=2, col.lab='#EE6222', col.axis='#EE6222',
		labels = round(seq(0,nDaysMax,length.out=length(newYlab)),0))
	mtext('Number of Days', side=4, line=3, col='#EE6222', cex=1.5*1.8)
	text('Number of Days', side=4, line=3, col='#EE6222', cex=1.5*1.8)
	dev.off()
}

	#rh


uniqYears = unique(nc_year)
for(thisLoc in 1:nrow(customerTable))	{
	png(paste0(customerFolder, clientName, '\\',  customerTable[thisLoc, ..locationHeader], '_historicalHumidity.png'), width=1200, height=600)
	par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
	windowsFonts(A = windowsFont("Roboto"))

#	currentAverage = mean(climDataPlot[thisLoc, 1:2, 17, 1:3, thisClimVar])
#	ylabPctVals = c(seq(-5,5,0.1))
#	ylabPctValLocs = currentAverage + currentAverage * ylabPctVals
		
	yMin = 0
	yMax = 100 * 1.025
	
	nDaysMin = 0
	nDaysMax = max(countArray, na.rm=TRUE) * 1.025
	
	plot(nc_date, humidTable[ , thisLoc + 1],
		ylim = c(yMin,yMax) ,
		type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
		main='', ylab='Humidity (%)', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	abline(h=mean(humidTable[ , thisLoc + 1][nc_year < 2000], na.rm=TRUE), lwd=2, lty =2, col='#1A232F')

	lines(nc_date, humidTable[ , thisLoc + 1], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
	kmeansSmooth = ksmooth(nc_date, humidTable[ , thisLoc + 1] , bandwidth = 365, kernel = 'box')
	lines(nc_date[nc_year < last(nc_year)], kmeansSmooth$y[nc_year < last(nc_year)],
		col='#0098B2', lwd=3)

	dev.off()
}



uniqYears = unique(nc_year)
for(thisLoc in 1:nrow(customerTable))	{
	png(paste0(customerFolder, clientName, '\\',  customerTable[thisLoc, ..locationHeader], '_historicalTemperature.png'), width=1200, height=600)
	par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
	windowsFonts(A = windowsFont("Roboto"))

#	currentAverage = mean(climDataPlot[thisLoc, 1:2, 17, 1:3, thisClimVar])
#	ylabPctVals = c(seq(-5,5,0.1))
#	ylabPctValLocs = currentAverage + currentAverage * ylabPctVals
		
	yMin = 0
	yMax = max(tempTable[ , thisLoc + 1]) * 1.025
	
	nDaysMin = 0
	nDaysMax = max(countArray, na.rm=TRUE) * 1.025
	
	plot(nc_date, tempTable[ , thisLoc + 1],
		ylim = c(yMin,yMax) ,
		type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
		main='', ylab='Daily Max Temperature (F)', xlab='',
		col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
		family='A')
	abline(h=mean(tempTable[ , thisLoc + 1][nc_year < 2000], na.rm=TRUE), lwd=2, lty =2, col='#1A232F')

	lines(nc_date, tempTable[ , thisLoc + 1], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
	kmeansSmooth = ksmooth(nc_date, tempTable[ , thisLoc + 1] , bandwidth = 365, kernel = 'box')
	lines(nc_date[nc_year < last(nc_year)], kmeansSmooth$y[nc_year < last(nc_year)],
		col='#0098B2', lwd=3)

	dev.off()
}