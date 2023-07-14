##########################################################
### loading needed libraries
library(ncdf4)
library(data.table)
library(lubridate)


# reading in customer data
userName = 'Rabo'	
customerFolder = 'J:\\Cai_data\\Rabo\\Locations\\'
clientName = 'BeefNW'
thisDate = Sys.Date()

customerTable = fread(paste0(customerFolder, clientName, '\\', 'Customer Onboarding Information_BNW_parsedForTitan.csv'),#'Customer Onboarding Information_BNW.csv'), 
	skip = 0) #'Customer_Hazards_and_Locations-Rabobank_grid - Sheet1.csv'


##########################################################
# loading data
dataPath = 'J:\\Cai_data\\Rabo\\Locations\\BeefNW\\' # 'C:\\Users\\18033\\Documents\\CaiData\\temp_locationsForRabo\\Locations\\BeefNW\\' #
cmipDataName = 'pp_future_daily.nc'
startDateCmip = '2015-01-01'
nc_allDat = nc_open(paste0(dataPath, cmipDataName))

nc_lat = ncvar_get(nc_allDat, 'lat')
nc_lon = ncvar_get(nc_allDat, 'lon')
nc_date = ncvar_get(nc_allDat, 'time') + as.Date(startDateCmip)
nc_year = unique(lubridate::year(nc_date))
nc_location = ncvar_get(nc_allDat, 'location')
nc_model = ncvar_get(nc_allDat, 'model')
nc_scen = ncvar_get(nc_allDat, 'scenario')
	#[time,model,scenario,location] 
nc_rh = ncvar_get(nc_allDat, 'rh')
nc_ssrd = ncvar_get(nc_allDat, 'ssrd')
nc_tmax = ncvar_get(nc_allDat, 't2m_max')
nc_tmin = ncvar_get(nc_allDat, 't2m_min')
nc_ws = ncvar_get(nc_allDat, 'ws')
nc_wsmax = ncvar_get(nc_allDat, 'ws_max')


hliThresholds = c(81, 83, 86, 89, 91, 94)# 
hliThresholdsDescription = c('sickOrNotAcclimitizedOrDeepManure', 'feedDays130', 'black', 'whiteOrLightShadeOrFeedDays80', 'strongShade', 'totalShadeAndCoolWater')

countArray = array(NA, dim = c(length(unique(nc_year)), length(hliThresholds), length(nc_model), length(nc_scen), length(nc_location)))
hliArray = array(NA, dim = c(length(c("HLI", "Tmax","RH", "SolRad", "Tbg","Twb","Windspeed")),length(nc_date), length(nc_model), length(nc_scen), length(nc_location)))

for(thisLoc in 1:length(nc_location))	{
	for(thisScen in 1:length(nc_scen))	{
		for(thisModel in 1:length(nc_model))	{
#			T2m = (nc_tmin[ , thisModel, thisScen, thisLoc] + nc_tmax[ , thisModel, thisScen, thisLoc]) / 2
			T2m = nc_tmax[ , thisModel, thisScen, thisLoc]
			RH_pct = nc_rh[ , thisModel, thisScen, thisLoc]
			RH_dec = RH_pct / 100
			windspeed = nc_ws[ , thisModel, thisScen, thisLoc] * 0.2777777777777778 # km / h to m / s
			solRad = nc_ssrd[ , thisModel, thisScen, thisLoc]

				# calculating balck globe temperture
				# wet bulb temp (Twb) from https://journals.physiology.org/doi/full/10.1152/japplphysiol.00738.2021
					# rh in %
			Twb = T2m * atan (0.151977 * (RH_pct + 8.313659)^(1/2)) + 
					atan (T2m + RH_pct) - atan (RH_pct - 1.676331) + 
					0.00391838 * (RH_pct^(3/2)) * atan (0.023101 * RH_pct) -
					4.686035

				# black globe temp (Tbg) from https://www.weather.gov/media/tsa/pdf/WBGTpaper2.pdf
			#	# BGT = 0.7NWB + 0.2GT + 0.1DB
			#Tbg = ((0.7 * (T2m * 9 / 5 + 32) + 0.3 * Twb) - 32) * (5/9)
			#bg = 0.7 * T2m + 0.3 * Twb 
				# alternative black globe temp (Tbg) https://www.engineeringtoolbox.com/resultant-globe-temperature-d_1806.html
			# Tbg = 0.5 tmr + 0.5 ta    , ta ia air empt and tmr is mean radiation temperature
				# another alternative from https://rmets.onlinelibrary.wiley.com/doi/pdf/10.1002/met.1631
			# Tbg = 0.01498SR + 1.184Ta – 0.0789RH – 2.739
			Tbg = 0.01498 * solRad + 1.184 * T2m - 0.0789 * RH_pct - 2.739
				
			# HLI from https://pubmed.ncbi.nlm.nih.gov/17911236/
				# windspeed in m/s; temps in C; rh in decimal
				# HLI(BG>25) = 8.62 + (0.38 x RH) + (1.55 x BG) - (0.5 x WS) + e((2.4-WS))
			HLI_high = 8.62 + (0.38 * RH_dec) + (1.55 * Tbg) - (0.5 * windspeed) + exp(2.4 - windspeed)
				# HLI(BG<25) = 10.66 + (0.28 x RH) + (1.3 x BG) - WS
			HLI_low = 10.66 + (0.28 * RH_dec) + (1.3 * Tbg) - windspeed
			
			HLI_threshold = 25
			HLIlowDays = which(Tbg < HLI_threshold)
			HLI_all = HLI_high
			HLI_all[HLIlowDays] = HLI_low[HLIlowDays]
	
			hliArray[1, , thisModel, thisScen, thisLoc] = HLI_all
			hliArray[2, , thisModel, thisScen, thisLoc] = T2m
			hliArray[3, , thisModel, thisScen, thisLoc] = RH_pct
			hliArray[4, , thisModel, thisScen, thisLoc] = solRad
			hliArray[5, , thisModel, thisScen, thisLoc] = Tbg
			hliArray[6, , thisModel, thisScen, thisLoc] = Twb
			hliArray[7, , thisModel, thisScen, thisLoc] = windspeed

			for(thisThresh in 1:length(hliThresholds))	{
				theseCounts = ifelse(all(is.na(HLI_all[which(year(nc_date) == nc_year[1])] > hliThresholds[thisThresh])), NA,
					length(which(HLI_all[which(nc_year == nc_year[1])] > hliThresholds[thisThresh])))
				for(thisYear in 2:length(nc_year))	{
					theseHLI = HLI_all[which(year(nc_date) == nc_year[thisYear])]
					theseCounts = c(theseCounts, 
						ifelse(all(is.na(HLI_all)), NA, length(which(theseHLI > hliThresholds[thisThresh]))))
				}
					
				countArray[ , thisThresh, thisModel, thisScen, thisLoc] = theseCounts
			}

			#A threshold HLI above which cattle of different genotypes gain body heat was developed for 7 genotypes.
			#The threshold for unshaded black B. taurus steers was 86, and for 
			#unshaded B. indicus (100%) the threshold was 96.
			#Threshold adjustments were developed for factors such as 
			#	coat color, health status, access to shade, drinking water temperature, and manure management.
			#Upward and downward adjustments are possible
			#	upward adjustments occur when cattle have access to shade (+3 to +7) and 
			#	downward adjustments occur when cattle are showing clinical signs of disease (-5). 
		}
	}
}




	# heat index
allYears = year(nc_date)
allDecades = trunc(nc_year / 10) * 10
historicYears = which(nc_year < 2023)
historicDates = which(allYears < 2023)

for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(nc_scen))	{
		png(paste0(customerFolder, clientName, '\\',  customerTable$Location[thisLoc], '_', nc_scen[thisScen], '_projectedHeatIndex.png'), width=1300, height=700)
		par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))

	#countArray [nc_year, hliThresholds, nc_model, thisScen, nc_location]
	#hliArray   [variable, nc_date, nc_model, thisScen, nc_location]
		
		
		yMin = -40
		yMax = 120 #max(hliArray, na.rm=TRUE) * 1.025
		
		nDaysMin = 0
		nDaysMax = max(apply(countArray[ ,1, , thisScen, thisLoc], 1, mean, na.rm=TRUE)) * 2.025
		
		countArrayScaled = (countArray * (yMax - yMin) / nDaysMax) + yMin
		
		plot(nc_date, apply(hliArray[1, , , thisScen, thisLoc], 1, mean, na.rm=TRUE),
			ylim = c(yMin,yMax),yaxt = 'n',
			type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
			main='', ylab='Daily Heat Load Index', xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
		newYlab = seq(yMin,yMax,by=20)
		axis(2, at=newYlab, col='#666D74', lwd=2, col.lab='#666D74', col.axis='#666D74')
			
		abline(h=mean(hliArray[1, historicDates, , thisScen, thisLoc], na.rm=TRUE), lwd=2, lty =2, col='#1A232F')
		abline(h=seq(yMin, yMax, 20), col='grey90', lwd=1.1)

	#	lines(nc_date[historicDates], apply(hliArray[1, historicDates, , thisScen, thisLoc], 1, mean, na.rm=TRUE), lwd=1, col=adjustcolor('#666D74', alpha=0.9))
		for(thisModel in 1:length(nc_model))	{
			lines(nc_date, hliArray[1, , thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
			#lines(nc_date[-historicDates], hliArray[1, -historicDates, thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
		}
		kmeansSmooth = ksmooth(nc_date,  apply(hliArray[1, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), bandwidth = 365, kernel = 'box')
		smthDaysRmv = c(1:365, (length(nc_date) - 365):length(nc_date))
		lines(nc_date[-smthDaysRmv], kmeansSmooth$y[-smthDaysRmv], col='#0098B2', lwd=5)

		loessSmooth = loess(apply(countArrayScaled[ , 1, , thisScen, thisLoc], 1, mean, na.rm=TRUE) ~ seq(1,length(nc_year),1))
		lines(ymd(nc_year, truncated = 2L),   predict(loessSmooth),
			col=adjustcolor('#EE6222', alpha=0.5), lwd=3)
		loessSmooth = loess(apply(countArrayScaled[ , 2, , thisScen, thisLoc], 1, mean, na.rm=TRUE) ~ seq(1,length(nc_year),1))
		lines(ymd(nc_year, truncated = 2L),   predict(loessSmooth),
			col=adjustcolor('#EE6222', alpha=0.6), lwd=3)
		loessSmooth = loess(apply(countArrayScaled[ , 3, , thisScen, thisLoc], 1, mean, na.rm=TRUE) ~ seq(1,length(nc_year),1))
		lines(ymd(nc_year, truncated = 2L),   predict(loessSmooth),
			col=adjustcolor('#EE6222', alpha=0.7), lwd=3)
		loessSmooth = loess(apply(countArrayScaled[ , 4, , thisScen, thisLoc], 1, mean, na.rm=TRUE) ~ seq(1,length(nc_year),1))
		lines(ymd(nc_year, truncated = 2L),   predict(loessSmooth),
			col=adjustcolor('#EE6222', alpha=0.8), lwd=3)
		loessSmooth = loess(apply(countArrayScaled[ , 5, , thisScen, thisLoc], 1, mean, na.rm=TRUE) ~ seq(1,length(nc_year),1))
		lines(ymd(nc_year, truncated = 2L),   predict(loessSmooth),
			col=adjustcolor('#EE6222', alpha=1.0), lwd=3)
		loessSmooth = loess(apply(countArrayScaled[ , 6, , thisScen, thisLoc], 1, mean, na.rm=TRUE) ~ seq(1,length(nc_year),1))
		lines(ymd(nc_year, truncated = 2L),   predict(loessSmooth),
			col=adjustcolor('#EE6222', alpha=1.0), lwd=3)

		axis(4, at=newYlab, col='#EE6222', lwd=2, col.lab='#EE6222', col.axis='#EE6222',
			labels = round(seq(nDaysMin,nDaysMax,length.out=length(newYlab)),0))
		mtext('Number of Days', line=3, side=4, col='#EE6222', cex=1.5*1.8)
		dev.off()
		
		summaryOut = data.frame(Decade = unique(allDecades))
		for(thisThresh in 1:length(hliThresholds))	{	
			theseVals = apply(countArray[ , thisThresh, , thisScen, thisLoc], 1, mean, na.rm=TRUE)
			decAvgVals = mean(theseVals[which(allDecades == unique(allDecades)[1])])
			for(thisDec in 2:length(unique(allDecades)))	{
				decAvgVals = c(decAvgVals, mean(theseVals[which(allDecades == unique(allDecades)[thisDec])]))
			}
			summaryOut[, paste0('HLI_', hliThresholds[thisThresh])] = round(decAvgVals, 1)
		}
		fwrite(summaryOut, paste0(customerFolder, clientName, '\\',  customerTable$Location[thisLoc], '_', nc_scen[thisScen], '_projectedHeatIndex.csv'))
	}
}

	#rh
for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(nc_scen))	{
		png(paste0(customerFolder, clientName, '\\',  customerTable$Location[thisLoc], '_', nc_scen[thisScen], '_projectedHumidity.png'), width=1500, height=500)
		par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
			
		yMin = 0
		yMax = 100 * 1.025
		

		plot(nc_date, apply(hliArray[3, , , thisScen, thisLoc], 1, mean, na.rm=TRUE),
			ylim = c(yMin,yMax) ,
			type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
			main='', ylab='Humidity (%)', xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
		abline(h=mean(apply(hliArray[3, nc_date < as.Date('2023/01/01'), , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE), lwd=2, lty =2, col='#1A232F')

		for(thisModel in 1:length(nc_model))	{
			lines(nc_date, hliArray[3, , thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
			#lines(nc_date[-historicDates], hliArray[1, -historicDates, thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
		}
		smthDaysRmv = c(1:365, (length(nc_date) - 365):length(nc_date))
		kmeansSmooth = ksmooth(nc_date,  apply(hliArray[3, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), bandwidth = 365, kernel = 'box')
		lines(nc_date[-smthDaysRmv], kmeansSmooth$y[-smthDaysRmv], col='#0098B2', lwd=5)

		usr <- par("usr")   # save old user/default/system coordinates
		par(usr = c(0, 1, 0, 1)) # new relative user coordinates
		text(x=0.02, y=0.95, 
			paste0(round(lm(hliArray[3, , thisModel, thisScen, thisLoc] ~ c(1:length(nc_date)))$coef[2] * 3653, 1), ' per decade'),
			col='#0098B2', cex=1.5*1.8, pos=4)
		par(usr = usr) # restore original user coordinates
		dev.off()
	}
}


	# tmax
for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(nc_scen))	{
		png(paste0(customerFolder, clientName, '\\',  customerTable$Location[thisLoc], '_', nc_scen[thisScen], '_projectedDailyMaxTemp.png'), width=1500, height=500)
		par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
		
		impArray = hliArray * (9/5) + 32
		
		yMin = min(impArray[2, , , thisScen, thisLoc], na.rm=TRUE) #min(apply(impArray[2, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) - 2
		yMax = max(impArray[2, , , thisScen, thisLoc], na.rm=TRUE) #max(apply(impArray[2, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) + 2
		

		plot(nc_date, apply(impArray[2, , , thisScen, thisLoc], 1, mean, na.rm=TRUE),
			ylim = c(yMin,yMax) ,
			type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
			main='', ylab='Daily Max Temperature (F)', xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
		abline(h=mean(apply(impArray[2, nc_date < as.Date('2023/01/01'), , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE), lwd=2, lty =2, col='#1A232F')

		for(thisModel in 1:length(nc_model))	{
			lines(nc_date, impArray[2, , thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
			#lines(nc_date[-historicDates], impArray[1, -historicDates, thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
		}
		smthDaysRmv = c(1:365, (length(nc_date) - 365):length(nc_date))
		kmeansSmooth = ksmooth(nc_date,  apply(impArray[2, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), bandwidth = 365, kernel = 'box')
		lines(nc_date[-smthDaysRmv], kmeansSmooth$y[-smthDaysRmv], col='#0098B2', lwd=5)

		usr <- par("usr")   # save old user/default/system coordinates
		par(usr = c(0, 1, 0, 1)) # new relative user coordinates
		text(x=0.02, y=0.95, 
			paste0(round(lm(impArray[2, , thisModel, thisScen, thisLoc] ~ c(1:length(nc_date)))$coef[2] * 3653, 1), ' per decade'),
			col='#0098B2', cex=1.5*1.8, pos=4)
		par(usr = usr) # restore original user coordinates
		dev.off()
	}
}



	# solrad
for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(nc_scen))	{
		png(paste0(customerFolder, clientName, '\\',  customerTable$Location[thisLoc], '_', nc_scen[thisScen], '_projectedDailySolarRadiation.png'), width=1500, height=500)
		par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
		
#		hliArray = hliArray * (9/5) + 32
		
		yMin = min(hliArray[4, , , thisScen, thisLoc], na.rm=TRUE) #min(apply(hliArray[4, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) - 2
		yMax = max(hliArray[4, , , thisScen, thisLoc], na.rm=TRUE) #max(apply(hliArray[4, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) + 2
		

		plot(nc_date, apply(hliArray[4, , , thisScen, thisLoc], 1, mean, na.rm=TRUE),
			ylim = c(yMin,yMax) ,
			type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
			main='', ylab='Daily Average Solar Rad (W / m^2)', xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
		abline(h=mean(apply(hliArray[4, nc_date < as.Date('2023/01/01'), , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE), lwd=2, lty =2, col='#1A232F')

		for(thisModel in 1:length(nc_model))	{
			lines(nc_date, hliArray[4, , thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
			#lines(nc_date[-historicDates], hliArray[1, -historicDates, thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
		}
		smthDaysRmv = c(1:365, (length(nc_date) - 365):length(nc_date))
		kmeansSmooth = ksmooth(nc_date,  apply(hliArray[4, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), bandwidth = 365, kernel = 'box')
		lines(nc_date[-smthDaysRmv], kmeansSmooth$y[-smthDaysRmv], col='#0098B2', lwd=5)

		usr <- par("usr")   # save old user/default/system coordinates
		par(usr = c(0, 1, 0, 1)) # new relative user coordinates
		text(x=0.02, y=0.95, 
			paste0(round(lm(hliArray[4, , thisModel, thisScen, thisLoc] ~ c(1:length(nc_date)))$coef[2] * 3653, 1), ' per decade'),
			col='#0098B2', cex=1.5*1.8, pos=4)
		par(usr = usr) # restore original user coordinates
		dev.off()
	}
}

	# Tbg
for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(nc_scen))	{
		png(paste0(customerFolder, clientName, '\\',  customerTable$Location[thisLoc], '_', nc_scen[thisScen], '_projectedDailyMaxBlackGlobeTemp.png'), width=1500, height=500)
		par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
		
		impArray = hliArray * (9/5) + 32
		
		yMin = min(impArray[5, , , thisScen, thisLoc], na.rm=TRUE) #min(apply(impArray[5, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) - 2
		yMax = max(impArray[5, , , thisScen, thisLoc], na.rm=TRUE) #max(apply(impArray[5, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) + 2
		

		plot(nc_date, apply(impArray[5, , , thisScen, thisLoc], 1, mean, na.rm=TRUE),
			ylim = c(yMin,yMax) ,
			type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
			main='', ylab='Daily Max Black Globe Temp (F)', xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
		abline(h=mean(apply(impArray[5, nc_date < as.Date('2023/01/01'), , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE), lwd=2, lty =2, col='#1A232F')

		for(thisModel in 1:length(nc_model))	{
			lines(nc_date, impArray[5, , thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
			#lines(nc_date[-historicDates], impArray[1, -historicDates, thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
		}
		smthDaysRmv = c(1:365, (length(nc_date) - 365):length(nc_date))
		kmeansSmooth = ksmooth(nc_date,  apply(impArray[5, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), bandwidth = 365, kernel = 'box')
		lines(nc_date[-smthDaysRmv], kmeansSmooth$y[-smthDaysRmv], col='#0098B2', lwd=5)

		usr <- par("usr")   # save old user/default/system coordinates
		par(usr = c(0, 1, 0, 1)) # new relative user coordinates
		text(x=0.02, y=0.05, 
			paste0(round(lm(impArray[5, , thisModel, thisScen, thisLoc] ~ c(1:length(nc_date)))$coef[2] * 3653, 1), ' per decade'),
			col='#0098B2', cex=1.5*1.8, pos=4)
		par(usr = usr) # restore original user coordinates
		dev.off()
	}
}

	# Twb
for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(nc_scen))	{
		png(paste0(customerFolder, clientName, '\\',  customerTable$Location[thisLoc], '_', nc_scen[thisScen], '_projectedDailyMaxWetBulbTemp.png'), width=1500, height=500)
		par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
		
		impArray = hliArray * (9/5) + 32
		
		yMin = min(impArray[6, , , thisScen, thisLoc], na.rm=TRUE) #min(apply(impArray[6, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) - 2
		yMax = max(impArray[6, , , thisScen, thisLoc], na.rm=TRUE) #max(apply(impArray[6, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) + 2
		

		plot(nc_date, apply(impArray[6, , , thisScen, thisLoc], 1, mean, na.rm=TRUE),
			ylim = c(yMin,yMax) ,
			type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
			main='', ylab='Daily Max Wet Bulb Temperature (F)', xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
		abline(h=mean(apply(impArray[6, nc_date < as.Date('2023/01/01'), , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE), lwd=2, lty =2, col='#1A232F')

		for(thisModel in 1:length(nc_model))	{
			lines(nc_date, impArray[6, , thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
			#lines(nc_date[-historicDates], impArray[1, -historicDates, thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
		}
		smthDaysRmv = c(1:365, (length(nc_date) - 365):length(nc_date))
		kmeansSmooth = ksmooth(nc_date,  apply(impArray[6, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), bandwidth = 365, kernel = 'box')
		lines(nc_date[-smthDaysRmv], kmeansSmooth$y[-smthDaysRmv], col='#0098B2', lwd=5)

		usr <- par("usr")   # save old user/default/system coordinates
		par(usr = c(0, 1, 0, 1)) # new relative user coordinates
		text(x=0.02, y=0.05, 
			paste0(round(lm(impArray[6, , thisModel, thisScen, thisLoc] ~ c(1:length(nc_date)))$coef[2] * 3653, 1), ' per decade'),
			col='#0098B2', cex=1.5*1.8, pos=4)
		par(usr = usr) # restore original user coordinates
		dev.off()
	}
}

	# windspeed
for(thisLoc in 1:nrow(customerTable))	{
	for(thisScen in 1:length(nc_scen))	{
		png(paste0(customerFolder, clientName, '\\',  customerTable$Location[thisLoc], '_', nc_scen[thisScen], '_projectedDailyAvgWindspeed.png'), width=1500, height=500)
		par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
		windowsFonts(A = windowsFont("Roboto"))
		
#		impArray = hliArray * (9/5) + 32
		
		yMin = min(hliArray[7, , , thisScen, thisLoc], na.rm=TRUE) #min(apply(hliArray[6, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) - 2
		yMax = max(hliArray[7, , , thisScen, thisLoc], na.rm=TRUE) #max(apply(hliArray[6, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE) + 2
		

		plot(nc_date, apply(hliArray[7, , , thisScen, thisLoc], 1, mean, na.rm=TRUE),
			ylim = c(yMin,yMax) ,
			type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
			main='', ylab='Daily Avg Windspeed (m / s)', xlab='',
			col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
			family='A')
		abline(h=mean(apply(hliArray[7, nc_date < as.Date('2023/01/01'), , thisScen, thisLoc], 1, mean, na.rm=TRUE), na.rm=TRUE), lwd=2, lty =2, col='#1A232F')

		for(thisModel in 1:length(nc_model))	{
			lines(nc_date, hliArray[7, , thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
			#lines(nc_date[-historicDates], hliArray[1, -historicDates, thisModel, thisScen, thisLoc], lwd=2, col=adjustcolor('#666D74', alpha=0.1))
		}
		smthDaysRmv = c(1:365, (length(nc_date) - 365):length(nc_date))
		kmeansSmooth = ksmooth(nc_date,  apply(hliArray[7, , , thisScen, thisLoc], 1, mean, na.rm=TRUE), bandwidth = 365, kernel = 'box')
		lines(nc_date[-smthDaysRmv], kmeansSmooth$y[-smthDaysRmv], col='#0098B2', lwd=5)

		usr <- par("usr")   # save old user/default/system coordinates
		par(usr = c(0, 1, 0, 1)) # new relative user coordinates
		text(x=0.02, y=0.05, 
			paste0(round(lm(hliArray[7, , thisModel, thisScen, thisLoc] ~ c(1:length(nc_date)))$coef[2] * 3653, 2), ' per decade'),
			col='#0098B2', cex=1.5*1.8, pos=4)
		par(usr = usr) # restore original user coordinates
		dev.off()
	}
}

































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
