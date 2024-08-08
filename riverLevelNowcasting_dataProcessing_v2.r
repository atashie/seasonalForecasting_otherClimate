########################
# linking to waterways gdb
library(dataRetrieval) # usgs data
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # data visualisation
library(sf) # simple features - geospatial geometries
library(osmdata) # obtaining OpenStreetMap vector data
library(units) # working with units
library(mapview) # interactive geometry viewing
library(ggmap) # downloading raster maps from a variety of sources
library(ggspatial) # map backgrounds and annotations for ggplot
library(tmap) # static/interactive map library with ggplot-like syntax

userDataLocation = "C:/Users/arik/Documents/GitHub/RiverLevelMonitor_Simplot_Beta/Data/"
waterWaysFolder = 'J:\\Cai_data\\Waterways\\'


######################################################################################################
######################################################################################################
######################################################################################################
## section 00: this section does not need to be re-run to update databases
######################################################################################################


#####################
# concatenating DoT river networks and USGS gages

  # reading in water data and assessing
waterWaysFolder = 'J:\\Cai_data\\Waterways\\'
waterWaysPath = paste0(waterWaysFolder, 'USwaterways.gdb')
waterWaysDb = sf::st_read(waterWaysPath)
waterWaysDb_noM = sf::st_zm(waterWaysDb, drop = TRUE, what = 'ZM')
waterWaysDb_noM[1,]


ggplot(data = waterWaysDb_noM)	+
  #  geom_sf(data = norAmBoundingBox_sf) +
  geom_sf(data = subset(waterWaysDb_noM, !is.na(LENGTH1)), aes(color = 'LENGTH1')) #+
#  scale_color_gradient(trans = 'log')


###############################################
##### (only needs to be run 1x, then table is saved)

#identify the gages w/ stage data 
iter = 0
for(thisState in unique(waterWaysDb_noM$STATE)){
    #hardcoding excptions for state id's that don't actually exist
  if(thisState %in% c('XX', NA, 'UM'))  {print(c(thisState, 'is not a state'))} else {
    iter = iter + 1
    strmflCd = '00060' # 00065 gage height; 00060 is Q
    dailyDataAvailable_Q = whatNWISdata(
        stateCd = thisState,
        statCd = '00003',
  #      service = 'dv', #c('dv','uv','iv'),
  #      siteStatus='active',
        parameterCd = strmflCd)
  
      dailyDataSub_Q = subset(dailyDataAvailable_Q,
                              end_date > as.Date('2024-01-01') & count_nu > (365*1) & stat_cd == '00003')
  
      gageHeightCd = '00065' # 00065 gage height; 00060 is Q
      hasGageData = NULL
        dailyDataAvailable_H = whatNWISdata(
          stateCd = thisState,
          statCd = '00003',
          #      paramCd = c('dv','uv','iv'),
          #      siteStatus='active',
          parameterCd = gageHeightCd)
        
        dailyDataSub_H = subset(dailyDataAvailable_H,
                                end_date > as.Date('2024-01-01') & count_nu > (365*1) & stat_cd == '00003')
   
        if(iter == 1)  {
          availableGages_Q = dailyDataSub_Q
          availableGages_H = dailyDataSub_H
        } else  {
          availableGages_Q = rbind(availableGages_Q, dailyDataSub_Q)
          availableGages_H = rbind(availableGages_H, dailyDataSub_H)
        }
    print(iter)      
  }
} 
     
  # commenting out to prevent accidental overwriting
#data.table::fwrite(availableGages_Q, 'J:\\Cai_data\\Waterways\\usDailyStreamStageGages_Q.csv')
#data.table::fwrite(availableGages_H, 'J:\\Cai_data\\Waterways\\usDailyStreamStageGages_H.csv')
#data.table::fwrite(availableGages_Q, 'J:\\Cai_data\\Waterways\\usDailyStreamStageGages_Q_long.csv')
#data.table::fwrite(availableGages_H, 'J:\\Cai_data\\Waterways\\usDailyStreamStageGages_H_long.csv')

availableGages_Q = data.table::fread(paste0(waterWaysFolder, 'usDailyStreamStageGages_Q_long.csv'), colClasses = c('site_no' = 'character'))
availableGages_H = data.table::fread(paste0(waterWaysFolder, 'usDailyStreamStageGages_H_long.csv'), colClasses = c('site_no' = 'character'))


availableGages_Q_sf = sf::st_as_sf(availableGages_Q, coords = c('dec_long_va', 'dec_lat_va'), crs = 4326)
availableGages_H_sf = sf::st_as_sf(availableGages_H, coords = c('dec_long_va', 'dec_lat_va'), crs = 4326)


## intersecting the gages with the streamlines
nearestNeighbor_Q = sf::st_nearest_feature(waterWaysDb_noM[1,], availableGages_Q_sf)
nearestDistance_Q = sf::st_distance(waterWaysDb_noM[1,], availableGages_Q_sf[nearestNeighbor_Q,])

nearestNeighbor_H = sf::st_nearest_feature(waterWaysDb_noM[1,], availableGages_H_sf)
nearestDistance_H = sf::st_distance(waterWaysDb_noM[1,], availableGages_H_sf[nearestNeighbor_Q,])

for(i in 2:nrow(waterWaysDb_noM)){
  nearestNeighbor_Q = c(nearestNeighbor_Q,
                      sf::st_nearest_feature(waterWaysDb_noM[i,], availableGages_Q_sf))
  nearestDistance_Q = c(nearestDistance_Q,
                      sf::st_distance(waterWaysDb_noM[i,], availableGages_Q_sf[nearestNeighbor_Q[i],]))

  nearestNeighbor_H = c(nearestNeighbor_H,
                        sf::st_nearest_feature(waterWaysDb_noM[i,], availableGages_H_sf))
  nearestDistance_H = c(nearestDistance_H,
                        sf::st_distance(waterWaysDb_noM[i,], availableGages_H_sf[nearestNeighbor_H[i],]))
  
    print(i / nrow(waterWaysDb_noM))
}

waterWaysDb_noM$nearestNeighbor_Q = nearestNeighbor_Q
waterWaysDb_noM$nearestNeighborDist_Q = nearestDistance_Q
waterWaysDb_noM$site_no_Q = availableGages_Q_sf$site_no[nearestNeighbor_Q]
waterWaysDb_noM$lat_Q = availableGages_Q$dec_lat_va[nearestNeighbor_Q]
waterWaysDb_noM$lon_Q = availableGages_Q$dec_long_va[nearestNeighbor_Q]
waterWaysDb_noM$nearestNeighbor_H = nearestNeighbor_H
waterWaysDb_noM$nearestNeighborDist_H = nearestDistance_H
waterWaysDb_noM$site_no_H = availableGages_H_sf$site_no[nearestNeighbor_H]
waterWaysDb_noM$lat_H = availableGages_H$dec_lat_va[nearestNeighbor_H]
waterWaysDb_noM$lon_H = availableGages_H$dec_long_va[nearestNeighbor_H]

  # commenting out so I don't accidentally overwrite
sf::st_write(waterWaysDb_noM, paste0(waterWaysFolder, 'waterWaysAndDistancesCONUS_QandH_sf_2.gpkg'))

######################################################################################################
## end of section 00: this section does not need to be re-run to update databases
######################################################################################################
######################################################################################################
######################################################################################################






######################################################################################################
######################################################################################################
######################################################################################################
## start of section 1: this section is used to update the database
######################################################################################################




	# Data Processing for regular updates
waterWaysDb_sf = sf::st_read(paste0(userDataLocation, 'waterWaysAndDistancesCONUS_QandH_sf_2.gpkg'))
inlandWW_sf = subset(waterWaysDb_sf, WTWY_TYPE %in% c(6,8,9)) # inland waterway types

gageList = which(inlandWW_sf$nearestNeighborDist_Q <= 1000)
gagedWaterways = inlandWW_sf#[gageList, ]
gageDoyAvgs_ls = list()
gageAvgs = data.frame(STAID = NA, Q10 = NA, Q25 = NA, Q50 = NA, Q75 = NA, Q90 = NA, min = NA, max = NA)
missingData = matrix(NA, 1, 2)
thisYear = lubridate::year(Sys.Date())
lastYear = thisYear - 1
lastYear2 = thisYear - 2

for(i in gageList){
  thisData = readNWISdv(siteNumber = gagedWaterways$site_no_Q[i],
                        parameterCd = '00060', #00065 stage; 00060 is Q
                        startDate = '1980-01-01')
  thisData$year = lubridate::year(thisData$Date)
 # ensuring there is a long record of data for quantile analysis
  #  if(length(!is.na(thisData$X_00060_00003)) > 365 * 10) {
  if(any(!is.na(thisData$year %in% thisYear))) {
    thisData$doy = lubridate::yday(thisData$Date)
 	
	justNAs = rep(NA, 365)
	dataDoyQuantilesAndRecent = data.frame(DOY = justNAs, Q10 = justNAs, Q25 = justNAs, Q50 = justNAs, Q75 = justNAs, Q90 = justNAs, min = justNAs, max = justNAs)
#    dataDoyQuantilesAndRecent = data.table::as.data.table(matrix(NA, nrow = 365, ncol = 8))
    for(j in 1:365) {
      # looping doy for smoothing data
      loopedDoys = seq(j - 30, j + 30, 1)
      # catching days at beginning / end of season
      if(any(loopedDoys <= 0)) {
        loopedDoys[which(loopedDoys <= 0)] = loopedDoys[which(loopedDoys <= 0)] + 365
      }
      if(any(loopedDoys > 365)) {
        loopedDoys[which(loopedDoys > 365)] = loopedDoys[which(loopedDoys > 365)] - 365
      }
      
      dataDoyQuantilesAndRecent[j, ] = c(j,
                               #                               quantile(subset(thisData, doy %in% loopedDoys)$X_00060_00003,
									   quantile(subset(thisData, doy %in% loopedDoys)[,4],
												c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
    }
   gageAvgs[i, ] = c(gagedWaterways$site_no_Q[i], 
                     #                      quantile(thisData$X_00065_00003, c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
                    quantile(thisData[,4], c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
 
	
	thisYearDF = data.frame(DOY = thisData[which(thisData$year == thisYear)[1:365], "doy"], thisYear = thisData[which(thisData$year == thisYear)[1:365], 4]) 
	lastYearDF = data.frame(DOY = thisData[which(thisData$year == lastYear)[1:365], "doy"], lastYear = thisData[which(thisData$year == lastYear)[1:365], 4]) 
	lastYear2DF = data.frame(DOY = thisData[which(thisData$year == lastYear2)[1:365], "doy"], lastYear2 = thisData[which(thisData$year == lastYear2)[1:365], 4]) 
	
	dataDoyQuantilesAndRecent = merge(dataDoyQuantilesAndRecent, thisYearDF, by = "DOY", all.x=TRUE)
	dataDoyQuantilesAndRecent = merge(dataDoyQuantilesAndRecent, lastYearDF, by = "DOY", all.x=TRUE)
	dataDoyQuantilesAndRecent = merge(dataDoyQuantilesAndRecent, lastYear2DF, by = "DOY", all.x=TRUE)

    # compiling the data
    gageDoyAvgs_ls[[i]] = dataDoyQuantilesAndRecent
   # squinty eye test    
    plot(dataDoyQuantilesAndRecent$DOY, dataDoyQuantilesAndRecent[ ,4], ylim=range(dataDoyQuantilesAndRecent[ ,-1], na.rm=TRUE))
    lines(dataDoyQuantilesAndRecent[ ,2])
    lines(dataDoyQuantilesAndRecent[ ,3])
    lines(dataDoyQuantilesAndRecent[ ,5])
    lines(dataDoyQuantilesAndRecent[ ,6])
    lines(dataDoyQuantilesAndRecent[ ,7])
    lines(dataDoyQuantilesAndRecent[ ,8])
	lines(dataDoyQuantilesAndRecent[ ,9], col='blue') # this yr
	lines(dataDoyQuantilesAndRecent[ ,10], col='purple') # last yr
	lines(dataDoyQuantilesAndRecent[ ,11], col='red') # 2 yrs ago
	
    print(names(thisData)[4])
  } else {print(c(i, 'skipped')); missingData = rbind(missingData, c(i, gagedWaterways$ID[i]))}
}


	# historic and recent data 
saveRDS(gageDoyAvgs_ls, paste0(userDataLocation, 'gageDoyAvgs_ls.rds'))
data.table::fwrite(gageAvgs, paste0(userDataLocation, 'gageAvgs.csv'))






plotter_sf = inlandWW_sf
plotter_sf$Annual_Avg_Pct = NA
plotter_sf$Season_Avg_Pct = NA
plotter_sf$Raw_Value = NA
recentDate = Sys.Date() - 30
for(i in gageList){
  
	currentStrmVal =  gageDoyAvgs_ls[[i]]
	if(any(!is.na(currentStrmVal$thisYear))) {
		print(i)
			# associating most recent value (raw) with gpkg for plotting
		lastNoNaDay = data.table::last(which(!is.na(currentStrmVal$thisYear)))
		plotter_sf$Raw_Value[i] = currentStrmVal$thisYear[lastNoNaDay]

			# associating most recent value with annual and seasonal quantiles
		quantSeq_tot = rep(NA, 100)
		quantSeq_tot[c(1,11,26,51,76,91,100)] = as.numeric(unlist(gageAvgs[i,c(7,2,3,4,5,6,8)]))
		quantSeq_tot = zoo::na.fill(quantSeq_tot, 'extend')

		quantSeq_seas = rep(NA, 100)
		quantSeq_seas[c(1,11,26,51,76,91,100)] = as.numeric(unlist(currentStrmVal[lastNoNaDay,c(7,2,3,4,5,6,8)]))
		quantSeq_seas = zoo::na.fill(quantSeq_seas, 'extend')
		   
		plotter_sf$Annual_Avg_Pct[i] = which.min(abs(currentStrmVal$thisYear[lastNoNaDay] - quantSeq_tot))
		plotter_sf$Season_Avg_Pct[i] = which.min(abs(currentStrmVal$thisYear[lastNoNaDay] - quantSeq_seas))
	} else {print(c("no data for gage", i))}
}

sf::st_write(plotter_sf, paste0(userDataLocation, 'waterwaysWithCurrentVals.gpkg'), append=FALSE)


# updating USACE data
#source("C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/gageRssFeed_function.r")
source("C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/gageRssFeed_NWS2024_function.r")
customerInputTable = data.table::fread("C:/Users/arik/Documents/GitHub/RiverLevelMonitor_Simplot_Beta/Data/CustomerOnboardingTemplate.csv")
#gageRssFeed_f(customerInputTable = customerInputTable, userDataLocation = userDataLocation)
gageRssFeed_f(customerInputTable = customerInputTable, userDataLocation = userDataLocation)


######################################################################################################
## end of section 1: this section is used to update the database
######################################################################################################
######################################################################################################
######################################################################################################




################## plotting data for inspection

availableGages_Q = data.table::fread(paste0(waterWaysFolder, 'usDailyStreamStageGages_Q_long.csv'), colClasses = c('site_no' = 'character'))
availableGages_H = data.table::fread(waterWaysFolder, 'usDailyStreamStageGages_H_long.csv'), colClasses = c('site_no' = 'character'))

availableGages_Q_sf = sf::st_as_sf(availableGages_Q, coords = c('dec_long_va', 'dec_lat_va'), crs = 4326)
availableGages_H_sf = sf::st_as_sf(availableGages_H, coords = c('dec_long_va', 'dec_lat_va'), crs = 4326)



ocean50 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_ocean\\ne_10m_ocean.shp'))
countries10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_0_countries\\ne_10m_admin_0_countries.shp'))
provinces10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_1_states_provinces\\ne_10m_admin_1_states_provinces.shp'))

ggplot(data = inlandWW_sf) +
  geom_sf(data=subset(provinces10, geonunit=='United States of America'),
          colour='grey80', fill='grey95') +
  geom_sf(data = subset(inlandWW_sf, WTWY_TYPE %in% c(6,8,9) & !is.na(LENGTH1)), 
          aes(color = 'SHAPE_length'), linewidth = 0.8) +
  geom_sf(data = availableGages_Q_sf, size = 0.5, color='grey50') +
  geom_sf(data = gagedWaterways, color = 'royalblue2', linewidth = 1.2) +
  coord_sf(xlim = c(-127, -65), ylim = c(23, 51), expand = FALSE)
#  scale_color_gradient(trans = 'log')




theme_set(theme_bw())
ocean50 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_ocean\\ne_10m_ocean.shp'))
countries10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_0_countries\\ne_10m_admin_0_countries.shp'))
provinces10 = st_as_sf(st_read('J:\\Cai_data\\ne_10m_admin_1_states_provinces\\ne_10m_admin_1_states_provinces.shp'))

library(maps)

ggplot(data = plotter_sf) +
  geom_sf(data = availableGages_Q_sf, size = 1, color='grey85') +
  borders('world', xlim=c(-100, -80), ylim=c(25, 51), 
          colour='gray90', size=.2, fill='grey80')	+
  geom_sf(data=subset(provinces10, geonunit=='United States of America'),
          colour='grey80', fill='grey95') +
  geom_sf(data = subset(inlandWW_sf, WTWY_TYPE %in% c(6,8,9) & !is.na(LENGTH1)), 
          color = 'black', linetype = '11', linewidth = 0.3) +
  #  geom_sf(data = plotter_sf, aes(colour = relVal_seas), linewidth = 1.4) +
  geom_sf(data = plotter_sf, aes(colour = relVal_tot), linewidth = 1.4) +
  #  geom_sf(data = plotter_sf, aes(colour = rawVal), linewidth = 1.4) +
  scale_colour_viridis_c(
    limits = c(0,1),
    labels = scales::percent, 
    name = 'Current %tile',
    option='plasma') +
  coord_sf(xlim = c(-100, -70), ylim = c(24, 50), expand = FALSE)
#  scale_color_gradient(trans = 'log')











###################### pulling in and processing USACE data
jj = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/obs/heea4.rss")
kk = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/fcst/heea4.rss")
ll = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/alert/heea4.rss")



dataPath = "J://Downloads//"
fileName = "Book2.csv"
usaceData = data.table::fread(paste0(dataPath, fileName))
#usaceData$Date <- sprintf("%08d", usaceData$Date)
usaceData$mydDate = usaceData$mydDate = as.Date(lubridate::mdy_hm(usaceData$Date))
usaceDataSort = usaceData[order(usaceData$mydDate), c("mydDate", "Stage")]


https://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfc?method=RGWML&meth=getValues&site=rcki2&location=rcki2&variable=HP&beginDate=2023-03-26T00:00&endDate=2023-03-26T23:59&authtoken=RiverGages&authToken=RiverGages


api1 = "https://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfc?"
api2 = "method=RGWML&"
#api3 = "meth=getSites&"
api3 = "meth=getValues&"
api4 = "site=rcki2&"
api5 = "location=crtm7&"
api6 = "variable=HP&"
api7 = "beginDate=2023-03-25T00:00&"
api8 = "endDate=2023-03-26T23:59&"
api9 = "authtoken=RiverGages&"
api10 = "authToken=RiverGages"
mySite = paste0(api1,api2,api3,api4,api5,api6,api7,api8,api9,api10)
	




	