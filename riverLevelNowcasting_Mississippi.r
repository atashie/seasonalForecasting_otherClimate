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




#####################
# v2

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
                              end_date > as.Date('2023-01-01') & count_nu > (365*10) & stat_cd == '00003')
  
      gageHeightCd = '00065' # 00065 gage height; 00060 is Q
      hasGageData = NULL
        dailyDataAvailable_H = whatNWISdata(
          stateCd = thisState,
          statCd = '00003',
          #      paramCd = c('dv','uv','iv'),
          #      siteStatus='active',
          parameterCd = gageHeightCd)
        
        dailyDataSub_H = subset(dailyDataAvailable_H,
                                end_date > as.Date('2023-01-01') & count_nu > (365*10) & stat_cd == '00003')
   
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

availableGages_Q = data.table::fread('J:\\Cai_data\\Waterways\\usDailyStreamStageGages_Q.csv', colClasses = c('site_no' = 'character'))
availableGages_H = data.table::fread('J:\\Cai_data\\Waterways\\usDailyStreamStageGages_H.csv', colClasses = c('site_no' = 'character'))


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
waterWaysDb_noM$nearestNeighbor_H = nearestNeighbor_H
waterWaysDb_noM$nearestNeighborDist_H = nearestDistance_H
waterWaysDb_noM$site_no_H = availableGages_H_sf$site_no[nearestNeighbor_H]

  # commenting out so I don't accidentally overwrite
sf::st_write(waterWaysDb_noM, 'J:\\Cai_data\\Waterways\\waterWaysAndDistancesCONUS_QandH_sf.gpkg')

##### end of section 
###############################################



waterWaysDb_sf = sf::st_read('J:\\Cai_data\\Waterways\\waterWaysAndDistancesCONUS_QandH_sf.gpkg')

'02268903' %in% waterWaysDb_sf$site_no_H
'02268903' %in% waterWaysDb_sf$site_no_Q
waterwaysSub = subset(waterWaysDb_sf, WTWY_TYPE %in% c(6,8,9))
which(waterwaysSub$BNODE %in% waterwaysSub$ANODE) # removes coastal
which(waterwaysSub$ANODE %in% waterwaysSub$BNODE) # removes inland

keepSearching = TRUE
inlandStop = 4397 #4646
downStreamNodes = 4397#4646
while(keepSearching)  {
  nextNode = which(waterWaysDb_sf$ANODE == waterWaysDb_sf$BNODE[data.table::last(downStreamNodes)])  
  if(length(nextNode) == 0) {
    keepSearching = FALSE
  } else  {
    downStreamNodes = c(downStreamNodes, nextNode)
  }
}

gageList = which(waterWaysDb_sf$nearestNeighborDist_Q <= 100)
ggplot(data = waterWaysDb_sf)	+
  #  geom_sf(data = norAmBoundingBox_sf) +
  geom_sf(data = subset(waterWaysDb_sf, WTWY_TYPE %in% c(6,8,9) & !is.na(LENGTH1)), aes(color = 'SHAPE1')) +
  geom_sf(data = waterWaysDb_sf[gageList, ], aes(color = 'SHAPE_length')) +
  geom_sf(data = waterWaysDb_sf[downStreamNodes, ], color='purple') +
  #  geom_sf(data = usgsStageGages_sf, size = 1, fill='black') +
  coord_sf(xlim = c(-100, -80), ylim = c(25, 51), expand = FALSE)
#  scale_color_gradient(trans = 'log')




gagedWaterways = waterWaysDb_sf[gageList, ]
gageAvgs = data.frame(STAID = NA, Q10 = NA, Q25 = NA, Q50 = NA, Q75 = NA, Q90 = NA, min = NA, max = NA)
gageDoyAvgs_ls = list()
missingData = matrix(NA, 1,2)

for(i in 1:nrow(gagedWaterways)){
  thisData = readNWISdv(siteNumber = gagedWaterways$site_no_Q[i],
                        parameterCd = '00060', #00065 stage; 00060 is Q
                        startDate = '1980-01-01')
  # ensuring there is a long record of data for quantile analysis
  #  if(length(!is.na(thisData$X_00060_00003)) > 365 * 10) {
  if(length(!is.na(thisData[,4])) > 365 * 10) {
    thisData$doy = lubridate::yday(thisData$Date)
    
    #    data.frame(DOY = NA, Q10 = NA, Q25 = NA, Q50 = NA, Q75 = NA, Q90 = NA, min = NA, max = NA)
    dataDoyQuantiles = matrix(NA, nrow = 365, ncol = 8)
    for(j in 1:365) {
      # looping doy for smoothing data
      loopedDoys = seq(j - 7, j + 7, 1)
      # catching days at beginning / end of season
      if(any(loopedDoys <= 0)) {
        loopedDoys[which(loopedDoys <= 0)] = loopedDoys[which(loopedDoys <= 0)] + 365
      }
      if(any(loopedDoys > 365)) {
        loopedDoys[which(loopedDoys > 365)] = loopedDoys[which(loopedDoys > 365)] - 365
      }
      
      dataDoyQuantiles[j,] = c(j,
                               #                               quantile(subset(thisData, doy %in% loopedDoys)$X_00060_00003,
                               quantile(subset(thisData, doy %in% loopedDoys)[,4],
                                        c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
    }
    # compiling the data
    gageDoyAvgs_ls[[i]] = dataDoyQuantiles
    gageAvgs[i, ] = c(gagedWaterways$site_no_Q[i], 
                      #                      quantile(thisData$X_00065_00003, c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
                      quantile(thisData[,4], c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
    
    # squinty eye test    
    plot(dataDoyQuantiles[ ,4], ylim=range(dataDoyQuantiles[ ,-1]))
    lines(dataDoyQuantiles[ ,2])
    lines(dataDoyQuantiles[ ,3])
    lines(dataDoyQuantiles[ ,5])
    lines(dataDoyQuantiles[ ,6])
    lines(dataDoyQuantiles[ ,7])
    lines(dataDoyQuantiles[ ,8])
    print(names(thisData)[4])
  } else {print(c(i, 'skipped')); missingData = rbind(missingData, c(i, gagedWaterways$STAID[i]))}
}


saveRDS(gageDoyAvgs_ls, paste0(waterWaysFolder, 'gageDoyAvgs_ls.rds'))
data.table::fwrite(gageAvgs, paste0(waterWaysFolder, 'gageAvgs.csv'))


availableGages_Q = data.table::fread('J:\\Cai_data\\Waterways\\usDailyStreamStageGages_Q.csv', colClasses = c('site_no' = 'character'))
availableGages_H = data.table::fread('J:\\Cai_data\\Waterways\\usDailyStreamStageGages_H.csv', colClasses = c('site_no' = 'character'))

availableGages_Q_sf = sf::st_as_sf(availableGages_Q, coords = c('dec_long_va', 'dec_lat_va'), crs = 4326)
availableGages_H_sf = sf::st_as_sf(availableGages_H, coords = c('dec_long_va', 'dec_lat_va'), crs = 4326)



which(waterWaysDb_sf$ANODE == waterWaysDb_sf$BNODE[1])


ggplot(data = waterWaysDb_sf) +
  geom_sf(data = subset(waterWaysDb_sf, WTWY_TYPE %in% c(6,8,9) & !is.na(LENGTH1)), 
          aes(color = 'SHAPE_length'), linewidth = 0.7) +
  geom_sf(data = availableGages_Q_sf, size = 1, color='grey50') +
  geom_sf(data = gagedWaterways, color = 'royalblue2', linewidth = 1.2) +
  coord_sf(xlim = c(-100, -80), ylim = c(25, 51), expand = FALSE)
#  scale_color_gradient(trans = 'log')


plotter_sf = gagedWaterways
plotter_sf$relVal_tot = NA
plotter_sf$relVal_seas = NA
plotter_sf$rawVal = NA
recentDate = Sys.Date() - 30
for(thisReach in 1:nrow(plotter_sf)){
  
  currentStrmVal =  readNWISdv(siteNumber = gagedWaterways$site_no_Q[thisReach],
                               parameterCd = '00060', #00065 stage; 00060 is Q
                               startDate = recentDate)
  
  if(any(!is.na(currentStrmVal[,4]))) {
    lastNoNaDay = data.table::last(which(!is.na(currentStrmVal[, 4])))
    plotter_sf$rawVal[thisReach] = currentStrmVal[lastNoNaDay, 4]
    
    if(any(!is.na(gageAvgs[thisReach, ]))) {
      currentStrmVal$DoY = lubridate::yday(currentStrmVal$Date)
      
      quantSeq_tot = rep(NA, 100)
      quantSeq_tot[c(1,11,26,51,76,91,2,100)] = c(0,as.numeric(unlist(gageAvgs[thisReach,-1])))
      quantSeq_tot = zoo::na.fill(quantSeq_tot, 'extend')

      quantSeq_seas = rep(NA, 100)
      quantSeq_seas[c(1,11,26,51,76,91,2,100)] = c(0,
          as.numeric(unlist(gageDoyAvgs_ls[[thisReach]][currentStrmVal$DoY[lastNoNaDay],-1])))
      quantSeq_seas = zoo::na.fill(quantSeq_seas, 'extend')
      
      plotter_sf$relVal_tot[thisReach] = which.min(abs(currentStrmVal[lastNoNaDay, 4] - quantSeq_tot)) / 100
      plotter_sf$relVal_seas[thisReach] = which.min(abs(currentStrmVal[lastNoNaDay, 4] - quantSeq_seas)) / 100
      
    }
  }      
} 





sf::st_write(plotter_sf, paste0('J:\\Cai_data\\Waterways\\waterways_', Sys.Date(), '.gpkg'))


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
  geom_sf(data = subset(waterWaysDb_sf, WTWY_TYPE %in% c(6,8,9) & !is.na(LENGTH1)), 
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

















# old



# reading in water data and assessing
waterWaysFolder = 'J:\\Cai_data\\Waterways\\'
waterWaysPath = paste0(waterWaysFolder, 'USwaterways.gdb')
waterWaysDb = sf::st_read(waterWaysPath)
waterWaysDb_noM = sf::st_zm(waterWaysDb, drop = TRUE, what = 'ZM')
waterWaysDb_noM[1,]



#norAmBoundingBox = st_polygon(list(cbind(c(-126, -126, -66, -66, -126), c(23, 55, 55, 23, 23))))
#norAmBoundingBox_sf = st_sfc(norAmBoundingBox, crs = 4326)
#waterWaysDb_sub = st_intersection(waterWaysDb_noM, norAmBoundingBox_sf)


ggplot(data = waterWaysDb_noM)	+
#  geom_sf(data = norAmBoundingBox_sf) +
  geom_sf(data = subset(waterWaysDb_noM, WTWY_TYPE %in% c(6,8,9) & !is.na(LENGTH1)), aes(color = 'LENGTH1')) #+
#  scale_color_gradient(trans = 'log')


###############################################
##### (only needs to be run 1x, then table is saved)

  #identify the gages w/ stage data 
usgsGageList = data.table::fread('C:\\Users\\arik\\Documents\\PhD Research\\D2\\spreadsheets-in-csv-format\\conterm_basinid.txt', colClasses = 'character')
gageHeightCd = '00060' # 00065 gage height; 00060 is Q
hasGageData = NULL
for(i in 1:nrow(usgsGageList)){
  if(i == 7357) {hasGageData = c(hasGageData, FALSE)} else {
  dailyDataAvailable = whatNWISdata(
    siteNumber = usgsGageList$STAID[i],
    parameterCd = gageHeightCd)
  
    hasGageData = c(hasGageData, ifelse(gageHeightCd %in% dailyDataAvailable$parm_cd, TRUE, FALSE))
    print(c(i, hasGageData[i]))
    }
}

#data.table::fwrite(usgsGageList[hasGageData,], 'J:\\Cai_data\\Waterways\\conusDailyStreamStageGages_stage.csv')
#usgsStageGages = data.table::fread('J:\\Cai_data\\Waterways\\conusDailyStreamStageGages_stage.csv', colClasses = c('STAID' = 'character'))
data.table::fwrite(usgsGageList[hasGageData,], 'J:\\Cai_data\\Waterways\\conusDailyStreamStageGages_q.csv')
usgsStageGages = data.table::fread('J:\\Cai_data\\Waterways\\conusDailyStreamStageGages_q.csv', colClasses = c('STAID' = 'character'))

usgsStageGages_sf = sf::st_as_sf(usgsStageGages, coords = c('LNG_GAGE', 'LAT_GAGE'), crs = 4326)


## intersecting the gages with the streamlines
nearestNeighbor = sf::st_nearest_feature(waterWaysDb_noM[1,], usgsStageGages_sf)
nearestDistance = sf::st_distance(waterWaysDb_noM[1,], usgsStageGages_sf[nearestNeighbor,])

for(i in 2:nrow(waterWaysDb_noM)){
  nearestNeighbor = c(nearestNeighbor,
                      sf::st_nearest_feature(waterWaysDb_noM[i,], usgsStageGages_sf))
  nearestDistance = c(nearestDistance,
                      sf::st_distance(waterWaysDb_noM[i,], usgsStageGages_sf[nearestNeighbor[i],]))
  print(i / nrow(waterWaysDb_noM))
}

ggplot(data = gagedWaterways)	+
  geom_sf(data = gagedWaterways, aes(color = 'LENGTH1')) 

waterWaysDb_noM$nearestNeighbor = nearestNeighbor
waterWaysDb_noM$nearestNeighborDist = nearestDistance
waterWaysDb_noM$STAID = usgsStageGages_sf$STAID[nearestNeighbor]

#sf::st_write(waterWaysDb_noM, 'J:\\Cai_data\\Waterways\\waterWaysAndDistancesCONUSstage_sf.gpkg')
sf::st_write(waterWaysDb_noM, 'J:\\Cai_data\\Waterways\\waterWaysAndDistancesCONUSq_sf.gpkg')

##### end of section 
###############################################




#waterWaysDb_sf = sf::st_read('J:\\Cai_data\\Waterways\\waterWaysAndDistancesCONUSstage_sf.gpkg')
waterWaysDb_sf = sf::st_read('J:\\Cai_data\\Waterways\\waterWaysAndDistancesCONUSq_sf.gpkg')
usgsStageGages_sf = sf::st_as_sf(usgsStageGages, coords = c('LNG_GAGE', 'LAT_GAGE'), crs = 4326)

'02268903' %in% waterWaysDb_sf$STAID

gageList = which(waterWaysDb_sf$nearestNeighborDist <= 500)
ggplot(data = waterWaysDb_sf)	+
  #  geom_sf(data = norAmBoundingBox_sf) +
  geom_sf(data = subset(waterWaysDb_sf, WTWY_TYPE %in% c(6,8,9) & !is.na(LENGTH1)), aes(color = 'SHAPE1')) +
  geom_sf(data = waterWaysDb_sf[gageList, ], aes(color = 'SHAPE_length')) +
  geom_sf(data = usgsStageGages_sf, size = 1, fill='black') +
  coord_sf(xlim = c(-100, -80), ylim = c(25, 51), expand = FALSE)
  #  scale_color_gradient(trans = 'log')




gagedWaterways = waterWaysDb_sf[gageList, ]
gageAvgs = data.frame(STAID = NA, Q10 = NA, Q25 = NA, Q50 = NA, Q75 = NA, Q90 = NA, min = NA, max = NA)
gageDoyAvgs_ls = list()
missingData = matrix(NA, 1,2)

for(i in 1:nrow(gagedWaterways)){
  thisData = readNWISdv(siteNumber = gagedWaterways$STAID[i],
             parameterCd = '00060', #00065 stage; 00060 is Q
             startDate = '1980-01-01')
    # ensuring there is a long record of data for quantile analysis
#  if(length(!is.na(thisData$X_00060_00003)) > 365 * 10) {
  if(length(!is.na(thisData[,4])) > 365 * 10) {
    thisData$doy = lubridate::yday(thisData$Date)
  
   #    data.frame(DOY = NA, Q10 = NA, Q25 = NA, Q50 = NA, Q75 = NA, Q90 = NA, min = NA, max = NA)
    dataDoyQuantiles = matrix(NA, nrow = 365, ncol = 8)
    for(j in 1:365) {
        # looping doy for smoothing data
      loopedDoys = seq(j - 7, j + 7, 1)
        # catching days at beginning / end of season
      if(any(loopedDoys <= 0)) {
        loopedDoys[which(loopedDoys <= 0)] = loopedDoys[which(loopedDoys <= 0)] + 365
      }
      if(any(loopedDoys > 365)) {
        loopedDoys[which(loopedDoys > 365)] = loopedDoys[which(loopedDoys > 365)] - 365
      }

       dataDoyQuantiles[j,] = c(j,
#                               quantile(subset(thisData, doy %in% loopedDoys)$X_00060_00003,
                                quantile(subset(thisData, doy %in% loopedDoys)[,4],
                                                 c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
    }
      # compiling the data
    gageDoyAvgs_ls[[i]] = dataDoyQuantiles
    gageAvgs[i, ] = c(gagedWaterways$STAID[i], 
#                      quantile(thisData$X_00065_00003, c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))
                      quantile(thisData[,4], c(.1, .25, .5, .75, .9, .01, .99), na.rm = TRUE))

      # squinty eye test    
    plot(dataDoyQuantiles[ ,4])
    lines(dataDoyQuantiles[ ,2])
    lines(dataDoyQuantiles[ ,3])
    lines(dataDoyQuantiles[ ,5])
    lines(dataDoyQuantiles[ ,6])
    lines(dataDoyQuantiles[ ,7])
    lines(dataDoyQuantiles[ ,8])
    print(names(thisData)[4])
  } else {print(c(i, 'skipped')); missingData = rbind(missingData, c(i, gagedWaterways$STAID[i]))}
}

QmissingData = missingData
saveRDS(gageDoyAvgs_ls, paste0(waterWaysFolder, 'gageDoyAvgs_ls.rds'))
data.table::fwrite(gageAvgs, paste0(waterWaysFolder, 'gageAvgs.csv'))



which(waterWaysDb_sf$STAID == '07374510')
which(waterWaysDb_sf$ANODE == waterWaysDb_sf$BNODE[1])


ggplot(data = waterWaysDb_sf)	+
  #  geom_sf(data = norAmBoundingBox_sf) +
  geom_sf(data = waterWaysDb_sf[1,], color = 'blue') +
  geom_sf(data = waterWaysDb_sf[1899,], color = 'red')
  
  
    geom_sf(data = waterWaysDb_sf[gageList, ], aes(color = 'SHAPE_length')) +
  geom_sf(data = usgsStageGages_sf, size = 1, fill='black') +
  coord_sf(xlim = c(-100, -80), ylim = c(25, 51), expand = FALSE)
#  scale_color_gradient(trans = 'log')



















data = st_read('J:\\Cai_data\\Rabo\\Locations\\BeefNW\\watershedBoundaries_BeefNW.gpkg')
data = st_read('J:\\Cai_data\\Rabo\\Locations\\BeefNW\\HydroBASINSdata_BeefNW.gpkg')
data = st_read('J:\\Cai_data\\Rabo\\Locations\\WestHillsFarms\\watershedBoundaries_WestHillsFarms.gpkg')
plot(data[1,])
summary(
data[3,]$geom

gg = readRDS('J:\\Cai_data\\Rabo\\Locations\\WestHillsFarms\\projectedOutputs_ssp245_WestHillsFarmsFALSETRUEFALSEFALSE.RData')

badCol = which(names(gg[[1]]) == 'blackBucketStor')
for(i in 1:length(gg)){
  tt = gg[[i]][ , -c(..badCol)]
  data.table::fwrite(tt, paste0('J:\\Cai_data\\Rabo\\Locations\\WestHillsFarms\\exGwOutput_', i, '.csv'))
}










whatNWISsites(parameterCd = '00065', stateCd = 'MS') # Gage height, feet

readNWISdv(siteNumber = '335915090423900'), parameterC = '00065')






activeWaterLevelSites = whatNWISsites(parameterCd = '00072', stateCd = 'OH')
whatNWISsites(parameterCd = '62600', stateCd = 'MS')
whatNWISsites(parameterCd = '62601', stateCd = 'MS')
whatNWISsites(parameterCd = '63159', stateCd = 'MS')
whatNWISsites(parameterCd = '63161', stateCd = 'MS')
whatNWISsites(parameterCd = '72293', stateCd = 'MS')
whatNWISsites(parameterCd = '72292', stateCd = 'MS')
whatNWISsites(parameterCd = '72333', stateCd = 'MS')
whatNWISsites(parameterCd = '72361', stateCd = 'MS')
whatNWISsites(parameterCd = '72362', stateCd = 'MS')
whatNWISsites(parameterCd = '72363', stateCd = 'MS')
whatNWISsites(parameterCd = '72364', stateCd = 'MS')
whatNWISsites(parameterCd = '72397', stateCd = 'MS')
whatNWISsites(parameterCd = '99020', stateCd = 'MS')
whatNWISsites(parameterCd = '99064', stateCd = 'MS')
whatNWISsites(parameterCd = '99065', stateCd = 'MS')
whatNWISsites(parameterCd = '00064', stateCd = 'MS')
whatNWISsites(parameterCd = '00072', stateCd = 'MS')
whatNWISsites(parameterCd = '00064', stateCd = 'MS') # Mean depth of stream, feet

whatNWISsites(parameterCd = '72020', stateCd = 'MS')
whatNWISsites(parameterCd = '63158', stateCd = 'MS')




whatNWISsites(parameterCd = '00004', stateCd = 'MS')
whatNWISsites(parameterCd="00665", stateCd="MS")

    