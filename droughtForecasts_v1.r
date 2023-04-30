#library(dataRetrieval)
library(ncdf4)			# for loading netcdf that contains lat-lons of climate data
library(data.table)		# for data.frame and fread
library(SPEI)
library(lubridate)
library(zoo)			# for na.approx
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
#library(gapminder)
library(gganimate)
library(transformr)
library(gifski)
library(mapiso)
 
	# these file locations remain the same for all watersheds of a region
seas5DataNCDF = 'J:\\Cai_data\\Nuveen\\surfaceWaterData_and_Output\\NuveenNorCal-testing-seas5.nc' 				# SoCal
era5DataNCDF = 'J:\\Cai_data\\Nuveen\\surfaceWaterData_and_Output\\NuveenNorCal-historic-testing-era.nc'
#era5DataNCDF =  'J:\\Cai_data\\Nuveen\\surfaceWaterData_and_Output\\NuveenNorCal-testing-recent-era.nc'			# SoCal

ncin_era5 = nc_open(era5DataNCDF)	;	ncin_era5
ncin_seas5 = nc_open(seas5DataNCDF)	;	ncin_seas5

ncatt_get(ncin_era5, 'time','units')$value
ncatt_get(ncin_seas5, 'valid_time','units')$value
#########################################################################

	# correct dates must be manually selected for now
#cfsStartDate = as.Date('2022-02-28') #  + ncvar_get(ncin_cfs, 'time')/24 for calculating actual dates
era5StartDate =  as.Date('2000-08-01') # + ncvar_get(ncin_era5, 'time') for calculating actual dates 
#era5RecentStartDate =  as.Date('2001-07-01') # + ncvar_get(ncin_recentEra5, 'time') for calculating actual dates 
	# seas5 is incorrectly showing the second of the month, but should be the first
seas5StartDate = as.Date('2022-11-01') # + ncvar_get(ncin_seas5, 'lead_time') for calculating actual dates
#seas5MultiStartDate = as.Date('1993-01-02') - 2 # + ncvar_get(ncin_seas5, 'lead_time') for calculating actual dates

era_lons = ncvar_get(ncin_era5, 'longitude')
era_lats = ncvar_get(ncin_era5, 'latitude')
era_dates = ncvar_get(ncin_era5, 'time') + era5StartDate
era_tmax = ncvar_get(ncin_era5, 't2m_max')
era_tmin = ncvar_get(ncin_era5, 't2m_min')
era_precip = ncvar_get(ncin_era5, 'tp_sum') * 30
era_tmn = (era_tmin + era_tmax) / 2
era_soilT = ncvar_get(ncin_era5, 'stl1_mean')

seas_datesSeas5 = ncvar_get(ncin_seas5, 'lead_time') + seas5StartDate
seas_tmax = ncvar_get(ncin_seas5, 't2m_max')
seas_tmin = ncvar_get(ncin_seas5, 't2m_min')
seas_precip = ncvar_get(ncin_seas5, 'tp') * 30
seas_tmn = (seas_tmax + seas_tmin) / 2

seas5Rows = (1 + which(as.character(seas_datesSeas5) == as.character(last(era_dates)))):length(seas_datesSeas5)
seas5Models = ncvar_get(ncin_seas5, 'member')
allDates = c(era_dates, seas_datesSeas5[seas5Rows])

yearMonth = year(allDates) + month(allDates) / 12
yearMonth_u = unique(yearMonth)
truncMonth = length(yearMonth_u)
dailyDF = data.frame(Date = allDates, Year = year(allDates), Month = month(allDates), Year_Month = year(allDates) + month(allDates) / 12)


dataOutput = data.frame(yearMonth = NA, spei_1 = NA, spei_3 = NA, spei_6 = NA, spei_12 = NA, spei_24 = NA, lon = NA, lat = NA)

for(thisLon in 1:length(era_lons))	{
	for(thisLat in 1:length(era_lats))	{
		#if(any(!is.na(era_soilT[thisLon, thisLat, ])))	{
			era_minTemps = na.fill(era_tmin[thisLon, thisLat, ], fill = 'extend')
			era_maxTemps = na.fill(era_tmax[thisLon, thisLat, ], fill = 'extend')
			era_mnTemps = na.fill(era_tmn[thisLon, thisLat, ], fill = 'extend')
			era_sumPrecip = na.fill(era_precip[thisLon, thisLat, ], fill = 'extend')
			
			spei_1_temp = data.frame(yearMonth = yearMonth_u)
			spei_3_temp = data.frame(yearMonth =  yearMonth_u)
			spei_6_temp = data.frame(yearMonth = yearMonth_u)
			spei_12_temp = data.frame(yearMonth = yearMonth_u)
			spei_24_temp = data.frame(yearMonth = yearMonth_u)
			
			for(thisModel in 1:length(seas5Models))	{
				seas_minTemps =	na.fill(seas_tmin[thisLon, thisLat, seas5Rows, thisModel], fill = 'extend')
				seas_maxTemps =	na.fill(seas_tmax[thisLon, thisLat, seas5Rows, thisModel], fill = 'extend')
				seas_mnTemps = 	na.fill(seas_tmn[thisLon, thisLat, seas5Rows, thisModel], fill = 'extend')
				seas_sumPrecip=	na.fill(seas_precip[thisLon, thisLat, seas5Rows, thisModel], fill = 'extend')

				climDF = data.frame(yearMonth = yearMonth, mnTemps = c(era_mnTemps, seas_mnTemps), precip = c(era_sumPrecip, seas_sumPrecip))
				
				monthlyDF = aggregate(cbind(mnTemps, precip) ~ yearMonth, data = climDF, FUN = mean)
				monthlyDF$pet = thornthwaite(monthlyDF$mnTemps, era_lats[thisLat])
				
				spei_1_temp = cbind(spei_1_temp, as.vector(spei(monthlyDF$precip - monthlyDF$pet, scale = 1)$fitted))
				spei_3_temp = cbind(spei_3_temp, as.vector(spei(monthlyDF$precip - monthlyDF$pet, scale = 3)$fitted))
				spei_6_temp =  cbind(spei_6_temp, as.vector(spei(monthlyDF$precip - monthlyDF$pet, scale = 6)$fitted))
				spei_12_temp =  cbind(spei_12_temp, as.vector(spei(monthlyDF$precip - monthlyDF$pet, scale = 12)$fitted))
				spei_24_temp =  cbind(spei_24_temp, as.vector(spei(monthlyDF$precip - monthlyDF$pet, scale = 24)$fitted))
			}
			
			dataOutput = rbind(dataOutput,
				data.frame(
					yearMonth = monthlyDF$yearMonth,
					spei_1 = apply(spei_1_temp[,-1], 1, mean),
					spei_3 = apply(spei_3_temp[,-1], 1, mean),
					spei_6 = apply(spei_6_temp[,-1], 1, mean),
					spei_12 = apply(spei_12_temp[,-1], 1, mean),
					spei_24 = apply(spei_24_temp[,-1], 1, mean),
					lon = era_lons[thisLon],
					lat = era_lats[thisLat])[-truncMonth,]
			)
		print(thisLat)
	} 
	print(thisLon)
}	
fwrite(dataOutput[-1,], 'J:\\Cai_data\\Nuveen\\surfaceWaterData_and_Output\\spei_testOut.csv')

dataInLoc = 'J:\\Cai_data\\Nuveen\\surfaceWaterData_and_Output\\spei_testOut.csv'



#spei_proj = subset(wsDat, Climate_Scenario == 'rcp6.0')

sf_use_s2(TRUE)

world <- ne_countries(scale = "medium", returnclass = "sf")
america = ne_countries(country = 'United States of America', scale='medium', returnclass = 'sf')
class(world)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
california = subset(states, ID == 'california')
st_crs(california) = st_crs(america)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("california", counties$ID))
st_crs(counties) = st_crs(america)

spei_proj = st_as_sf(fread(dataInLoc), coords=c('lon','lat'), remove=FALSE, crs=4326, agr='constant')
spei_proj$year = floor(spei_proj$yearMonth - 1/13)
spei_proj$month = round((spei_proj$yearMonth - spei_proj$year) * 12)

#borders = america#subset(world, name_sort=='Mexico')
#borders_2 = subset(world, su_a3=='USA')[1,]
#borders = california
#borders = counties

#mask <- borders %>%
#st_bbox() %>%
#st_as_sfc() %>%
#st_buffer(5) %>%
#st_difference(borders) %>%
#st_as_sf()

#borders = subset(world, subregion=='Northern America')
#borders = subset(world, name_sort=='United States of America')
#mask1 = st_as_sf(st_difference(borders, bbox))
#mask2 = st_difference(bbox, st_union(st_combine(borders)))

#bbox = st_buffer(st_as_sfc(st_bbox(america)), 5)
#mask = st_difference(bbox, california)

borders = subset(world, su_a3=='USA')[1,]

bbox = st_buffer(st_as_sfc(st_bbox(california)), 5)
mask = st_as_sf(st_difference(bbox, borders))

spei_subset = subset(spei_proj, year == 2023 & month == 3)


ggplot(data = spei_proj) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
	scale_colour_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = 0, na.value = NA, limits = c(-2,2),
		breaks = c(-1.5, -.5, 1), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 12 month') +
#	scale_colour_gradient(low='#F2F3F3', high='#B91863') +
	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = spei_subset, size=6.4, shape=15, aes(col=spei_12)) +
#	ylim(-2,2) +
	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
	geom_sf(data = counties, fill = NA, color = '#666D74') +
	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = c(0.84, 0.74),
		legend.background = element_rect(fill='#F2F3F3', colour=NA)) +
	coord_sf(xlim = c(-125, -117), ylim = c(34, 43), expand = FALSE)
#	






lastYearSub
spei_sub = subset(spei_proj, year > 2020)


gganim = ggplot(data = spei_sub) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
	scale_colour_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = 0, na.value = NA, limits = c(-2,2),
		breaks = c(-1.5, -.5, 1), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 12 month') +
#	scale_colour_gradient(low='#F2F3F3', high='#B91863') +
	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = spei_sub, size=6.4, shape=15, aes(col=spei_12)) +
#	ylim(-2,2) +
	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
	geom_sf(data = counties, fill = NA, color = '#666D74') +
	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = c(0.84, 0.74),
		legend.background = element_rect(fill='#F2F3F3', colour=NA)) +
	coord_sf(xlim = c(-125, -117), ylim = c(34, 43), expand = FALSE) +
#	animation
  labs(title = ': {frame_time}') +
  transition_time(yearMonth) +
  ease_aes('linear')


anim_save("filenamehere.gif", gganim)

animate(gganim, duration = 20, fps = 20, width = 200, height = 400, renderer = gifski_renderer())
anim_save("output.gif")







spei_subset = subset(spei_proj, year == 2023 & month == 5)

dataIn = fread(dataInLoc)
theseBreaks = c(-1.3, -0.5, 0, 1)
isoHets = mapiso(x = spei_subset, var = 'spei_24',  crs = 'epsg:4326',
	breaks = c(-1.3, -0.5, 0, 1))


ggplot(data = isoHets) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = 0, na.value = NA, limits = c(-2,2),
		breaks = c(-1.5, -.5, 1), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 24 month') +
#	scale_colour_gradient(low='#F2F3F3', high='#B91863') +
	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
#	scale_fill_manual(name = 'Drought', 
#		values = c('Severe Drought' = 'red4', 'Minor Drought' = 'red2', 'Dry Conditions' = 'red1', 'Wet Conditions' = 'skyblue2'))	+
	geom_sf(data = isoHets, aes(fill=(isomin + isomax)/2)) +
#	ylim(-2,2) +
	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
	geom_sf(data = counties, fill = NA, color = 'grey70') +
	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = c(0.84, 0.74),
		legend.background = element_rect(fill='#F2F3F3', colour=NA)) +
	coord_sf(xlim = c(-125, -117), ylim = c(34, 43), expand = FALSE)
#	









sf_use_s2(FALSE)
spei_recon = spei_subset
st_crs(spei_recon) = st_crs(counties)
speiCounties <- st_join(counties, spei_recon, join = st_intersects)
st_crs(speiCounties) = st_crs(counties)

avg_spei = aggregate(spei_3 ~ ID, mean, data = speiCounties)
avgSpeiCounties = merge(avg_spei, counties, by = 'ID')
countySpei_sf = st_as_sf(avgSpeiCounties)

ggplot(data = avgSpeiCounties) +
#	scale_colour_viridis_c(alpha = 1) +
#	scale_colour_gradientn(colours=rev(c('#F2F3F3', '#F2F3F3', '#FFF2F2', '#FFE5E5', 'orangered1', 'red1','red3','firebrick')),name='drought') +
	scale_fill_gradient2(low='firebrick', mid='#DBDDDF', high='#196CE1', midpoint = 0, na.value = NA, limits = c(-2,2),
		breaks = c(-1.5, -.5, 1), labels = c('severe drought', 'drought', 'wet conditions'), name='SPEI 3 month') +
#	scale_colour_gradient(low='#F2F3F3', high='#B91863') +
	geom_sf(data = california, fill = '#DBDDDF', color = '#1A232F', size=1.4) +
	geom_sf(data = states, fill = '#DBDDDF')	+
#	geom_sf(data = counties, fill = NA, color = gray(.5))
#	geom_sf(data = world, fill = NA, color = gray(0.05), size=0.8) +
	geom_sf(data = countySpei_sf, size=6.4, shape=15, aes(fill=spei_3)) +
#	ylim(-2,2) +
	geom_sf(data = mask, fill='#F2F3F3', color='#F2F3F3') +
	geom_sf(data = counties, fill = NA, color = '#666D74') +
	geom_sf(data = states, fill = NA, color = '#1A232F', size=1.4) +
#	geom_sf(data = cityLocs_sf, size=4.4, stroke = 1.1, shape=21, col='white', fill='grey20')+ #c(rep('#039CE2',8), rep('#FDB600',7), '#23AF41')) +
	theme(panel.grid = element_line(colour='#F2F3F3')) +
	theme(panel.background = element_rect(fill = '#F2F3F3', colour='#F2F3F3')) +
	theme(legend.position = c(0.84, 0.74),
		legend.background = element_rect(fill='#F2F3F3', colour=NA)) +
	coord_sf(xlim = c(-125, -117), ylim = c(34, 43), expand = FALSE)
#	



















basemap(data = oceans, bathymetry = FALSE) +
    geom_sf() +
geom_sf(data = subset(thisHaz, Decade == '2020s'), size=2.1, shape=15, aes(col=plotDat)) +
 #   geom_sf(data = counties, fill = NA, color = gray(.5)) +
scale_colour_viridis_c(alpha = 1) +
geom_sf(data = cityLocs_sf, size=4, shape=24, fill='darkred') +
basemap(data = oceans, bathymetry =FALSE)

geom_sf(data = world, fill = NA, color = gray(0.75), size=2) +
theme(panel.background = element_rect(fill='grey90'),
panel.grid = element_line(colour=NA)) +
    coord_sf(xlim = c(-118, -85), ylim = c(14, 33), expand = FALSE)














thisHaz = subset(gwDat, Decade=='2020s' & Lon < -85 & Lon > -120 & Lat < 33 & Lat > 14)
thisHaz$plotDat = as.numeric(thisHaz$Q_50_indx)
thisHaz$plotDat = thisHaz$plotDat + abs(min(thisHaz$plotDat))
thisHaz$plotDat = 1 - thisHaz$plotDat / max(thisHaz$plotDat)

ggplot(data = world) +
    geom_sf() +
geom_sf(data = cityLocs_sf, size=4, shape=24, fill='darkred') +
geom_sf(data = subset(thisHaz, Decade == '2020s'), size=2.00, shape=15, aes(col=plotDat)) +
 #   geom_sf(data = counties, fill = NA, color = gray(.5)) +
scale_colour_viridis_c(trans='sqrt',alpha = 1) +
geom_sf(data = world, fill = NA, color = gray(0.75)) +
    coord_sf(xlim = c(-120, -85), ylim = c(14, 33), expand = FALSE)


	
	
	









































			$spei_1
			monthlyDF$lon = era_lons[thisLon]
				monthlyDF$lat = era_lats[thisLat]

				dataOutput = rbind(dataOutput, monthlyDF)
#				plot(spei_24)
				
		#}	soil t is generating data for water locations for some reason	
	lastHistData = last(era5ClimateInput$Date)
			seas5Rows = (1 + which(as.character(seas5ClimateInput[[1]]$Date) == as.character(lastHistData))):length(seas5ClimateInput[[1]]$Date)


			dailyPET = PET_fromTemp(yday(nc_dates), maxTemps, minTemps,
							lat_radians =  min((era_lats[thisLat]*pi/180), 1.1)) * 1000
			plot(dailyPET, col='blue')
			
	allPET = allPET + PET_fromTemp(yday(nc_date), theseTmax , theseTmin,
								lat_radians =  min(nc_lats[thisLat] * pi/180, 1.1)) * 1000  *  basinWatersheds$SUB_AREA[numberOfWatersheds]	# output in m, convert to mm






# Load data
data(wichita)
attach(wichita)

# One and tvelwe-months SPI
spi_1 <- spi(PRCP,1)
spi_12 <- spi(PRCP,12)
plot(cbind(spi_1,spi_12))
# Notice that the first eleven values of spei_12 are NA

# One and tvelwe-months SPEI
wichita$PET <- thornthwaite(TMED,37.6475)
spei1 <- spei(PRCP-wichita$PET, scale = 1, start = c(2000, 1), ref.start = c(1980, 1), ref.end = c(2010,12))
spei12 <- spei(wichita[,'BAL'], scale = 12, ref.start = c(1990, 1), ref.end = c(1995, 12))
plot(cbind(spei1,spei12))

# Data series not starting in January: define the properties
# of the time series with ts()
plot(spei(ts(PRCP-wichita$PET,freq=12,start=c(1900,6)),12))

# Different kernels
spei24 <- spei(PRCP-wichita$PET,24)
spei24_gau <- spei(PRCP-wichita$PET,24,kernel=list(type='gaussian',shift=0))
plot(ts(cbind(spei24,spei24_gau),start=c(1900,1),freq=12))

# Several time series at a time
data(balance)
names(balance)
bal_spei12 <- spei(balance,12)
colnames(bal_spei12) <- names(balance)
plot(ts(bal_spei12[,1:6],start=c(1900,1),freq=12),main='12-month SPEI')
plot(ts(bal_spei12[,7:11],start=c(1900,1),freq=12),main='12-month SPEI')

















