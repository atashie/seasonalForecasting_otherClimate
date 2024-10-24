############################################
## this is the only section that should change when processing new data

# path to new data
dataPath = "C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\seasonalHurricaneForecast_beta\\"
unformattedCsv = TRUE # boolean to trigger reformatting to standard out
	
seasonName = "Oct01-Dec31"				# ensure this name exactly matches the seasonalFilter column in "basinLevelValues.csv"	# previous iterations: JJASON Aug10-Nov30 Sep1-Dec31 Sep20-Nov30  01-Oct Oct01-Dec31 Oct21-Dec31
basinName = "north_atlantic"		# ensure this name exactly matches the basin column in "basinLevelValues.csv"  # previous iterations north_atlantic western_pacific indian_ocean

## end of section that needs to be updated
############################################
#library(raster)
library(terra)
library(data.table)
library(sf)

# not sure why, but we want to set a minimum risk for impacts else set to NA
minThresholdImpact = 0.1


# read in shapfile for masking
land_shp = sf::st_read("C:\\Users\\arik\\Documents\\GitHub\\seasonalForecasting_otherClimate_\\seasonalHurricaneForecast_beta\\ne_10m_land.shp")
#land_sp <- as(land_shp, "Spatial")# Convert sf object to Spatial for compatibility with raster package
land_sp <- vect(land_shp)  # Convert sf object to SpatVector


# read in hurricane data
fileName = paste0(basinName, "_", seasonName, ".csv")
if(unformattedCsv){															  # triggers when data is not pre-formatted
	new_df = data.table::fread(paste0(dataPath, "rawCsvs/", fileName), skip=1) # number of columns is not consistently the same as the number of column names
	if(basinName == "north_atlantic"){
		new_df = subset(new_df, V2 >= -102); print("subsetting df"); print(new_df)
	}
	summaryOutput_df = data.table(Lat = NULL, Lon = NULL, Season = NULL, Ocean_Basin = NULL, Cat = NULL, Climatology = NULL, Forecast = NULL)
	thisCat = 0
	while(TRUE){
		climCol = 3 + thisCat * 2; foreCol = climCol + 1
		if(foreCol > ncol(new_df)) {break}
		newClim = unlist(new_df[,..climCol])
		newFore = unlist(new_df[,..foreCol])
		summaryOutput_df = rbind(summaryOutput_df, 
			data.table(Lat = unlist(new_df[,1]), Lon = unlist(new_df[,2]), Season = seasonName, Ocean_Basin = basinName, Cat=paste0("Cat-",thisCat), Climatology = newClim, Forecast = newFore)
		)
		thisCat = thisCat + 1
	}
} else {
summaryOutput_df = data.table::fread(paste0(dataPath, "rawCsvs/", fileName)) # triggers when data is pre-formatted 
}		
	



# mask low values
summaryOutput_df[, uniqueLocs := paste0("Loc_", Lat, "_", Lon)]# Create a unique location identifier
locations_to_mask <- summaryOutput_df[Cat == "Cat-0" & Climatology < minThresholdImpact & Forecast < minThresholdImpact, unique(uniqueLocs)]# Identify locations that meet the conditions
summaryOutput_df[uniqueLocs %in% locations_to_mask, `:=`(Climatology = NA_real_, Forecast = NA_real_)]# Set Climatology and Forecast to NA for all rows with these locations
summaryOutput_df$diffVals = summaryOutput_df$Forecast - summaryOutput_df$Climatology

rasterList_abs = list()
rasterList_clm = list()
rasterList_dif = list()

for(thisCat in unique(summaryOutput_df$Cat))	{
	summaryOutput_df_thisCat = subset(summaryOutput_df, Cat == thisCat)
	summaryOutput_rs_abs = terra::rast(raster::rasterFromXYZ(data.table::data.table(lon = summaryOutput_df_thisCat$Lon, lat = summaryOutput_df_thisCat$Lat, rasVal = summaryOutput_df_thisCat$Forecast), crs = "+proj=longlat +datum=WGS84 +no_defs"))  # Assigning WGS84 CRS
	summaryOutput_rs_clm = terra::rast(raster::rasterFromXYZ(data.table::data.table(lon = summaryOutput_df_thisCat$Lon, lat = summaryOutput_df_thisCat$Lat, rasVal = summaryOutput_df_thisCat$Climatology), crs = "+proj=longlat +datum=WGS84 +no_defs"))  # Assigning WGS84 CRS
	summaryOutput_rs_dif = terra::rast(raster::rasterFromXYZ(data.table::data.table(lon = summaryOutput_df_thisCat$Lon, lat = summaryOutput_df_thisCat$Lat, rasVal = summaryOutput_df_thisCat$diffVals), crs = "+proj=longlat +datum=WGS84 +no_defs"))

	# Mask the raster using the shapefile's geometry
	summaryOutput_rs_abs_clip <- raster::raster(terra::mask(summaryOutput_rs_abs, land_sp, touches = TRUE))
	summaryOutput_rs_clm_clip <- raster::raster(terra::mask(summaryOutput_rs_clm, land_sp, touches = TRUE))
	summaryOutput_rs_dif_clip <- raster::raster(terra::mask(summaryOutput_rs_dif, land_sp, touches = TRUE))

	rasterList_abs[[thisCat]] = summaryOutput_rs_abs_clip
	rasterList_clm[[thisCat]] = summaryOutput_rs_clm_clip
	rasterList_dif[[thisCat]] = summaryOutput_rs_dif_clip
}

summaryOutput_brick_abs = raster::brick(rasterList_abs)
summaryOutput_brick_clm = raster::brick(rasterList_clm)
summaryOutput_brick_dif = raster::brick(rasterList_dif)
raster::writeRaster(summaryOutput_brick_abs, filename=paste0(dataPath, "tiffOuts\\",basinName, "_", seasonName, "_abs.tif"), format = "GTiff", datatype="FLT4S", overwrite=TRUE)
raster::writeRaster(summaryOutput_brick_clm, filename=paste0(dataPath, "tiffOuts\\",basinName, "_", seasonName, "_clm.tif"), format = "GTiff", datatype="FLT4S", overwrite=TRUE)
raster::writeRaster(summaryOutput_brick_dif, filename=paste0(dataPath, "tiffOuts\\",basinName, "_", seasonName, "_dif.tif"), format = "GTiff", datatype="FLT4S", overwrite=TRUE)



########################################################################################
# subsequent code is for manually assessing outputs; should not be necessary to run once the dashboard is up

mydat = raster::brick(paste0(dataPath, "tiffOuts\\",basinName, "_", seasonName, "_abs.tif"))#paste0(dataPath, whichSeason, "_season_abs.tif"), format = "GTiff")

# calculating province level avgs
#thisCountry = sf::st_read("C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/regionalSoilMoisture_shinyApp/regionalSoilMoistureForecast/STE_2021_AUST_GDA2020.shp")
#thisCountry = thisCountry[!sf::st_is_empty(thisCountry),]
#thisCountry = sf::st_cast(thisCountry, "POLYGON")
#thisCountry = sf::st_simplify(thisCountry, dTolerance = 5000)
#thisCountry = sf::st_transform(thisCountry, crs="+proj=longlat +datum=WGS84")


#for(thisMonth in 1:length(names(summaryOutput_brick_dif)))	{
#	extractedValues_dif = raster::extract(summaryOutput_brick_dif[[thisMonth]], thisCountry)
#	extractedValues_abs = raster::extract(summaryOutput_brick_abs[[thisMonth]], thisCountry)
#
#	avgValues_dif <- sapply(extractedValues_dif, function(x) mean(x, na.rm = TRUE))
#	avgValues_abs <- sapply(extractedValues_abs, function(x) mean(x, na.rm = TRUE))

#	thisCountry[, paste0("dif_", thisMonth)] = avgValues_dif
#	thisCountry[, paste0("abs_", thisMonth)] = avgValues_abs
#}
#sf::st_write(thisCountry, "C:/Users/arik/Documents/GitHub/seasonalForecasting_otherClimate_/regionalSoilMoisture_shinyApp/regionalSoilMoistureForecast/provinceLevelForecast.shp", append=FALSE)

### basic plotting of the data for qa
library(ggplot2)
library(viridis) 


thisCat = "Cat-0"
thisIndex = 1
#ggplot(subset(summaryOutput_df, Cat == thisCat), aes(x=Lon, y=Lat)) + geom_point(aes(colour=(Forecast - Climatology))) + scale_colour_gradient2()
mapview::mapview(mydat[[thisIndex]])

ggplot(subset(summaryOutput_df, monthsOut == 0), aes(x=Lon, y=Lat)) + geom_point(aes(colour=(projectedQ50))) + scale_colour_gradient2()
mapview::mapview(mydat[[thisIndex]])



