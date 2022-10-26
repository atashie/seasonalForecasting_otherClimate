###########################################################
### forecasting SM
library(ncdf4)

# download new soil moisture data from https://awo.bom.gov.au/products/historical/soilMoisture-rootZone/6,-33.560,153.062/nat,-34.146,116.135/r/d/2022-06-06

####################################################################################################
##	define all names, file locations, and variables
	# names and variables
dataOrigination = c('ERA5', 'CFS', 'SEAS5')
dataPath = 'J:\\Cai_data\\Advanta\\SoilMoisture\\'
era5RecentDataName = 'AdvantaAusSW-testing-recent-era.nc'
smClimatologyDataName = 'testing-climatology-era.nc'
smClimatologyDataYears = 2017:2022
cfsDataName = 'AdvantaAusSW-testing-cfs.nc'
seas5DataName = 'AdvantaAusSW-testing-seas5.nc'
#startDateEra5 = '2022-07-01'
startDateCfs = '2022-10-16'
startDateSeas5 = '2022-10-01'
saveDate = '2022-10-26'
seas5Models = 1:51
cfsModels = 1:4#seq(1, 43, 5)
rootDepth = 1000 


	# define the 'sortable' variables
userName = 'Advanta'
forecastDate = ncvar_get(nc_open(paste0(dataPath, cfsDataName)), 'lead_time')[1] + as.Date(startDateCfs) 
	# test last seas5
assessForecastDateSeas5 = ncvar_get(nc_open(paste0(dataPath, seas5DataName)), 'lead_time')[1] + as.Date(startDateSeas5) 


soilMoistureOutput = f_projectedSoilMoisture(
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
	saveDate = saveDate)



summaryOutput_df = fread(paste0(dataPath, "SoilMoisture_projectionOutput_", saveDate, ".csv"))
library(ggplot2)
library(viridis) 

ggplot(subset(summaryOutput_df, monthsOut == 0), aes(x=Lon, y=Lat)) +
	geom_point(aes(colour=(projectedQ50 - climatologyQ50))) +
	scale_colour_gradient2()

ggplot(subset(summaryOutput_df, monthsOut == 0), aes(x=Lon, y=Lat)) + geom_point(aes(colour=(projectedQ50))) + scale_colour_gradient2()

