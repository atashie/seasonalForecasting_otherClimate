gageRssFeed_f = function(customerInputTable = customerInputTable, userDataLocation = userDataLocation) {
#	secsPerDay = 60*60*24
	theDate = Sys.Date()
	theDay = lubridate::day(theDate)
	theMonth = lubridate::month(theDate)
	theYear = lubridate::year(theDate)
	myStart = Sys.time()
	for(thisRow in 1:nrow(customerInputTable))	{
		if(!is.na(customerInputTable$File_name[thisRow])) {
			recentObserved = jsonlite::fromJSON(paste0("https://api.water.noaa.gov/nwps/v1/gauges/",
				customerInputTable$NWS_Gage_Name[thisRow],
				"/stageflow/observed")
			)	
			if(recentObserved$primaryName != "Stage") { print("Warning!!! Feed is reporting ", recentObserved$primaryName)}
			recentValue = last(recentObserved$data$primary)

			histVals = data.table::fread(paste0(
				"C:/Users/arik/Documents/GitHub/RiverLevelMonitor_Simplot_Beta/Data/",
				customerInputTable$File_name[thisRow],
				".csv")
				)

			newVals = histVals[1,]
			newVals$Date = paste0(theMonth, "/", theDay, "/", theYear, " 8:00")
			newVals$Stage = recentValue
			concatVals = rbind(newVals, histVals)
			
			data.table::fwrite(concatVals, 
				paste0(
					userDataLocation,
					customerInputTable$File_name[thisRow],
					".csv"
				)
			)
		}
	}
	print(Sys.time() - myStart)
	###################### pulling in and processing USACE data
	#jj = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/obs/heea4.rss")
	#kk = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/fcst/heea4.rss")
	#ll = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/alert/heea4.rss")
}
		

