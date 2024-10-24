gageRssFeed_recentData_f = function(customerInputTable = customerInputTable, userDataLocation = userDataLocation) {
	#recentObserved = tidyRSS::tidyfeed(
	#	paste0("https://water.weather.gov/ahps2/rss/obs/",
	#	customerInputTable$NWS_Gage_Name[userDataLocation],
	#	".rss")
	#)
	#recentValue_ugh = unlist(strsplit(unlist(recentObserved)[13], "Latest Observation: "))[2]
	#recentValue = as.numeric(unlist(strsplit(recentValue_ugh, " ft"))[1])

  recentObserved = jsonlite::fromJSON(paste0("https://api.water.noaa.gov/nwps/v1/gauges/",
                                             customerInputTable$NWS_Gage_Name[userDataLocation],
                                             "/stageflow/observed")
  )	
  if(recentObserved$primaryName != "Stage") { print("Warning!!! Feed is reporting ", recentObserved$primaryName)}
  recentValue = last(recentObserved$data$primary)
  
	return(recentValue)
	###################### pulling in and processing USACE data
	#jj = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/obs/heea4.rss")
	#kk = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/fcst/heea4.rss")
	#ll = tidyRSS::tidyfeed("https://water.weather.gov/ahps2/rss/alert/heea4.rss")
}
		

