iceDat = data.table::fread('J:\\Downloads\\global_river_ice_dataset(1).csv')
iceDat$DoY = lubridate::yday(iceDat$date)

iceAvgOut = data.table::data.table(Row_num = NA, Path_num = NA,
	S_P025 = NA, S_P05 = NA, S_P10 = NA, S_P25 = NA, S_P50 = NA,
	E_P025 = NA, E_P05 = NA, E_P10 = NA, E_P25 = NA, E_P50 = NA)
	
allPaths = 1:max(unique(iceDat$PATH))
allRows = 1:max(unique(iceDat$ROW))
for(thisPath in allPaths)	{
	for(thisRow in allRows)	{
		iceDatSub = subset(iceDat, PATH == thisPath & ROW == thisRow)
		print(c(thisPath, thisRow))
		
		if(nrow(iceDatSub) > 50)	{
			iceDatSort = cbind(iceDatSub$DoY[order(iceDatSub$DoY)], iceDatSub$river_ice_fraction[order(iceDatSub$DoY)])
			
			allDoY = matrix(1:366, nrow=366, ncol=1)

			iceDatMrg = merge(iceDatSort, allDoY, all.y = TRUE)

			# accounting for hemispheres, w/ 60 and below being north of equator, and 60 and above being below the equator
			if(thisRow < 60)	{
	#			iceDatStack = rbind(iceDatMrg, iceDatMrg, iceDatMrg)
			
	#			iceDatStack[,3] = zoo::na.fill(iceDatStack[,2],'extend')
	#			loessXs = 1:nrow(iceDatStack)
	#			iceDatStack[,4] = loess(iceDatStack[,3] ~ loessXs)$fitted 
				iceDatMrg[,3] = zoo::na.fill(iceDatMrg[,2],'extend')
				iceDatMrg[c(162:252),3] = 0		# july 26 as hottest / coldest day of year 			iceDatStack[33,2] = 0		# july 26 as hottest / coldest day of year

				iceDatMrg[,4] = ksmooth(iceDatMrg[,1], iceDatMrg[,3],kernel='normal', bandwidth=30, n.points = nrow(iceDatMrg))$y
		
				iceAvgOut = rbind(iceAvgOut,
					data.table(Row_num = thisRow, Path_num = thisPath, 
											S_P025 =ifelse(any(iceDatMrg[1:207, 4] > 0.025), last(which(iceDatMrg[1:207, 4] > 0.025)), 1), 
											S_P05 = ifelse(any(iceDatMrg[1:207, 4] > 0.05), last(which(iceDatMrg[1:207, 4] > 0.05)), 1),
											S_P10 = ifelse(any(iceDatMrg[1:207, 4] > 0.10), last(which(iceDatMrg[1:207, 4] > 0.10)), 1),
											S_P25 = ifelse(any(iceDatMrg[1:207, 4] > 0.25), last(which(iceDatMrg[1:207, 4] > 0.25)), 1),
											S_P50 = ifelse(any(iceDatMrg[1:207, 4] > 0.50), last(which(iceDatMrg[1:207, 4] > 0.50)), 1),
											E_P025 =ifelse(any(iceDatMrg[207:366, 4] > 0.025), 206 + first(which(iceDatMrg[207:366, 4] > 0.025)), 366),
											E_P05 = ifelse(any(iceDatMrg[207:366, 4] > 0.05), 206 + first(which(iceDatMrg[207:366, 4] > 0.05)), 366),
											E_P10 = ifelse(any(iceDatMrg[207:366, 4] > 0.10), 206 + first(which(iceDatMrg[207:366, 4] > 0.10)), 366),
											E_P25 = ifelse(any(iceDatMrg[207:366, 4] > 0.25), 206 + first(which(iceDatMrg[207:366, 4] > 0.25)), 366),
											E_P50 = ifelse(any(iceDatMrg[207:366, 4] > 0.50), 206 + first(which(iceDatMrg[207:366, 4] > 0.50)), 366)))
			} else {
				iceDatMrg[,3] = zoo::na.fill(iceDatMrg[,2],'extend')
				iceDatMrg[c(1:90),3] = 0		# july 26 as hottest / coldest day of year 			iceDatStack[33,2] = 0		# july 26 as hottest / coldest day of year

				iceDatMrg[,4] = ksmooth(iceDatMrg[,1], iceDatMrg[,3],kernel='normal', bandwidth=30, n.points = nrow(iceDatMrg))$y
		
				iceAvgOut = rbind(iceAvgOut,
					data.table(Row_num = thisRow, Path_num = thisPath, 
											S_P025 = ifelse(any(iceDatMrg[1:207, 4] > 0.025), first(which(iceDatMrg[1:207, 4] > 0.025)), 207), 
											S_P05 = ifelse(any(iceDatMrg[1:207, 4] > 0.05), first(which(iceDatMrg[1:207, 4] > 0.05)), 207),
											S_P10 = ifelse(any(iceDatMrg[1:207, 4] > 0.10), first(which(iceDatMrg[1:207, 4] > 0.10)), 207),
											S_P25 = ifelse(any(iceDatMrg[1:207, 4] > 0.25), first(which(iceDatMrg[1:207, 4] > 0.25)), 207),
											S_P50 = ifelse(any(iceDatMrg[1:207, 4] > 0.50), first(which(iceDatMrg[1:207, 4] > 0.50)), 207),
											E_P025 =ifelse(any(iceDatMrg[207:366, 4] > 0.025), 206 + last(which(iceDatMrg[207:366, 4] > 0.025)), 207),
											E_P05 = ifelse(any(iceDatMrg[207:366, 4] > 0.05), 206 + last(which(iceDatMrg[207:366, 4] > 0.05)), 207),
											E_P10 = ifelse(any(iceDatMrg[207:366, 4] > 0.10), 206 + last(which(iceDatMrg[207:366, 4] > 0.10)), 207),
											E_P25 = ifelse(any(iceDatMrg[207:366, 4] > 0.25), 206 + last(which(iceDatMrg[207:366, 4] > 0.25)), 207),
											E_P50 = ifelse(any(iceDatMrg[207:366, 4] > 0.50), 206 + last(which(iceDatMrg[207:366, 4] > 0.50)), 207)))
			}
		} else	{
			iceAvgOut = rbind(iceAvgOut,
				data.table(Row_num = thisRow, Path_num = thisPath, 
										S_P025 = NA, 
										S_P05 = NA,
										S_P10 = NA,
										S_P25 = NA,
										S_P50 = NA,
										E_P025 = NA,
										E_P05 = NA,
										E_P10 = NA,
										E_P25 = NA,
										E_P50 = NA))
		}
	}
}	

fwrite(iceAvgOut, 'J:\\Downloads\\globRivIce_AvgDoYs.csv')

