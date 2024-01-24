gracePlotter_f = function(graceData=NULL){
  # water index deficits
  #  png(paste0(customerFolder_input, clientName_input, '\\',  customerTable_input[thisLoc, "Location_Name"], '_', "_GRACE-historicalGW.png"), width=900, height=600)
  par(mar=2*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.0, cex.axis=1.5*1.0, cex.main=1.5*1.0, col='#1A232F')
#  windowsFonts(A = windowsFont("Roboto"))
  
  
  plotRange = c(min(graceData$LWE_Depth_Median) - max(graceData$LWE_Depth_SD), max(graceData$LWE_Depth_Median) + max(graceData$LWE_Depth_SD))
  plot(graceData$Date, graceData$LWE_Depth_Median,  ylim = plotRange,
       type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
       main='', ylab='', xlab='',
       col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F')#,
#       family='A')
  abline(h=0, lwd=2, lty=1, col='#1A232F')
  #	axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
  #		labels = nc_decade)
  #	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
  polygon(x=c(graceData$Date, rev(graceData$Date)), y=c(graceData$LWE_Depth_Median + graceData$LWE_Depth_SD, rev(graceData$LWE_Depth_Median - graceData$LWE_Depth_SD)),
          col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
  polygon(x=c(graceData$Date, rev(graceData$Date)), y=c(graceData$LWE_Depth_Median + 2*graceData$LWE_Depth_SD, rev(graceData$LWE_Depth_Median - 2*graceData$LWE_Depth_SD)),
          col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
  polygon(x=c(graceData$Date, rev(graceData$Date)), y=c(graceData$LWE_Depth_Median + 4*graceData$LWE_Depth_SD, rev(graceData$LWE_Depth_Median - 4*graceData$LWE_Depth_SD)),
          col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
  #	loessSmooth = loess(graceData$LWE_Depth_Median ~ nc_time)
  linearTrend = lm(graceData$LWE_Depth_Median ~ graceData$Date)
  #	lines(nc_decade, waterIndexDataPlot[thisLoc, , 17, thisScen, thisIndexVal], 
  #		col='#54575a', lwd=5)	#4cbfad
  lines(graceData$Date, graceData$LWE_Depth_Median,
        col='#0098B2', lwd=1.5)
  lines(graceData$Date, predict(linearTrend),
        col='#EE6222', lwd=3)
  text(data.table::last(graceData$Date), 0, '2004-2010 avg', adj = c(1,-0.2), cex=1.25, col='#666D74')
#  text(data.table::first(graceData$Date), min(graceData$LWE_Depth_Median), paste0(round(linearTrend$coefficients[2]* 365, 0), ' mm per Year'), adj = c(0,0), cex=1.25, col ='#EE6222')
  #	lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
  #		col='#4cbfad', lwd=3) #015f6f
  #dev.off()
}
