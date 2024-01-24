climatePlotter_f = function(climateDataPlot = NULL, thisLoc = NULL, thisScen = NULL)  {
  par(mar=1*c(2.25,3.25,0.50,2.25), mgp=2*c(1.5,.6,0), mfrow=c(3,2), font.lab=1.05, bty='l', cex.lab=2.75, cex.axis=1.5, cex.main=1.05, col='#1A232F')
#  windowsFonts(A = windowsFont("Roboto"))
  climVarNames = c('Precip (mm)', 'PET (mm)', 'GW Rech (mm)', 'Streamflow (km^2)', 'RZ Soil Moisture (%)', 'Total Storage (mm)')
  nc_decade = seq(2010, 2090, 10)
  
  for(thisClimVar in 1:length(climVarNames))	{
    # climateDataPlot is arranged as 	[location, decade, valueClass, scenario, climateVariable]
    
    currentAverage = mean(climateDataPlot[thisLoc, 1:2, 17, 1:3, thisClimVar])
    ylabPctVals = c(seq(-5,5,0.1))
    ylabPctValLocs = currentAverage + currentAverage * ylabPctVals
    
    yMin = max(min(climateDataPlot[thisLoc, , 14, , thisClimVar])*1, 0)#0.985
    yMax = max(climateDataPlot[thisLoc, , 20, , thisClimVar])*1.025
    plot(nc_decade, climateDataPlot[thisLoc, , 14, thisScen, thisClimVar],  ylim = c(yMin,yMax) ,
         type='l', lwd=1, col='white', xaxt = 'n', #log='y',
         main='', ylab='', xlab='',
         col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F')#,
#         family='A')
    abline(h=mean(climateDataPlot[thisLoc, 1:2, 13, thisScen, thisClimVar]), lwd=2, lty =2, col='#1A232F')
    axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
         labels = nc_decade)
    #			if(ylabPctValLocs[2] != 0)	{
    axis(4, at = ylabPctValLocs, col.lab='#1A232F', col.axis='#666D74', 
         labels = paste0(round(ylabPctVals * 100, 0), '%'))
    #			}
    #	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(climateDataPlot[thisLoc, , 14, thisScen, thisClimVar], rev(climateDataPlot[thisLoc, , 20, thisScen, thisClimVar])),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(climateDataPlot[thisLoc, , 19, thisScen, thisClimVar], rev(climateDataPlot[thisLoc, , 15, thisScen, thisClimVar])),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(climateDataPlot[thisLoc, , 18, thisScen, thisClimVar], rev(climateDataPlot[thisLoc, , 16, thisScen, thisClimVar])),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    loessSmooth = loess(climateDataPlot[thisLoc, , 13, thisScen, thisClimVar] ~ nc_decade)
    #			lines(nc_decade, climateDataPlot[thisLoc, , 17, thisScen, thisClimVar], 
    #				col='#54575a', lwd=5)	#4cbfad
    lines(nc_decade, predict(loessSmooth),
          col='#EE6222', lwd=3)
    text(nc_decade[1], yMin, climVarNames[thisClimVar], adj = c(0,0), cex=1.85)
    #			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
    #				col='#4cbfad', lwd=3) #015f6f
  }
}