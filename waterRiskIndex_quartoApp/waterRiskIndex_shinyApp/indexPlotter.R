indexPlotter_f = function(waterIndexDataPlot = NULL, thisLoc = NULL, thisScen = NULL, indexValuesToPlot = NULL, thisValType = NULL) {
  par(mar=1*c(2.25,3.25,0.50,1.25), mgp=2*c(1.5,.6,0), mfrow=c(2,2), font.lab=1.6, bty='l', cex.lab=1.4, cex.axis=1.8, cex.main=1.4, col='#1A232F')
#  windowsFonts(A = windowsFont("Roboto"))
  nc_decade = seq(2010, 2090, 10)
  indexValues = c('Aridity Index - Avg', 'Local (avg)', 'Aridity Index - Drought', 'Local (drought)', 'Aridity Index w/ Irrigation - Avg', 'L + Regional (avg)', 'Aridity Index w/ Irrigation - Drought', 'L + Regional (drought)')
  for(thisIndexVal in indexValuesToPlot)	{
    # waterIndexDataPlot is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
    plotRange = c(min(min(waterIndexDataPlot[thisLoc, , , , indexValuesToPlot, thisValType])*1.025, 0.9),  max(max(waterIndexDataPlot[thisLoc, , , , indexValuesToPlot, thisValType])*1.025, 1.1))
    plot(nc_decade, waterIndexDataPlot[thisLoc, , 8, thisScen, thisIndexVal, thisValType],  
         ylim = plotRange, #log='y',
         type='l', lwd=1, col='white', xaxt = 'n', #log='y',
         main='', ylab='', xlab='',
         col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F')#,
#         family='A')
    abline(h=1, lwd=2, lty=1, col='#1A232F')
    abline(h=mean(waterIndexDataPlot[thisLoc, 1:2, 8, 1:3, thisIndexVal, thisValType]), lwd=2, lty =2, col='#1A232F')
    axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
         labels = nc_decade)
    #	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 1, thisScen, thisIndexVal, thisValType], rev(waterIndexDataPlot[thisLoc, , 7, thisScen, thisIndexVal, thisValType])),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 2, thisScen, thisIndexVal, thisValType], rev(waterIndexDataPlot[thisLoc, , 6, thisScen, thisIndexVal, thisValType])),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(waterIndexDataPlot[thisLoc, , 3, thisScen, thisIndexVal, thisValType], rev(waterIndexDataPlot[thisLoc, , 5, thisScen, thisIndexVal, thisValType])),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    loessSmooth = loess(waterIndexDataPlot[thisLoc, , 8, thisScen, thisIndexVal, thisValType] ~ nc_decade)
    #			lines(nc_decade, waterIndexDataPlot[thisLoc, , 17, thisScen, thisIndexVal], 
    #				col='#54575a', lwd=5)	#4cbfad
    lines(nc_decade, predict(loessSmooth),
          col='#EE6222', lwd=3)
    text(nc_decade[1], plotRange[1],  paste0(indexValues[thisIndexVal]), adj = c(0,0), cex=1.85)
    #			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
    #				col='#4cbfad', lwd=3) #015f6f
  }
}