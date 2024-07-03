locationPlotter_f = function(
    customerGageData = customerGageData,
    shippingLoc = as.numeric(shippingLoc),
    customerInputTable = customerInputTable){
  
  locationData = customerGageData[[shippingLoc]]
  customerName = customerInputTable$Location_Name[[shippingLoc]]
  plotData = locationData[-366,]
  annualData = locationData[366,]
  theDates = as.Date(plotData[,1] - 1, origin = paste0(lubridate::year(Sys.Date()), "-01-01"))

  yRange = range(plotData[,-1], na.rm=TRUE)
  
  # log y or not log y depending on whether it is Q or stage that is being reported
  if(customerInputTable$Use_USGS[shippingLoc]) {
    plot(theDates, plotData[,2],  ylim = yRange ,
         type='l', lwd=1, col='white', 
         log= 'y',
         #xaxt = 'n', 
         ylab='Streamflow (CFS)', xlab='', main=customerName,
         col.lab='#666D74', col.axis='#666D74', col.main='#1A232F')#,
    axis(4, at = annualData[,c(7,2,3,4,5,6,8)] ,col.lab='#EE6222', col.axis='#EE6222', 
         labels = names(annualData)[c(7,2,3,4,5,6,8)], col="#EE6222")
    #			if(ylabPctValLocs[2] != 0)	{
    #  axis(4, at = ylabPctValLocs, col.lab='#1A232F', col.axis='#666D74', 
    #       labels = paste0(round(ylabPctVals * 100, 0), '%'))
    #			}
    
    polygon(x=c(theDates, rev(theDates)), y=c(plotData[,7], rev(plotData[,8])),
            col=adjustcolor('#0098B2', alpha.f=0.05), border=NA)
    polygon(x=c(theDates, rev(theDates)), y=c(plotData[,2], rev(plotData[,6])),
            col=adjustcolor('#0098B2', alpha.f=0.05), border=NA)
    polygon(x=c(theDates, rev(theDates)), y=c(plotData[,3], rev(plotData[,5])),
            col=adjustcolor('#0098B2', alpha.f=0.05), border=NA)
    abline(h=annualData$min, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=1))
    abline(h=annualData$Q10, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.5))
    abline(h=annualData$Q25, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.2))
    abline(h=annualData$Q50, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.1))
    abline(h=annualData$Q75, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.05))
    abline(h=annualData$Q90, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.01))
    lines(x=theDates, y = plotData$lastYear2, col=adjustcolor('royalblue2', alpha.f=.5), lty=3, lwd=3)
    lines(x=theDates, y = plotData$lastYear, col=adjustcolor('royalblue3', alpha.f=.7), lwd=3, lty=2)
    lines(x=theDates, y = plotData$this, col=adjustcolor('royalblue4', alpha.f=1), lwd=3, lty=1)
    
  } else {
    plot(theDates, plotData[,2],  ylim = yRange ,
         type='l', lwd=1, col='white', 
         #log='y'.
         #xaxt = 'n', 
         ylab="River Stage (ft)", xlab='', main=customerName,
         col.lab='#666D74', col.axis='#666D74', col.main='#1A232F')#,
    axis(4, at = annualData[,c(7,2,3,4,5,6,8)] ,col.lab='#EE6222', col.axis='#EE6222', 
         labels = names(annualData)[c(7,2,3,4,5,6,8)], col="#EE6222")
    #			if(ylabPctValLocs[2] != 0)	{
    #  axis(4, at = ylabPctValLocs, col.lab='#1A232F', col.axis='#666D74', 
    #       labels = paste0(round(ylabPctVals * 100, 0), '%'))
    #			}
    
    polygon(x=c(theDates, rev(theDates)), y=c(plotData[,7], rev(plotData[,8])),
            col=adjustcolor('#0098B2', alpha.f=0.05), border=NA)
    polygon(x=c(theDates, rev(theDates)), y=c(plotData[,2], rev(plotData[,6])),
            col=adjustcolor('#0098B2', alpha.f=0.05), border=NA)
    polygon(x=c(theDates, rev(theDates)), y=c(plotData[,3], rev(plotData[,5])),
            col=adjustcolor('#0098B2', alpha.f=0.05), border=NA)
    abline(h=annualData$min, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=1))
    abline(h=annualData$Q10, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.5))
    abline(h=annualData$Q25, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.2))
    abline(h=annualData$Q50, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.1))
    abline(h=annualData$Q75, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.05))
    abline(h=annualData$Q90, lwd=2, lty =5, col=adjustcolor('#EE6222', alpha.f=.01))
    lines(x=theDates, y = plotData$lastYear2, col=adjustcolor('royalblue2', alpha.f=.5), lty=3, lwd=3)
    lines(x=theDates, y = plotData$lastYear, col=adjustcolor('royalblue3', alpha.f=.7), lwd=3, lty=2)
    lines(x=theDates, y = plotData$this, col=adjustcolor('royalblue4', alpha.f=1), lwd=3, lty=1)
    
  }
  
  
}

