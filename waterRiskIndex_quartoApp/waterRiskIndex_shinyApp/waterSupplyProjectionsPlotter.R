waterSupplyProjectionsPlotter_f = function(waterIndexDataPlot = NA, groundwaterAccess = NA, basinSummary = NA, thisLoc = NA, thisScen = NA) {

  par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')

  
  cultArea = basinSummary$thisFrcCultAreaWthIrr[thisLoc]
  kmToMm = (10^6)
  Q50_raw = waterIndexDataPlot[thisLoc, , 4, thisScen, 6, 2]
  
  streamflowRecharge = rep(0,10) 
  precipRecharge = rep(0,10)
  if(groundwaterAccess == 0)  {
    streamflowRecharge = c(0, rep(
          0.5 * kmToMm * (basinSummary$streamflowRechargeScalar[thisLoc] * basinSummary$currentStreamflow_avg[thisLoc] / basinSummary$SUB_AREA[thisLoc]),
          9))
    precipRecharge = c(0, rep(
          0.5 * basinSummary$currentPrecip_avg[thisLoc]* (1 - basinSummary$runoffRatio[thisLoc]),
          9))
  }
  totalRecharge = precipRecharge + streamflowRecharge 
    # rescaling for gw mounding
  rechScalarFunc = cumsum(c(0, Q50_raw) + totalRecharge)
  if(any(rechScalarFunc > 5000)){rechScalarFunc[rechScalarFunc > 5000] = 5000}
  if(any(rechScalarFunc < -5000)){rechScalarFunc[rechScalarFunc < -5000] = -5000}
  rechScalar = abs((rechScalarFunc + 5000) / 10000 - 1)
  if(any(rechScalar > 0.7)){rechScalar[rechScalar > .7] = 0.7}
  rechScaled = totalRecharge * rechScalar
  
     
  nc_decade =  seq(2010, 2100, 10)
  if(!is.na(basinSummary$marInmm[thisLoc])) {
    marAllotment = rep(basinSummary$marInmm[thisLoc], 10) * 10
  } else {marAllotment = rep(0, 10)}
    # rescaling for gw mounding
  marScalarFunc = cumsum(c(0, Q50_raw) + marAllotment)
  if(any(marScalarFunc > 5000)){marScalarFunc[marScalarFunc > 5000] = 5000}
  if(any(marScalarFunc < -5000)){marScalarFunc[marScalarFunc < -5000] = -5000}
  marScalar = abs((marScalarFunc + 5000) / 10000 - 1)
  if(any(marScalar > 0.7)){marScalar[marScalar > .7] = 0.7}
  marScaled = marAllotment * marScalar
  

  Q05 = 10 * (c(0, waterIndexDataPlot[thisLoc, , 1, thisScen, 6, 2]) + totalRecharge)
  Q10 = 10 * (c(0, waterIndexDataPlot[thisLoc, , 2, thisScen, 6, 2]) + totalRecharge)
  Q25 = 10 * (c(0, waterIndexDataPlot[thisLoc, , 3, thisScen, 6, 2]) + totalRecharge)
  Q50 = 10 * (c(0, waterIndexDataPlot[thisLoc, , 4, thisScen, 6, 2]) + totalRecharge)
  Q75 = 10 * (c(0, waterIndexDataPlot[thisLoc, , 5, thisScen, 6, 2]) + totalRecharge)
  Q90 = 10 * (c(0, waterIndexDataPlot[thisLoc, , 6, thisScen, 6, 2]) + totalRecharge)
  Q95 = 10 * (c(0, waterIndexDataPlot[thisLoc, , 7, thisScen, 6, 2]) + totalRecharge)
  Q_mn = 10 * (c(0, waterIndexDataPlot[thisLoc, , 8, thisScen, 6, 2]) + totalRecharge)
  
  
  
  
  
  
  # waterIndexPlot is in format [location, decade, indexValueQuant, scenario, indexValue, indexValueClass]
  plotRange = c(min(min(cumsum(Q05))*1.025, 0.9),  max(max(cumsum(Q95))*1.025, 1.1))
  plot(nc_decade, Q50,  
       ylim = plotRange, #log='y',
       type='l', lwd=1, col='white', xaxt = 'n', #log='y',
       main='', ylab='', xlab='',
       col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F')#,
    #         family='A')
    abline(h=1, lwd=2, lty=1, col='#1A232F')
 #   abline(h=mean(waterIndexPlot[thisLoc, 1:2, 8, 1:3, thisIndexVal, 2]), lwd=2, lty =2, col='#1A232F')
    axis(1, at = nc_decade ,col.lab='#1A232F', col.axis='#666D74', 
         labels = nc_decade)
    #	abline(v=fstOfMnths, lwd=1, col=adjustcolor('#666D74', alpha.f=0.1))
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(cumsum(Q05), rev(cumsum(Q95))) + c(marScaled, rev(marScaled)),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(cumsum(Q10), rev(cumsum(Q90))) + c(marScaled, rev(marScaled)),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    polygon(x=c(nc_decade, rev(nc_decade)), y=c(cumsum(Q25), rev(cumsum(Q75))) + c(marScaled, rev(marScaled)),
            col=adjustcolor('#0098B2', alpha.f=0.1), border=NA)
    loessSmooth = loess(cumsum(Q_mn + marScaled) ~ nc_decade)
    #			lines(nc_decade, waterIndexPlot[thisLoc, , 17, thisScen, thisIndexVal], 
    #				col='#54575a', lwd=5)	#4cbfad
    lines(nc_decade, predict(loessSmooth),
          col='#EE6222', lwd=3)
    text(nc_decade[1], plotRange[1], paste0("Potential Storage (mm)"), 
         adj = c(0,0), cex=1.85)
    #			lines(nc_decade, nc_testDat[thisLon, thisLat, , 1, 1], 
    #				col='#4cbfad', lwd=3) #015f6f
    
}

