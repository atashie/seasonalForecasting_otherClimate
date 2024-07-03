gracePlusProjectionsPlotter_f = function(theseGraceData, updatedProjectionsData) {

  par(mar=3*c(1.75,1.75,0.75,1.75), mgp=2*c(1.5,.6,0), mfrow=c(1,1), font.lab=1.5, bty='l', cex.lab=1.5*1.8, cex.axis=1.5*1.4, cex.main=1.5*1.8, col='#1A232F')
#  windowsFonts(A = windowsFont("Roboto"))
  
  whichCalDates = which(projectedData[[1]]$Date < max(graceTS$Date))
  calDates = projectedData[[1]]$Date[whichCalDates]
  calMtrx = thisVarMtrx[whichCalDates, ]
  thisYlab = 'Groundwater Storage (mm)'
  
  kmeansSmoothQ05 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.05, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
  kmeansSmoothQ95 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.95, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
  plot(calDates, apply(calMtrx, 1, mean),
       ylim=c(min(kmeansSmoothQ05), max(kmeansSmoothQ95)),
       type='l', lwd=1, col='white', #xaxt = 'n', #log='y',
       main='', ylab=thisYlab, xlab='',
       col.lab='#1A232F', col.axis='#666D74', col.main='#1A232F',
       family='A')
  
  polygon(x=c(calDates, rev(calDates)), 
          y=c(kmeansSmoothQ05, rev(kmeansSmoothQ95)),
          #col=adjustcolor('#0098B2', offset = c(0.95,0.95,0.95,0)), border=NA)
          col=adjustcolor('#0098B2', alpha=.1), border=NA)
  
  kmeansSmoothQ10 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.10, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
  kmeansSmoothQ90 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.90, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
  polygon(x=c(calDates, rev(calDates)), 
          y=c(kmeansSmoothQ10, rev(kmeansSmoothQ90)),
          #		col=adjustcolor('#0098B2', offset = c(0.9,0.9,0.9,0)), border=NA)
          col=adjustcolor('#0098B2', alpha=.1), border=NA)
  
  kmeansSmoothQ25 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.25, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
  kmeansSmoothQ75 = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) quantile(x, probs = 0.75, na.rm=TRUE)), bandwidth = 1, kernel = 'box')$y, 'extend')
  polygon(x=c(calDates, rev(calDates)), 
          y=c(kmeansSmoothQ25, rev(kmeansSmoothQ75)),
          #		col=adjustcolor('#0098B2', offset = c(0.8,0.8,0.8,0)), border=NA)
          col=adjustcolor('#0098B2', alpha=.1), border=NA)
  
  abline(h=0, lwd=3, lty=3, col='grey65')
  
  kmeansSmooth = zoo::na.fill(ksmooth(calDates, apply(calMtrx, 1, function(x) mean(x, na.rm=TRUE)) , bandwidth = 1, kernel = 'box')$y, 'extend')
  lines(calDates, kmeansSmooth,
        col='#0098B2', lwd=2)
  
  points(graceTS$Date, graceTS$Anomaly, 
         col='grey25', lwd=2, pch=1)
  lines(graceTS$Date, graceTS$Anomaly, 
        col='grey25', lwd=1, lty=1)
  
  usr <- par("usr")   # save old user/default/system coordinates
  par(usr = c(0, 1, 0, 1)) # new relative user coordinates
  text(x=0.02, y=0.15, 'Model Ensemble', col='#0098B2', cex=2.2, pos=4)
  text(x=0.02, y=0.08, 'Benchmark Data', col='grey25', cex=2.2, pos=4)
  par(usr = usr) # restore original user coordinates
  
  dev.off()
  
}

