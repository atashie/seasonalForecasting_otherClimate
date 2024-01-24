twoByTwoPlotter_f = function(basinSummary = NULL, customerInputTable = NULL, thisScen = NULL, thisStressClass = NULL)  {
  
  if(thisScen == 1){
    myTable2 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_B, 2),
      "Trajectory" =       round((basinSummary$trendLow_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable1 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_A, 2),
      "Trajectory" =       round((basinSummary$trendLow_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable4 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_D, 2),
      "Trajectory" =       round((basinSummary$trendLow_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable3 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_C, 2),
      "Trajectory" =       round((basinSummary$trendLow_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
  }

  if(thisScen == 2){
    myTable2 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_B, 2),
      "Trajectory" =       round((basinSummary$trendMed_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable1 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_A, 2),
      "Trajectory" =       round((basinSummary$trendMed_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable4 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_D, 2),
      "Trajectory" =       round((basinSummary$trendMed_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable3 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_C, 2),
      "Trajectory" =       round((basinSummary$trendMed_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
  } 
  
  if(thisScen == 3){
    myTable2 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_B, 2),
      "Trajectory" =       round((basinSummary$trendHigh_Deficit_B + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable1 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_A, 2),
      "Trajectory" =       round((basinSummary$trendHigh_Deficit_A + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable4 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_D, 2),
      "Trajectory" =       round((basinSummary$trendHigh_Deficit_D + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
    myTable3 = data.table::data.table(
      "Location" = customerInputTable$Location_Name, 
      "Current_Value" =        round(basinSummary$currentRatio_C, 2),
      "Trajectory" =       round((basinSummary$trendHigh_Deficit_C + basinSummary$thisWplant) / basinSummary$thisWplant, 2) - 1
    )
  }  

  # replacing trend nas with zeroes
  if(any(is.na(myTable2$Trajectory))) {myTable2$Trajectory[is.na(myTable2$Trajectory)] = 0}
  if(any(is.na(myTable1$Trajectory))) {myTable1$Trajectory[is.na(myTable1$Trajectory)] = 0}
  if(any(is.na(myTable4$Trajectory))) {myTable4$Trajectory[is.na(myTable4$Trajectory)] = 0}
  if(any(is.na(myTable3$Trajectory))) {myTable3$Trajectory[is.na(myTable3$Trajectory)] = 0}
  personal_theme = theme(
    panel.background = element_rect(fill = 'transparent'), panel.grid.major = element_line(color = 'grey80'), panel.grid.minor = element_line(color = 'grey90'),
    axis.title.x = element_blank(), axis.title.y = element_blank())
  myPlot2 = ggplot(myTable2) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=.1),fill='yellow3', alpha=.1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=-.1,ymax=0),fill='yellow3', alpha=.1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=-.1,ymax=0),fill='red4', alpha = .1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=.1),fill='blue4', alpha=.1) +
    geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
    geom_text_repel(data=myTable2, aes(x=Current_Value, y=Trajectory, label=Location),
                    max.overlaps=20,  color='grey20', segment.color='grey20') +
    xlim(0,2) +
    ylim(-.1,.1) +
    ggtitle("Local (drought)") +
    personal_theme
  myPlot1 = ggplot(myTable1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=.1),fill='yellow3', alpha=.1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=-.1,ymax=0),fill='yellow3', alpha=.1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=-.1,ymax=0),fill='red4', alpha = .1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=.1),fill='blue4', alpha=.1) +
    geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
    geom_text_repel(data=myTable1, aes(x=Current_Value, y=Trajectory, label=Location),
                    max.overlaps=20,  color='grey20', segment.color='grey20') +
    xlim(0,2) +
    ylim(-.1,.1) +
    ggtitle("Local (typical)") +
    personal_theme # + theme(plot.title=element_text(hjust=0.5))
  myPlot4 = ggplot(myTable4) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=.1),fill='yellow3', alpha=.1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=-.1,ymax=0),fill='yellow3', alpha=.1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=-.1,ymax=0),fill='red4', alpha = .1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=.1),fill='blue4', alpha=.1) +
    geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
    geom_text_repel(data=myTable4, aes(x=Current_Value, y=Trajectory, label=Location),
                    max.overlaps=20,  color='grey20', segment.color='grey20') +
    xlim(0,2) +
    ylim(-.1,.1) +
    ggtitle("L + Regional (drought)") +
    personal_theme # + theme(plot.title=element_text(hjust=0.5))
  myPlot3 = ggplot(myTable3) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=.1),fill='yellow3', alpha=.1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=-.1,ymax=0),fill='yellow3', alpha=.1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=-.1,ymax=0),fill='red4', alpha = .1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=.1),fill='blue4', alpha=.1) +
    geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
    geom_text_repel(data=myTable3, aes(x=Current_Value, y=Trajectory, label=Location),
                    max.overlaps=20,  color='grey20', segment.color='grey20') +
    xlim(0,2) +
    ylim(-.1,.1) +
    ggtitle("L + Regional (typical)") +
    personal_theme # + theme(plot.title=element_text(hjust=0.5))
  grid.arrange(myPlot2, myPlot1, myPlot4, myPlot3, ncol=2, bottom="Current Index Value", left = "Trajectory of Change (per decade)")
    
}
