twoByTwoPlotter_f = function(twoByTwoTables = NULL, whichLocs = NULL)  {

  locRange = seq(whichLocs[1], whichLocs[2])
  ylimRange = max(abs(range(twoByTwoTables$Trajectory)))
  # replacing trend nas with zeroes
  if(any(is.na(twoByTwoTables$Trajectory))) {twoByTwoTables$Trajectory[is.na(twoByTwoTables$Trajectory)] = 0}

  personal_theme = theme(
    panel.background = element_rect(fill = 'transparent'), panel.grid.major = element_line(color = 'grey80'), panel.grid.minor = element_line(color = 'grey90'),
    axis.title.x = element_blank(), axis.title.y = element_blank())
  myTable = subset(twoByTwoTables, waterClass == 2)[locRange]
  myPlot2 = ggplot(myTable) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=.1),fill='beige', alpha=.1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=-.1,ymax=0),fill='beige', alpha=.1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=-.1,ymax=0),fill='tomato', alpha = .1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=.1),fill='royalblue', alpha=.1) +
    geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
    geom_text_repel(data=myTable, aes(x=Current_Value, y=Trajectory, label=Location),
                    max.overlaps=50,  color='grey0', segment.color='grey20') +
    xlim(0,2) +
    ylim(-ylimRange,ylimRange) +
    ggtitle("Local (drought)") +
    personal_theme
  myTable = subset(twoByTwoTables, waterClass == 1)[locRange]
  myPlot1 = ggplot(myTable) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=.1),fill='beige', alpha=.1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=-.1,ymax=0),fill='beige', alpha=.1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=-.1,ymax=0),fill='tomato', alpha = .1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=.1),fill='royalblue', alpha=.1) +
    geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
    geom_text_repel(data=myTable, aes(x=Current_Value, y=Trajectory, label=Location),
                    max.overlaps=50,  color='grey0', segment.color='grey20') +
    xlim(0,2) +
    ylim(-ylimRange,ylimRange) +
    ggtitle("Local (typical)") +
    personal_theme # + theme(plot.title=element_text(hjust=0.5))
  myTable = subset(twoByTwoTables, waterClass == 4)[locRange]
  myPlot4 = ggplot(myTable) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=.1),fill='beige', alpha=.1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=-.1,ymax=0),fill='beige', alpha=.1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=-.1,ymax=0),fill='tomato', alpha = .1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=.1),fill='royalblue', alpha=.1) +
    geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
    geom_text_repel(data=myTable, aes(x=Current_Value, y=Trajectory, label=Location),
                    max.overlaps=50,  color='grey0', segment.color='grey20') +
    xlim(0,2) +
    ylim(-ylimRange,ylimRange) +
    ggtitle("Regional (drought)") +
    personal_theme # + theme(plot.title=element_text(hjust=0.5))
  myTable = subset(twoByTwoTables, waterClass == 3)[locRange]
  myPlot3 = ggplot(myTable) +
    geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=.1),fill='beige', alpha=.1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=-.1,ymax=0),fill='beige', alpha=.1) +
    geom_rect(aes(xmin=0,xmax=1,ymin=-.1,ymax=0),fill='tomato', alpha = .1) +
    geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=.1),fill='royalblue', alpha=.1) +
    geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
    geom_text_repel(data=myTable, aes(x=Current_Value, y=Trajectory, label=Location),
                    max.overlaps=50,  color='grey0', segment.color='grey20') +
    xlim(0,2) +
    ylim(-ylimRange,ylimRange) +
    ggtitle("Regional (typical)") +
    personal_theme # + theme(plot.title=element_text(hjust=0.5))
  grid.arrange(myPlot2, myPlot1, myPlot4, myPlot3, ncol=2, bottom="Current Index Value", left = "Trajectory of Change (per decade)")
    
}
