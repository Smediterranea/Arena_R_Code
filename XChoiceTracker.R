require(plyr)

XChoiceTracker.ProcessXTracker<-function(tracker){
  class(tracker)<-c("XChoiceTracker",class(tracker))
  tracker
}


PlotX.XChoiceTracker<-function(tracker,time = c(0,0)){  
  rd<-Tracker.GetRawData(tracker,time)
  tmp2<-rep("Moving",length(rd$X))
  tmp2[rd$Sleeping]<-"Sleeping"
  tmp2[rd$Resting]<-"Resting"
  tmp2[rd$MicroMoving]<-"Micromoving"
  Movement<-tmp2
  rd<-data.frame(rd,Movement)
  
  means <- ddply(rd, "Movement", summarise, Movement.mean = mean(RelX))
  ggplot(rd, aes(x = RelX,fill=Movement)) +
    geom_density(alpha = .3) + #alpha used for filling the density
    ggtitle(paste(" Tracker:",tracker$ID,sep="")) +
    geom_vline(data = means, aes(xintercept = Movement.mean,color=Movement),
               linetype = "longdash", size=1) + xlim(tracker$ROI[1]/-2,tracker$ROI[1]/2)
}


