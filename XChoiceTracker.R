require(plyr)

XChoiceTracker.ProcessXTracker<-function(tracker){
  if(is.null(tracker$ExpDesign)){
    stop("Two choice tracker requires experimental design.")
  }
  
  a<-"ID" %in% colnames(tracker$ExpDesign)   
  b<-"Region" %in% colnames(tracker$ExpDesign)   
  c<-"Treatment" %in% colnames(tracker$ExpDesign)   
  d<-"PIMult" %in% colnames(tracker$ExpDesign)   
  e<-c(a,b,c,d)
  if(sum(e)<3){
    stop("Experimental design file requires ID, Region, PiMult, and Treatments columns.")
  }
 
  class(tracker)<-c("XChoiceTracker",class(tracker))
  tracker
}


PlotX.XChoiceTracker<-function(tracker,range = c(0,0)){  
  rd<-Tracker.GetRawData(tracker,range)
  tmp2<-rep("Moving",length(rd$X))
  tmp2[rd$Sleeping]<-"Sleeping"
  tmp2[rd$Resting]<-"Resting"
  tmp2[rd$MicroMoving]<-"Micromoving"
  Movement<-tmp2
  rd<-data.frame(rd,Movement)
  
  means <- ddply(rd, "Movement", summarise, Movement.mean = mean(RelX))
  xlims<-c(tracker$ROI[1]/-2,tracker$ROI[1]/2)*tracker$Parameters$mmPerPixel
  ggplot(rd, aes(x = Xpos_mm,fill=Movement)) + xlab("XPos (mm)")+
    geom_density(alpha = .3) + #alpha used for filling the density
    ggtitle(paste(" Tracker:",tracker$ID,sep="")) +
    geom_vline(data = means, aes(xintercept = Movement.mean,color=Movement),
               linetype = "longdash", size=1) + xlim(xlims)
}
