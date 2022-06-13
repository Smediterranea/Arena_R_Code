
## Private Functions
TwoChoiceTracker.ProcessTwoChoiceTracker <- function(tracker) {
  if(is.null(tracker$ExpDesign)){
    stop("Two choice tracker requires experimental design.")
  }

  a<-"ObjectID" %in% colnames(tracker$ExpDesign)
  b<-"TrackingRegion" %in% colnames(tracker$ExpDesign)   
  c<-"CountingRegion" %in% colnames(tracker$ExpDesign)   
  d<-"Treatment" %in% colnames(tracker$ExpDesign)   
  e<-c(a,b,c,d)
  if(sum(e)<4){
    stop("Experimental design file requires ObjectID, TrackingRegion, CountingRegion, and Treatments columns.")
  }

  if(length(unique(tracker$ExpDesign$Treatment))!=2){
    stop("Two choice tracker requires exactly two treatments.")
  }  
  tracker <- TwoChoiceTracker.SetPIData(tracker)
  class(tracker) <- c("TwoChoiceTracker", class(tracker))
  tracker
}


TwoChoiceTracker.SetPIData<-function(tracker){
  rd<-Tracker.GetRawData(tracker)
  nm<-names(rd)  
  treatments<-unique(tracker$ExpDesign$Treatment)  
  
  if(length(treatments)!=2) {    
    stop("Wrong number of treatments!") 
  }

  tmp<-tracker$ExpDesign
  a<-rd$CountingRegion==tmp$CountingRegion[tmp$Treatment==treatments[1]]
  b<-rd$CountingRegion==tmp$CountingRegion[tmp$Treatment==treatments[2]]  
 
  pi<-a-b  
  
  rd<-data.frame(rd$Minutes,a,b,pi,rd$Indicator)
  names(rd)<-c("Minutes",treatments[1],treatments[2],"PI","Indicator")
  
  row.names(rd)<-NULL  
  tracker$PIData<-rd
  tracker
}

Summarize.TwoChoiceTracker<-function(tracker,range=c(0,0),ShowPlot=TRUE){  
  rd<-Tracker.GetRawData(tracker,range)  
  
  ## Now get the summary on the rest
  total.min<-rd$Minutes[nrow(rd)]-rd$Minutes[1]
  total.dist<-(rd$TotalDistance[nrow(rd)]-rd$TotalDistance[1])*tracker$Parameters$mmPerPixel  
  perc.Sleeping<-sum(rd$Sleeping)/length(rd$Sleeping)
  perc.Walking<-sum(rd$Walking)/length(rd$Walking)
  perc.MicroMoving<-sum(rd$MicroMoving)/length(rd$MicroMoving)
  perc.Resting<-sum(rd$Resting)/length(rd$Resting)
  
  avg.speed<-mean(rd$ModifiedSpeed_mm_s)
  
  treatments<-unique(tracker$ExpDesign$Treatment)    
  if(length(treatments)!=2) {    
    stop("Wrong number of treatments!") 
  }

  treatments<-c(treatments,"None")
  tmp<-tracker$ExpDesign
  a<-sum(rd$CountingRegion==tmp$CountingRegion[tmp$Treatment==treatments[1]])
  b<-sum(rd$CountingRegion==tmp$CountingRegion[tmp$Treatment==treatments[2]])
  c<-sum(rd$CountingRegion==treatments[3])
  
  r.tmp<-matrix(c(a,b,c),nrow=1)
  results<-data.frame(tracker$ID,total.min,total.dist,perc.Sleeping,perc.Walking,perc.MicroMoving,perc.Resting,avg.speed,range[1],range[2],r.tmp)
  names(results)<-c("ObjectID","TrackingRegion","ObsMinutes","TotalDist_mm","PercSleeping","PercWalking","PercMicroMoving","PercResting","AvgSpeed","StartMin","EndMin",treatments)
  
  if(ShowPlot){
    tmp<-data.frame(c(results$PercWalking,results$PercMicroMoving,results$PercResting,results$PercSleeping),rep("one",4), factor(c("Walking","MicroMoving","Resting","Sleeping")))
    names(tmp)<-c("a","b","Movement")
    print(qplot(x=b,y=a,data=tmp, fill=(Movement)) + geom_bar(stat="identity")+ xlab("Treatment") + ylab("Percentage")) 
  }
  
  results
}

## Public Functions
FinalPI.TwoChoiceTracker<-function(tracker,range=c(0,0)) {
  tmp<-GetPIData(tracker,range)
  n<-sum(tmp$PI)
  d<-sum(abs(tmp$PI))    
  if(d==0)
    result<-0
  else
    result<-n/d  
  result
}

CumulativePI.TwoChoiceTracker<-function(tracker,range=c(0,0)){
  
  d<-GetPIData(tracker,range)
  n<-cumsum(d$PI)
  den<-cumsum(abs(d$PI))
  y<-n/den
  y[den==0]<-0
  PI<-y  
  
  ## Since we know this function is only called for TwoChoiceTracker
  ## we can assume the column positions.
  
  regions<-c(colnames(d))
  regions<-regions[c(2,3)]
  
  CumPosA<-cumsum(d[,2])
  CumPosB<-cumsum(d[,3])
  
  result<-data.frame(d$Minutes,PI,CumPosA,CumPosB,d$Indicator)
  names(result)<-c("Minutes","CumPI",regions[1],regions[2],"Indicator")
  result
}

Plot.TwoChoiceTracker<-function(tracker,range=c(0,0)){
  PIPlots.TwoChoiceTracker(tracker,range)
}

PIPlots.TwoChoiceTracker<-function(tracker,range=c(0,0)){
  pd<-CumulativePI(tracker,range)
  nms<-names(pd)
  
  cumsums<-data.frame(c(pd[,1],pd[,1]),c(pd[,3],pd[,4]),rep(c(nms[3],nms[4]),c(length(pd[,1]),length(pd[,1]))),c(pd[,5],pd[,5]))
  names(cumsums)<-c("Minutes","CumSum","Treatment","Indicator")
  
  ymax<-max(pd[c(3,4)])
  x<-ggplot(cumsums) + 
    geom_rect(aes(xmin = Minutes, xmax = dplyr::lead(Minutes,default=0), ymin = -Inf, ymax = Inf, fill = factor(Indicator)), show.legend=F)+  
    scale_fill_manual(values = alpha(c("gray", "red"), .07)) +
    geom_point(aes(Minutes,CumSum,color=Treatment)) +
    geom_line(aes(Minutes,CumSum,color=Treatment)) +
    ggtitle(paste("Tracker:",tracker$Name, "   Treatment Counts",sep="")) +
    xlab("Minutes") + ylab("Frames") + ylim(0,ymax)
    
  y<-ggplot(pd) + 
    geom_rect(aes(xmin = Minutes, xmax = dplyr::lead(Minutes,default=0), ymin = -Inf, ymax = Inf, fill = factor(Indicator)), show.legend=F)+  
    scale_fill_manual(values = alpha(c("gray", "red"), .07)) +
    geom_point(aes(Minutes,CumPI)) +
    geom_line(aes(Minutes,CumPI)) +
    ggtitle(paste("Tracker:",tracker$Name, "   Cumulative PI",sep="")) +
    xlab("Minutes") + ylab("PI") + ylim(-1,1)
  
  print(x)
  print(y)
}

TimeDependentPIPlots.TwoChoiceTracker<-function(tracker,window.size.min=10,step.size.min=3,symbol.mult=5){

  ## Get earliest minute possible to avoid edge effects
  low<-floor(Tracker.FirstSampleData(tracker)$Minutes)+window.size.min
  ## Get latest minute possible
  high<-floor(Tracker.LastSampleData(tracker)$Minutes)
  tmp<-seq(low,high,by=step.size.min)
  results<-data.frame(matrix(rep(-99,(length(tmp)*5)),ncol=5))
  
  names(results)<-c("Minutes","PI","CumLicksA","CumLicksB","Indicator")
  
  for(i in 1:length(tmp)){
    results[i,1]<-tmp[i]
    
    
    pp<-CumulativePI(tracker,c(tmp[i]-window.size.min,tmp[i]))        
    ii<-nrow(pp)
    
    tmp.names<-colnames(pp)
    
    results[i,3]<-pp[ii,3]
    results[i,4]<-pp[ii,4]
    results[i,2]<-pp[ii,2]
    results[i,5]<-mean(pp[,5])
  }  
  
  tmp<-results[,3]+results[,4]
  max.tmp<-max(tmp)
  min.tmp<-min(tmp)
  Size<-tmp/max.tmp*symbol.mult  
  NObs<-tmp
  
  results<-data.frame(results,Size,NObs)
  
  x<-ggplot(results, aes(Minutes,PI,label=NObs))+
    geom_rect(aes(xmin = Minutes, xmax = dplyr::lead(Minutes,default=0), ymin = -Inf, ymax = Inf, fill = Indicator), alpha=0.1)+  
    scale_fill_continuous(name="Light",type = "viridis")+
    geom_line() +
    geom_point(size=Size,color=Size) +
    scale_colour_gradient2(name="Test") +
    geom_text(check_overlap = TRUE, vjust="inward",hjust="inward", color="red")+
    xlim(Tracker.FirstSampleData(tracker)$Minutes,Tracker.LastSampleData(tracker)$Minutes) +
    ylim(-1,1) +
    ggtitle(paste("ID:",tracker$Name,"    Time-Dependent PI"))
    
  print(x)
  results  
}

GetPIData.TwoChoiceTracker<-function(tracker,range=c(0,0)){
  pd<-tracker$PIData
  if(sum(range)!=0) {    
    pd<- pd[(pd$Minutes>range[1]) & (pd$Minutes<range[2]),]
  }
  if(tracker$Parameters$Filter.Sleep==TRUE)
    pd<-pd[pd$Sleeping==0,]
  if(tracker$Parameters$Filter.Tracker.Error==1)
    pd<-pd[pd$DataQuality=="High",]
  pd
}