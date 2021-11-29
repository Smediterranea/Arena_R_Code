
## Private Functions
TwoChoiceTracker.ProcessTwoChoiceTracker<-function(tracker){
  tracker<-TwoChoiceTracker.SetPIData(tracker)
  class(tracker)<-c("TwoChoiceTracker",class(tracker))
  tracker
}
TwoChoiceTracker.SetPIData<-function(tracker){
  rd<-Tracker.GetRawData(tracker)
  nm<-names(rd)
  #regions<-as.character(GetRegions(rd))
  regions<-tracker$CountingROI
  if(length(regions)!=2) {
    stop("Wrong number of regions!") 
  }
  a<-rd$Region==regions[1]
  b<-rd$Region==regions[2]
 
  pi<-a-b  
  
  rd<-data.frame(rd$Minutes,a,b,pi)
  names(rd)<-c("Minutes",regions[1],regions[2],"PI")
  
  row.names(rd)<-NULL  
  tracker$PIData<-rd
  tracker
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
  
  dd<-Tracker.GetRawData(tracker,range)
  
  ## Since we know this function is only called for TwoChoiceTracker
  ## we can assume the column positions.
  
  regions<-c(colnames(d))
  regions<-regions[c(2,3)]
  
  CumPosA<-cumsum(dd$Region==regions[1])
  CumPosB<-cumsum(dd$Region==regions[2])
  
  result<-data.frame(d$Minutes,PI,CumPosA,CumPosB)
  names(result)<-c("Minutes","CumPI",regions[1],regions[2])
  result
}

PIPlots.TwoChoiceTracker<-function(tracker,range=c(0,0)){
  pd<-CumulativePI(tracker,range)
  nms<-names(pd)
  regions<-colnames(pd)
  regions<-regions[c(3,4)]
  
  cumsums<-data.frame(c(pd[,1],pd[,1]),c(pd[,3],pd[,4]),rep(c(nms[3],nms[4]),c(length(pd[,1]),length(pd[,1]))))
  names(cumsums)<-c("Minutes","CumSum","Region")
  
  ymax<-max(pd[c(3,4)])
  x<-ggplot(cumsums) + 
    geom_point(aes(Minutes,CumSum,color=Region)) +
    geom_line(aes(Minutes,CumSum,color=Region)) +
    ggtitle(paste("Tracker:",tracker$ID, "   Region Counts",sep="")) +
    xlab("Minutes") + ylab("Frames") + ylim(0,ymax)
    
  y<-ggplot(pd) + 
    geom_point(aes(Minutes,CumPI)) +
    geom_line(aes(Minutes,CumPI)) +
    ggtitle(paste("Tracker:",tracker$ID, "   Cumulative PI",sep="")) +
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
  results<-data.frame(matrix(rep(-99,(length(tmp)*4)),ncol=4))
  
  names(results)<-c("Minutes","PI","CumLicksA","CumLicksB")
  
  for(i in 1:length(tmp)){
    results[i,1]<-tmp[i]
    
    pp<-CumulativePI(tracker,c(tmp[i]-window.size.min,tmp[i]))        
    ii<-nrow(pp)
    
    tmp.names<-colnames(pp)
    
    results[i,3]<-pp[ii,3]
    results[i,4]<-pp[ii,4]
    results[i,2]<-pp[ii,2]
  
  }  
  
  tmp<-results[,3]+results[,4]
  max.tmp<-max(tmp)
  min.tmp<-min(tmp)
  Size<-tmp/max.tmp*symbol.mult  
  NObs<-tmp
  
  results<-data.frame(results,Size,NObs)
  
  x<-ggplot(results, aes(Minutes,PI,color=Size,label=NObs))+
    geom_line() +
    geom_point(size=Size) +
    geom_text(check_overlap = TRUE, vjust="inward",hjust="inward", color="red")+
    xlim(Tracker.FirstSampleData(tracker)$Minutes,Tracker.LastSampleData(tracker)$Minutes) +
    ylim(-1,1) +
    scale_colour_gradient2() +
    ggtitle(paste("ID:",tracker$ID,"    Time-Dependent PI"))
    
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