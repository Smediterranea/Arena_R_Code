
GetCountingRegions<-function(data){
  if("CountingRegion" %in% colnames(data)){
    tmp<-unique(data$CountingRegion)
    r<-tmp[tmp!="None"]
  }
  else {
    r<-NA
  }
  r
}



ZeroDDropResults<-function(results){
  new.results<-results
  colsToZero<-c("SecFirstSeen","SecTo25","SecTo50","SecTo75","SecTo90")
  new.results[,colsToZero]<-new.results[,colsToZero] - new.results[,"SecFirstSeen"]
  new.results
}













## Type specific functions
Summarize<-function(tracker, ...) UseMethod("Summarize",tracker)
FinalPI<-function(tracker, ...) UseMethod("FinalPI",tracker)
CumulativePI<-function(tracker, ...) UseMethod("CumulativePI",tracker)
PIPlots<-function(tracker, ...) UseMethod("PIPlots",tracker)
TimeDependentPIPlots<-function(tracker, ...) UseMethod("TimeDependentPIPlots",tracker)
Summarize<-function(tracker, ...) UseMethod("Summarize",tracker)
PlotXY<-function(tracker, ...) UseMethod("PlotXY",tracker)
PlotX<-function(tracker, ...) UseMethod("PlotX",tracker)
PlotY<-function(tracker, ...) UseMethod("PlotY",tracker)
GetPIData<-function(tracker, ...) UseMethod("GetPIData",tracker)
AnalyzeTransitions<-function(tracker, ...) UseMethod("AnalyzeTransitions",tracker)
SmoothTransitions<-function(tracker, ...) UseMethod("SmoothTransitions",tracker)
ReportDuration<-function(tracker, ...) UseMethod("ReportDuration",tracker)