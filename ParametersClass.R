

ParametersClass=function(){
  Filter.Sleep=FALSE
  Filter.Tracker.Error=0
  Sleep.Threshold.Distance.mm=1
  Sleep.Threshold.Min=5
  mmPerPixel=0.056
  MicroMove.mm.sec<-c(0.2,2.0)
  Walking.mm.sec<-2.0
  Smooth.Speed.Data<-TRUE
  FPS=NA
  TType="ArenaTracker"
  tmp<-list(mmPerPixel=mmPerPixel,FPS=FPS, Smooth.Speed.Data=Smooth.Speed.Data,Filter.Sleep=Filter.Sleep,Filter.Tracker.Error=Filter.Tracker.Error,Sleep.Threshold.Min=Sleep.Threshold.Min,
            Sleep.Threshold.Distance.mm=Sleep.Threshold.Distance.mm,MicroMove.mm.sec=MicroMove.mm.sec,Walking.mm.sec=Walking.mm.sec,TType=TType)
  class(tmp)="ParametersObject"
  tmp
}



## change the initial values using this function
Parameters.SetParameter<-function(p,mmPerPixel=NA,Filter.Sleep=NA,Filter.Tracker.Error=NA, Sleep.Threshold.Min=NA, 
                                  Sleep.Threshold.Distance.mm=NA,MicroMove.mm.sec=NA,Walking.mm.sec=NA,TType=NA, Smooth.Speed.Data=NA, FPS=NA){
  tmp.O<-options()
  options(warn=-1)
  ## Change only those that are listed
  
  if(!is.na(FPS)) {
    p$FPS=FPS
  }  
  if(!is.na(Smooth.Speed.Data)) {
    p$Smooth.Speed.Data=Smooth.Speed.Data  
  }
  if(!is.na(Filter.Sleep)) {
    p$Filter.Sleep=Filter.Sleep  
  }
  if(!is.na(Filter.Tracker.Error)) {
    p$Filter.Tracker.Error=Filter.Tracker.Error
  }
  if(!is.na(Sleep.Threshold.Min)) {
    p$Sleep.Threshold.Min=Sleep.Threshold.Min
  }
  if(!is.na(Sleep.Threshold.Distance.mm)) {
    p$Sleep.Threshold.Distance.mm=Sleep.Threshold.Distance.mm
  }
  if(!is.na(Sleep.Threshold.Distance.mm)) {
    p$Sleep.Threshold.Distance.mm=Sleep.Threshold.Distance.mm
  }
  if(!is.na(mmPerPixel)) {
    p$mmPerPixel=mmPerPixel
  }
  if(!is.na(MicroMove.mm.sec)) {
    p$MicroMove.mm.sec=MicroMove.mm.sec
  }
  if(!is.na(Walking.mm.sec)) {
    p$Walking.mm.sec=Walking.mm.sec
  }
  if(!is.na(TType)){
    p$TType=TType 
  }
  options(tmp.O)
  p  
}

ParametersClass.XChoiceTracker=function(){
  p<-ParametersClass()
  p<-Parameters.SetParameter(p,TType="XChoiceTracker")
  p
}

ParametersClass.InteractionCounter=function(){
  p<-ParametersClass()
  p<-Parameters.SetParameter(p,TType="InteractionCounter")
  p
}

ParametersClass.TwoChoiceTracker=function(){
  p<-ParametersClass()
  p<-Parameters.SetParameter(p,TType="TwoChoiceTracker")
  p
}

ParametersClass.DDrop=function(){
  p<-ParametersClass()
  p<-Parameters.SetParameter(p,TType="DDropTracker")
  p$mmPerPixel<-0.079 # This is for the latest DDrop with BlackFly
  p$FPS<-NA
  p$Smooth.Speed.Data<-FALSE
  p$Filter.Sleep<-FALSE
  p$Filter.Tracker.Error<-0
  p
}

Parameters.AreParametersEqual<-function(p1,p2){
  result<-TRUE
  if(p1$Filter.Sleep!=p2$Filter.Sleep) {
    result<-FALSE
  }
  if(p1$mmPerPixel != p2$mmPerPixel) {
    result<-FALSE
  }
  if(p1$Filter.Tracker.Error!=p2$Filter.Tracker.Error) {
    result<-FALSE
  }
  if(p1$Sleep.Threshold.Distance.mm!=p2$Sleep.Threshold.Distance.mm) {
    result<-FALSE
  }
  if(p1$Sleep.Threshold.Min!=p2$Sleep.Threshold.Min) {
    result<-FALSE
  }
  if(p1$Walking.mm.sec!=p2$Walking.mm.sec) {
    result<-FALSE
  }
  if(p1$Smooth.Speed.Data!=p2$Smooth.Speed.Data) {
    result<-FALSE
  }
  if(p1$FPS!=p2$FPS) {
    result<-FALSE
  }
  if((p1$MicroMove.mm.sec[1] != p2$MicroMove.mm.sec[1]) ||(p1$MicroMove.mm.sec[2] != p2$MicroMove.mm.sec[2]))  {
    result<-FALSE
  }
  if(p1$TType != p2$TType){
    result<-false
  }
  result
}
