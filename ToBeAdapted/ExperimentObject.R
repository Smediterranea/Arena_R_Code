source("ChamberObject.R")
source("ParametersClass.R")
require(ggplot2)



Summarize<-function(experiment, ...) UseMethod("Summarize",experiment)
SummarizeXMeans<-function(experiment, ...) UseMethod("SummarizeXMeans",experiment)
SummarizeQuartile<-function(experiment, ...) UseMethod("SummarizeQuartile",experiment)
Analyze<-function(experiment, ...) UseMethod("Analyze",experiment)

ExperimentClass<-function(parameters,expdesign.fn){
  exp<-list(Chambers=chambers)
  expdesign<-Experiment.GetExpDesign(expdesign.fn)
  exp[["ExpDesign"]]<-expdesign
  for(i in chambers){
    tmp <-ChamberClass.Camera(i,parameters)        
    if(!is.null(tmp)){
      tmp2<-paste("Chamber",i,sep="")
      exp[[tmp2]]<-tmp
    }
  }  
  if(p$TType=="TwoChoiceTracker"){
    class(exp)<-c("TwoChoiceExperiment","Experiment")
  }
  else if(p$TType=="XChoiceTracker"){
    class(exp)<-c("XChoiceExperiment","Experiment")
  }
  else {
    class(exp)="Experiment"
  }
  exp
}
Experiment.GetChamber<-function(experiment,chamber){
  s<-paste("Chamber",chamber,sep="")
  experiment[[s]]
}
Experiment.GetExpDesign<-function(filename){
  d<-read.csv(filename,header=TRUE)
  d$Region<-factor(d$Region)
  d$Treatment<-factor(d$Treatment)
  d  
}
Experiment.ChangeParameterObject<-function(experiment,newP) {  
  for(i in experiment$Cameras){
    s<-paste("Chamber",i,sep="")    
    tmp<-Experiment.GetChamber(experiment,i)
    tmp<-Chamber.ChangeParameterObject(tmp,newP)
    experiment[[s]]<-tmp
  }  
  experiment
}




#### "Private Functions ####
Experiment.GetRegionForTreatment<-function(expdesign,chamber, id, trt){
  result<-"NA"
  for(i in 1:nrow(expdesign)){
    tc<-expdesign$Chamber[i]
    tid<-expdesign$ID[i]
    ttrt<-expdesign$Treatment[i]
    if(chamber==tc && id==tid && trt==ttrt)
      result<-expdesign$Region[i]    
  }
  as.character(result)
}
Experiment.GetTreatment<-function(expdesign,chamber,id){
  result<-"NA"
  for(i in 1:nrow(expdesign)){
    tc<-expdesign$Chamber[i]
    tid<-expdesign$ID[i]
    if(chamber==tc && id==tid)
      result<-expdesign$Treatment[i]    
  }
  as.character(result)
}
Experiment.GetPIMultiplier<-function(expdesign,chamber,id){
  result<-"NA"
  for(i in 1:nrow(expdesign)){
    tc<-expdesign$Chamber[i]
    tid<-expdesign$ID[i]
    if(chamber==tc && id==tid)
      result<-expdesign$PIMult[i]    
  }
  result
}
Experiment.Chamber.Summarize<-function(experiment,chamber,time=c(0,0),include.none=TRUE){
  trts<-levels(experiment$ExpDesign$Treatment)
  chambernum<-chamber
  chamber<-Experiment.GetChamber(experiment,chambernum)
  expsummary<-Summarize(chamber,time,ShowPlot=FALSE)
 
  if("XChoiceExperiment" %in% class(experiment)){
    Treatment<-rep(NA,nrow(expsummary))
    for(j in 1:nrow(expsummary)){
      Treatment[j]<-Experiment.GetTreatment(experiment$ExpDesign,expsummary$Chamber[j],expsummary$ID[j])
    }
    final.results<-data.frame(Treatment,expsummary)
  }
  else if("TwoChoiceExperiment" %in% class(experiment)){
    results<-matrix(rep(-1,length(trts)*nrow(expsummary)),ncol=length(trts))
    results<-data.frame(results)
    names(results)<-trts
      for(j in 1:length(trts)){
        currtrt<-trts[j]
        for(i in 1:nrow(results)) {
          reg<-Experiment.GetRegionForTreatment(experiment$ExpDesign,expsummary$Chamber[i],expsummary$ID[i],currtrt)
          if(reg %in% names(expsummary)){
            tmp<-expsummary[reg]
            results[i,j]<-tmp[i,]
          }
          else {
            results[i,j]<-0
          }    
        }
      }
    final.results<-data.frame(expsummary,results)
  }
  final.results
}
Experiment.Chamber.GetXMeans<-function(experiment,chamber,time=c(0,0)){
  chambernum<-chamber
  chamber<-Experiment.GetChamber(experiment,chambernum)
  expsummary<-GetMeanXPositions.Chamber(chamber,time)
  
  if("XChoiceExperiment" %in% class(experiment)){
    Treatment<-rep(NA,nrow(expsummary))
    for(j in 1:nrow(expsummary)){
      Treatment[j]<-Experiment.GetTreatment(experiment$ExpDesign,expsummary$Chamber[j],expsummary$ID[j])
      tmp<-Experiment.GetPIMultiplier(experiment$ExpDesign,expsummary$Chamber[j],expsummary$ID[j])
      expsummary[j,3:7]<-expsummary[j,3:7]*tmp
    }
    final.results<-data.frame(Treatment,expsummary)
  }
  final.results
}
## NOTE:This function doesn't work because it doesn't correct properly for the PI multiplier.
Experiment.Chamber.GetQuartile<-function(experiment,chamber,quartile=3,time=c(0,0)){
  chambernum<-chamber
  chamber<-Experiment.GetChamber(experiment,chambernum)
  expsummary<-GetQuartileXPositions.Chamber(chamber,quartile,time)
  
  if("XChoiceExperiment" %in% class(experiment)){
    Treatment<-rep(NA,nrow(expsummary))
    for(j in 1:nrow(expsummary)){
      Treatment[j]<-Experiment.GetTreatment(experiment$ExpDesign,expsummary$Chamber[j],expsummary$ID[j])
      tmp<-Experiment.GetPIMultiplier(experiment$ExpDesign,expsummary$Chamber[j],expsummary$ID[j])
      expsummary[j,3:7]<-expsummary[j,3:7]*tmp
    }
    final.results<-data.frame(Treatment,expsummary)
  }
  final.results
}

#### Public Functions ####
Summarize.XChoiceExperiment<-function(experiment,time=c(0,0),RemoveRegionData=TRUE){
  ## First output all of the plots
  pdf("Experiment.pdf",paper="USr", width=7.75, height=10)
  par(mfrow=c(6,4))
  for(i in experiment$Chambers){
    tmp<-Experiment.GetChamber(experiment,i)
    ## Plot the chamber
    PlotXY.Chamber(tmp,time,FALSE)
    PlotX2.Chamber(tmp,time,FALSE) 
    PlotX(tmp,time,FALSE)
    
    ## Summarize the chamber
    tmp<-Experiment.Chamber.Summarize(experiment,i,time)
    if(exists("results",inherits=FALSE)==FALSE){
      results<-tmp
   }
    else
      results<-rbind(results,tmp)
  }
  tmp.result<-results[,c("Chamber","ID","PercSleeping","PercWalking","PercMicroMoving","PercResting")]
  tmp.result1<-melt(tmp.result,id.var=c("Chamber","ID"))
  names(tmp.result1)<-c("Chamber","ID","Type","value")
  
  print(ggplot(tmp.result1, aes(x = ID, y = value, fill = Type))+ 
          geom_bar(stat = "identity")+ggtitle(paste("Experiment Movement For Each Chamber")) +
          labs(x="Tracker ID",y="Fraction") +
          facet_wrap(~Chamber))
  graphics.off()
  write.csv(results,"Experiment.csv",row.names=FALSE)  
  results
}
Summarize.TwoChoiceExperiment<-function(experiment,time=c(0,0),RemoveRegionData=TRUE){
  ## First output all of the plots
  pdf("Experiment.pdf",paper="USr", width=7.75, height=10)
  par(mfrow=c(6,4))
  for(i in experiment$Chambers){
    tmp<-Experiment.GetChamber(experiment,i)
    ## Plot the chamber
    PlotXY(tmp,time,FALSE)
    ## Summarize the chamber
    tmp<-Experiment.Chamber.Summarize(experiment,i,time)
    if(RemoveRegionData==TRUE){
      None<-tmp$None
      ## Now trim summary data to remove regions and keep treatments
      ## This will likely need to be fixed because of the added treatment column.
      n.trt.levels<-length(levels(experiment$ExpDesign$Treatment))
      end.col<-ncol(tmp)-n.trt.levels
      cols.to.remove<-(-1.0)*(10:end.col)
      tmp<-tmp[,cols.to.remove]
      tmp<-data.frame(tmp,None)  
    }
    if(exists("results",inherits=FALSE)==FALSE){
      results<-tmp
    }
    else
      results<-rbind(results,tmp)
  }
  tmp.result<-results[,c("Chamber","ID","PercSleeping","PercWalking","PercMicroMoving","PercResting")]
  tmp.result1<-melt(tmp.result,id.var=c("Chamber","ID"))
  names(tmp.result1)<-c("Chamber","ID","Type","value")
  
  print(ggplot(tmp.result1, aes(x = ID, y = value, fill = Type))+ 
          geom_bar(stat = "identity")+ggtitle(paste("Experiment Movement For Each Chamber")) +
          labs(x="Tracker ID",y="Fraction") +
          facet_wrap(~Chamber))
  graphics.off()
  write.csv(results,"Experiment.csv",row.names=FALSE)  
  results
}
SummarizeXMeans.XChoiceExperiment<-function(experiment,time=c(0,0),WriteToPDF=TRUE){
  for(i in experiment$Chambers){
    ## Summarize the chamber
    tmp<-Experiment.Chamber.GetXMeans(experiment,i,time)
    if(exists("results",inherits=FALSE)==FALSE){
      results<-tmp
    }
    else
      results<-rbind(results,tmp)
  }
  
  cat("\n*** Walking ***\n")
  tmp<-aov(Walking~Treatment,data=results)
  print(summary(tmp))
  
  cat("\n*** MicroMoving ***\n")
  tmp<-aov(MicroMoving~Treatment,data=results)
  print(summary(tmp))
  
  cat("\n*** Resting ***\n")
  tmp<-aov(Resting~Treatment,data=results)
  print(summary(tmp))
  
  cat("\n*** Sleeping ***\n")
  tmp<-aov(Sleeping~Treatment,data=results)
  print(summary(tmp))
  
  cat("\n*** Total ***\n")
  tmp<-aov(Total~Treatment,data=results)
  print(summary(tmp))
  
  if(WriteToPDF==TRUE) {
    fname<-paste("ExperimentXMeans.pdf")
    pdf(fname,paper="USr",onefile=TRUE)
    par(mfrow=c(3,2))
  }
  
  tmp<-melt(results,id.vars=c("Treatment","Chamber","ID"))
  p<-ggplot(tmp, aes(x=Treatment, y = value,fill = Treatment, colour = Treatment)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter()+
    scale_y_continuous(name = "Mean X Position") +
    scale_x_discrete(name = "Treatment") +
    ggtitle("Average X Position") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          text = element_text(size = 12),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11)) +
    facet_wrap(~ variable)
  
  print(p)
  
  if(WriteToPDF==TRUE){
    graphics.off()
  }
  
  results
}
SummarizeXMeans.TwoChoiceExperiment<-function(experiment,time=c(0,0)) {
  stop("This function is not implemented for a TwoChoice Experiment") 
}
SummarizeQuartile.TwoChoiceExperiment<-function(experiment,time=c(0,0)) {
  stop("This function is not implemented for a TwoChoice Experiment") 
}
## NOTE: This function doesn't work because it doesn't correct properly for the PI multiplier.
SummarizeQuartile.XChoiceExperiment<-function(experiment,quartile=3,time=c(0,0)){
  for(i in experiment$Chambers){
    ## Summarize the chamber
    tmp<-Experiment.Chamber.GetQuartile(experiment,i,quartile,time)
    if(exists("results",inherits=FALSE)==FALSE){
      results<-tmp
    }
    else
      results<-rbind(results,tmp)
  }
  
  cat("\n*** Walking ***\n")
  tmp<-aov(Walking~Treatment,data=results)
  print(summary(tmp))
  
  cat("\n*** MicroMoving ***\n")
  tmp<-aov(MicroMoving~Treatment,data=results)
  print(summary(tmp))
  
  cat("\n*** Resting ***\n")
  tmp<-aov(Resting~Treatment,data=results)
  print(summary(tmp))
  
  cat("\n*** Sleeping ***\n")
  tmp<-aov(Sleeping~Treatment,data=results)
  print(summary(tmp))
  
  cat("\n*** Total ***\n")
  tmp<-aov(Total~Treatment,data=results)
  print(summary(tmp))
  
  tmp<-melt(results,id.vars=c("Treatment","Chamber","ID"))
  ttl<-paste("Mean Quartile (",quartile,") Position",sep="")
  p<-ggplot(tmp, aes(x=Treatment, y = value,fill = Treatment, colour = Treatment)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter()+
    scale_y_continuous(name = "X Position") +
    scale_x_discrete(name = "Treatment") +
    ggtitle(ttl) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          text = element_text(size = 12),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11)) +
    facet_wrap(~ variable)
  
  print(p)
  
  results
}