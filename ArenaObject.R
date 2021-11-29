source("TrackerObject.R")
source("GeneralUtility.R")
require(data.table)
require(reshape2)
require(readxl)
require(tibble)



## Public Functions ##
ArenaClass<-function(parameters,dirname="Data"){
  datadir<-paste("./",dirname,"/",sep="")
  files <- list.files(path=datadir,pattern = "*TrackingData_[0-9]*.csv")    
  if(length(files)<1) {
    cat("Error reading file!")
    flush.console()      
  }
  files<-paste(datadir,files,sep="")
  
  theData<-rbindlist(lapply(files, function(x){read.csv(x, header=TRUE)}))

  trackers<-unique(theData$ObjectID)
 
  
  ## Get the tracking ROI and the Counting ROI
  file<-list.files(datadir,pattern="*.xlsx")
  if(length(file)>1)
    stop("Only allowed one experiment file in the directory")
  file<-paste(datadir,file,sep="")
  roi<-read_excel(file,sheet="ROI")
  
  arena<-list(Name=dirname,Trackers=trackers,ROI=roi)
  
  if(length(trackers)>0){
    for(i in trackers){
      nm<-paste("Tracker",i,sep="_")
      roinm<-paste("T",i,sep="_")
      theROI<-c(roi$Width[roi$Name==roinm],roi$Height[roi$Name==roinm])
      tmp<-TrackerClass.RawDataFrame(i,parameters,theData,theROI)
      arena<-c(arena,setNames(list(nm=tmp),nm))
    }
  }
  class(arena)="Arena"
  arena
}

Arena.ChangeParameterObject<-function(arena,newP) {
  for(i in arena$Trackers){
    tmp<-paste("Tracker_",i,sep="")
    t<-Arena.GetTracker(arena,i)
    arena[[tmp]]<-Tracker.ChangeParameterObject(t,newP)
  }
  chamber
}

Arena.GetTracker<-function(arena,id){
  tmp<-paste("Tracker_",id,sep="")
  arena[[tmp]]
}

GetMeanXPositions.Arena<-function(arena,time=c(0,0)){
  for(i in arena$Trackers){
    tmp<-paste("Tracker_",i,sep="")
    t<-Arena.GetTracker(arena,i)
    tmps<-GetMeanXPositions.Tracker(t,time)
    if(exists("result",inherits = FALSE)==TRUE){
      result<-rbind.missing(result,tmps)       
    }
    else {
      result<-tmps     
    }
  }
  result
}

GetQuartileXPositions.Arena<-function(arena,quartile=1,time=c(0,0)){
  for(i in arena$Trackers){
    tmp<-paste("Tracker_",i,sep="")
    t<-Arena.GetTracker(arena,i)
    tmps<-GetQuartileXPositions.Tracker(t,quartile,time)
    if(exists("result",inherits = FALSE)==TRUE){
      result<-rbind.missing(result,tmps)       
    }
    else {
      result<-tmps     
    }
  }
  result
}

Summarize.Arena<-function(arena,time=c(0,0),ShowPlot=TRUE, WriteToPDF=TRUE){
  for(i in arena$Trackers){
    tmp<-paste("Tracker_",i,sep="")
    t<-Arena.GetTracker(arena,i)
    tmps<-Summarize(t,time,FALSE)
    if(exists("result",inherits = FALSE)==TRUE){
      ##result<-rbind(result,tmps)       
      result<-rbind.missing(result,tmps)       
    }
    else {
      result<-tmps     
    }
  }
  if(ShowPlot==TRUE){
    if(WriteToPDF==TRUE) {
      fname<-"Arena_SummaryPlots.pdf"
      pdf(fname,paper="USr",onefile=TRUE)
      par(mfrow=c(3,2))
    }
    tmp.result<-result[,c(1,4,5,6,7)]
    tmp.result1<-melt(tmp.result,id.var="ID")
    names(tmp.result1)<-c("ID","Type","value")
    print(ggplot(tmp.result1, aes(x = ID, y = value, fill = Type))+ 
      geom_bar(stat = "identity")+ggtitle(paste("Arena"," -- Distribution")) +
      labs(x="Tracker ID",y="Fraction"))
    
    ## Now reflect total movement
    tmp.result<-result[,c(1,4,5,6,7)]
    tmp.result[,2]<-result[,4]*result$TotalDist_mm
    tmp.result[,3]<-result[,5]*result$TotalDist_mm
    tmp.result[,4]<-result[,6]*result$TotalDist_mm
    tmp.result[,5]<-result[,7]*result$TotalDist_mm
    tmp.result1<-melt(tmp.result,id.var="ID")
    names(tmp.result1)<-c("ID","Type","value")
    print(ggplot(tmp.result1, aes(x = ID, y = value, fill = Type))+ 
            geom_bar(stat = "identity")+ggtitle(paste("Arena",arena$Name," -- Total Movement")) +
            labs(x="Tracker ID",y="Frames"))
    if(WriteToPDF==TRUE){
      graphics.off()
    }
  }
  result
}

PlotXY.Arena<-function(arena,time=c(0,0),WriteToPDF=TRUE){
  fname<-"Arena_XYPlots.pdf"
  tmp.list<-list()
  if(WriteToPDF==TRUE) {
    pdf(fname,paper="letter",onefile=TRUE)
    par(mfrow=c(3,2))
  }
  for(i in arena$Trackers){
    tmp<-Arena.GetTracker(arena,i)
    print(PlotXY(tmp,time))
  }
  if(WriteToPDF==TRUE){
    graphics.off()
  }
}

PlotX.Arena<-function(arena,time=c(0,0),WriteToPDF=TRUE){
  fname<-"Arena_XPlots.pdf"
  tmp.list<-list()
  if(WriteToPDF==TRUE) {
    pdf(fname,paper="USr",onefile=TRUE)
    par(mfrow=c(3,2))
  }
  for(i in arena$Trackers){
    tmp<-Arena.GetTracker(arena,i)
    print(PlotX(tmp,time))
  }
  if(WriteToPDF==TRUE){
    graphics.off()
  }
}

PlotX2.Arena<-function(arena,time=c(0,0),WriteToPDF=TRUE){
  fname<-"Arena_XPlots2.pdf"
  tmp.list<-list()
  if(WriteToPDF==TRUE) {
    pdf(fname,paper="USr",onefile=TRUE)
    par(mfrow=c(3,2))
  }
  for(i in arena$Trackers){
    tmp<-Arena.GetTracker(arena,i)
    print(PlotX.Tracker(tmp,time))
  }
  if(WriteToPDF==TRUE){
    graphics.off()
  }
}

## Private Functions ##
rbind.missing <- function(A, B) { 
  A<-data.table(A)
  B<-data.table(B)
  cols.A <- names(A)
  cols.B <- names(B)
  
  missing.A <- setdiff(cols.B,cols.A)
  # check and define missing columns in A
  if(length(missing.A) > 0L){
    class.missing.A <- lapply(B[,missing.A,with = FALSE], class)
    nas.A <- lapply(class.missing.A, as, object = NA)
    A[,c(missing.A) := nas.A]
  }
  # check and define missing columns in B
  missing.B <- setdiff(names(A), cols.B)
  if(length(missing.B) > 0L){
    class.missing.B <- lapply(A[,missing.B,with = FALSE], class)
    nas.B <- lapply(class.missing.B, as, object = NA)
    B[,c(missing.B) := nas.B]
  }
  # reorder so they are the same
  setcolorder(B, names(A))
  tmp<-data.frame(rbind(A, B))
  tmp[is.na(tmp)]<-0
  tmp
  
  
}

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

SmoothTransitions.Arena<-function(arena,minRun=1){
  for(i in arena$Trackers){
    tmp<-paste("Tracker_",i,sep="")
    t<-Arena.GetTracker(arena,i)
    t2<-SmoothTransitions(t)
    arena[[tmp]]<-t2
  }
  arena
}

AnalyzeTransitions.Arena<-function(arena,range=c(0,0),ShowPlot=TRUE){
  result<-list()
  for(i in arena$Trackers){
    tmp<-paste("Tracker_",i,sep="")
    t<-Arena.GetTracker(arena,i)
    result[[tmp]]<-AnalyzeTransitions(t,range,ShowPlot)
  }
  result
}

PIPlots.Arena<-function(arena,range=c(0,0),WriteToPDF=TRUE){
  fname<-"Arena_PIPlots.pdf"
  if(WriteToPDF==TRUE) {
    ##mypdf(fname,paper="letter",onefile=TRUE)
    mypdf(fname,res = 600, height = 11, width = 9, units = "in")
  }
  for(i in arena$Trackers){
    t<-Arena.GetTracker(arena,i)
    PIPlots(t,range)
  }
  if(WriteToPDF==TRUE){
    mydev.off(fname)
  }
}

mypdf = function(pdfname, mypattern = "MYTEMPPNG", ...) {
  fname = paste0(mypattern, "%05d.png")
  gpat = paste0(mypattern, ".*\\.png")
  takeout = list.files(path = tempdir(), pattern = gpat, full.names = TRUE)
  if (length(takeout) > 0) 
    file.remove(takeout)
  pngname = file.path(tempdir(), fname)
  png(pngname, ...)
  return(list(pdfname = pdfname, mypattern = mypattern))
}
# copts are options to sent to convert
mydev.off = function(pdfname, mypattern="MYTEMPPNG", copts = "") {
  dev.off()
  print("Here")
  gpat = paste0(mypattern, ".*\\.png")
  pngs = list.files(path = tempdir(), pattern = gpat, full.names = TRUE)
  mystr = paste(pngs, collapse = " ", sep = "")
  print(copts)
  pdfname<-paste(getwd(),"/",pdfname,sep="")
  tmp<-(sprintf("convert %s -quality 100 %s %s", mystr, pdfname, copts))
  system(tmp)
}