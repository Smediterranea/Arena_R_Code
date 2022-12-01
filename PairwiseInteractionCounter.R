require(data.table)
require(reshape2)
require(readxl)
require(tibble)
require(plyr)
require(dplyr)
require(tidyr)

PairwiseInteractionCounter.ProcessPairwiseInteractionCounter<-function(counter){
  if (!is.null(counter$ExpDesign)) {
    a <- "TrackingRegion" %in% colnames(tracker$ExpDesign)
    b <- "Treatment" %in% colnames(tracker$ExpDesign)
    f <- c(a, b)
    if (sum(f) < 2) {
      stop(
        "Experimental design file requires TrackingRegion and Treatments columns."
      )
    }
  }
  counter <- PairwiseInteractionCounter.SetInteractionData(counter)
  class(counter) <- c("PairwiseInteractionCounter", class(counter))
  counter
}

PairwiseInteractionCounter.GetFrameCounts<-function(counter, range=c(0,0)) {
  library(dplyr, warn.conflicts = FALSE)
  
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)
  theData<-InteractionCounter.GetRawData(counter,range)
  p<-counter$Parameters
  counts <-
    theData %>% group_by(Frame) %>% summarise(Objects = sum(NObjects))
  missings <-
    length(which(!(seq(
      min(counts$Frame), max(counts$Frame)
    ) %in% counts$Frame)))
  ones <- sum(counts$Objects == 1)
  twos <- sum(counts$Objects == 2)
  more <- sum(counts$Objects > 2)
  
  frequencies <- c(missings, ones, twos, more)
  names(frequencies) <- c("Zero", "One", "Two", "More")
  frequencies
}


PairwiseInteractionCounter.SetInteractionData<-function(counter) {
  SubFunction<-function(a){
    result<-NA
    if(sum(a$NObjects)==2){
      if(length(a$RelX)==1){
        result<-0
      }
      else if(length(a$RelX)>2){
        a<-a[a$NObjects>0,]
      }
      else {
        diffx <- diff(a$RelX)
        diffy <- diff(a$RelY)
        d <- sqrt(diffx * diffx + diffy * diffy)
        result<-d
      }
    }
    result
  }
  theData<-counter$RawData
  p<-counter$Parameters
  frequencies<-PairwiseInteractionCounter.GetFrameCounts(counter)
  
  tmp<-theData %>% group_by(Frame) %>% mutate(Dist=SubFunction(cur_data())) %>% summarise(Dist = mean(Dist))
  results<-tmp[!is.na(tmp$Dist),]
  
  
  Distance_mm<-results$Dist*p$mmPerPixel
  
  IsInteracting<-Distance_mm<=p$Interaction.Distance.mm
  
  results <- data.frame(results,Distance_mm,IsInteracting)
  names(results) <- c("Frame", "Distance","Distance_mm","IsInteracting")
  print(" ")
  print(paste("Tracking region:",counter$Name))
  print(paste(length(results$Distance),"two blob frames found."))
  print(" ")
  
  counter$InteractionData<-list(Frequencies = frequencies, Results = results)
  counter
}

## Public Functions ##
PairwiseInteractionCounterOldAndSlow.SetInteractionData<-function(counter) {
  theData<-counter$RawData
  p<-counter$Parameters
  frequencies<-PairwiseInteractionCounter.GetFrameCounts(counter)
  counts <-
    theData %>% group_by(Frame) %>% summarise(Objects = sum(NObjects))
  frameIndex <- subset(counts, Objects == 2)$Frame
  distances <- rep(NA, length(frameIndex))
  ElapsedTimeMin<-distances
  IsInteracting<-rep(FALSE,length(ElapsedTimeMin))
  
  results <- data.frame(frameIndex, distances,distances,ElapsedTimeMin,IsInteracting)
  names(results) <- c("Frame", "Distance","Distance_mm","ElapsedTimeMin","IsInteracting")
  print(" ")
  print(paste("Tracking region:",counter$Name))
  print(paste(length(results$Distance),"two blob frames found."))
  print(" ")
  for (i in 1:length(results$Distance)) {
    if (i %% 2000 == 0)
      print(paste("Rep", i, "of", length(results$Distance)))
    tmp <- subset(theData, Frame == results$Frame[i])
    ## Drop spurious blobs
    tmp<-subset(tmp,BlobType!="None")
    if (nrow(tmp) != 2 && nrow(tmp) != 1){
      print(tmp)
      stop("Oops1")
      
    }
    results$ElapsedTimeMin[i]=tmp$Minutes[1]
    x <- tmp$RelX
    y <- tmp$RelY
    if (length(x) == 2) {
      diffx <- x[1] - x[2]
      diffy <- y[1] - y[2]
      d <- sqrt(diffx * diffx + diffy * diffy)
      results$Distance[i] <- d
      results$Distance_mm[i] <- d*p$mmPerPixel
      if(results$Distance_mm[i]<=p$Interaction.Distance.mm){
        results$IsInteracting[i]=TRUE
      }
           
    }
    else {
      results$Distance[i] <- 0
      results$Distance_mm[i] <- 0
      results$IsInteracting[i]<-TRUE
    }
  }
  
  counter$InteractionData<-list(Frequencies = frequencies, Results = results)
  counter
}

InteractionCounter.GetRawData <- function(counter, range = c(0, 0)) {
  rd <- counter$RawData
  if (sum(range) != 0) {
    rd <- rd[(rd$Minutes > range[1]) & (rd$Minutes < range[2]), ]
  }
  rd
}
InteractionCounter.GetInteractionData <- function(counter, range = c(0, 0)) {
  rd <- counter$InteractionData$Results
  if (sum(range) != 0) {
    rd <- rd[(rd$ElapsedTimeMin > range[1]) & (rd$ElapsedTimeMin < range[2]), ]
  }
  rd
}

Summarize.PairwiseInteractionCounter<-function(counter,range=c(0,0),ShowPlot=TRUE){
  rd<-InteractionCounter.GetInteractionData(counter,range) 
  ff<-PairwiseInteractionCounter.GetFrameCounts(counter,range)
  
  interacting<-rd[rd$IsInteracting==TRUE,]
  notinteracting<-rd[rd$IsInteracting==FALSE,]
  
  results<-data.frame(counter$ID,counter$Parameters$Interaction.Distance.mm,sum(rd$IsInteracting),sum(rd$IsInteracting==FALSE),sum(rd$IsInteracting)/length(rd$IsInteracting),ff[1],ff[2],ff[3],ff[4],ff[3]/(sum(ff)),
                      range[1],range[2])
  names(results)<-c("TrackingRegion","IDistance","FramesInteracting","FramesNotInteracting","PercentInteraction","Zero","One","Two","More","PercTwo","StartMin","EndMin")
  rownames(results)<-1:nrow(results)
  results
}

Plot.PairwiseInteractionCounter<-function(counter,range = c(0, 0)){
  id<-counter$InteractionData$Results
  x <- ggplot(id, aes(Frame, Distance_mm, color = IsInteracting)) +
    geom_point() +
    ggtitle(paste("Counter:", counter$Name, sep =
                    "")) +
    geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
    xlab("Frame") + ylab("Distance (mm)")
  print(x)
  
}

OutputAliData.PairwiseInteractionCounterArena <- function(arena,dirname) {
  trackers.to.get <- arena$Trackers
  
  max.rows<-0
  for(i in 1:nrow(trackers.to.get)){
    tmp <- Arena.GetTracker(arena, trackers.to.get[i, ])
    tmp2<-length(tmp$InteractionData$Results$Distance_mm)
    if(tmp2>max.rows)
      max.rows<-tmp2
  }
  
  results<-data.frame(matrix(rep(NA,max.rows*(nrow(trackers.to.get)+1)),ncol=nrow(trackers.to.get)+1))
  
  results[,1]<-1:nrow(results)
  for (i in 1:nrow(trackers.to.get)) {
      tmp <- Arena.GetTracker(arena, trackers.to.get[i, ])$InteractionData$Results$Distance_mm
      results[1:length(tmp),i+1] <-tmp
  }
  names(results)<-c("Index",trackers.to.get[,1])
  write.csv(results,paste(dirname,"/AliOutput.csv",sep=""),row.names = FALSE)
}

GetAverageNeighborDistance.PairwiseInteractionCounterArena<-function(arena){
  
  trackers.to.get <- arena$Trackers
  
  max.rows<-0
  for(i in 1:nrow(trackers.to.get)){
    tmp <- Arena.GetTracker(arena, trackers.to.get[i, ])
    tmp2<-length(tmp$InteractionData$Results$Distance_mm)
    if(tmp2>max.rows)
      max.rows<-tmp2
  }
  
  results<-data.frame(matrix(rep(NA,max.rows*(nrow(trackers.to.get)+1)),ncol=nrow(trackers.to.get)+1))
  
  results[,1]<-1:nrow(results)
  for (i in 1:nrow(trackers.to.get)) {
    tmp <- Arena.GetTracker(arena, trackers.to.get[i, ])$InteractionData$Results$Distance_mm
    results[1:length(tmp),i+1] <-tmp
  }
  names(results)<-c("Index",trackers.to.get[,1])
  
  
  MeanDist<-apply(results[,-1],2,mean,na.rm=TRUE)
  tmp<-!is.na(results)
  Count<-apply(tmp[,-1],2,sum)
  rbind(means,counts)
}



###################################
## Still need to be incorporated

GetBinnedInteractionTime <- function(results, binsize.min = 10) {
  et <- as.numeric(results$Results$ElapsedTimeMin)
  
  y <- seq(min(et), max(et), by = binsize.min)
  
  tmpMatrix <- cbind(y[-length(y)], y[-1])
  intervals <- cut(y + 0.000001,
                   y,
                   include.lowest = TRUE,
                   dig.lab = 8)
  intervals <- intervals[-length(intervals)]
  midpoint <- (tmpMatrix[, 1] + tmpMatrix[, 2]) / 2
  intDuration <- rep(NA, nrow(tmpMatrix))
  percDuration <- rep(NA, nrow(tmpMatrix))
  result <-
    data.frame(intervals,
               tmpMatrix[, 2] - tmpMatrix[, 1],
               midpoint,
               tmpMatrix,
               intDuration,
               percDuration)
  names(result) <-
    c(
      "Interval",
      "Duration",
      "MidPoint",
      "Start",
      "End",
      "InteractionTime",
      "PercentageInteraction"
    )
  for (i in 1:nrow(result)) {
    indexer <- (results$Results$ElapsedTimeMin > result$Start[i]) &
      (results$Results$ElapsedTimeMin <= result$End[i]) &
      (results$Results$IsInteracting == TRUE)
    tmpData <- subset(results$Results, indexer)
    result$InteractionTime[i] <- sum(tmpData$DiffTimeMin)
    result$PercentageInteraction[i] <-
      result$InteractionTime[i] / result$Duration[i]
  }
  result
}


UpdateDistanceCutoff.Arena<-function(arena,newcutoff.mm){
  for(i in 1:nrow(arena$Trackers)){
    t<-Arena.GetTracker(arena,arena$Trackers[i,1])
    t<-UpdateDistanceCutoff.PairwiseInteractionCounter(t,newcutoff.mm)
    nm<-paste("Tracker",t$Name,sep="_")
    arena[[nm]]<-t
  }
  arena
}

UpdateDistanceCutoff.PairwiseInteractionCounter <- function(tracker, newcutoff.mm) {
  tracker$Parameters$Interaction.Distance.mm<-newcutoff.mm
  tracker$InteractionData$Results$IsInteracting <-
    tracker$InteractionData$Results$Distance_mm <= newcutoff.mm
  tracker
}

## Functions that just catch misapplied higher functions
FinalPI.PairwiseInteractionCounter<-function(tracker){
  cat("This function not available for this type of tracker")
}
CumulativePI.PairwiseInteractionCounter<-function(tracker){
  cat("This function not available for this type of tracker")
}
GetPIData.PairwiseInteractionCounter<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}
PIPlots.PairwiseInteractionCounter<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}
TimeDependentPIPlots.PairwiseInteractionCounter<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}
