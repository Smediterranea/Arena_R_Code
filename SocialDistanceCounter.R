require(data.table)
require(reshape2)
require(readxl)
require(tibble)
require(plyr)
require(dplyr)
require(tidyr)

SocialDistanceCounter.ProcessSocialDistanceCounter<-function(counter){
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
  counter <- SocialDistanceCounter.SetSocialDistanceData(counter)
  class(counter) <- c("SocialDistanceCounter", class(counter))
  counter
}

SocialDistanceCounter.GetFrameCounts<-function(counter, range=c(0,0)) {
  library(dplyr, warn.conflicts = FALSE)
  # Suppress summarize info
  options(dplyr.summarise.inform = FALSE)
  
  theData<-counter$RawData
  p<-counter$Parameters
  counts <-
    theData %>% group_by(Frame) %>% summarise(Objects = sum(NObjects))
  missings <-
    length(which(!(seq(
      min(counts$Frame), max(counts$Frame)
    ) %in% counts$Frame)))
  OnTarget <- sum(counts$Objects == p$Interacting.Entities)
  tmp <- sum(counts$Objects > p$Interacting.Entities)
  tmp2 <- sum(counts$Objects < p$Interacting.Entities)
  OffTarget<-tmp + tmp2
  
  frequencies <- c(missings, OnTarget, OffTarget)
  names(frequencies) <- c("Zero", "OnTarget", "OffTarget")
  frequencies
}


 test<-function(a){
   print(a)
   print("Hi")
 }
   
SocialDistanceCounter.GetDistance<-function(row1, row2){
  tmp<-row1[1]-row2[1]
  tmp2<-row1[2]-row2[2]
  sqrt(tmp*tmp+tmp2*tmp2)
}

SocialDistanceCounter.NeighborApplyFunction<-function(row,theData){
  tmp<-subset(theData,theData[,3]==row[3])
  dists<-unlist(apply(tmp[,c(1,2)],1,SocialDistanceCounter.GetDistance,row))
  dists<-dists[dists>0]
  min(dists)
}

SocialDistanceCounter.CalculateClosestNeighbor<-function(counter){
  theData<-counter$RawData
  print("Calculating NeighborDistance.")
  print("This will take some time.")
  ClosestNeighbor<-unlist(apply(theData[,c("X","Y","Frame")],1,SocialDistanceCounter.NeighborApplyFunction,theData[,c("X","Y","Frame")]))
  theData<-data.frame(theData,ClosestNeighbor)
  theData
}

SocialDistanceCounter.SetSocialDistanceData<-function(counter) {
  theData<-counter$RawData
  p<-counter$Parameters
  frequencies<-SocialDistanceCounter.GetFrameCounts(counter)
  
  percIdentified<-frequencies[2]/(frequencies[2]+frequencies[3])
  if(percIdentified < .20){
    theData$NObjects[theData$NObjects==0]<-1
    print("Data Corrected!!!!!")
  }
  
  ## Isolate only those frames with the correct number of objects
  counts <-
    theData %>% group_by(Frame) %>% summarise(Objects = sum(NObjects))
  
  frameIndex <- subset(counts, Objects == p$Interacting.Entities)$Frame
  if(!("ClosestNeighbor" %in% names(theData))){
    counter$RawData<-SocialDistanceCounter.CalculateClosestNeighbor(counter)
    theData<-counter$RawData
  }
  
  
  AvgNeighborDistance <- rep(NA, length(frameIndex))
  SENeighborDistance <- rep(NA, length(frameIndex))
  ElapsedTimeMin<-AvgNeighborDistance
  DiffTimeMin<-AvgNeighborDistance
  
  results <- data.frame(frameIndex, AvgNeighborDistance,SENeighborDistance,ElapsedTimeMin,DiffTimeMin)
  names(results) <- c("Frame","AvgNeighborDistance","SENeighborDistance","ElapsedTimeMin","DiffTimeMin")
  results
  print(" ")
  print(paste("Tracking region:",counter$Name))
  print(paste(length(results$AvgNeighborDistance),"target blob frames found."))
   print(" ")
   for (i in 1:length(results$AvgNeighborDistance)) {
     if (i %% 2000 == 0)
       print(paste("Rep", i, "of", length(results$AvgNeighborDistance)))
     tmp <- subset(theData, Frame == results$Frame[i])
     ## Drop spurious blobs
     tmp<-subset(tmp,BlobType!="None")
     results$ElapsedTimeMin[i]=tmp$Minutes[1]
     if(i==1){
       results$DiffTimeMin[i]=results$ElapsedTimeMin[i]
     }
     else {
       results$DiffTimeMin[i]=results$ElapsedTimeMin[i]-results$ElapsedTimeMin[i-1]
     }
     ## Need to account for frames with multi-object blobs
     if(nrow(tmp)<p$Interacting.Entities){
       tmp2<-rep(1,p$Interacting.Entities-nrow(tmp))
       tmp3<-c(tmp$ClosestNeighbor,tmp2)
       results$AvgNeighborDistance[i] <- mean(tmp3)
       results$SENeighborDistance[i] <- sqrt(var(tmp3) / length(tmp3))
     }
     else {
      results$AvgNeighborDistance[i] <- mean(tmp$ClosestNeighbor)
      results$SENeighborDistance[i] <- sqrt(var(tmp$ClosestNeighbor) / length(tmp$ClosestNeighbor))
     }
   }
   
   counter$InteractionData<-list(Frequencies = frequencies, Results = results)
   counter
}


Summarize.SocialDistanceCounter<-function(counter,range=c(0,0),ShowPlot=TRUE){
  rd<-InteractionCounter.GetInteractionData(counter,range) 
  ff<-SocialDistanceCounter.GetFrameCounts(counter,range)
  
  total.time<-sum(rd$DiffTimeMin) 
  avg.distance<-mean(rd$AvgNeighborDistance)
  avg.sem.distance<-mean(rd$SENeighborDistance)
  sem.distance<-sqrt(var(rd$AvgNeighborDistance) / length(rd$AvgNeighborDistance))
  
  results<-data.frame(counter$ID,total.time,avg.distance,sem.distance,avg.sem.distance,ff[1],ff[2],ff[3],ff[2]/(sum(ff)),
                      range[1],range[2])
  names(results)<-c("TrackingRegion","ObsMinutes","AvgClosestNeighbor","SEMClosestNeighbor","AvgSEMClosestNeighbor","Zero","OnTarget","OffTarget","PercOnTarget","StartMin","EndMin")
  rownames(results)<-1:nrow(results)
  results
}

Plot.SocialDistanceCounter<-function(counter,range = c(0, 0)){
  id<-counter$InteractionData$Results
  x <- ggplot(id, aes(Frame, AvgNeighborDistance, color = SENeighborDistance)) +
    geom_point() +
    ggtitle(paste("Counter:", counter$Name, sep =
                    "")) +
    geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
    xlab("Frame") + ylab("Avg Closest Neighbor Distance")
  print(x)
  
}