source("PairwiseBatchFunctions.R")

## The next value is for the old CCD cameras
## mm.per.pixel<-0.2156
## The next value is for the new CCD camera setup
## mm.per.pixel<-0.132
## The next value is roughly good for the Arenas
# mm.per.pixel<-0.0.056
mm.per.pixel<-0.132

## Set FPS=NA if using the live tracking in the arenas.
## Set FPS equal to the actual recorded frames per second if you tracked movies.
#fps<-10
fps<-NA

RunBatchAnalysis("Females",mm.per.pixel,fps)