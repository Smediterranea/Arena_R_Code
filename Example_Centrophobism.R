## Note, high density and PI plots require ImageMagik to be installed for
## simpler PDF outputs.
## Get it here: https://imagemagick.org/script/download.php#windows
## Also required are the following packages: ggplot2, markovchain, Gmisc,
##  data.table, reshape2, readxl, tibble, stringr, readr


rm(list = ls())
## Do one of the following two
source("ArenaObject.R")

## Need to source this function
Execute <- function(analyze.subdirectories) {
  if (analyze.subdirectories == FALSE) {
    ## If there is only a single movie's worth of data in that directory, then
    ## run this code.
    arena <- ArenaClass(p, dirname = directory)
    ## This will make a plot of the flies movement.
    ## It
    Plot.Arena(arena)
    results <- Summarize(arena)
    write.csv(
      results,
      file = paste(directory, "/Results.csv", sep = ""),
      row.names = FALSE
    )
  }
  else {
    thefolders <- list.dirs(directory)
    thefolders <- thefolders[-1]
    for (f in thefolders) {
      print(paste("Analyzing folder: ", f, sep = ""))
      arena <- ArenaClass(p, dirname = f)
      Plot.Arena(arena)
      results <- Summarize(arena)
      write.csv(
        results,
        file = paste(f, "/Results.csv", sep = ""),
        row.names = FALSE
      )
      CleanTrackers()
    }
  }
}

## Always put this here to remove any old variables
CleanTrackers()

## First make a parameter class
## You can define a generic tracker
p <- ParametersClass()
## If you tracked a movie, set the original movie recording FPS.
p <- Parameters.SetParameter(p, FPS = 30)

## The next value is for the old CCD cameras
## mm.per.pixel<-0.2156
## The next value is for the new CCD camera setup
## mm.per.pixel<-0.131
## The next value is roughly good for the Arenas
# mm.per.pixel<-0.0.056
p <- Parameters.SetParameter(p, mmPerPixel = 0.131)

directory <- "CentrophobismData"

## If there is only a single movie's worth of data in that directory, then
## set analyze.subdirectories to FALSE.
## If you have a series of folders in the directory, each with a single
## movie's worth of data and you want to run the analysis on each
## directory, then set this equal to true.
analyze.subdirectories <- TRUE
#analyze.subdirectories<-FALSE

Execute(analyze.subdirectories)

