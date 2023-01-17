#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#             Extract range centroids and range extent              #
#               Based on thresholded ensemble ranges                #
#                      Clipped by dispersal                         #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls()); gc()

#-#-# Load libraries #-#-#
library(dplyr); library(tidyr); library(magrittr)
library(raster); library(lattice)

# Set disp, rcp, year
disp <- "disp_eigth"
years <- 2080
rcps <- "rcp60"

# Load data
group_names <- c("croc", "lizard", "snake", "turtle", "worm lizard")
dat <- lapply(group_names, function(x){
  print(x)
  readRDS(paste0("data/Reptile_", x, "_thresh_", disp, "_sp_ensemble_locs.rds"))
})
dat <- data.table::rbindlist(dat, use.names=T); invisible(gc())

# Create species list from data
spList <- unique(dat$species)

# Create current data
current <- dat %>% filter(year == "1995") %>% as.data.frame()

# Create future data
future <- dat %>% as.data.frame() %>% filter(year == years) %>% 
  filter(rcp == rcps); rm(dat); invisible(gc())

#-#-# Extract centroids #-#-#
rangedata <- lapply(spList,function(spname){
  print(spname)
  
  current_sub <- current %>% filter(species == spname)
  head(current_sub)
  
  #-#-# current file #-#-#
  current_sub <- unique(current_sub)
  
  #-#-# Current range data #-#-#
  current.x <- round(as.numeric(as.character(mean(current_sub$x))),2)
  current.y <- round(as.numeric(as.character(mean(current_sub$y))),2)
  currentRange <- nrow(current_sub)
  test <- nrow(current_sub)
  print(test)

  #-#-# future file #-#-#
  future_sub <- future %>% filter(species == spname)
  future_sub <- unique(future_sub)

  #-#-# Future range data #-#-#
  future.x <- round(as.numeric(as.character(mean(future_sub$x))),2)
  future.y <- round(as.numeric(as.character(mean(future_sub$y))),2)
  
  futureRange <- nrow(future_sub)
  
  #-#-# Extract current and future range overlap #-#-#
  combi <- full_join(current_sub,future_sub,by=c("x","y"))
  combi$both <- combi$presence.x + combi$presence.y
  combi <- subset(combi,both > 1)
  combi <- unique(combi)
  OverlapRange <- nrow(combi)
  
  # Add group name
  spgroup <- unique(current$group)
  
  speciesdata <- cbind(spname,spgroup,current.x,current.y,currentRange,future.x,future.y,futureRange,OverlapRange)
  speciesdata <- as.data.frame(speciesdata)
  colnames(speciesdata) <- c("SpName","SpGroup", "CurrentCentroid.x","CurrentCentroid.y","CurrentRange",
                             "FutureCentroid.x","FutureCentroid.y","FutureRange","OverlapRange")
  return(speciesdata)
}) 
RangeData <- data.table::rbindlist(rangedata)
head(RangeData)

colnames(RangeData) <- c("SpName","SpGroup", "CurrentCentroid.x","CurrentCentroid.y","CurrentRange",
                         "FutureCentroid.x","FutureCentroid.y","FutureRange","OverlapRange")
RangeData$CurrentCentroid.x <- as.numeric(as.character(RangeData$CurrentCentroid.x))
RangeData$CurrentCentroid.y <- as.numeric(as.character(RangeData$CurrentCentroid.y))
RangeData$FutureCentroid.x <- as.numeric(as.character(RangeData$FutureCentroid.x))
RangeData$FutureCentroid.y <- as.numeric(as.character(RangeData$FutureCentroid.y))
RangeData$CurrentRange <- as.numeric(as.character(RangeData$CurrentRange))
RangeData$FutureRange <- as.numeric(as.character(RangeData$FutureRange))
RangeData$OverlapRange <- as.numeric(as.character(RangeData$OverlapRange))

saveRDS(RangeData,paste0("data/range_centroids_extent_", disp, "_", year, "_", rcp, ".rds"), compress="xz")
