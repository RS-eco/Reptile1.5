#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                Extract percentage range change and                #
#         distance between range centroids per species in km        #
#   based on current and future range centroids and range overlap   #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls()); invisible(gc())

#-#-# Load libraries #-#-#
library(sp)

#-#-# Read in the range centroid and range overlap file #-#-#
Distdata <- readRDS("data/range_centroids_extent_disp_eigth_2080_rcp60.rds")

#-#-# Loop throug datafile to calculate distance between centroids and percentage range overlap #-#-#
List <- c(1:nrow(Distdata))

Distances <- lapply(List, function(x){
  print(x)
  data <- Distdata[x,] %>% as.data.frame()
  coordscurrent <- data[3:4] # check this is corrent
  #colnames(coordscurrent) <- c("x","y")
  coordscurrent$x <- as.numeric(coordscurrent$x)
  coordscurrent$y <- as.numeric(coordscurrent$y)
  coordscurrent <- as.matrix(coordscurrent)
  
  coordsfuture <- data[6:7]
  #colnames(coordsfuture)
  colnames(coordsfuture) <- c("x","y")
  coordsfuture$x <- as.numeric(coordsfuture$x)
  coordsfuture$y <- as.numeric(coordsfuture$y)
  coordsfuture <- as.matrix(coordsfuture)
  #print(coordsfuture)
  
  if(!anyNA(coordsfuture)){
    distance <- spDistsN1(coordscurrent,coordsfuture,longlat = TRUE) 
    print(distance)
  } else{
    distance <- "NA"  
  }
  return(distance)
})

DistCenter <- do.call(rbind,Distances)
DistCenter <- as.data.frame(DistCenter)
DistCenter <- round(as.numeric(as.character(DistCenter$V1)),2)

Distdata$DistCenter <- DistCenter
head(Distdata)

Distdata$PercRangeChange <- Distdata$FutureRange/(Distdata$CurrentRange/100)
Distdata$PercRangeChange <- round(Distdata$PercRangeChange,2)
head(Distdata)

Distdata$PercOverlap <- Distdata$OverlapRange/(Distdata$CurrentRange/100)
Distdata$PercOverlap <- round(Distdata$PercOverlap,2)
head(Distdata)

saveRDS(Distdata,"range_centroids_extent_km_perc_overlap_2080_rcp60.rds", compress="xz")
