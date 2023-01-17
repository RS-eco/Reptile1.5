#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                    Calculate bearing between                    #
#                current and future range centroids               #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Empty working environment
rm(list=ls()); invisible(gc())

#-#-# Load libraries #-#-#
library(geosphere)
library(circular)
library(plyr)
library(CircStats)

#-#-# Load range extent and percentage overlap file #-#-#
Coords <- readRDS("data/range_centroids_extent_km_perc_overlap_2080_rcp60.rds")

## Select relevant rows (species name, current.x, current.y, future.x, future.y)
Coords <- Coords %>% select(c(species name, current.x, current.y, future.x, future.y))
head(Coords)
Coords <- na.omit(Coords)
nrow(Coords)

#-#-# Extract bearing between range centers #-#-#  
## Set empty dataframe to save values 
DegData <- data.frame(name= character(0), bearing= numeric(0))
head(DegData)
for(i in 1:(nrow(Coords))){
  
  print(i)
  
  n <- Coords[i,]
  name <- n[1]
  
  cur  <- cbind(n[2],n[3])
  colnames(cur) <- c("x","y")

  fut <- cbind(n[4],n[5])
  colnames(fut) <- c("x","y")
  
  bear <- bearing(cur,fut)
  print(bear)
  
  alldata <- (c(name=name,bearing=bear))
  alldata <- as.data.frame(alldata)
  DegData <- rbind(DegData,alldata)
}
  
saveRDS(DegData,"bearing_range_centroids_2080_rcp60.rds", compress="xz")  

#-#-# Average bearing per cell (this is needed for the cellwise plot) #-#-#
## Get bearing per species
bearings <- readRDS("bearing_range_centroids_2080_rcp60.rds")
head(bearings)
colnames(bearings) <- c("spname","bearing")

## Get current richness data (this is just the x, y, species1, species2,... 
# matrix (split in three files due to computational limitations))
setwd("H:/Manuscript chapter 3 SDMs/SDMs range changes/")

cells <- get(load("H:/Alke - Thesis chapters/Alke - Global SDMs/SDM results summarized/Summary plots/
                   Global plots/Baseline plot tables/Original distribution 9201 species/
                   Full data files/OrigDist_modelled_species_1_3500.Rdata"))
cells[is.na(cells)] <- 0
coords <- cells1[c(1,2)]
head(coords)

#rad2deg <- function(rad) {(rad * 180) / (pi)}

rbind.match.columns <- function(input1, input2) {
  n.input1 <- ncol(input1)
  n.input2 <- ncol(input2)
  
  if (n.input2 < n.input1) {
    TF.names <- which(names(input2) %in% names(input1))
    column.names <- names(input2[, TF.names])
  } else {
    TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])
  }
  
  return(rbind(input1[, column.names], input2[, column.names]))
}


#-#-# Calculate mean bearing between range centroids across species per cell #-#-#
## Going through one cell at a time
bearData <- apply(coords,1,function(n){
  
  print(n)
  coordset <- t(n)
  coordset <- as.data.frame(coordset)
  
  x <- coordset[1]
  y <- coordset[2]
  
  ## This step is not needed if the data matrix is in one file 
  coord1 <- merge(coordset,cells1,by=c("x","y"),all.x=TRUE)
  coord2 <- merge(coord1,cells2,by=c("x","y"),all.x=TRUE)
  coord3 <- merge(coord2,cells3,by=c("x","y"),all.x=TRUE)
  
  ## Format the presence data (which species are in the cell)
  Pdata <- coord3[3:ncol(coord3)]
  Pdata <- t(Pdata)
  Pdata <- as.data.frame(Pdata)
  Pdata$spname<-rownames(Pdata)
  Pdata <- subset(Pdata,V1==1)
  
  ## Merge the species present in the cell with their individual bearings #-#-#
  CDdata <- merge(Pdata,bearings,by="spname") #,all.x=TRUE
  CDdata <- na.omit(CDdata)
  print(nrow(CDdata))
  if(nrow(CDdata) >0){
    
  ## Calculate the mean bearing for the cell  
  beardata <- round(circular(CDdata$bearing, units = c("degrees"),zero = 0, rotation = c("counter", "clock")),0)
  beardataT <- as.data.frame(table(beardata))
  beardataT$beardata <- as.numeric(as.character(beardataT$beardata))
  beardataT$Freq <- as.numeric(as.character(beardataT$Freq))
  beardataDF <- as.data.frame(CDdata$bearing)
  colnames(beardataDF) <- "beardata"
  beardataDF <- round(beardataDF,0)
  beardataW <- merge(beardataDF,beardataT,by="beardata",all.x=TRUE)
                       
  Cellbear <- mean.circular(beardata)

  ## Calculate the weighted mean bearing for the cell (frequency based)
  WeighCellbear <- weighted.mean.circular(beardata,beardataW$Freq, units = c("degree"),zero = 0, rotation = c("counter", "clock"))## Not working
  #AltCellbear <- circ.mean(beardata)
  #AltCellbear <- rad2deg(AltCellbear)
  CellMin <- min(beardata)
  CellMax <- max(beardata)
  CellMedian <- median(beardata)
  CellStdDev <- sd.circular(beardata)
  CellVar <- var.circular(beardata)
  
  ## Summarize bearings into direction categories
  CDdata$Direction <- 1
  CDdata$Direction[CDdata$bearing > 0 & CDdata$bearing <= 45] <- "NNE"
  CDdata$Direction[CDdata$bearing > 45 & CDdata$bearing <= 90] <- "ENE"
  CDdata$Direction[CDdata$bearing > 90 & CDdata$bearing <= 135] <- "ESE"
  CDdata$Direction[CDdata$bearing > 135 & CDdata$bearing <= 180] <- "SSE"
  
  CDdata$Direction[CDdata$bearing > -180 & CDdata$bearing <= -135] <- "SSW"
  CDdata$Direction[CDdata$bearing > -135 & CDdata$bearing <= -90] <- "WSW"
  CDdata$Direction[CDdata$bearing > -90 & CDdata$bearing <= -45] <- "WNW"
  CDdata$Direction[CDdata$bearing > -45 & CDdata$bearing <= -1] <- "NNW"
  
  ## Calculate percentage of species in the cell per category
  Allsp <- nrow(CDdata)
  PercNNE <- sum(CDdata$Direction == "NNE")/(Allsp/100)
  PercENE <- sum(CDdata$Direction == "ENE")/(Allsp/100)
  PercESE <- sum(CDdata$Direction == "ESE")/(Allsp/100)
  PercSSE <- sum(CDdata$Direction == "SSE")/(Allsp/100)
  PercSSW <- sum(CDdata$Direction == "SSW")/(Allsp/100)
  PercWSW <- sum(CDdata$Direction == "WSW")/(Allsp/100)
  PercWNW <- sum(CDdata$Direction == "WNW")/(Allsp/100)
  PercNNW <- sum(CDdata$Direction == "NNW")/(Allsp/100)

  ## Save relevant values
  alldata <- (c(x=x,y=y,Bearing=Cellbear, WeighBearing=WeighCellbear,MinDir=CellMin,MaxDir=CellMax,MedDir=CellMedian,StdDev=CellStdDev,Variance=CellVar,SpNumber=Allsp,
                PercNNE=PercNNE,PercENE=PercENE,PercESE=PercESE,PercSSE=PercSSE,PercSSW=PercSSW,PercWSW=PercWSW,PercWNW=PercWNW,PercNNW=PercNNW))
  alldata <- as.data.frame(alldata)
  return(alldata)
  }
})

str(bearData)

Bearings <- do.call(rbind.fill,bearData)
head(Bearings)
str(Bearings)
levelplot(Bearing~x.x*y.y,data=Bearings)

saveRDS(Bearings, "data/Bearings_range_centroids_xy_2080_rcp60_WeighM.rds", compress="xz")
