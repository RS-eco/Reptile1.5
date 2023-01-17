#-#-# Add dispersal to predictions data

# Load libraries
rm(list=ls()); invisible(gc())
packages <- c("raster", "readr", "rgeos", "tidyr", "sp", "ggplot2", "snowfall")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load required packages
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)

########################################

# Set file directory
filedir <- "C:/Users/admin/Documents"

# Set working directory
setwd("C:/Users/admin/Documents/BioScen1.5_SDM") 

########################################

# Set taxa
taxa <- "Reptile"
i <- 1

# Model type
k <- 1; model_type <- c("GAM", "GBM")[k]

#-#-# Get future data #-#-#
predpath <- paste0(filedir, "/", taxa[i], "_", model_type, "_predictions/")

# List all files
modsAll <- list.files(predpath, full.names = T)
spNames <- sapply(basename(modsAll), function(x){
  paste0(strsplit(x, split="_")[[1]][1:2], collapse="_")
})
length(spNames)

# Extract species names 
AUC_data <- lapply(c("GAM", "GBM"), function(model_type){
  read.csv(paste0(filedir, "/AUCvalues_All_", 
                  model_type, "_", taxa[i], ".csv.xz"))})
AUC_data <- do.call(rbind, AUC_data)

AUC_sum <- AUC_data %>% group_by(Species, taxa, model_type) %>% 
  dplyr::summarise(mean = mean(AUC, na.rm=T)) %>% filter(mean >= 0.7) %>% ungroup() %>% 
  group_by(Species, taxa) %>% dplyr::summarise(n = n()) %>% filter(n == 2)

spNames <- unique(spNames[spNames %in% AUC_sum$Species])
length(spNames)

#-#-# Set filepath original distribution and future #-#-#
SpeciesData <- paste0(filedir, "/GARD_SpeciesData/")
resultspath <- paste0(filedir, "/", taxa[i], "_", model_type, "_results_climate/")
if(!dir.exists(resultspath)){dir.create(resultspath)}

#-#-# Get area of each gridcell #-#-#
area <- read.csv("https://github.com/christianhof/BioScen1.5_SDM/raw/master/data/realm_coordinates.csv")
area <- area[,c("x", "y", "area")]
colnames(area) <- c("x","y","areaKM2")

# Get missing names
names_mis <- lapply(spNames, function(x){
  if(!file.exists(paste0(resultspath,  x, "_", model_type, "_dispersal.csv.xz"))){
    return(x)
  }
})
names_mis <- Filter(Negate(is.null), names_mis)
length(names_mis)

# List all summary prediction files, where dispersal is missing
#files <- unlist(lapply(names_mis, function(species){
#  list.files(paste0(filedir, "/", taxa[i], "_", model_type, "_predictions"), 
#             pattern=species, full.names=T)}))
#length(files)

# Check for corrupt files
#corrupt_files <- lapply(files, function(x){
#  data <- tryCatch(readr::read_csv(x), error=function(e) e) #The prediction files
#  if(inherits(data, "error")){
#    return(x)
#  } else{
#    return(NULL)
#  }
#})
#corrupt_files <- unlist(Filter(Negate(is.null), corrupt_files))
#length(corrupt_files)
#file.remove(corrupt_files) # Remove corrupt files

#-#-# Add area size, clip and calculate loss of climatically suitable space #-#-#
n <- ceiling(0.4*parallel::detectCores())
sfInit(parallel=TRUE, cpus=n)
sfLibrary(raster); sfLibrary(sp); sfLibrary(dplyr); sfLibrary(tidyr)
sfLibrary(rgeos); sfLibrary(readr)
sfExport(list=c("area", "SpeciesData", "predpath", "resultspath", "model_type")) 
sfLapply(names_mis, function(x){
  #print(x)
  if(!file.exists(paste0(resultspath,  x, "_", model_type, "_dispersal.csv.xz"))){
    ## Future dist
    spPred <- readr::read_csv(paste0(predpath,x,"_", model_type, "_predict.csv.xz"))
    
    ## Current dist
    spOrig <- raster::raster(paste0(SpeciesData,x,"_0.5.tif"))
    coord <- round(coordinates(spOrig),4)
    values <- getValues(spOrig)
    origDist <- (as.data.frame(cbind(coord,values)))
    origDist <- subset(origDist,values==1)
    colnames(origDist) <- c("x","y","presence")
    
    # Calculate dispersal distance
    poly <- raster::rasterToPolygons(spOrig,fun=function(x){x == 1},dissolve=TRUE)
    poly <- sp::disaggregate(poly)
    largepoly <- poly[which.max(sapply(poly@polygons, function(x) x@Polygons[[1]]@area)),]
    xdist <- xmax(largepoly) - xmin(largepoly)
    ydist <- ymax(largepoly) - ymin(largepoly)
    
    # Various distances
    dist1 <- sqrt((xdist^2+ydist^2))
    disp1 <- rgeos::gBuffer(poly, byid=TRUE, width=dist1)
    disp1 <- crop(disp1, extent(spOrig))
    disp1 <- raster::rasterize(disp1, spOrig, field=1)
    dispDist1 <- as.data.frame(raster::rasterToPoints(disp1))
    colnames(dispDist1) <- c("x","y","disp_one")
    
    dist2 <- sqrt((xdist^2+ydist^2))/2
    disp2 <- rgeos::gBuffer(poly, byid=TRUE, width=dist2)
    disp2 <- crop(disp2, extent(spOrig))
    disp2 <- raster::rasterize(disp2, spOrig, field=1)
    dispDist2 <- as.data.frame(rasterToPoints(disp2))
    colnames(dispDist2) <- c("x","y","disp_half")
    
    dist3 <- sqrt((xdist^2+ydist^2))/4
    disp3 <- rgeos::gBuffer(poly, byid=TRUE, width=dist3)
    disp3 <- crop(disp3, extent(spOrig))
    disp3 <- raster::rasterize(disp3, spOrig, field=1)
    dispDist3 <- as.data.frame(raster::rasterToPoints(disp3))
    colnames(dispDist3) <- c("x","y","disp_quarter")
    
    dist4 <- sqrt((xdist^2+ydist^2))/8
    disp4 <- rgeos::gBuffer(poly, byid=TRUE, width=dist4)
    disp4 <- crop(disp4, extent(spOrig))
    disp4 <- raster::rasterize(disp4, spOrig, field=1)
    dispDist4 <- as.data.frame(raster::rasterToPoints(disp4))
    colnames(dispDist4) <- c("x","y","disp_eigth")
    
    dist5 <- sqrt((xdist^2+ydist^2))/12
    disp5 <- rgeos::gBuffer(poly, byid=TRUE, width=dist5)
    disp5 <- crop(disp5, extent(spOrig))
    disp5 <- raster::rasterize(disp5, spOrig, field=1)
    dispDist5 <- as.data.frame(raster::rasterToPoints(disp5))
    colnames(dispDist5) <- c("x","y","disp_twelth")
    
    dist6 <- sqrt((xdist^2+ydist^2))/16
    disp6 <- rgeos::gBuffer(poly, byid=TRUE, width=dist6)
    disp6 <- crop(disp6, extent(spOrig))
    disp6 <- raster::rasterize(disp6, spOrig, field=1)
    dispDist6 <- as.data.frame(raster::rasterToPoints(disp6))
    colnames(dispDist6) <- c("x","y","disp_sixteenth")
    
    dist7 <- sqrt((xdist^2+ydist^2))/20
    disp7 <- rgeos::gBuffer(poly, byid=TRUE, width=dist7)
    disp7 <- crop(disp7, extent(spOrig))
    disp7 <- raster::rasterize(disp7, spOrig, field=1)
    dispDist7 <- as.data.frame(raster::rasterToPoints(disp7))
    colnames(dispDist7) <- c("x","y","disp_twentieth")
    
    dataList <- list(dispDist7, dispDist6, dispDist5, dispDist4, dispDist3, dispDist2, dispDist1)
    
    Dispersal <- Reduce(function(...) merge(..., all=T), dataList)
    
    ## Merge dist files
    CP <- dplyr::full_join(origDist, Dispersal, by=c("x","y"))
    CP <- dplyr::left_join(spPred, CP, by=c("x", "y"))
    CP$fulldisp <- 1
    
    ## Add cell size 
    CPA <- dplyr::left_join(CP, area, by=c("x","y"))
    
    ## Set 0 values to NA
    CPA[CPA == 0] <- NA
    
    # Remove rows where all values are NA
    CPA <- CPA[apply(CPA, 1, function(y) !all(is.na(y))),]
    
    # Change column names to correct names
    colnames(CPA) <- sub("-", ".", colnames(CPA))
    
    # Remove dispersal scenario for 1995
    #CPA$EWEMBI_1995[is.na(CPA$presence)] <- 0
    
    # Save to file
    readr::write_csv(CPA, paste0(resultspath,  x, "_", model_type, "_dispersal.csv.xz"))
    return(x)
  }
})
sfStop()
