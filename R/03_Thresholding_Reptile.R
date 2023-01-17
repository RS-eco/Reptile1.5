#-#-#  Clear workspace
rm(list=ls(all=TRUE)); gc()

#-#-# Libraries
library(SDMTools) # remotes::install_github("jjvanderwal/SDMTools")
library(raster)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(snowfall)
library(lattice)
library(geosphere)
library(plyr)

#-#-# Set file paths
filedir <- "C:/Users/admin/Documents" 
# filedir <- "/dss/dsshome1/lxc09/ge82nob2/" # LRZ

# Set taxa, model and years
taxa <- "Reptile"
model <- "GBM"
years <- c(2050, 2080)

predictions <- paste0(filedir, "/", taxa, "_", model, "_results_climate/")
resultpath <- paste0(filedir, "/", taxa, "_", model, "_results_thresholded/")
if(!dir.exists(resultpath)){dir.create(resultpath)}

#-#-# Get species list
modelfiles <- list.files(path=predictions, full.names=T)
sp.sample <- unlist(lapply(modelfiles,function(x){paste0(strsplit(basename(x),"_")[[1]][1],"_",strsplit(basename(x),"_")[[1]][2])}))

# Identify missing species
species_thresh <- list.files(resultpath)
sp_done <- unlist(lapply(species_thresh,function(x){paste0(strsplit(x,"_")[[1]][1],"_",strsplit(x,"_")[[1]][2])}))
sp_missing <- sp.sample[!sp.sample %in% sp_done]
length(sp_missing)

#broken_models <- modelfiles[!sp.sample %in% sp_done]
#broken_models

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##                                              Threshold
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

#-#-# Threshold function
n <- ceiling(0.4*parallel::detectCores())
sfInit(parallel=TRUE, cpus=n)
sfLibrary(snowfall);sfLibrary(SDMTools);sfLibrary(raster)
sfExport(list=c("sp.sample", "predictions", "model", "resultpath", "years"))     
sfLapply(sp_missing,function(sp){
  if(!file.exists(paste0(resultpath,sp, "_TSS_", model, ".csv.xz"))){
    print(sp)
    
    # Read in observed distribution
    preds <- read.csv(paste0(predictions, sp, "_", model, "_dispersal.csv.xz"))
    obs <- preds[,c("x","y", "presence")]
    obs <- na.omit(obs)
    test <- nrow(obs)

    if(test > 0){
      
      # Read in baseline predicted distribution
      coords <- preds[c("x","y")]
      
      # Calculate threshold for baseline scenario
      
      # Get base scenario
      base <- preds[c("x","y","EWEMBI_1995")]
      
      # Join obs and pred baseline and remove lat and lon, to calculate thresholds
      both <- merge(obs, base, by=c("x", "y"),all.y=TRUE)
      colnames(both) <- c("x","y","obs", "pred")
      both[is.na(both)] <- 0
      
      #_#_#_#_#  Calculate the optimal thresholds
      ot <- optim.thresh(both$obs, both$pred)
      ot2 <- unlist(ot)
      
      threshholds <- ot2["max.sensitivity+specificity"]
      thresh.type <- "TSS"
      print(threshholds)
      
      ## If statement for files with two equally good threshholds
      testthresh <- as.vector(threshholds)
      testthresh[is.na(testthresh)] <- 500
      if(testthresh == 500){
        threshholds <- ot2["max.sensitivity+specificity1"]
        thresh.type <- "TSS"
        print(threshholds)
      }
      
      Allthresh <- threshholds
      Allthresh <- as.data.frame(as.matrix(Allthresh))
      colnames(Allthresh) <- c("EWEMBI_1995")
      
      futlist <- tidyr::unite(expand.grid(c("GFDL.ESM2M_rcp26", "HadGEM2.ES_rcp26", "IPSL.CM5A.LR_rcp26", "MIROC5_rcp26", 
                             "GFDL.ESM2M_rcp60", "HadGEM2.ES_rcp60", "IPSL.CM5A.LR_rcp60", "MIROC5_rcp60", 
                             "GFDL.ESM2M_rcp85", "HadGEM2.ES_rcp85", "IPSL.CM5A.LR_rcp85", "MIROC5_rcp85"), years),
                             col="var")$var
        
      threshfut <- lapply(futlist,function(x){
        print(x)
        # Get the relevant threshold
        #futname <- strsplit(x,"_")[[1]][1]
        #relthresh <- grep(futname,colnames(Allthresh))
        #relthresh <- Allthresh[[relthresh]]
        relthresh <- Allthresh[1,1]
        
        # Get the future (base) data to threshold
        thresdata <- preds[c("x","y",x)]
        thresdata <- na.omit(thresdata)
        
        pred.bin.pres <- thresdata[thresdata[,3] >= relthresh,]
        
        if(nrow(pred.bin.pres)>0){
          
          pred.bin.pres$binary <-1
          
          # Give the cells below this threshold a 0
          pred.bin.abs <- thresdata[thresdata[,3] < relthresh,]
          
          if(nrow(pred.bin.abs) > 0){
            
            pred.bin.abs$binary <- 0
            
            # Combine the presence absence
            dat.thresh <- rbind(pred.bin.abs, pred.bin.pres)
            
            dat.thresh <- dat.thresh[,c(1:2,4)]
            colnames(dat.thresh) <- c("x","y",x)
            
            dat.thresh <- merge(coords,dat.thresh,all.x=TRUE) 
            
            return(dat.thresh)
            
          }else{
            
            dat.thresh <- pred.bin.pres[,c(1:2,4)]
            colnames(dat.thresh) <- c("x","y",x)
            dat.thresh <- merge(coords,dat.thresh,all.x=TRUE) 
            return(dat.thresh)
          }
          
        }else{
          
          number <- nrow(coords)
          print(number)
          dat.thresh <- as.data.frame(matrix(0, ncol = 1, nrow = number))
          dat.thresh <- cbind(coords,dat.thresh)
          colnames(dat.thresh) <- c("x","y",x)
          return(dat.thresh)  
        }  
      })
      
      allscenarioes <- Reduce(function(...) merge(...,by=c("x","y"),all=T),threshfut)
      
      # Add Thresholded EWEMBI, orig presence and dispersal
      relthresh <- Allthresh[1,1]
      EWEMBI <- preds[,c("x", "y", "EWEMBI_1995", "presence", 
                         "disp_one", "disp_half", "disp_quarter", "disp_eigth", 
                         "disp_twelth", "disp_sixteenth", "disp_twentieth", "fulldisp")]
      EWEMBI$EWEMBI_1995[EWEMBI$EWEMBI_1995 >= relthresh] <- 1
      EWEMBI$EWEMBI_1995[EWEMBI$EWEMBI_1995 < relthresh] <- 0
      
      Final <- merge(EWEMBI,allscenarioes,by=c("x","y"))
      
      # Save the thresholded files 
      readr::write_csv(Final, path= paste0(resultpath,sp, "_TSS_", model, ".csv.xz"))
    }
  }
}) 
sfStop()

#=#=# Test #-#-#
#test <- get(load(paste0(resultpath, "/Abeomelomys_sevia_TSS_GAM.Rdata")))
#head(test)
#library(ggplot2)
#ggplot() + geom_tile(data=test, aes(x=x,y=y,fill=as.factor(EWEMBI_1995)))
