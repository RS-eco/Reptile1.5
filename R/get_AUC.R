#' ## Get AUC stats across species and predictor combinations
#' July 2017

rm(list=ls())

library(snowfall)
library(plyr)

setwd("/home/avoskamp/BioScen1.5_1/Reptile_VariableSelectionModels/")
filedir <- "/home/avoskamp/BioScen1.5_1/Reptile_VariableSelectionModels/"


# List all files
GAMfiles <- list.files(filedir)
head(GAMfiles)

spList <- lapply(GAMfiles,function(sp){
  name <- lapply(sp, function(x){paste(strsplit(x,"_",fixed=TRUE)[[1]][1:2],collapse="_")})
})

spList <- unlist(unique(spList))

# Try on one output
GAM1File <- get(load(paste0(filedir, GAMfiles[100])))
head(GAM1File)

## Look at AUC in first output 
AUC1 <- GAM1File[[1]]$AUC

# Set path to model files
mod.path <- "/home/avoskamp/BioScen1.5_1/Reptile_VariableSelectionModels/"
resultsPath <- "/home/avoskamp/BioScen1.5_1/Reptile_variable_AUC_new"
if(!dir.exists(resultsPath)){dir.create(resultsPath)}

#modList <- GAMfiles[10]
#AUClist <- seq(1, 10, 1) 

#' Loop through all model output files to extract AUC and save summarized output
sfInit(parallel=TRUE, cpus=ceiling(0.2*parallel::detectCores()))
sfExport(list=c("GAMfiles","mod.path","resultsPath")) #Import all the data, file path and model function needed to each CPU

#Turn warning into error - if the model does not convert the code should stop rather than giving a warning
#options(warn=2)

sfLapply(GAMfiles,function(modList){
  
  print(modList)
  
  PA <- paste0(strsplit(modList,split="_",fixed=TRUE)[[1]][3])
  
  if(PA == 1||PA == 2||PA == 3||PA == 4||PA == 5||PA == 6||PA == 7||PA == 8||PA == 9||PA ==10){
  
  ##Get species name ### Reptile model files have nbo gap between name and iteration
  #iteration <- paste0(strsplit(modList,split="_",fixed=TRUE)[[1]][1],"_",strsplit(modList,split="_",fixed=TRUE)[[1]][2])
  
  # Check if model has 3 or 4 variables, by checking the length of the object
  LT <- length(unlist(strsplit(modList,split="_")))
  
  if(LT == 9){variables <- paste0(strsplit(modList,split="_",fixed=TRUE)[[1]][4:6],collapse="_")}
  if(LT == 10){variables <- paste0(strsplit(modList,split="_",fixed=TRUE)[[1]][4:7],collapse="_")}
  if(LT == 11){variables <- paste0(strsplit(modList,split="_",fixed=TRUE)[[1]][4:8],collapse="_")}

  print(variables)
  #}else{variables <- paste0(strsplit(modList,split="_",fixed=TRUE)[[1]][4:7],collapse="_")}
  
  spName <- paste0(strsplit(modList,split="_",fixed=TRUE)[[1]][1:2],collapse="_")
  
  if(!file.exists(paste(resultsPath,"/",spName,"_",PA,"_",variables,"_model_output_GAM.RData",sep=""))){
    
    ##Import species data
    mtry <- try(data.list <- get(load(paste0(mod.path,modList))))
    
    if(!inherits(mtry, "try-error")) {
      if(length(data.list)>0){
      
      ##Extract AUC values
      AUCdata <- c()  
      
      for(i in 1:length(data.list)){
        blk <- data.list[i]
        AUC <- blk[[1]]$AUC
        print(AUC)
        AUCdata <- rbind(AUCdata,AUC)
      }
      
      finaldata <- c(spName,PA, variables, unname(unlist(AUCdata)))
      finaldata <- as.data.frame(as.matrix(t(finaldata)))
      save(finaldata, file=paste(resultsPath,"/",spName,"_",PA,"_",variables,"_model_output_GAM.RData",sep=""),compress="xz") 
      rm(data.list,AUCdata)
    }}
    rm(data.list)
  }}
}); sfStop()

# Read all results back in and save in one dataframe
All.files <- list.files(paste0(resultsPath,"/"), 
                        pattern=".RData", full.names = TRUE)

all.sp.auc <- lapply(All.files,function(x){
  print(x)
  spdata <- get(load(x))
  head(spdata)
  return(spdata)
})

all.sp.auc <- do.call(rbind.fill,all.sp.auc)
colnames(all.sp.auc)<-c("Species","PA","Variables","AUC.1","AUC.2","AUC.3","AUC.4","AUC.5","AUC.6","AUC.7","AUC.8","AUC.9","AUC.10")
head(all.sp.auc)
nrow(all.sp.auc)
## Save AUC file as csv
filedir <- "/home/avoskamp/BioScen1.5_1/"
write.csv(all.sp.auc, "/home/avoskamp/BioScen1.5_1/AUCvaluesAllModelsGAM_Reptiles.csv") 

# Aggregate the different AUC values from the 10 iterations per species
All.AUC <- read.csv(paste0(filedir, "AUCvaluesAllModelsGAM_Reptiles.csv"))
head(All.AUC)

AUCdata <- All.AUC[c(5:14)]
AUCdata <- rowMeans(AUCdata,na.rm=TRUE)
All.AUC$MeanAUCblocks <- AUCdata
All.Sub <- All.AUC[c(2,4,15)] #species, variables, mean

All.Sub.M <- tidyr::unite(All.Sub, newcol, c(Species, Variables),remove=FALSE)
All.Sub.M <- All.Sub.M[c(1,4)]
colnames(All.Sub.M) <- c("Models","AUC")
head(All.Sub.M)
nrow(All.Sub.M)

All.AUC.MeanPerSp <- aggregate(.~Models, data=All.Sub.M, mean)
head(All.AUC.MeanPerSp)
nrow(All.AUC.MeanPerSp)

readr::write_csv(All.AUC.MeanPerSp, path=paste0(filedir,"AUCvaluesAllModelsGAM_Sum_", "Reptiles", ".csv"))

# Rank combinations from aggregated file
All.AUC <- read.csv(paste0(filedir, "/AUCvaluesAllModelsGAM_Sum_", "Reptiles", ".csv"))
head(All.AUC)

# Spit Models
All.AUC$Species <- sapply(All.AUC$Models, function(x) paste0(strsplit(as.character(x), split="_", fixed=TRUE)[[1]][1:2], collapse="_")) 
All.AUC$Models <- sapply(All.AUC$Models, function(x) paste0(strsplit(as.character(x), split="_", fixed=TRUE)[[1]][-c(1,2)], collapse="_")) 

##Loop through species and rank by AUC value
spList <- unique(as.vector(All.AUC$Species))

AUCdata <- lapply(spList,function(x){
  print(x)
  sp <- subset(All.AUC,Species == x)
  sp <- sp[with(sp,order(-AUC)),]
  rankNr <- c(1:nrow(sp))
  sp$rank <- rankNr
  return(sp)
})

AUCrank <- do.call(rbind,AUCdata)
readr::write_csv(AUCrank, path=paste0(filedir,"AUCvaluesAllModelsGAM_Rank_", "Reptiles", ".csv"))
