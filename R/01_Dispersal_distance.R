#' ---
#' title: "Dispersal distances of Reptile species"
#' author: "RS-eco"
#' ---

# Empty directory
rm(list=ls()); invisible(gc())

# Set file directory
filedir1 <- sub("/R$", "", getwd())
filedir2 <- "I:/"

#' ## Calculate dispersal distances
#+ eval=F, echo=F
if(!file.exists(paste0(filedir1, "/data/dispersal_distances.rda"))){
  # Load libraries
  packages <- c("raster", "readr", "dplyr", "rgeos", "tidyr", "sp")
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages); rm(new.packages)
  
  # Load required packages
  l <- sapply(packages, require, character.only = TRUE); rm(packages, l)
  
  # Set file directory

  # Set taxa
  taxa <- "Reptile"
  
  # Read AUC data
  AUC_data <- lapply(c("GAM", "GBM"), function(model_type){
    readRDS(paste0(filedir1, "/data/AUCvalues_All_", 
                             model_type, "_", taxa, ".rds"))
  })
  AUC_data <- do.call(rbind, AUC_data)
  
  # Aggregate the different AUC values from the 10 iterations per species
  # and filter by AUC > 0.7
  AUC_sum <- AUC_data %>% group_by(Species, taxa, model_type) %>% 
    summarise(mean = mean(AUC, na.rm=T)) %>% filter(mean >= 0.7) %>% ungroup() %>% 
    group_by(Species, taxa) %>% summarise(n = n()) %>% filter(taxa == "Reptile", n == 2)
  spNames <- unique(AUC_sum$Species)
  
  #-#-# Set filepath original distribution and future #-#-#
  SpeciesData <- paste0(filedir2, "/GARD_SpeciesData/")
  
  #-#-# Get area of each gridcell #-#-#
  area <- read.csv("https://github.com/christianhof/BioScen1.5_SDM/raw/master/data/realm_coordinates.csv")
  area <- area[,c("x", "y", "area")]
  colnames(area) <- c("x","y","areaKM2")
  
  # Load group names of reptile species
  # Load rasterSp package
  #remotes::install_github("RS-eco/rasterSp")
  library(rasterSp)
  data(gard_reptiles)
  
  #-#-# Calculate dispersal distances
  dist <- lapply(spNames, function(x){
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
    dist2 <- sqrt((xdist^2+ydist^2))/2
    dist3 <- sqrt((xdist^2+ydist^2))/4
    dist4 <- sqrt((xdist^2+ydist^2))/8
    dist5 <- sqrt((xdist^2+ydist^2))/12
    dist6 <- sqrt((xdist^2+ydist^2))/16
    dist7 <- sqrt((xdist^2+ydist^2))/20
    data <- data.frame(dist7=dist7, dist6=dist6, dist5=dist5, 
                       dist4=dist4,dist3=dist3,dist2=dist2,dist1=dist1)
    data$species <- x
    return(data)
  })
  dist <- do.call(rbind, dist)
  
  # Degree to km
  dist$dist1 <- dist$dist1*111.2
  dist$dist2 <- dist$dist2*111.2
  dist$dist3 <- dist$dist3*111.2
  dist$dist4 <- dist$dist4*111.2
  dist$dist5 <- dist$dist5*111.2
  dist$dist6 <- dist$dist6*111.2
  dist$dist7 <- dist$dist7*111.2
  dispersal_distances <- dplyr::left_join(dist, AUC_sum, by=c("species"="Species")) %>%
    left_join(gard_reptiles %>% select(Binomial, Group), by=c("species"="Binomial")) 
  dispersal_distances <- dplyr::select(dispersal_distances, -n)
  colnames(dispersal_distances)
  saveRDS(dispersal_distances, file="data/dispersal_distances.rds", compress="xz")
}

#' Read dispersal distances
load(paste0(filedir1, "/data/dispersal_distances.rda"))

#' ## Plot density plot of dispersal distances
#+ message=F, echo=F, fig.width=10, fig.height=6
library(ggplot2); library(ggridges); library(dplyr)
dispersal_distances %>% tidyr::gather(disp, value, -c(species, taxa)) %>% 
  dplyr::mutate(disp = factor(disp, labels=c("d", "d/2", "d/4", "d/8", "d/12", "d/16", "d/20"))) %>% 
  ggplot() + 
  geom_density_line(aes(x=value, colour=taxa), stat="bin", bins=100, fill="transparent") + 
  #geom_histogram(aes(x=value, fill=taxa), bins=100) + 
  facet_wrap(. ~ disp, scales="free") + theme_bw() + 
  theme(strip.background = element_blank()) + 
  labs(x="Distance (km)", y="Frequency")

#' ## Table of mean & median dispersal distances
#+ echo=F
dispersal_distances %>% tidyr::gather(disp, value, -c(species, taxa)) %>% 
  dplyr::mutate(disp = factor(disp, labels=c("d", "d/2", "d/4", "d/8", "d/12", "d/16", "d/20"))) %>%
  dplyr::group_by(disp) %>% dplyr::summarise(mean=mean(value),
                                             mean_yr_2050 = mean(value)/55,
                                             mean_yr_2080 = mean(value)/85,
                                             median=median(value),
                                             median_yr_2050 = median(value)/55,
                                             median_yr_2080 = median(value)/85) %>% t() %>% 
  knitr::kable()

