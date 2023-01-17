#' ---
#' title: "Calculate range overlap between current and future projections"
#' author: "RS-eco"
#' ---

# Clear working environment
rm(list=ls()); invisible(gc())

# Set working directory
setwd("/home/matt/Documents/Reptile1.5")

# Load packages
library(dplyr); library(magrittr); library(tidyr); library(dtplyr)

#' ### Calculate range overlap information ###
if(!file.exists("data/range_overlap.rds")){
  
  # Create algorithm, dispersal, gcm, rcp, year and group combinations
  combs <- expand.grid(algorithm = c("GAM", "GBM"),
                       disp = c("disp_quarter", "disp_eigth", "disp_sixteenth"),
                       group = c("croc", "lizard", "snake", "turtle", "worm lizard"))
  
  # Missing files
  files <- lapply(1:nrow(combs), function(x){
    list.files("extdata/", pattern=paste0("Reptile_thresh_", combs$algorithm[x], "_", 
                                          combs$disp[x], "_.*\\_", combs$group[x], ".rds"), full.names=T)})
  
  # Create area data.frame
  r <- raster::raster(nrow=360, ncol=720)
  area <- raster::area(r)
  area_df <- as.data.frame(raster::rasterToPoints(area))
  colnames(area_df) <- c("x", "y", "area")
  
  # Calculate range overlap per species
  dat <- lapply(1:nrow(combs), function(x){
    if(!file.exists(paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], 
                           "_", combs$group[x], "_rangeoverlap.rds"))){
      if(length(files[[x]]) >= 2){
        # Load data file
        sp1 <- readRDS(files[[x]][[1]])
        col_name <- colnames(sp1[,-c(1:2)])
        sp_range_overlap <- lapply(2:length(files[[x]]), function(y){
          sp2 <- readRDS(files[[x]][[y]])
          dat <- lapply(1:length(col_name), function(z){
            dat1 <- sp1 %>% select(x,y,starts_with(col_name[z])) %>% drop_na()
            dat2 <- sp2 %>% select(x,y,starts_with(col_name[z])) %>% drop_na()
            colnames(dat1) <- c("x", "y", "pres_cur")
            colnames(dat2) <- c("x", "y", "pres_fut")
            dat <- full_join(dat1, dat2) %>% left_join(area_df) %>% 
              replace_na(list(pres_cur = 0, pres_fut = 0)) %>%
              mutate(dif = pres_fut + (2*pres_cur),
                     species = col_name[z]) %>% 
              group_by(species, dif, pres_cur, pres_fut) %>%
              summarise(tot_area = sum(area), tot_cells = n()); rm(dat1, dat2); gc()
            return(dat)
          }); rm(sp2); gc()
          dat <- bind_rows(dat)
          return(dat)
        })
        names(sp_range_overlap) <- sub(".rds", "", basename(files[[x]][2:length(files[[x]])]))
        sp_range_overlap <- bind_rows(sp_range_overlap, .id="groups")
        saveRDS(sp_range_overlap, file=paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], 
                                              "_", combs$group[x], "_rangeoverlap.rds"), compress="xz")
        rm(sp1, col_name); gc()
      }
    } else{
      sp_range_overlap <- readRDS(paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], 
                                         "_", combs$group[x], "_rangeoverlap.rds"))
    }
    return(sp_range_overlap)
  }); gc()
  range_overlap <- bind_rows(dat)
  saveRDS(range_overlap, file="data/range_overlap.rds", compress="xz"); 
  #file.remove(list.files("extdata", pattern="_rangeoverlap.rds", full.names=T))
}
