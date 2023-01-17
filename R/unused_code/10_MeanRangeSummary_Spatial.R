rm(list=ls()); invisible(gc())

# Load packages
library(tidyverse); library(patchwork); library(magrittr)
library(dtplyr); library(scico)

#########################

# Specify colour scheme
bluered <- rev(scico(n=255, palette="roma"))

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

#########################

#' ### Calculate mean species locations
if(!file.exists("data/mean_sp_locs.rds")){
  # Create algorithm, dispersal, gcm, rcp, year and group combinations
  comb_pres <- expand.grid(disp = "dispersal2",
                           gcm = "EWEMBI", year = 1995, group = c("lizard", "snake", "turtle", "worm lizard"))
  comb_fut <- expand.grid(disp = "dispersal2",
                          rcp = c("rcp26", "rcp60", "rcp85"),
                          year = c(2050, 2080), group = c("lizard", "snake", "turtle", "worm lizard"))
  combs <- bind_rows(comb_pres, comb_fut)
  
  #' **Note:** Some files are missing!!!
  
  files_pres <- lapply(1:nrow(comb_pres), function(x){list.files("extdata", pattern=paste0(comb_pres$disp[x], ".*",
                                                                                           comb_pres$year[x], "_", comb_pres$group[x], ".csv.xz"), full.names=T)})
  files_fut <- lapply(1:nrow(comb_fut), function(x){list.files("extdata", pattern=paste0(comb_fut$disp[x], ".*",
                                                                                         comb_fut$rcp[x], "_", comb_fut$year[x], "_", comb_fut$group[x], ".csv.xz"), full.names=T)})
  files_pres[[2]]
  files_fut[[1]]
  file_list <- c(files_pres, files_fut)
  
  # Loop through all combinations
  dat <- lapply(1:length(file_list), function(x){
    if(!file.exists(paste0("data/Reptile_thresh_", combs$disp[x], "_", 
                           combs$year[x], "_", combs$rcp[x], "_", combs$group[x], "_mean_dist.rds"))){
      # Load data file
      sp1 <- vroom::vroom(file_list[[x]])
      
      # Calculate sum distribution per species
      sp_sum_dist <- lapply(3:ncol(sp1), function(y){
        dat <- sp1 %>% select(x,y,colnames(sp1)[y]) %>% group_by(x,y) %>%
          summarise_at(colnames(sp1)[y], sum)
        colnames(dat) <- c("x", "y", "presence")
        dat$species <- colnames(sp1)[y]
        dat <- dat %>% filter(presence == 1)
        return(dat)
      }); rm(sp1); gc()
      sp_sum_dist %<>% bind_rows() 
      sp_sum_dist$disp <- combs$disp[x]
      sp_sum_dist$rcp <- combs$rcp[x]
      sp_sum_dist$year <- combs$year[x]
      sp_sum_dist$group <- combs$group[x]
      saveRDS(sp_sum_dist, file=paste0("data/Reptile_thresh_", combs$disp[x], "_", 
                                       combs$year[x], "_", combs$rcp[x], "_", combs$group[x], "_mean_dist.rds"), compress="xz")
    } else{
      sp_sum_dist <- readRDS(paste0("data/Reptile_thresh_", combs$disp[x], "_", 
                                    combs$year[x], "_", combs$rcp[x], "_", combs$group[x], "_mean_dist.rds"))
    }
    return(sp_sum_dist)
  })
  dat <- bind_rows(dat)
  saveRDS(dat, file="data/mean_sp_locs.rds", compress="xz")
}