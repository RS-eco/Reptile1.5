rm(list=ls()); invisible(gc())

# Load packages

library(data.table); library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr); library(magrittr)

#########################

#' ### Calculate range size information ###
if(!file.exists("data/range_sizes.rds")){
  # Create algorithm, dispersal, gcm, rcp, year and group combinations
  comb_pres <- expand.grid(algorithm = c("GAM", "GBM"),
                           disp = c("disp_quarter", "disp_eigth", "disp_sixteenth"),
                           gcm = "EWEMBI", year = 1995, group = c("croc", "lizard", "snake", "turtle", "worm lizard"))
  comb_future <- expand.grid(algorithm = c("GAM", "GBM"),
                             disp = c("disp_quarter", "disp_eigth", "disp_sixteenth"),
                             gcm = c("GFDL.ESM2M_rcp26", "HadGEM2.ES_rcp26", "IPSL.CM5A.LR_rcp26", "MIROC5_rcp26",
                                     "GFDL.ESM2M_rcp60", "HadGEM2.ES_rcp60", "IPSL.CM5A.LR_rcp60", "MIROC5_rcp60",
                                     "GFDL.ESM2M_rcp85", "HadGEM2.ES_rcp85", "IPSL.CM5A.LR_rcp85", "MIROC5_rcp85"),
                             year = c(2050, 2080), group = c("croc", "lizard", "snake", "turtle", "worm lizard"))
  combs <- bind_rows(comb_pres, comb_future)
  
  # Create area data.frame
  r <- raster::raster(nrow=360, ncol=720)
  area <- raster::area(r)
  area_df <- as.data.frame(raster::rasterToPoints(area))
  colnames(area_df) <- c("x", "y", "area")
  
  # Missing files
  files <- sapply(1:nrow(combs), function(x){paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], "_", 
                                                    combs$gcm[x], "_", combs$year[x], "_", combs$group[x], ".rds")})
  mis <- files[!file.exists(files)]
  print(mis)

  # Loop through all combinations
  dat <- lapply(1:nrow(combs), function(x){
    if(file.exists(paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], "_", 
                          combs$gcm[x], "_", combs$year[x], "_", combs$group[x], ".rds"))){
      if(!file.exists(paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], "_", 
                             combs$gcm[x], "_", combs$year[x], "_", combs$group[x], "_rangesize.rds"))){
        print(x)
        # Load data file
        sp1 <- readRDS(paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], "_", 
                              combs$gcm[x], "_", combs$year[x], "_", combs$group[x], ".rds"))
        # Calculate range size per species
        sp_range_size <- sp1 %>% left_join(area_df) %>% # Add area to species data
          dplyr::select(-c(x,y)) %>% mutate_all(list(~ . * area)) %>% select(-area) %>% 
          summarise_all(sum) %>% pivot_longer(everything(), names_to = "species", values_to = "range_size")
        #head(sp_range_size)
        
        # Calculate latitudinal/longitudinal range per species
        sp_xy_range <- lapply(colnames(sp1[,-c(1:2)]), function(z){
          dat <- sp1 %>%  dplyr::select(x,y,starts_with(z))
          colnames(dat) <- c("x", "y", "presence")
          dat <- dat %>% filter(presence == 1) %>% 
            mutate(species = z) %>% group_by(species) %>%
            summarise(lat_range=max(y)-min(y), 
                      min_lat = min(y), max_lat = max(y),
                      mean_lat = mean(y),
                      long_range=max(x)-min(x),
                      min_long = min(x), max_long=max(x),
                      mean_long = mean(x),
                      ncells=n())
          return(dat)
        }); gc()
        sp_xy_range <- bind_rows(sp_xy_range)
        #head(sp_xy_range)
        
        sp_dat <- full_join(sp_range_size, sp_xy_range)
        sp_dat$algorithm <- combs$algorithm[x]
        sp_dat$disp <- combs$disp[x]
        sp_dat$gcm_rcp <- combs$gcm[x]
        sp_dat$year <- combs$year[x]
        sp_dat$group <- combs$group[x]
        saveRDS(sp_dat, file=paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], "_", 
                                    combs$gcm[x], "_", combs$year[x], "_", combs$group[x], "_rangesize.rds"), compress="xz")
      } else{
        sp_dat <- readRDS(paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], "_", 
                                 combs$gcm[x], "_", combs$year[x], "_", combs$group[x], "_rangesize.rds"))
      }
      return(sp_dat)
    } else{
      return(NULL)
      print(paste0("extdata/Reptile_thresh_", combs$algorithm[x], "_", combs$disp[x], "_", 
                   combs$gcm[x], "_", combs$year[x], "_", combs$group[x], ".rds"))
    }
  })
  range_sizes <- bind_rows(dat)
  saveRDS(range_sizes, file="data/range_sizes.rds", compress="xz"); 
  file.remove(list.files("extdata", pattern="_rangesize.rds", full.names=T))
}
