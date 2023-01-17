#' ---
#' title: "Create ensemble reptile distribution per species split into groups"
#' author: "RS-eco"
#' ---

# Empty R environment
rm(list=ls()); gc()

# Check if libraries are installed
packages <- c("data.table", "dtplyr", "tidyr")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load required packages
library(dplyr); library(data.table); library(tidyr)

# Set file directory
filedir <- "~/Documents/"
#filedir <- "C:/Users/Admin/Documents/"

# Set working directory
setwd("/home/matt/Documents/Reptile1.5")

# Set taxa
taxa <- "Reptile"

# Result type
j <- 1; res_type <- c("climate", "thresholded")[j]

# Sub type according to result type
if(res_type == "climate"){sub <- "prob"} else{sub <- "thresh"}

# Dispersal type
dispersal <- c("disp_quarter", "disp_eigth", "disp_sixteenth") 
if(sub == "prob"){dispersal <- dispersal[2]}
#c("presence", "disp_one", "disp_half", "disp_quarter", "disp_eigth", 
#  "disp_twelth", "disp_sixteenth", "disp_twentieth", "fulldisp")

# Groups
group_names <- c("croc", "lizard", "snake", "turtle", "worm lizard")

# Create scenario combinations
pres_scen <- expand.grid(rcp="EWEMBI", year=1995)
scen_2050 <- expand.grid(rcp=c("rcp26", "rcp60", "rcp85"), year=2050)
scen_2080 <- expand.grid(rcp=c("rcp26", "rcp60", "rcp85"), year=2080)

lapply(group_names, function(groups){
  lapply(dispersal, function(disp){
    lapply(list(pres_scen, scen_2050, scen_2080), function(df){
      if(!file.exists(paste0("data/", taxa, "_", groups, "_", sub, "_", 
                             disp, "_sp_locs.rds"))){
        if(!file.exists(paste0("data/", taxa, "_", groups, "_", sub, "_", 
                             disp, "_", df$year[1], "_sp_locs.rds"))){
        dat_all <- lapply(1:nrow(df),function(y){
          print(paste(groups, disp, df[y,]))
          files <- list.files(paste0(filedir, "Reptile1.5/extdata/"),
                              pattern=paste0(taxa, "_", sub, ".*", "_", disp, "_", ".*\\", 
                                             ".*", df$rcp[y], "_", df$year[y], "_", groups, ".rds"),
                              full.names=T)
          if(df$rcp[y] == "EWEMBI" & length(files) == 2 | df$rcp[y] != "EWEMBI" & length(files) == 8){
            if(sub == "thresh"){
              digits = 0
            } else if(sub == "prob"){
              digits = 2
            }   
            dat_try <- readRDS(files[1])
            colname <- colnames(dat_try)
            ncols <- ncol(dat_try); rm(dat_try); invisible(gc())
            col1 <- colname[1:ceiling(ncols/5)]
            dat1 <- lapply(files, function(x){
              dat <- readRDS(x) %>% as.data.table() %>% .[, col1, with=F] %>%
                melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
                .[, presence := round(presence, digits=digits)] %>% 
                .[presence != 0,]
              dat$algorithm <- strsplit(basename(x), split="_")[[1]][[3]]
              dat$gcm <- strsplit(basename(x), split="_")[[1]][[6]]
              return(dat)
            }) %>% rbindlist(); invisible(gc())
            col2 <- c("x", "y", colname[(ceiling(ncols/5)+1):(2*ceiling(ncols/5))])
            dat2 <- lapply(files, function(x){
              dat <- readRDS(x) %>% as.data.table() %>% .[, col2, with=F] %>%
                melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
                .[, presence := round(presence, digits=digits)] %>% 
                .[presence != 0,]
              dat$algorithm <- strsplit(basename(x), split="_")[[1]][[3]]
              dat$gcm <- strsplit(basename(x), split="_")[[1]][[6]]
              return(dat)
            }) %>% rbindlist(); invisible(gc())
            col3 <- c("x", "y", colname[((2*ceiling(ncols/5))+1):(3*ceiling(ncols/5))])
            dat3 <- lapply(files, function(x){
              dat <- readRDS(x) %>% as.data.table() %>% .[, col3, with=F] %>%
                melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
                .[, presence := round(presence, digits=digits)] %>% 
                .[presence != 0,]
              dat$algorithm <- strsplit(basename(x), split="_")[[1]][[3]]
              dat$gcm <- strsplit(basename(x), split="_")[[1]][[6]]
              return(dat)
            }) %>% rbindlist(); invisible(gc())
            col4 <- c("x", "y", colname[((3*ceiling(ncols/5))+1):(4*ceiling(ncols/5))])
            dat4 <- lapply(files, function(x){
              dat <- readRDS(x) %>% as.data.table() %>% .[, col4, with=F] %>%
                melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
                .[, presence := round(presence, digits=digits)] %>% 
                .[presence != 0,]
              dat$algorithm <- strsplit(basename(x), split="_")[[1]][[3]]
              dat$gcm <- strsplit(basename(x), split="_")[[1]][[6]]
              return(dat)
            }) %>% rbindlist(); invisible(gc())
            col5 <- c("x", "y", colname[((4*ceiling(ncols/5))+1):ncols])
            dat5 <- lapply(files, function(x){
              dat <- readRDS(x) %>% as.data.table() %>% .[, col5, with=F] %>%
                melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
                .[, presence := round(presence, digits=digits)] %>% 
                .[presence != 0,]
              dat$algorithm <- strsplit(basename(x), split="_")[[1]][[3]]
              dat$gcm <- strsplit(basename(x), split="_")[[1]][[6]]
              return(dat)
            }) %>% rbindlist(); invisible(gc())
            dat <- data.table::rbindlist(list(dat1, dat2, dat3, dat4, dat5)); invisible(gc())
            dat$disp <- disp
            dat$rcp <- df$rcp[y]
            dat$year <- df$year[y]
            dat$group <- groups
          } else{
            print("Error")
          }
          return(dat); invisible(gc())
        }); invisible(gc())
        dat_all <- data.table::rbindlist(dat_all); invisible(gc())
        saveRDS(dat_all, file=paste0("data/", taxa, "_", groups, "_", sub, "_", 
                                     disp, "_", df$year[1], "_sp_locs.rds"), 
                compress="xz"); rm(dat_all); invisible(gc())
        }
      }
    })
  })
})

# Merge files of individual years
lapply(group_names, function(groups){
  lapply(dispersal, function(disp){
    if(!file.exists(paste0("data/", taxa, "_", groups, "_", sub, "_", 
                           disp, "_sp_locs.rds"))){
      files <- list.files(paste0(filedir, "Reptile1.5/data/"),
                          pattern=paste0(taxa, "_", groups, "_", sub, "_", disp), full.names=T)
      dat <- lapply(files, function(x){readRDS(x) %>%
          unite("gcm_rcp_year", c(gcm,rcp,year), sep="_") %>% 
          tidyr::pivot_wider(names_from="gcm_rcp_year", values_from="presence")})
      dat <- Reduce(function(...) dplyr::full_join(...), dat) %>% 
        mutate_at(vars(-c(x,y,species,algorithm,disp,group)), replace_na, 0)
      colnames(dat) <- sub("EWEMBI_", "", colnames(dat)); invisible(gc())
      saveRDS(dat, file=paste0("data/", taxa, "_", groups, "_", sub, "_", disp, "_sp_locs.rds"), 
              compress="xz"); rm(dat); file.remove(files); invisible(gc())
    }
  })
})
