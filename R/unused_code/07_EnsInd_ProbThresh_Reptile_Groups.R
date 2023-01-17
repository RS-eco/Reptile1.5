#' ---
#' title: "Create ensemble reptile distribution per species split into groups"
#' author: "RS-eco"
#' ---

# Empty R environment
rm(list=ls()); gc()

# Check if libraries are installed
packages <- c("data.table", "dtplyr", "tidyverse")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load required packages
library(dplyr); library(data.table)

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
#c("presence", "disp_one", "disp_half", "disp_quarter", "disp_eigth", 
#  "disp_twelth", "disp_sixteenth", "disp_twentieth", "fulldisp")

# Groups
group_names <- c("croc", "lizard", "snake", "turtle", "worm lizard")

# Create scenario combinations
pres_scen <- expand.grid(rcp="EWEMBI", year=1995)
rcp_year <- expand.grid(rcp=c("rcp26", "rcp60", "rcp85"), year=c(2050,2080))
df <- dplyr::bind_rows(pres_scen, rcp_year)

lapply(group_names, function(groups){
  lapply(dispersal, function(disp){
    if(!file.exists(paste0("data/", taxa, "_", groups, "_", sub, "_", 
                           disp, "_sp_ensemble_locs.rds"))){
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
          col1 <- colname[1:ceiling(ncols/4)]
          dat1 <- lapply(files, function(x) readRDS(x) %>% 
                           as.data.table() %>% .[, col1, with=F]); invisible(gc())
          dat1 <- dat1 %>% data.table::rbindlist() %>% 
            .[, lapply(.SD, mean, na.rm=T), by=.(x,y)] %>% 
            melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
            .[, presence := round(presence, digits=digits)] %>% 
            .[presence != 0,]; invisible(gc())
          col2 <- c("x", "y", colname[(ceiling(ncols/4)+1):(2*ceiling(ncols/4))])
          dat2 <- lapply(files, function(x) readRDS(x) %>% 
                           as.data.table() %>% .[, col2, with=F]); invisible(gc())
          dat2 <- dat2 %>% data.table::rbindlist() %>% 
            .[, lapply(.SD, mean, na.rm=T), by=.(x,y)] %>% 
            melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
            .[, presence := round(presence, digits=digits)] %>% 
            .[presence != 0,]; invisible(gc())
          col3 <- c("x", "y", colname[((2*ceiling(ncols/4))+1):(3*ceiling(ncols/4))])
          dat3 <- lapply(files, function(x) readRDS(x) %>% 
                           as.data.table() %>% .[, col3, with=F]); invisible(gc())
          dat3 <- dat3 %>% data.table::rbindlist() %>% 
            .[, lapply(.SD, mean, na.rm=T), by=.(x,y)] %>% 
            melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
            .[, presence := round(presence, digits=digits)] %>% 
            .[presence != 0,]; invisible(gc())
          col4 <- c("x", "y", colname[((3*ceiling(ncols/4))+1):ncols])
          dat4 <- lapply(files, function(x) readRDS(x) %>% 
                           as.data.table() %>% .[, col4, with=F]); invisible(gc())
          dat4 <- dat4 %>% data.table::rbindlist() %>% 
            .[, lapply(.SD, mean, na.rm=T), by=.(x,y)] %>% 
            melt(id.vars=c("x","y"), variable.name="species", value.name="presence") %>% 
            .[, presence := round(presence, digits=digits)] %>% 
            .[presence != 0,]; invisible(gc())
          dat <- data.table::rbindlist(list(dat1, dat2, dat3, dat4))
          rm(dat1, dat2, dat3, dat4); invisible(gc())
          dat$disp <- disp
          dat$rcp <- df$rcp[y]
          dat$year <- df$year[y]
          dat$group <- groups
        } else{
          print("Error")
        }
        return(dat); invisible(gc())
      })
      dat_all <- data.table::rbindlist(dat_all); invisible(gc())
      saveRDS(dat_all, file=paste0("data/", taxa, "_", groups, "_", sub, "_", 
                                   disp, "_sp_ensemble_locs.rds"), 
              compress="xz"); rm(dat_all); invisible(gc())
    }
  })
})
