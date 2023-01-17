#' ---
#' title: "Calculate reptile summed probability/presence per group"
#' author: "RS-eco"
#' ---

# Empty R environment
rm(list=ls()); gc()

# Load required packages
library(vroom); library(tidyr)
library(data.table); library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# Load rasterSp package
#remotes::install_github("RS-eco/rasterSp")
library(rasterSp)
data(gard_reptiles)

# Set file directory
filedir <- "C:/Users/admin/Documents/"
#filedir <- "/home/matt/Documents/"

# Set working directory
setwd("C:/Users/admin/Documents/Reptile1.5")
#setwd("/home/matt/Documents/Reptile1.5")

# Set taxa
taxa <- "Reptile"

# Model type
k <- 2; model_type <- c("GAM", "GBM")[k]

# Result type
j <- 2; res_type <- c("climate", "thresholded")[j]

# Sub type according to result type
if(res_type == "climate"){sub <- "prob"} else{sub <- "thresh"}

# Final species names
spNames <- sapply(list.files(paste0(filedir, "/", taxa, "_", 
                                    model_type, "_results_", res_type)), function(x){
                                      paste0(strsplit(x, split="_")[[1]][1:2], collapse="_")
                                    })

# Final prediction files
predFiles <- list.files(paste0(filedir, "/", taxa, "_", model_type, "_results_", res_type), 
                        full.names=T)
predFiles <- sapply(spNames, function(x) predFiles[grepl(predFiles, pattern=x)][[1]])
length(predFiles)

dat <- data.frame(Binomial=sub("_", " ", spNames)) %>% left_join(gard_reptiles)
predFiles <- split(predFiles, dat$Group)
group_names <- names(predFiles)

predData <- lapply(1:length(predFiles), function(x){
  dat <- vroom::vroom(predFiles[[x]])
  dat$Group <- group_names[x]
  dat <- dat %>% as.data.frame() %>% 
    dplyr::select(x, y, Group, disp_quarter, disp_eigth, disp_sixteenth, 
                  EWEMBI_1995:MIROC5_rcp85_2080) %>%
    mutate_at(c("disp_quarter", "disp_eigth", "disp_sixteenth"), as.numeric) %>% 
    group_by(x, y, Group, disp_quarter, disp_eigth, disp_sixteenth) %>% 
    summarise_all(sum, na.rm=T) %>% as.data.frame() %>% ungroup()
  return(dat); rm(dat); gc()
})
dat <- data.table::rbindlist(predData) %>% as.data.frame() %>%
  dplyr::select(x, y, Group, disp_quarter, disp_eigth, disp_sixteenth, 
                EWEMBI_1995:MIROC5_rcp85_2080) %>% 
  mutate_at(c("disp_quarter", "disp_eigth", "disp_sixteenth"), as.numeric) %>% 
  group_by(x, y, Group, disp_quarter, disp_eigth, disp_sixteenth) %>% 
  summarise_all(sum, na.rm=T) %>% as.data.frame() %>% ungroup()
rm(predData); gc()

# Save summed probability to .rds file
if(sub == "prob"){
  disp <-"disp_eigth"
    dat %>% ungroup() %>% filter(1 == !!as.name(disp)) %>% 
      group_by(x,y,Group) %>% dplyr::select(x,y,EWEMBI_1995:MIROC5_rcp85_2080) %>% 
      mutate_if(is.character, as.numeric) %>% summarise_all(mean, na.rm=T) %>% 
      ungroup() %>% group_by(x,y) %>%
      mutate_at(vars(-group_cols()), ~signif(., digits=2)) %>%
      as.data.frame() %>% ungroup() %>%
      saveRDS(file=paste0("data/", taxa, "_", sub, "_", model_type, "_", disp, "_groups.rds"), 
              compress="xz"); rm(list=ls()); invisible(gc())
} else if(sub == "thresh"){
  lapply(c("disp_quarter", "disp_eigth", "disp_sixteenth"), function(disp){
    dat %>% ungroup() %>% filter(1 == !!as.name(disp)) %>% 
      group_by(x,y,Group) %>% dplyr::select(x,y,EWEMBI_1995:MIROC5_rcp85_2080) %>% 
      mutate_if(is.character, as.numeric) %>% summarise_all(mean, na.rm=T) %>% 
      as.data.frame() %>% ungroup() %>%
      saveRDS(file=paste0("data/", taxa, "_", sub, "_", model_type, "_", disp, "_groups.rds"), 
              compress="xz")
    return(NULL)
  }); rm(list=ls()); invisible(gc())
}
