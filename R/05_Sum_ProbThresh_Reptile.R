#' ---
#' title: "Create summed probability/presence for each GCM, rcp and timestep combination"
#' author: "RS-eco"
#' ---

# Empty R environment
rm(list=ls())

# Load required packages
library(vroom); library(tidyr)
library(data.table); library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# Set file directory
filedir <- "C:/Users/admin/Documents/"

# Set working directory
setwd("C:/Users/admin/Documents/Reptile1.5")

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

#Read csv files and calculate summed probability/presence
n <- 10
predFiles <- split(predFiles, sort(1:length(predFiles)%%n))
predData <- lapply(1:length(predFiles), function(x){
  print(x)
  dat <- vroom::vroom(unlist(predFiles[x])) %>% 
    group_by(x, y, disp_quarter, disp_eigth, disp_sixteenth) %>% 
    select(EWEMBI_1995:MIROC5_rcp85_2080) %>% 
    mutate_if(is.character, as.numeric) %>% 
    summarise_all(sum, na.rm=T) %>% as.data.frame() %>% ungroup()
  return(dat); gc()
})
dat <- data.table::rbindlist(predData) %>% 
  group_by(x, y, disp_quarter, disp_eigth, disp_sixteenth) %>% 
  select(EWEMBI_1995:MIROC5_rcp85_2080) %>% mutate_if(is.character, as.numeric) %>% 
  summarise_all(sum, na.rm=T) %>% as.data.frame() %>% ungroup()
rm(predData); invisible(gc())

if(sub == "prob"){
  # Save summed probability to .rds file
  disp <- "disp_eigth"
    dat %>% ungroup() %>% filter(1 == !!as.name(disp)) %>% group_by(x,y) %>% 
      select(EWEMBI_1995:MIROC5_rcp85_2080) %>% 
      mutate_if(is.character, as.numeric) %>% summarise_all(sum, na.rm=T) %>% 
      ungroup() %>% group_by(x,y) %>%
      mutate_at(vars(-group_cols()), ~signif(., digits=2)) %>%
      as.data.frame() %>% ungroup() %>%
      saveRDS(file=paste0("data/", taxa, "_", sub, "_", model_type, "_", disp, ".rds"), 
              compress="xz"); rm(dat); invisible(gc())
} else if (sub == "thresh"){
  # Save summed probability to .rds file
  lapply(c("disp_quarter", "disp_eigth", "disp_sixteenth"), function(disp){
    dat %>% ungroup() %>% filter(1 == !!as.name(disp)) %>% group_by(x,y) %>% 
      select(EWEMBI_1995:MIROC5_rcp85_2080) %>% 
      mutate_if(is.character, as.numeric) %>% summarise_all(sum, na.rm=T) %>% 
      as.data.frame() %>% ungroup() %>%
      saveRDS(file=paste0("data/", taxa, "_", sub, "_", model_type, "_", disp, ".rds"), 
              compress="xz")
    return(NULL)
  }); rm(dat); invisible(gc())
}