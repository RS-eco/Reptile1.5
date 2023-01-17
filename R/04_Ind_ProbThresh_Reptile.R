#' ---
#' title: "Split reptile projections per group"
#' author: "RS-eco"
#' ---

# Empty R environment
rm(list=ls()); gc()

# Load required packages
library(vroom); library(tidyr)
library(data.table); library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

# Load rasterSp package
#remotes::install_github("RS-eco/rasterSp)
library(rasterSp)
data(gard_reptiles) # Needed for group splitting

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
if(res_type == "climate"){sub <- "prob"} else{sub <- "thresh"}

# Final species names
spNames <- sapply(list.files(paste0(filedir, "/", taxa, "_", 
                                    model_type, "_results_", res_type)), function(x){
                                      paste0(strsplit(x, split="_")[[1]][1:2], collapse="_")
                                    })

# Prediction files of group
predFiles <- list.files(paste0(filedir, "/", taxa, "_", model_type, "_results_", res_type), 
                        full.names=T)
predFiles <- sapply(spNames, function(x) predFiles[grepl(predFiles, pattern=x)][[1]])
length(predFiles)

dat <- data.frame(Binomial=sub("_", " ", spNames)) %>% left_join(gard_reptiles); rm(gard_reptiles)
predFiles <- split(predFiles, dat$Group); rm(dat)
group_names <- names(predFiles)

disp <- c("disp_quarter", "disp_eigth", "disp_sixteenth")

dat_sample <- vroom::vroom(predFiles[[5]]) %>% 
  select(EWEMBI_1995, GFDL.ESM2M_rcp26_2050:MIROC5_rcp85_2080)
gcm_rcp_year <- colnames(dat_sample); rm(dat_sample)

df <- expand.grid(disp=disp, gcm_rcp_year=gcm_rcp_year); gc()

# Run through groups individually
x <- 2 # 1:5

# Run through individual dispersal and gcm_rcp_year combination
lapply(1:nrow(df), function(y){
  ind_disp <- as.character(df$disp[y])
  if(!file.exists(paste0(filedir, "/Reptile1.5/extdata/", taxa, "_", sub, "_", model_type, "_", ind_disp, 
                         "_", df$gcm_rcp_year[y],"_", group_names[x], ".rds"))){
    dat_sub <- lapply(predFiles[[x]], function(file){
      dat <- vroom::vroom(file, col_select=c(x,y, df$disp[y], df$gcm_rcp_year[y])); gc()
      dat$species <- paste0(strsplit(basename(file), split="_")[[1]][1:2], collapse="_")
      dat <- dat %>% mutate_at(ind_disp, as.numeric) %>% filter(.data[[ind_disp]] == 1) %>% 
        select(x, y, species, df$gcm_rcp_year[y]); gc()
      return(dat)
    })
    dat_sub <- data.table::rbindlist(dat_sub) %>% as.data.frame(); gc()
    dat_sub <- dat_sub %>% pivot_wider(names_from = species, values_from = !!df$gcm_rcp_year[y]); gc()
    dat_sub <- dat_sub[which(unlist(apply(dat_sub[,-c(1,2)], MARGIN=1, FUN=function(x)!all(is.na(x))))),]; gc()
    dat_sub <- dat_sub %>% mutate_all(list(~ tidyr::replace_na(., 0))); gc()
    if(sub == "prob"){
      dat_sub <- dat_sub %>% group_by(x,y) %>%
        mutate_at(vars(-group_cols()), ~signif(., digits=2))
    }
    
    saveRDS(dat_sub, paste0(filedir, "/Reptile1.5/extdata/", taxa, "_", sub, 
                            "_", model_type, "_", ind_disp, "_", df$gcm_rcp_year[y],
                            "_", group_names[x], ".rds"), compress="xz")
    raster::removeTmpFiles(h=1)
    rm(dat_sub); gc()
  }
})
rm(list=ls()); gc()
#q(save="no")
