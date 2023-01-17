setwd("C:/Users/Admin/Documents/Reptile1.5")

files <- list.files("extdata", pattern="prob", full.names=T)
#files <- list.files("data", pattern="prob", full.names=T)
#files[]

library(dplyr)
lapply(files, function(z){
  dat <- readRDS(z)
  head(dat)
  dat_new <- dat %>% group_by(x,y) %>%
    mutate_at(vars(-group_cols()), ~signif(., digits=2)); rm(dat)
  saveRDS(dat_new, file=z, compress="xz"); rm(dat_new); gc()
})

files <- list.files("data", pattern="prob.*\\groups", full.names=T)
files

library(dplyr)
lapply(files, function(z){
  dat <- readRDS(z)
  head(dat)
  dat_new <- dat %>% group_by(x,y, Group) %>%
    mutate_at(vars(-group_cols()), ~signif(., digits=2)); rm(dat)
  saveRDS(dat_new, file=z, compress="xz"); rm(dat_new); gc()
})

files <- c(list.files("data", pattern="_prob.*\\_disp_quarter.rds", full.names=T),
           list.files("data", pattern="_prob.*\\_disp_eigth.rds", full.names=T),
           list.files("data", pattern="_prob.*\\_disp_sixteenth.rds", full.names=T))
files

library(dplyr)
lapply(files, function(z){
  dat <- readRDS(z)
  dat_new <- dat %>% group_by(x,y) %>%
    mutate_at(vars(-group_cols()), ~signif(., digits=2)); rm(dat)
  saveRDS(dat_new, file=z, compress="xz"); rm(dat_new); gc()
})
