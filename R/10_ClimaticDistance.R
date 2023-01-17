#' ---
#' title: "Calculate climate distance between current and future projections"
#' author: "RS-eco"
#' ---

rm(list=ls(all=TRUE))

## Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom.mixed)
library(broom)

# Load rISIMIP package
if(!"rISIMIP" %in% installed.packages()[,"Package"]) 
  remotes::install_github("RS-eco/rISIMIP", build_vignettes = TRUE)
library(rISIMIP)

## Load current climate data
curclim <- get(data("bioclim_ewembi_1995_landonly")) %>% 
  dplyr::select("x", "y", "bio4", "bio5", "bio12", "bio15") %>% 
  mutate(year = 1995); rm(bioclim_ewembi_1995_landonly)

## PC current climate data
clim_pca_current <- curclim %>% select(bio4, bio5, bio12, bio15, year) %>%
  nest(data = -year) %>%
  mutate(pca = map(data, ~ prcomp(.x, center= TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y))) %>% 
  unnest(pca_aug) %>% select(-data)
clim_pca_current$x <- curclim$x
clim_pca_current$y <- curclim$y
rm(curclim); gc()

pca1 <- clim_pca_current$pca[[1]]
current_pca_loadings <- tidy(pca1, "loadings")
current_pca_eigenvalues <- tidy(pca1, "eigenvalues")
saveRDS(current_pca_loadings, file="data/current_pca_loadings.rds", compress="xz")
saveRDS(current_pca_eigenvalues, file="data/current_pca_eigenvalues.rds", compress="xz")

clim_pca_current <- clim_pca_current %>% select(-pca); gc()

# Check PCA results
#plot(pca1) #Which principal component captures most of the variance
#plot(pca1, type="l") #Ellbow method to decide on number of components - Bend at 3 
#biplot(pca1) #Shows bearing of the input - little correlation between components

####################

## Future climate

# Specify parameters
rcp <- c("rcp26", "rcp60", "rcp85")
year <- c(2050, 2080)
gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")

# Select climate files
climatecombs <- expand.grid(gcm,rcp,year) %>% rowwise() %>% 
  transmute(name = paste(Var1,Var2,Var3, sep="_")) %>% unlist(); rm(rcp, year, gcm)
(climatefiles <- paste0("bioclim_", tolower(climatecombs), "_landonly"))

# Load climate data into list
climatedata <- lapply(1:length(climatefiles), function(x){
  data <- get(data(list=climatefiles[[x]])) %>% 
    dplyr::select("x", "y", "bio4", "bio5", "bio12", "bio15")
  return(data)
})
names(climatedata) <- sub("_landonly", "", sub("bioclim_", "", climatefiles))
climatedata <- data.table::rbindlist(climatedata, idcol="gcm_rcp_year") %>% as.data.frame()
rm(list=climatefiles); rm(climatefiles); gc()

## Run PCA separated by scenario, year, gcm
clim_pca_future <- climatedata %>% 
  select(bio4, bio5, bio12, bio15, gcm_rcp_year) %>% 
  nest(data = -gcm_rcp_year) %>%
  mutate(pca = map(data, ~ prcomp(.x, center= TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y))
  ) %>% unnest(pca_aug) %>% select(-data)
clim_pca_future$x <- climatedata$x
clim_pca_future$y <- climatedata$y

pca2 <- clim_pca_future %>% group_by(gcm_rcp_year) %>% slice(1) %>% ungroup()
future_pca_loadings <- pca2 %>% group_by(gcm_rcp_year) %>% 
  summarise(loadings = map(pca, ~ tidy(.x, "loadings"))) %>% unnest(loadings) %>% 
  separate(gcm_rcp_year, c("gcm", "rcp", "year"), sep="_", fill="right")
future_pca_loadings <- future_pca_loadings %>%
  mutate(gcm = as.character(factor(gcm, levels = unique(future_pca_loadings$gcm), 
                                   labels = c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")))); gc()
saveRDS(future_pca_loadings, file="data/future_pca_loadings.rds", compress="xz")
future_pca_eigenvalues <- pca2 %>% group_by(gcm_rcp_year) %>% 
  summarise(eigenvalues = map(pca, ~ tidy(.x, "eigenvalues"))) %>% unnest(eigenvalues) %>% 
  separate(gcm_rcp_year, c("gcm", "rcp", "year"), sep="_", fill="right")
future_pca_eigenvalues <- future_pca_eigenvalues %>%
  mutate(gcm = as.character(factor(gcm, levels = unique(future_pca_eigenvalues$gcm), 
                                   labels = c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")))); gc()
saveRDS(future_pca_eigenvalues, file="data/future_pca_eigenvalues.rds", compress="xz")

clim_pca_future <- clim_pca_future %>% select(-pca) %>% 
  separate(gcm_rcp_year, c("gcm", "rcp", "year"), sep="_", fill="right"); gc()
rm(climatedata); gc()

# Check PCA results
#plot(pca2$pca[[1]]) #Which principal component captures most of the variance
#plot(pca2$pca[[1]], type="l") #Ellbow method to decide on number of components - Bend at 3 
#biplot(pca2$pca[[1]]) #Shows bearing of the input - little correlation between components

## Merge current and future data
clim_pca_current <- clim_pca_current %>% select(-c(.rownames, .fittedPC3, .fittedPC4))
saveRDS(clim_pca_current, file="data/clim_pca_current.rds", compress="xz")

clim_pca_future <- clim_pca_future %>% select(-c(.rownames, .fittedPC3, .fittedPC4)) %>%
  mutate(gcm = as.character(factor(gcm, levels = unique(clim_pca_future$gcm), 
                                   labels = c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"))))
saveRDS(clim_pca_future, file="data/clim_pca_future.rds", compress="xz")

####################

# Calculate climatic distance

rm(list=ls()); gc()

# Define climatic combinations 
rcp <- c("rcp26", "rcp60", "rcp85")
year <- c(2050, 2080)
gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
climatecombs <- expand.grid(gcm,rcp,year) %>% rowwise() %>% 
  transmute(name = paste(Var1,Var2,Var3, sep="_")) %>% unlist(); rm(rcp, year, gcm)

# Load PCA data
clim_pca_current <- readRDS("data/clim_pca_current.rds")
clim_pca_future <- readRDS("data/clim_pca_future.rds")

# Multiple current data by future climate combinations
curpca <- lapply(climatecombs, function(z){
  data <- clim_pca_current %>% as.data.frame()
  data$gcm_rcp_year <- z
  return(data)
}); rm(clim_pca_current); gc()
curpca <- data.table::rbindlist(curpca)
curpca <- separate(curpca, gcm_rcp_year, c("gcm", "rcp", "year"), 
                   sep="_", fill="right"); gc()
curpca$year <- 1995

clim_pca_all <- clim_pca_future %>% bind_rows(curpca)
rm(clim_pca_future, curpca); gc()

# Calculate euclidean distance
edist_2050 <- clim_pca_all %>% filter(year %in% c("1995", "2050")) %>% 
  ungroup() %>% group_by(x, y, gcm, rcp) %>% 
  dplyr::summarise(dist=dist(cbind(.fittedPC1, .fittedPC2), method="euclidean"))
edist_2050$year <- 2050

edist_2080 <- clim_pca_all %>% filter(year %in% c("1995", "2080")) %>% 
  group_by(x, y, gcm, rcp) %>% 
  summarise(dist=dist(cbind(.fittedPC1, .fittedPC2), method="euclidean"))
edist_2080$year <- 2080
climatic_distance <- bind_rows(edist_2050, edist_2080)
rm(clim_pca_all, edist_2050, edist_2080); gc()

# Save to file
saveRDS(climatic_distance, file="data/climatic_distance.rds", compress="xz")
rm(climatic_distance); gc()