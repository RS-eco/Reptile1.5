#' ---
#' title: "Spatial changes in reptile richness split by taxonomic group"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#' ### Load packages ###

library(dtplyr); library(dplyr); library(tidyr); library(ggplot2)
library(patchwork); library(magrittr); library(scales); 
library(scico); library(ggpmisc)

#' ### Load general data ###

# Specify colour schemes
bluered <- rev(scico(n=255, palette="roma"))
whiteblue <- rev(scico(n=255, palette="davos"))
redwhite <- rev(scico(n=255, palette="lajolla"))
redwhiteblue <- rev(scico(n=255, palette="vik"))

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), 
                                              "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

#########################

#' ###  Create richness maps with modeled species data ###

# Specify dispersal
disp <- "disp_eigth"

# Specify sub
sub <- "thresh"

#########################

# See 07_EnsInd_ProbThresh_Reptile_Groups.R file

group_names <- c("croc", "lizard", "snake", "turtle", "worm lizard")
dat <- lapply(group_names, function(x){
  print(x)
  readRDS(paste0("data/Reptile_", x, "_thresh_", disp, "_sp_locs.rds"))
})
dat <- data.table::rbindlist(dat, use.names=T) %>% as.data.frame()
dat <- dat %>% mutate(group = factor(group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard")))
unique(dat$group); invisible(gc())

#########################

# Plot decrease in richness

dat_dec <- dat %>% dplyr::select(c(x,y,species,group,algorithm, matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ . - `EWEMBI_1995`)) %>% 
  dplyr::select(-c(`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ if_else(. > 0, 0, .))) %>% 
  ungroup() %>% dplyr::select(-species) %>% group_by(x,y,group,algorithm) %>% 
  summarise_at(vars(matches("rcp60_2080")), sum); gc()
dat_dec <- dat_dec %>% as.data.frame() %>% group_by(x,y,group,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm", values_to="change") %>%
  ungroup %>% group_by(x,y,group) %>% summarise(mean=mean(change, na.rm=T)); gc()

# Project data to Mollweide
dat_dec <- dat_dec %>% spread(group, mean, fill=0) %>% 
  terra::rast()
terra::crs(dat_dec) <- "+proj=longlat +datum=WGS84"

# Create empty grid
r_grid <- terra::rast(nrows=360, ncols=720, xmin=-180, xmax=180, ymin=-90, ymax=90, 
               crs="+proj=longlat + datum=WGS84", vals=1)
r_grid <- terra::crop(r_grid, dat_dec)
r_grid <- terra::mask(r_grid, outline)

dat_dec <- c(dat_dec, r_grid)
r_dat_dec <- terra::project(dat_dec, method="near",
                       y="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dat_dec <- as.data.frame(r_dat_dec, xy=TRUE) %>% dplyr::select(-lyr.1) %>%
  mutate_at(vars(-c(x,y)), ~replace_na(., 0)) %>%
  gather(group, sum, -c(x,y)) %>% group_by(group) %>% group_split()

# Project outline
outline_moll <- sf::st_transform(outline, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

p1 <- lapply(1:length(dat_dec), function(x){
  lim_map <- c(min(dat_dec[[x]]$sum, na.rm=T),0)
  col_val <- scales::rescale(seq(min(dat_dec[[x]]$sum, na.rm=T), 0, length=9))
  
  p <- dat_dec[[x]] %>% ggplot() + geom_tile(aes(x=x, y=y, fill=sum)) + 
    facet_grid(group~., switch="y") + 
    geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
    geom_text_npc(npcx=0.025, npcy=0.95, label = c("a)", "c)", "e)")[[x]]) + 
    scale_fill_gradientn(name="", colours=redwhite, values=col_val, limits=lim_map) + 
    coord_sf(expand=F, xlim=c(-14269066, 17829034), 
             ylim=c(-6431255, 9623945)) + theme_classic() + 
    theme(axis.title = element_blank(), axis.line = element_blank(), 
          axis.ticks = element_blank(), axis.text = element_blank(),
          legend.title = element_text(vjust=0.8), legend.background = element_blank(),
          legend.key.width=unit(1.5, "cm"), legend.position="bottom",
          strip.background = element_blank(), panel.border = element_blank(),
          strip.text = element_text(size=12, face="bold"))
  if(x == 1){
    p <- p + ggtitle("Species decrease (2080)") + 
      theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))
  }
  return(p)
})

#########################

# Plot increase in richness

dat_inc <- dat %>% select(c(x,y,species,group,algorithm, matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ . - `EWEMBI_1995`)) %>% select(-c(`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ if_else(. < 0, 0, .))) %>% 
  ungroup() %>% select(-species) %>% group_by(x,y,group,algorithm) %>% 
  summarise_at(vars(matches("rcp60_2080")), sum); gc()
dat_inc <- dat_inc %>% as.data.frame() %>% group_by(x,y,group,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm", values_to="change") %>%
  ungroup %>% group_by(x,y,group) %>% summarise(mean=mean(change, na.rm=T)); gc()

# Project data to Mollweide
dat_inc <- dat_inc %>% spread(group, mean) %>% terra::rast()
terra::crs(dat_inc) <- "+proj=longlat + datum=WGS84"
dat_inc <- terra::extend(dat_inc, r_grid)
dat_inc <- c(dat_inc, r_grid)
r_dat_inc <- terra::project(dat_inc, method="near",
                            y="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dat_inc <- as.data.frame(r_dat_inc,xy=T) %>% dplyr::select(-lyr.1) %>%
  mutate_at(vars(-c(x,y)), ~replace_na(., 0)) %>%
  gather(group, sum, -c(x,y)) %>% group_by(group) %>% group_split()

p2 <- lapply(1:length(dat_inc), function(x){
  lim_map <- c(0, max(dat_inc[[x]]$sum, na.rm=T))
  col_val <- scales::rescale(seq(0, max(dat_inc[[x]]$sum, na.rm=T), length=9))

  p <- dat_inc[[x]] %>% ggplot() + geom_tile(aes(x=x, y=y, fill=sum)) + 
    facet_grid(group~.) + 
    geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
    geom_text_npc(npcx=0.025, npcy=0.95, label = c("b)", "d)", "f)")[[x]]) + 
    scale_fill_gradientn(name="", colours=whiteblue, 
                         values=col_val, limits=lim_map) + 
    coord_sf(expand=F, xlim=c(-14269066, 17829034), 
             ylim=c(-6431255, 9623945)) + theme_classic() + 
    theme(axis.title = element_blank(), axis.line = element_blank(), 
          axis.ticks = element_blank(), axis.text = element_blank(),
          legend.title = element_text(vjust=0.8), legend.background = element_blank(),
          legend.key.width=unit(1.5, "cm"), legend.position="bottom",
          strip.background = element_blank(), panel.border = element_blank(),
          strip.text = element_blank())
  if(x == 1){
    p <- p + ggtitle("Species increase (2080)") + 
      theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))
  }
  return(p)
})

p <- (p1[[1]] + p2[[1]]) / (p1[[2]] + p2[[2]]) / (p1[[3]] + p2[[3]])
ggsave("figures/FigureS12.png", p, width=9, height=9.8, dpi=400, bg="transparent")
rm(dat_inc, dat_dec); gc()

#########################

# Plot net change in richness

# Calculate change in richness across all groups
data <- dat %>% select(c(x,y,species,group,algorithm, matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ . - `EWEMBI_1995`)) %>% 
  dplyr::select(-c(`EWEMBI_1995`, species)) %>% group_by(x,y,group,algorithm) %>% 
  summarise_at(vars(matches("rcp60_2080")), sum); gc()
data <- data %>% as.data.frame() %>% group_by(x,y,group,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm_rcp_year", values_to="change") %>%
  ungroup() %>% group_by(x,y,group) %>% summarise(mean=mean(change)); gc()
head(data)

# Project data to Mollweide
data <- data %>% spread(group, mean) %>% terra::rast()
terra::crs(data) <- "+proj=longlat + datum=WGS84"
data <- terra::extend(data, r_grid)
data <- c(data, r_grid)
r_data <- terra::project(data, method="near",
                         y="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dat_change <- as.data.frame(r_data, xy=T) %>% dplyr::select(-lyr.1) %>%
  mutate_at(vars(-c(x,y)), ~replace_na(., 0)) %>%
  gather(group, sum, -c(x,y)) %>% group_by(group) %>% group_split()

p3 <- lapply(1:length(dat_change), function(x){
  (lim_map <- c(min(dat_change[[x]]$sum, na.rm=T), max(dat_change[[x]]$sum, na.rm=T)))
  col_val <- scales::rescale(unique(c(seq(min(dat_change[[x]]$sum, na.rm=T), 0, length=5), 
                                      seq(0, max(dat_change[[x]]$sum, na.rm=T), length=5))))
  
  p <- dat_change[[x]] %>% ggplot() + geom_tile(aes(x=x, y=y, fill=sum)) + 
    facet_grid(group~., switch="y") + 
    geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
    geom_text_npc(npcx=0.025, npcy=0.95, label = c("a)", "c)", "e)")[[x]]) + 
    scale_fill_gradientn(name="", colours=redwhiteblue, 
                         values=col_val, limits=lim_map) + 
    coord_sf(expand=F, xlim=c(-14269066, 17829034), 
             ylim=c(-6431255, 9623945)) + theme_classic() + 
    theme(axis.title = element_blank(), axis.line = element_blank(), 
          axis.ticks = element_blank(), axis.text = element_blank(),
          legend.title = element_text(vjust=0.8), legend.background = element_blank(),
          legend.key.width=unit(1.5, "cm"), legend.position="bottom",
          strip.background = element_blank(), panel.border = element_blank(),
          strip.text = element_text(size=12, face="bold"))
  if(x == 1){
    p <- p + ggtitle("Net change to 1995 (2080)") + 
      theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))
  }
  return(p)
})

#########################

# Plot percentage change in richness

# Calculate percentage change in richness across all groups
data <- dat %>% select(c(x,y,species,group,algorithm, matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  group_by(x,y,group,algorithm) %>% summarise_at(vars(c(matches("rcp60_2080"),`EWEMBI_1995`)), sum) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ (. - `EWEMBI_1995`)/`EWEMBI_1995`*100)) %>% 
  dplyr::select(-c(`EWEMBI_1995`)); gc()
data <- data %>% as.data.frame() %>% group_by(x,y,group,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm_rcp_year", values_to="change") %>%
  ungroup() %>% group_by(x,y,group) %>% summarise(mean=mean(change)); gc()
head(data)

# Project data to Mollweide
data <- data %>% spread(group, mean) %>% terra::rast()
terra::crs(data) <- "+proj=longlat + datum=WGS84"
data <- terra::extend(data, r_grid)
data <- c(data, r_grid)
r_data <- terra::project(data, method="near",
                         y="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dat_change <- as.data.frame(r_data, xy=T) %>% dplyr::select(-lyr.1) %>%
  mutate_at(vars(-c(x,y)), ~replace_na(., 0)) %>%
  gather(group, sum, -c(x,y)) %>% group_by(group) %>% group_split()

p4 <- lapply(1:length(dat_change), function(x){
  # Manually set data limit to -100 - 100
  dat_change[[x]]$sum[dat_change[[x]]$sum >= 100] <- 100
  (lim_map <- c(min(dat_change[[x]]$sum, na.rm=T), max(dat_change[[x]]$sum, na.rm=T)))
  col_val <- scales::rescale(unique(c(seq(min(dat_change[[x]]$sum, na.rm=T), 0, length=5), 
                                      seq(0, max(dat_change[[x]]$sum, na.rm=T), length=5))))
  
  p <- dat_change[[x]] %>% ggplot() + geom_tile(aes(x=x, y=y, fill=sum)) + 
    facet_grid(group~.) + geom_sf(data=outline_moll, fill=NA, 
                                  color = "black", size = 1/.pt) +
    geom_text_npc(npcx=0.025, npcy=0.95, label = c("b)", "d)", "f)")[[x]]) + 
    scale_fill_gradientn(name="", colours=redwhiteblue, 
                         values=col_val, limits=lim_map,
                         breaks=c(-100, -50, 0, 50, 100), 
                         labels=c("-100", "-50", "0", "50", 
                                  parse(text=paste("''",">= 100",sep="")))) + 
    coord_sf(expand=F, xlim=c(-14269066, 17829034), 
             ylim=c(-6431255, 9623945)) + theme_classic() + 
    theme(axis.title = element_blank(), axis.line = element_blank(), 
          axis.ticks = element_blank(), axis.text = element_blank(),
          legend.title = element_text(vjust=0.8), legend.background = element_blank(),
          legend.key.width=unit(1.5, "cm"), legend.position="bottom",
          strip.background = element_blank(), panel.border = element_blank(),
          strip.text = element_blank())
  if(x == 1){
    p <- p + ggtitle("% change to 1995 (2080)") + 
      theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))
  }
  return(p)
})

p <- (p3[[1]] + p4[[1]]) / (p3[[2]] + p4[[2]]) / (p3[[3]] + p4[[3]])
ggsave("figures/FigureS13.png", p, width=9, height=9.8, dpi=400, bg="transparent")
