#' ---
#' title: "Spatial changes in reptile richness split by Year/RCP"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#' ### Load packages ###

library(dtplyr); library(dplyr); library(tidyr); 
library(ggplot2); library(patchwork); library(magrittr)
library(scales); library(scico)

#remotes::install_github("eliocamp/tagger")
library(tagger)

#' ### Load general data ###

# Specify colour schemes
bluered <- rev(scico(n=255, palette="roma"))
whiteblue <- rev(scico(n=255, palette="davos"))
redwhite <- rev(scico(n=255, palette="lajolla"))
redwhiteblue <- rev(scico(n=255, palette="vik"))

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

# Project outline
outline_moll <- sf::st_transform(outline, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Create empty grid
r_grid <- raster::raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90, 
                         crs="+proj=longlat + datum=WGS84", vals=1)
r_grid <- raster::crop(r_grid, y=c(-178.5, 180, -56, 81.5))
r_grid <- raster::mask(r_grid, outline)

#########################

#' ###  Create richness maps with modelled species data ###

# Specify dispersal
disp <- "disp_eigth"

# Specify sub
sub <- "thresh"

# See 07_EnsInd_ProbThresh_Reptile_Groups.R file

# Load individual species data
group_names <- c("croc", "lizard", "snake", "turtle", "worm lizard")
dat <- lapply(group_names, function(x){
  print(x)
  readRDS(paste0("data/Reptile_", x, "_thresh_", disp, "_sp_locs.rds"))
})
dat <- data.table::rbindlist(dat, use.names=T); invisible(gc())

#########################

# Calculate mean total decrease in richness
dat_dec <- dat %>% select(-c(disp,group)) %>% 
  mutate_at(vars(matches("20")), list(~ . - `EWEMBI_1995`)) %>% 
  dplyr::select(-c(`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("20")), list(~ if_else(. > 0, 0, .))) %>% 
  ungroup() %>% select(-species) %>% group_by(x,y,algorithm) %>% 
  summarise_at(vars(matches("20")), sum); gc()
dat_dec <- dat_dec %>% as.data.frame() %>% group_by(x,y,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm_rcp_year", values_to="change") %>%
  separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", extra="merge") %>% 
  ungroup() %>% group_by(x,y,rcp_year) %>% summarise(mean=mean(change)); gc()

#########################

# Project data to Mollweide
dat_dec <- dat_dec %>% spread(rcp_year, mean) %>% raster::rasterFromXYZ()
raster::projection(dat_dec) <- "+proj=longlat + datum=WGS84"

dat_dec <- raster::stack(dat_dec, r_grid)
dat_dec <- raster::projectRaster(dat_dec, method="ngb",
                                 crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dat_dec <- data.frame(raster::rasterToPoints(dat_dec)) %>% select(-layer) %>%
  gather(rcp_year, sum, -c(x,y))
dat_dec$rcp_year <- factor(dat_dec$rcp_year, 
                           levels=c("rcp26_2050", "rcp60_2050", "rcp85_2050",
                                    "rcp26_2080", "rcp60_2080", "rcp85_2080"), 
                           labels=c("2050 RCP2.6", "2050 RCP6.0", "2050 RCP8.5",
                                    "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5"))
dat_dec <- dat_dec %>% separate(rcp_year, c("year", "rcp"), sep=" "); gc()
lim_map <- c(min(dat_dec$sum, na.rm=T),0)
col_val <- scales::rescale(seq(min(dat_dec$sum, na.rm=T), 0, length=9))

p1 <- ggplot() + geom_tile(data=dat_dec, aes(x=x, y=y, fill=sum)) + 
  facet_grid(rcp~year, switch="y") + 
  tag_facets(position = list(x = 0.025, y = 0.95)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Species decrease", colours=redwhite, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + theme_classic() + 
  theme(axis.title = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.title = element_text(vjust=0.8), legend.background = element_blank(),
        legend.position="bottom", legend.key.width=unit(1.25, "cm"), 
        strip.background = element_blank(), panel.border = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        tagger.panel.tag.text = element_text(color = "black", size = 10),
        tagger.panel.tag.background = element_blank())
ggsave("figures/FigureS27.png", p1, width=9, height=7.7, dpi=400, bg="transparent")
rm(dat_dec); gc()

#########################

# Plot increase in richness

# Calculate mean total increase in richness
dat_inc <- dat %>% select(-c(disp,group)) %>% 
  mutate_at(vars(matches("20")), list(~ . - `EWEMBI_1995`)) %>% 
  dplyr::select(-c(`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("20")), list(~ if_else(. < 0, 0, .))) %>% 
  ungroup() %>% select(-species) %>% group_by(x,y,algorithm) %>% 
  summarise_at(vars(matches("20")), sum); gc()
dat_inc <- dat_inc %>% as.data.frame() %>% group_by(x,y,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm_rcp_year", values_to="change") %>%
  separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", extra="merge") %>% 
  ungroup() %>% group_by(x,y,rcp_year) %>% summarise(mean=mean(change)); gc()
head(dat_inc)

# Project data to Mollweide
dat_inc <- dat_inc %>% spread(rcp_year, mean) %>% raster::rasterFromXYZ()
raster::projection(dat_inc) <- "+proj=longlat + datum=WGS84"
dat_inc <- raster::extend(dat_inc, r_grid)
dat_inc <- raster::stack(dat_inc, r_grid)
dat_inc <- raster::projectRaster(dat_inc, method="ngb",
                                 crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dat_inc <- data.frame(raster::rasterToPoints(dat_inc)) %>% select(-layer) %>%
  gather(rcp_year, sum, -c(x,y))
dat_inc$rcp_year <- factor(dat_inc$rcp_year, 
                           levels=c("rcp26_2050", "rcp60_2050", "rcp85_2050",
                                    "rcp26_2080", "rcp60_2080", "rcp85_2080"), 
                           labels=c("2050 RCP2.6", "2050 RCP6.0", "2050 RCP8.5",
                                    "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5"))
dat_inc <- dat_inc %>% separate(rcp_year, c("year", "rcp"), sep=" ")

lim_map <- c(0, max(dat_inc$sum, na.rm=T))
col_val <- scales::rescale(seq(0, max(dat_inc$sum, na.rm=T), length=9))

p2 <- ggplot() + geom_tile(data=dat_inc, aes(x=x, y=y, fill=sum)) + 
  facet_grid(rcp~year, switch="y") + 
  tag_facets(position = list(x = 0.025, y = 0.95)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Species increase", colours=whiteblue, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + theme_classic() + 
  theme(axis.title = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.title = element_text(vjust=0.8), legend.background = element_blank(),
        legend.position="bottom", legend.key.width=unit(1.25, "cm"), 
        strip.background = element_blank(), panel.border = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        tagger.panel.tag.text = element_text(color = "black", size = 10),
        tagger.panel.tag.background = element_blank())
ggsave("figures/FigureS28.png", p2, width=9, height=7.7, dpi=400, bg="transparent")
rm(dat_inc); gc()

#########################

# Plot net change in richness

# Calculate change in richness across all year_rcp combinations
data <- dat %>% select(-c(disp,group)) %>% 
  mutate_at(vars(matches("20")), list(~ . - `EWEMBI_1995`)) %>% 
  dplyr::select(-c(`EWEMBI_1995`, species)) %>% group_by(x,y,algorithm) %>% 
  summarise_at(vars(matches("20")), sum); gc()
data <- data %>% as.data.frame() %>% group_by(x,y,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm_rcp_year", values_to="change") %>%
  separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", extra="merge") %>% 
  ungroup() %>% group_by(x,y,rcp_year) %>% summarise(mean=mean(change)); gc()
head(data)

# Project data to Mollweide
data <- data %>% spread(rcp_year, mean) %>% raster::rasterFromXYZ()
raster::projection(data) <- "+proj=longlat + datum=WGS84"
data <- raster::extend(data, r_grid)
data <- raster::stack(data, r_grid)
data <- raster::projectRaster(data, method="ngb",
                              crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
data <- data.frame(raster::rasterToPoints(data)) %>% select(-layer) %>%
  gather(rcp_year, sum, -c(x,y))
data$rcp_year <- factor(data$rcp_year, 
                           levels=c("rcp26_2050", "rcp60_2050", "rcp85_2050",
                                    "rcp26_2080", "rcp60_2080", "rcp85_2080"), 
                           labels=c("2050 RCP2.6", "2050 RCP6.0", "2050 RCP8.5",
                                    "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5"))
data <- data %>% separate(rcp_year, c("year", "rcp"), sep=" ")
(lim_map <- c(min(data$sum, na.rm=T), max(data$sum, na.rm=T)))
col_val <- scales::rescale(unique(c(seq(min(data$sum, na.rm=T), 0, length=5), 
                                    seq(0, max(data$sum, na.rm=T), length=5))))

p3 <- ggplot() + geom_tile(data=data, aes(x=x, y=y, fill=sum)) + 
  facet_grid(rcp~year, switch="y") +
  tag_facets(position = list(x = 0.025, y = 0.95)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Net change to 1995", colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + 
  theme_classic() + theme(axis.title = element_blank(), axis.line = element_blank(), 
                          axis.ticks = element_blank(), axis.text = element_blank(),
                          legend.title = element_text(vjust=0.8), legend.background = element_blank(),
                          legend.position="bottom", legend.key.width=unit(1.25, "cm"), 
                          strip.background = element_blank(), panel.border = element_blank(),
                          strip.text = element_text(size=12, face="bold"),
                          tagger.panel.tag.text = element_text(color = "black", size = 10),
                          tagger.panel.tag.background = element_blank())
ggsave("figures/FigureS29.png", p3, width=9, height=7.7, dpi=400, bg="transparent")
rm(data); gc()

#########################

# Plot percentage change in richness

dat <- data.table::rbindlist(dat) %>% as.data.frame() %>%  
  select(c(x,y,species,disp,algorithm,matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  group_by(x,y,disp,algorithm) %>% summarise_at(vars(c(matches("rcp60_2080"), `EWEMBI_1995`)), sum) %>%
  mutate_at(vars(matches("rcp60_2080")), list(~ (. - EWEMBI_1995)/EWEMBI_1995*100)) %>%
  dplyr::select(-c(`EWEMBI_1995`)) %>% group_by(x,y,disp,algorithm) %>%
  pivot_longer(cols=!group_cols(), names_to="gcm", values_to="change") %>% 
  group_by(x,y,disp) %>% summarise(mean=mean(change, na.rm=T)); gc()
return(dat)

# Calculate percentage change in richness across all year_rcp combinations
data <- dat %>% select(-c(disp,group,species)) %>% group_by(x,y,algorithm) %>% 
  summarise_at(vars(c(matches("20"), `EWEMBI_1995`)), sum) %>%
  mutate_at(vars(matches("20")), list(~ (. - EWEMBI_1995)/EWEMBI_1995*100)) %>%
  dplyr::select(-c(`EWEMBI_1995`))
data <- data %>% as.data.frame() %>% group_by(x,y,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm_rcp_year", values_to="change") %>%
  separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", extra="merge") %>% 
  ungroup() %>% group_by(x,y,rcp_year) %>% summarise(mean=mean(change)); gc()

# Project data to Mollweide
data <- data %>% spread(rcp_year, mean) %>% raster::rasterFromXYZ(); gc()
raster::projection(data) <- "+proj=longlat + datum=WGS84"
data <- raster::extend(data, r_grid)
data <- raster::stack(data, r_grid)
data <- raster::projectRaster(data, method="ngb",
                              crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
data <- data.frame(raster::rasterToPoints(data)) %>% select(-layer) %>%
  gather(rcp_year, sum, -c(x,y))
data$rcp_year <- factor(data$rcp_year, 
                        levels=c("rcp26_2050", "rcp60_2050", "rcp85_2050",
                                 "rcp26_2080", "rcp60_2080", "rcp85_2080"), 
                        labels=c("2050 RCP2.6", "2050 RCP6.0", "2050 RCP8.5",
                                 "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5"))
data <- data %>% separate(rcp_year, c("year", "rcp"), sep=" ")

# Manually set data limit to -100 - 100
data$sum[data$sum >= 100] <- 100
data$sum[data$sum == Inf] <- 100

(lim_map <- c(min(data$sum, na.rm=T), max(data$sum, na.rm=T)))
col_val <- scales::rescale(unique(c(seq(min(data$sum, na.rm=T), 0, length=5), 
                                    seq(0, max(data$sum, na.rm=T), length=5))))

p4 <- ggplot() + geom_tile(data=data, aes(x=x, y=y, fill=sum)) + 
  facet_grid(rcp~year, switch="y") + 
  tag_facets(position = list(x = 0.025, y = 0.95)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="% change to 1995", colours=redwhiteblue, 
                       values=col_val, limits=lim_map, breaks=c(-100, -50, 0, 50, 100), 
                       labels=c("-100", "-50", "0", "50", parse(text=paste("''",">= 100",sep="")))) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + theme_classic() + 
  theme(axis.title = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank(),
        legend.title = element_text(vjust=0.8), legend.background = element_blank(),
        legend.position="bottom", legend.key.width=unit(1.25, "cm"), 
        strip.background = element_blank(), panel.border = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        tagger.panel.tag.text = element_text(color = "black", size = 10),
        tagger.panel.tag.background = element_blank())
ggsave("figures/FigureS30.png", p4, width=9, height=7.71, dpi=400, bg="transparent")
rm(data); gc()
