#' ---
#' title: "Spatial reptile richness split by taxonomic group"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#' ### Load packages ###

library(dplyr); library(tidyr); library(ggplot2)
library(patchwork); library(magrittr); library(scales)
library(scico); library(ggsci); library(ggpmisc)

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

#########################

#' ###  Create richness maps with modelled species data ###

# Specify dispersal
disp <- "disp_eigth"
sub <- "thresh"

# Load data files
sr1 <- readRDS(paste0("data/Reptile_", sub, "_GAM_", disp, "_groups.rds"))
sr2 <- readRDS(paste0("data/Reptile_", sub, "_GBM_", disp ,"_groups.rds"))

# Create time_rcp combination
time_rcp <- expand.grid(time=c(2050, 2080), rcp= c("rcp26", "rcp60", "rcp85")) %>% 
  tidyr::unite("time_rcp", c(rcp, time))
time_rcp <- as.vector(time_rcp$time_rcp)
time_rcp <- c("1995", time_rcp)
time_rcp_lab <- c("1995", "2050 RCP2.6", "2080 RCP2.6", "2050 RCP6.0", 
                  "2080 RCP6.0", "2050 RCP8.5", "2080 RCP8.5")

# Calculate ensemble mean for each time_rcp combination
mean_taxa <- lapply(time_rcp, function(x){
  sumData <- full_join(sr1, sr2, by=c("x","y", "Group")) %>% 
    dplyr::select(x,y,Group,matches(x)) %>%
  mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard")))
  sumData <- sumData %>% group_by(x,y,Group) %>%
    summarise_at(vars(-group_cols()), sum)
  sumData$sum <- apply(sumData[,-c(1,2,3)], 1, mean, na.rm=TRUE)
  sumData <- sumData %>% dplyr::select(c(x,y,Group,sum))
  sumData$time_rcp <- x
  return(sumData)
}); rm(sr1, sr2); gc()
mean_taxa <- do.call("rbind", mean_taxa)
mean_taxa$time_rcp <- factor(mean_taxa$time_rcp, labels=time_rcp_lab)
mean_taxa <- mean_taxa %>% filter(time_rcp == 1995) %>% spread(Group, sum) %>%
  select(-time_rcp) %>% mutate_at(vars(Lizard:Turtle), ~replace_na(., 0))

#########################

# Plot richness for 1995

# Turn data to raster
r_dat <- mean_taxa %>% raster::rasterFromXYZ()
raster::projection(r_dat) <- "+proj=longlat + datum=WGS84"

# Create empty grid
r_grid <- raster::raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90, 
                         crs="+proj=longlat + datum=WGS84", vals=1)
r_grid <- raster::crop(r_grid, r_dat)
r_grid <- raster::mask(r_grid, outline)

# Project data to Mollweide
r_dat <- raster::stack(r_dat, r_grid)
data <- raster::projectRaster(r_dat, method="ngb",
                              crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
data <- data.frame(raster::rasterToPoints(data)) %>% dplyr::select(-layer) %>%
  gather(group, sum, -c(x,y)) %>% group_by(group) %>% group_split()

# Project outline
outline_moll <- sf::st_transform(outline, 
                                 crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

####################

p1 <- lapply(1:length(data), function(x){
  lim_map <- c(0, max(data[[x]]$sum, na.rm=T))
  col_val <- scales::rescale(seq(0, max(data[[x]]$sum, na.rm=T), length=9))
  
  p <- data[[x]] %>% ggplot() + geom_tile(aes(x=x, y=y, fill=sum)) + 
    facet_grid(group~., switch="y") + 
    geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
    geom_text_npc(npcx=0.025, npcy=0.95, label = c("a)", "b)", "c)")[[x]]) + 
    scale_fill_gradientn(name="", colours=bluered, values=col_val, limits=lim_map) + 
    coord_sf(expand=F, xlim=c(-14269066, 17829034), 
             ylim=c(-6431255, 9623945)) + theme_classic() + 
    theme(axis.title = element_blank(), axis.line = element_blank(), 
          axis.ticks = element_blank(), axis.text = element_blank(),
          legend.title = element_text(vjust=0.8), legend.background = element_blank(),
          legend.key.width=unit(1.5, "cm"), legend.position="bottom",
          strip.background = element_blank(), panel.border = element_blank(),
          strip.text = element_text(size=12, face="bold"))
  if(x == 1){
    p <- p + ggtitle("Species richness (1995)") + 
      theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))
  }
  return(p)
})
p <- p1[[1]] / p1[[2]] / p1[[3]]
ggsave("figures/FigureS8.png", p, width=4.51, height=9, dpi=600, bg="transparent")
