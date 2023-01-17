#' ---
#' title: "Spatial patterns in current and future summed probability"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#' ### Load packages ###

library(tidyverse); library(patchwork); library(magrittr)

#' ### Load general data ###

# Specify colour scheme
bluered <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(255)
bluewhitered <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "white", "yellow", "#FF7F00", "red", "#7F0000"))(255)

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

# Transform outline to Mollweide projection
#outline_moll <- sf::st_transform(outline, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

#data(outline, package="ggmap2")
#outline <- sf::st_as_sf(outline)

#########################

#' ###  Create richness maps with modelled species data ###

# Set dispersal
disp <- "dispersal2"

# Load data files
sumProb1 <- readRDS(paste0("data/Reptile_prob_GAM_", disp, ".rds"))
sumProb2 <- readRDS(paste0("data/Reptile_prob_GBM_", disp ,".rds"))

# Create time_rcp combination
time_rcp <- expand.grid(time=c(2050, 2080), rcp= c("rcp26", "rcp60", "rcp85")) %>% 
  tidyr::unite("time_rcp", c(rcp, time))
time_rcp <- as.vector(time_rcp$time_rcp)
time_rcp <- c("1995", time_rcp)
time_rcp_lab <- c("1995", "2050 RCP2.6", "2080 RCP2.6", "2050 RCP6.0", 
                  "2080 RCP6.0", "2050 RCP8.5", "2080 RCP8.5")

# Calculate ensemble mean for each time_rcp combination
mean_taxa <- lapply(time_rcp, function(x){
  sumData <- left_join(sumProb1, sumProb2, by=c("x","y")) %>% select(x,y,matches(x))
  sumData$sum <- apply(sumData[,-c(1,2,3)], 1, mean, na.rm=TRUE)
  sumData <- sumData %>% dplyr::select(c(x,y,sum))
  sumData$time_rcp <- x
  return(sumData)
}); rm(sumProb1, sumProb2); gc()
mean_taxa <- do.call("rbind", mean_taxa)
mean_taxa$time_rcp <- factor(mean_taxa$time_rcp, labels=time_rcp_lab)
mean_taxa$disp <- disp

# Plot richness for 1995
data <- mean_taxa %>% filter(time_rcp == 1995) %>% group_by(x,y) %>% summarise(sum=sum(sum)) %>% 
  tidyr::drop_na()

# Project data to Mollweide
#data <- data %>% raster::rasterFromXYZ()
#raster::projection(data) <- "+proj=longlat + datum=WGS84"
#data <- raster::projectRaster(data, crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")
#data <- data.frame(raster::rasterToPoints(data))

# Does not work for histograms!!!

lim_map <- c(0, max(data$sum))
col_val <- scales::rescale(seq(0, max(data$sum), length=9))

p1 <- ggplot() + geom_tile(data=data, aes(x=x, y=y, fill=sum)) + 
  geom_sf(data=outline, fill="transparent", colour="black") + 
  scale_fill_gradientn(name="Number of species", colours=bluered, 
                       na.value="transparent", values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(min(data$x), max(data$x)), ylim=c(min(data$y),max(data$y))) + theme_classic() + labs(y="1995") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=12),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.width=unit(1.5, "cm"), legend.position="bottom",
        legend.box.background = element_rect(fill = "transparent", colour=NA))

# Calculate & plot mean richness per Latitude for 1995
data <- data %>% group_by(y) %>% summarise(mn_sr=mean(sum)) %>% tidyr::drop_na()
p2 <- ggplot() + geom_histogram(data=data, aes(x=y, y=mn_sr), width=1,
                            stat="identity", position="stack", colour=NA) + 
    coord_flip(expand=FALSE) + theme_classic() +
    theme(legend.position="bottom", axis.title = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(), axis.text.x = element_text(size=rel(0.8))) + 
    guides(fill = guide_legend(direction = "vertical"))

# Calculate change in richness across all groups
delta_sum <- mean_taxa %>% group_by(x,y, time_rcp, disp) %>% summarise(sum=sum(sum)) %>% 
  tidyr::spread(time_rcp, sum) %>% 
  tidyr::drop_na() %>% mutate_at(time_rcp_lab[-1], funs((. - `1995`)/`1995`*100)) %>% 
  dplyr::select(-`1995`) %>% tidyr::gather(time_rcp, sum, -c(x,y,disp))

# Plot change in richness
data <- delta_sum %>% filter(time_rcp == "2080 RCP8.5") %>% filter(disp=="dispersal2") %>% 
  select(x,y,sum) %>% tidyr::drop_na() %>% filter(sum != "Inf")
(lim_map <- c(min(data$sum), max(data$sum)))
col_val <- scales::rescale(unique(c(seq(min(data$sum), 0, length=5), seq(0, max(data$sum), length=5))))

# Manually set lim map to -100 - 100
lim_map <- c(-100, 100)
col_val <- scales::rescale(unique(c(seq(-100, 0, length=5), seq(0, 100, length=5))))
p3 <- ggplot() + geom_tile(data=data, aes(x=x, y=y, fill=sum)) + 
  geom_sf(data=outline, fill="transparent", colour="black") + 
  scale_fill_gradientn(name="Difference to year 1995 (%)", colours=bluewhitered, 
                       na.value="transparent", values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(min(data$x), max(data$x)), ylim=c(min(data$y),max(data$y))) + theme_classic() + labs(y="2080") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=12),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        legend.key.width=unit(1.5, "cm"), legend.position="bottom",
        legend.box.background = element_rect(fill = "transparent", colour=NA))

# Calculate and plot change in richness per latitudinal band
data <- data %>% group_by(y) %>% summarise(mn_sr=mean(sum)) %>% tidyr::drop_na()
p4 <- ggplot() + geom_histogram(data=data, aes(x=y, y=mn_sr), width=1,
                                stat="identity", position="stack", colour=NA) + 
  coord_flip(expand=FALSE) + theme_classic() + 
theme(legend.position="bottom", axis.title = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
      axis.text.y = element_blank(), axis.text.x = element_text(size=rel(0.8))) + 
  guides(fill = guide_legend(direction = "vertical"))

# Combine plots
pall <- {p1 + p2 + plot_layout(nrow=1, widths=c(4,1))} /
  {p3 + p4 + plot_layout(nrow=1, widths=c(4,1))}
ggsave("figures/FigureS4.png", pall, width=6, height=6, unit="in", dpi=600, bg="transparent")

# Change 100 to > 100
# Add Inf values as > 100 and add to figure caption!

# Numbers of second histogram should also be maximum 100!

# Add different species groups (proportion of entire species) to histogram (see BioScen1.5 paper)
# Add dots to map for highlighting correspondance across GCMs/Model algorithm.