rm(list=ls()); invisible(gc())

# Load packages
library(tidyverse); library(patchwork); library(magrittr)

#' ### Load raw reptile data ###

#remotes::install_github("RS-eco/rasterSp")
library(rasterSp)
data(gard_reptiles)
unique(length(gard_reptiles$Binomial))

table(gard_reptiles$Group)

#########################

# Histogram of range sizes

p1 <- gard_reptiles %>% filter(Group %in% c("lizard", "snake", "turtle")) %>% 
  ggplot(aes(x=Area/1e05)) + facet_wrap(.~Group, scales="free") + 
  geom_histogram(position="stack") + theme_bw() + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.02))) + 
  labs(x="Extent of occurrence (x 10^5 km^2)", y="Number of species") + 
  scale_fill_manual(values=gray.colors(3)) + 
  theme(strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))

# Histogram of latitudinal range

data("gard_reptiles_dist")
length(unique(gard_reptiles_dist$species))

gard_range <- gard_reptiles_dist %>% left_join(gard_reptiles, by=c("species"="Binomial")) %>%
  group_by(species, Group) %>% summarise(lat_range=max(y)-min(y))

p2 <- gard_range %>% filter(Group %in% c("lizard", "snake", "turtle")) %>% 
  ggplot(aes(x=lat_range)) + facet_wrap(.~Group, scales="free") + 
  geom_histogram(position="stack") + theme_bw() + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand=expansion(mult=c(0,.02))) + 
  labs(x="Latitudinal range (°)", y="Number of species") + 
  scale_fill_manual(values=gray.colors(3)) + 
  theme(strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))

p <- p1 + p2 + plot_layout(ncol=1)
ggsave("figures/gard_range_size_lat.png", dpi=600, width=8, height=6)

#########################

# Map of median range sizes and scatterplot of median range size per Latitude

# Specify colour scheme
bluered <- colorRampPalette(rev(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", 
                                  "#FF7F00", "red", "#7F0000")))(255)

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

gard_med_lat_range <- gard_reptiles_dist %>% left_join(gard_reptiles, by=c("species"="Binomial")) %>%
  group_by(y, Group) %>% summarise(med_range=median(Area)/1e05) %>% filter(Group %in% c("lizard", "snake", "turtle")) %>%
  arrange(y)

gard_med_range <- gard_reptiles_dist %>% left_join(gard_reptiles, by=c("species"="Binomial")) %>%
  group_by(x, y, Group) %>% summarise(med_range=median(Area)/1e05) %>% filter(Group %in% c("lizard", "snake", "turtle"))

p1 <- ggplot() + geom_point(data=gard_med_range, aes(x=med_range, y=y), alpha=0.5, colour="grey") + 
  geom_path(data=gard_med_lat_range, aes(x=med_range, y=y), size=1) + facet_wrap(.~Group, ncol=1) + 
  theme_bw() + theme(strip.background = element_blank(), strip.text=element_text(size=12, face="bold")) + 
  labs(x="", y="")

p2 <- gard_med_range %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=med_range)) + 
  facet_wrap(.~Group, ncol=1) + geom_sf(data=outline, fill="transparent", colour="black") + 
  scale_fill_gradientn(name="", colours=rev(bluered), na.value= "grey50") + 
  coord_sf(expand=F, ndiscr=0, ylim=c(-65,84), xlim=c(-180,180)) + labs(x="", y="") + theme_bw() + 
  theme(strip.background = element_blank(), strip.text=element_text(size=12, face="bold"))

p <- p1 + p2 + plot_layout(nrow=1, widths=c(1,5))
ggsave("figures/gard_range-size_map.png", dpi=600, width=8, height=8)

#########################