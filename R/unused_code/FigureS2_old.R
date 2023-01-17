#' ## Analysis changes among different reptile groups ##

rm(list=ls()); gc()

#' ### Load general data ###

# Specify colour scheme
bluered <- colorRampPalette(rev(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", 
                                  "#FF7F00", "red", "#7F0000")))(255)

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

# Load packages
library(dplyr); library(ggplot2); library(patchwork); library(magrittr)

#########################

#' ### Load raw reptile data ###

#remotes::install_github("RS-eco/rasterSp")
library(rasterSp)
data(gard_reptiles)
length(unique(gard_reptiles$Binomial))

table(gard_reptiles$Group)

#########################

#' ### Calculate species richness from raw data ###

data(gard_reptiles_dist)
length(unique(gard_reptiles_dist$species))

gard_reptiles_dist %<>% left_join(gard_reptiles, by=c("species"="Binomial")) %>%
  mutate(occ=1) 

#########################

#' ### Load modelled species names

AUC_data <- lapply(c("GAM", "GBM"), function(model_type){
  read.csv(paste0("data/AUCvalues_All_", 
                  model_type, "_Reptile.csv.xz"))})
AUC_data <- do.call(rbind, AUC_data)

# Aggregate the different AUC values from the 10 iterations per species
# and filter by AUC > 0.7
AUC_sum <- AUC_data %>% group_by(Species, model_type) %>% 
  dplyr::summarise(mean = mean(AUC, na.rm=T)) %>% filter(mean >= 0.7) %>% ungroup() %>% 
  group_by(Species) %>% dplyr::summarise(n = n()) %>% filter(n == 2)
spNames <- AUC_sum$Species
length(spNames)

#########################

#' ### Subset gard data by modelled species ###

gard_mod <- gard_reptiles_dist %>% filter(species %in% sub("_", " ", unlist(spNames)))
table(gard_mod %>% group_by(Group) %>% select(species) %>% distinct() %$% Group)

#########################

# Plot species richness of modelled species

p1 <- gard_mod %>% group_by(x,y) %>% summarise(sr=sum(occ)) %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=sr)) + geom_sf(data=outline, fill="transparent", colour="black") + 
  scale_fill_gradientn(name="Total", colours=rev(bluered), na.value= "grey50") + 
  coord_sf(expand=F, ndiscr=0, ylim=c(-65,75), xlim=c(-130,180)) + theme_classic() + 
  labs(x="", y="") + 
  theme(plot.background = element_rect(fill = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.box.background = element_rect(fill = "transparent", colour=NA))
p2 <- gard_mod %>% filter(Group == "lizard") %>% group_by(x,y) %>% summarise(sr=sum(occ)) %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=sr)) + geom_sf(data=outline, fill="transparent", colour="black") + 
  scale_fill_gradientn(name="Lizard", colours=rev(bluered), na.value= "grey50") + 
  coord_sf(expand=F, ndiscr=0, ylim=c(-65,75), xlim=c(-130,180)) + theme_classic() + 
  labs(x="", y="") + 
  theme(plot.background = element_rect(fill = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.box.background = element_rect(fill = "transparent", colour=NA))
p3 <- gard_mod %>% filter(Group == "snake") %>% group_by(x,y) %>% summarise(sr=sum(occ)) %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=sr)) + geom_sf(data=outline, fill="transparent", colour="black") + 
  scale_fill_gradientn(name="Snake", colours=rev(bluered), na.value= "grey50") + 
  coord_sf(expand=F, ndiscr=0, ylim=c(-65,75), xlim=c(-130,180)) + theme_classic() + 
  labs(x="", y="") + 
  theme(plot.background = element_rect(fill = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.box.background = element_rect(fill = "transparent", colour=NA))
p4 <- gard_mod %>% filter(Group == "turtle") %>% group_by(x,y) %>% summarise(sr=sum(occ)) %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=sr)) + geom_sf(data=outline, fill="transparent", colour="black") + 
  scale_fill_gradientn(name="Turtle", colours=rev(bluered), na.value= "grey50") + 
  coord_sf(expand=F, ndiscr=0, ylim=c(-65,75), xlim=c(-130,180)) + theme_classic() + 
  labs(x="", y="") + 
  theme(plot.background = element_rect(fill = "transparent"), 
        legend.background = element_rect(fill = "transparent"), 
        strip.background = element_blank(),
        strip.text = element_text(size=14),
        legend.box.background = element_rect(fill = "transparent", colour=NA))
p <- p1 + p2 + p3 + p4 + plot_layout(ncol=2)
ggsave("figures/FigureS2.png", dpi=600, width=11.5, height=5)