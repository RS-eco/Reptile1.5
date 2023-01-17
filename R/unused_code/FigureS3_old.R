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

#' ### Relationship between all species and modelled species

p1 <- gard_mod %>% group_by(x,y) %>% summarise(sr=sum(occ)) %>% 
  inner_join(gard_reptiles_dist %>% group_by(x,y) %>% summarise(sr2=sum(occ))) %>%
  ggplot(aes(x=sr, y=sr2)) + geom_point() + geom_smooth(method="lm") + 
  theme_bw() + labs(x="Modelled reptile species richness", y="Total reptile species richness")
p2 <- gard_mod %>% filter(Group == "lizard") %>% group_by(x,y) %>% summarise(sr=sum(occ)) %>% 
  inner_join(gard_reptiles_dist %>% filter(Group == "lizard") %>% group_by(x,y) %>% summarise(sr2=sum(occ))) %>%
  ggplot(aes(x=sr, y=sr2)) + geom_point() + geom_smooth(method="lm") + 
  theme_bw() + labs(x="Modelled species richness of lizards", y="Total species richness of lizards")
p3 <- gard_mod %>% filter(Group == "snake") %>% group_by(x,y) %>% summarise(sr=sum(occ)) %>% 
  inner_join(gard_reptiles_dist %>% filter(Group == "snake") %>% group_by(x,y) %>% summarise(sr2=sum(occ))) %>%
  ggplot(aes(x=sr, y=sr2)) + geom_point() + geom_smooth(method="lm") + 
  theme_bw() + labs(x="Modelled species richness of snakes", y="Total species richness of snakes")
p4 <- gard_mod %>% filter(Group == "turtle") %>% group_by(x,y) %>% summarise(sr=sum(occ)) %>% 
  inner_join(gard_reptiles_dist %>% filter(Group == "turtle") %>% group_by(x,y) %>% summarise(sr2=sum(occ))) %>%
  ggplot(aes(x=sr, y=sr2)) + geom_point() + geom_smooth(method="lm") + 
  theme_bw() + labs(x="Modelled species richness of turtles", y="Total species richness of turtles")

p <- p1 + p2 + p3 + p4
ggsave("figures/FigureS3.png", dpi=600, width=8, height=8)

#########################