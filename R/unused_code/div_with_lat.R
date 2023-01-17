## Relationship of species, number of phyla and shannon index with latitude

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

# Species richness against latitude

gard_reptiles_dist %>% left_join(gard_reptiles, by=c("species"="Binomial")) %>%
  filter(Group %in% c("lizard", "snake", "turtle")) %>% 
  group_by(y, Group) %>% summarise(sr=n_distinct(species)) %>% 
  ggplot(aes(x=y, y=sr)) + geom_point() + geom_line() + 
  facet_wrap(.~Group, scales="free") + #coord_flip() + 
  theme_bw() + scale_x_continuous(expand = expansion(mult=c(0,.02))) + 
  scale_y_continuous(expand=expansion(mult=c(0,.02))) + 
  labs(x="Latitude (°)", y="Number of species") + 
  scale_fill_manual(values=gray.colors(3)) + 
  theme(strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))

# Number of Families against latitude

#remotes::install_github("RS-eco/traitdata")
library(traitdata)
data(taxonomyStd)

gard_reptiles_dist %>% left_join(gard_reptiles, by=c("species"="Binomial")) %>%
  left_join(taxonomyStd, by=c("species"="scientificNameStd")) %>% 
  filter(Group %in% c("lizard", "snake", "turtle")) %>% 
  group_by(y, Group) %>% summarise(fam_rich=n_distinct(family)) %>% 
  ggplot(aes(x=y, y=fam_rich)) + geom_point() + geom_line() + 
  facet_wrap(~Group, scales="free") + #coord_flip() + 
  theme_bw() + scale_x_continuous(expand = expansion(mult=c(0,.02))) + 
  scale_y_continuous(expand=expansion(mult=c(0,.02))) + 
  labs(x="Latitude (°)", y="Number of families") + 
  scale_fill_manual(values=gray.colors(3)) + 
  theme(strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))

# Shannon index against latitude

#' Shannon index can be calculated with the diversity() function from the vegan package

# Turn data into right format
rep_wide <- gard_reptiles_dist %>% mutate(occ=1) %>% 
  filter(species %in% unique(gard_reptiles_dist$species)[1:100]) %>% 
  spread(species, occ) %>% mutate_all(~replace_na(., 0))

# Calculate Shannon index
library(vegan)
rep_wide$H <- diversity(rep_wide %>% select(-c("x","y")))

# Plot shannon index against latitude

rep_wide %>% select(x,y,H) %>% group_by(y) %>% 
  summarise(mn_H=mean(H), sd_H=sd(H)) %>% 
  ggplot(aes(x=y, y=mn_H, ymin=mn_H-sd_H, ymax=mn_H+sd_H)) + 
  geom_ribbon(fill="lightgrey") + geom_point() + geom_line() + 
  theme_bw() + scale_x_continuous(expand = expansion(mult=c(0,.02))) + 
  scale_y_continuous(expand=expansion(mult=c(0,.02))) + 
  labs(x="Latitude (°)", y="Shannon index") + 
  scale_fill_manual(values=gray.colors(3)) + 
  theme(strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))

#rep_wide_group <- gard_reptiles_dist %>% left_join(gard_reptiles, by=c("species"="Binomial")) %>%
#  filter(Group %in% c("lizard", "snake", "turtle")) %>% 
#  mutate(occ=1) %>% spread(species, occ) %>% 
#  mutate_all(~replace_na(., 0))

# Species evenness against latitude

# Calculate species evenness
rep_wide$J <- rep_wide$H/log(specnumber(rep_wide %>% select(-c(x,y,H))))

rep_wide %>% select(x,y,J) %>% group_by(y) %>% 
  summarise(mn_J=mean(J, na.rm=T), sd_J=sd(J, na.rm=T)) %>% 
  ggplot(aes(x=y, y=mn_J, ymin=mn_J-sd_J, ymax=mn_J+sd_J)) + 
  geom_ribbon(fill="lightgrey") + geom_point() + geom_line() + 
  theme_bw() + scale_x_continuous(expand = expansion(mult=c(0,.02))) + 
  scale_y_continuous(expand=expansion(mult=c(0,.02))) + 
  labs(x="Latitude (°)", y="Species evenness") + 
  scale_fill_manual(values=gray.colors(3)) + 
  theme(strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))

bt_rep <- betadiver(rep_wide %>% select(-c(x,y,H,J)))
# also see ?vegdist and bookmark with community stats

#rep_wide %>% select(x,y,beta) %>% group_by(y) %>% 
#  summarise(mn_J=mean(J, na.rm=T), sd_J=sd(J, na.rm=T)) %>% 
#  ggplot(aes(x=y, y=mn_J, ymin=mn_J-sd_J, ymax=mn_J+sd_J)) + 
#  geom_ribbon(fill="lightgrey") + geom_point() + geom_line() + 
#  theme_bw() + scale_x_continuous(expand = expansion(mult=c(0,.02))) + 
#  scale_y_continuous(expand=expansion(mult=c(0,.02))) + 
#  labs(x="Latitude (°)", y="Species evenness") + 
#  scale_fill_manual(values=gray.colors(3)) + 
#  theme(strip.background = element_blank(), 
#        strip.text=element_text(size=12, face="bold"))


