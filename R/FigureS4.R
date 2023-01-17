library(rasterSp) # remotes::install_github("RS-eco/rasterSp")
library(ggplot2); library(ggridges); library(dplyr); library(ggrepel)
#devtools::install_github("eliocamp/tagger")
library(tagger)

data(gard_reptiles)

#' Read dispersal distances
dispersal_distances <- readRDS("data/dispersal_distances.rds") %>%
  mutate(species = sub("_", " ", species)) %>%
  left_join(gard_reptiles, by=c("species"="Binomial"))

#' ## Plot density plot of dispersal distances
#+ message=F, echo=F, fig.width=10, fig.height=6
total_disp <- dispersal_distances %>% select(-c(Area, FID_2, TID, Value)) %>%
  tidyr::gather(disp, value, -c(species, taxa, Group)) %>%
  mutate(Group = "total")
dat <- dispersal_distances %>% select(-c(Area, FID_2, TID, Value)) %>%
  tidyr::gather(disp, value, -c(species, taxa, Group)) %>% 
  bind_rows(total_disp) %>%
  dplyr::mutate(disp = factor(disp, labels=c("d", "d/2", "d/4", "d/8", 
                                             "d/12", "d/16", "d/20"))) %>% 
  filter(disp %in% c("d/4", "d/8", "d/16")) %>% 
  mutate(Group = factor(Group, levels = c("croc", "lizard", "Rhynchocephalia", 
                                          "snake", "turtle", "worm lizard", "total"),
                        labels = c("Lizard", "Lizard", "Lizard", "Snake", 
                                   "Turtle", "Lizard", "Total")))
dat_sum <- dat %>% group_by(disp, Group) %>%
  summarise(mn_dist=round(mean(value, na.rm=T),0),
            md_dist=median(value, na.rm=T))
dat_sum$y <- rep(c(23,9,1.5,34), 3)

dat %>% 
  ggplot() + 
  geom_freqpoly(aes(x=value, colour=disp), stat="bin", bins=100) +
  geom_vline(data=dat_sum, aes(xintercept=mn_dist, colour=disp, lty=disp)) + 
  geom_text(data=dat_sum, aes(x=mn_dist, y=y, label=mn_dist)) + 
  facet_grid(Group ~ ., scales="free_y") + theme_bw() + 
  tag_facets(position = list(x=0.016, y=0.95)) + 
  scale_colour_manual(name="Dispersal", values= ggsci::pal_d3("category10")(3)) + 
  scale_x_sqrt(expand=expansion(add=c(0,0))) + 
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +  
  scale_linetype_manual(name="Dispersal", values=c("twodash", "dotted", "longdash"))+
  labs(x="Distance (km, sqrt-transformed)", y="Number of species") + 
  theme(legend.position=c(0.9,0.9), strip.background = element_blank(), 
        strip.text = element_text(size=12, face="bold")) 
ggsave("figures/FigureS4.png", width = 8, height = 10, dpi = 300, bg="transparent") 

#' ## Table of mean & median dispersal distances
#+ echo=F
dispersal_distances %>% select(-c(Area, FID_2, TID, Value)) %>%
  tidyr::gather(disp, value, -c(species, taxa, Group)) %>%
  dplyr::mutate(disp = factor(disp, labels=c("d", "d/2", "d/4", "d/8", "d/12", "d/16", "d/20"))) %>%
  filter(disp %in% c("d/4", "d/8", "d/16")) %>% 
  mutate(Group = factor(Group, levels = c("croc", "lizard", "Rhynchocephalia", 
                                          "snake", "turtle", "worm lizard"),
                        labels = c("Lizard", "Lizard", "Lizard", "Snake", 
                                   "Turtle", "Lizard"))) %>% 
  dplyr::group_by(disp, Group) %>% 
  dplyr::summarise(mean=mean(value), mean_yr_2050 = mean(value)/55,
                   mean_yr_2080 = mean(value)/85, median=median(value),
                   median_yr_2050 = median(value)/55, median_yr_2080 = median(value)/85) %>% t() %>% 
  knitr::kable()
# Mean distance, Mean distance (yr-1) - 2050, Mean distance (yr-1) - 2080, 
# Median distance, Median distance (yr-1) - 2050, Median distance (yr-1) - 2080