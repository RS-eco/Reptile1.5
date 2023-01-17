#' ---
#' title: "Mean climatic distance of range-restricted vs. modeled species"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

library(ggplot2)
library(dplyr)
library(tidyr)
library(tagger)

# Calculate climate top25
climatic_distance <- readRDS("data/climatic_distance.rds")

# Calculate mean climatic distance
mean_dist <- climatic_distance %>% group_by(x,y,year,rcp) %>% 
  summarise(dist = mean(dist, na.rm=T)) %>% unite("year_rcp", year:rcp) %>%
  pivot_wider(names_from="year_rcp", values_from="dist")
rm(climatic_distance); gc()

##################################################

#-#-# Overlap small range species and highest change cells #-#-#

## Get species data
library(rasterSp)
#data("gard_reptiles_dist")
gard_reptiles_dist <- readRDS("data/gard_reptiles_dist.rds")

# Read data
AUC_data <- lapply(c("GAM", "GBM"), function(model_type){
  readRDS(paste0("data/AUCvalues_All_", 
                 model_type, "_Reptile.rds"))})
AUC_data <- do.call(rbind, AUC_data)
length(unique(AUC_data$Species))

# Aggregate the different AUC values from the 10 iterations per species
# and filter by AUC >= 0.7
AUC_high <- AUC_data %>% mutate(Species = sub("_", " ", Species)) %>%
  group_by(Species, model_type) %>% 
  summarise(mean = mean(AUC, na.rm=T)) %>% filter(mean >= 0.7) %>% ungroup() %>% 
  group_by(Species) %>% summarise(n = n()) %>% filter(n == 2)
rm(AUC_data); gc()
AUC_high <- AUC_high %>% mutate(modeled=1) %>% select(-c(n))

Reptile_dist <- gard_reptiles_dist %>% 
  left_join(AUC_high, by=c("species"="Species")) %>% group_by(species) %>% 
  left_join(mean_dist)
rm(gard_reptiles_dist, mean_dist, AUC_high); gc()

# Calculate mean and sd climatic distance for each species
mean_dist <- Reptile_dist %>% select(-c(x,y)) %>%
  pivot_longer(names_to="year_rcp", values_to="dist", -c(species, modeled)) %>%
  group_by(species, year_rcp, modeled) %>% 
  summarise(mean_dist = mean(dist, na.rm=T), sd_dist = sd(dist, na.rm=T)) %>%
  mutate(modeled = replace_na(modeled,0)) %>% ungroup() %>%
  mutate(modeled = factor(modeled, levels=c(1,0), labels=c("modeled", "non-modeled"))) %>%
  mutate(year_rcp = factor(year_rcp, levels=c("2050_rcp26", "2050_rcp60", "2050_rcp85",
                                         "2080_rcp26", "2080_rcp60", "2080_rcp85"), 
                           labels=c("a) \t 2050 RCP2.6", "b) \t 2050 RCP6.0", 
                                    "c) \t 2050 RCP8.5", "d) \t 2080 RCP2.6", 
                                    "e) \t 2080 RCP6.0", "f) \t 2080 RCP8.5")))

#-#-# Barplot small range vs. large range species

# Plot across years and RCPs
#neg_dist <- mean_dist %>% filter(modeled != "modeled") %>% mutate(mean_dist = -mean_dist)
#mean_dist <- mean_dist %>% filter(modeled == "modeled") %>% bind_rows(neg_dist)

#sub_dat <- mean_dist %>% separate(year_rcp, c("year", "rcp"), sep="_") %>% 
#  filter(rcp == "rcp60") %>% group_by(year) %>%
#  arrange(modeled, desc(mean_dist), .by_group = T) %>% 
#  mutate(spec = 1) %>% mutate(spec = cumsum(spec))
#(vline_dat <- sub_dat %>% filter(modeled == "modeled") %>% summarise(xmax=max(spec)))
#sub_dat$mean_dist <- abs(sub_dat$mean_dist)

#max(sub_dat$mean_dist, na.rm=T)

#ggplot() + geom_ribbon(data=sub_dat, aes(x=spec, ymin=mean_dist-sd_dist, 
#                                         ymax=mean_dist+sd_dist, fill=modeled)) + 
#  geom_line(data=sub_dat, aes(x=spec, y=mean_dist, lty=modeled)) + 
#  geom_vline(data=vline_dat, aes(xintercept=xmax), size=1/.pt) + 
#  facet_wrap(year~., scales="free", ncol=1, strip.position="right") + 
#  scale_fill_grey(name="", start=0.5, end=0.75) + scale_linetype(name="") + 
#  scale_y_continuous(name="Mean climatic distance", expand=expansion(mult=c(0,0.001)), 
#                     breaks=c(0,0.25,0.5,0.75,1)) + tag_facets() +
#  scale_x_continuous(name="Species", expand=expansion(mult=c(0.005,0.005))) + theme_bw() + 
#  theme(strip.text = element_text(size=12, face="bold"), strip.background= element_blank(), 
#        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
#        legend.position=c(0.633,0.95), legend.direction = "horizontal", 
#        legend.background = element_blank())
#ggsave("figures/FigureS34.png", width=9, height=6, dpi=300, bg="transparent")

#sub_dat <- mean_dist %>% separate(year_rcp, c("year", "rcp"), sep="_") %>% 
#  filter(year == "2080") %>% group_by(rcp) %>%
#  arrange(modeled, desc(mean_dist), .by_group = T) %>% 
#  mutate(spec = 1) %>% mutate(spec = cumsum(spec)) %>%
#  mutate(rcp = factor(rcp, levels=c("rcp26", "rcp60", "rcp85"), 
#                      labels=c("RCP2.6", "RCP6.0", "RCP8.5")))
#(vline_dat <- sub_dat %>% filter(modeled == "modeled") %>% summarise(xmax=max(spec)))
#sub_dat$mean_dist <- abs(sub_dat$mean_dist)

#ggplot() + geom_ribbon(data=sub_dat, aes(x=spec, ymin=mean_dist-sd_dist, 
#                                         ymax=mean_dist+sd_dist, fill=modeled)) + 
#  geom_line(data=sub_dat, aes(x=spec, y=mean_dist, lty=modeled)) + 
#  geom_vline(data=vline_dat, aes(xintercept=xmax), size=1/.pt) + 
#  facet_wrap(rcp~., scales="free", ncol=1, strip.position="right") + 
#  scale_fill_grey(name="", start=0.5, end=0.75) + scale_linetype(name="") + 
#  scale_y_continuous(name="Mean climatic distance", expand=expansion(mult=c(0,0.001))) + 
#  tag_facets() +
#  scale_x_continuous(name="Species", expand=expansion(mult=c(0.005,0.005))) + theme_bw() + 
#  theme(strip.text = element_text(size=12, face="bold"), strip.background= element_blank(), 
#        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
#        legend.position=c(0.632,0.965), legend.direction = "horizontal", 
#        legend.background = element_blank())
#ggsave("figures/FigureS35.png", width=9, height=9, dpi=300, bg="transparent")

#' **Supporting Figure 35.** Mean climatic distance (line) for each reptile species under 
#' different representative concentration pathways (a) RCP2.6, b) RCP6.0 and c) RCP8.5 for the year 2080. 
#' Species were divided into those that were modeled using species distribution models (solid line, darkgrey) 
#' and species that could not be modeled due to a low sample size (dashed line, lightgrey). 
#' Error margins show standard deviation across spatial distribution of species.

mean_dat <- mean_dist %>% ungroup() %>% group_by(year_rcp, modeled) %>%
  dplyr::summarise(median_dist2 = median(mean_dist, na.rm = T), 
                   mean_dist2 = round(mean(mean_dist, na.rm = T),2),
                   sd = sd(mean_dist, na.rm = T),
                   se = sd(mean_dist, na.rm = T)/
                     sqrt(length(mean_dist)))
test <- ggpubr::compare_means(mean_dist ~ modeled, mean_dist, group.by = "year_rcp",
                              method="t.test", p.adjust.method = "holm",
                              paired=F) %>% as.data.frame() %>% group_by(year_rcp)
test_y <- mean_dist %>% group_by(year_rcp) %>% summarise(y=max(mean_dist, na.rm=T))
test <- left_join(test, test_y)
test$y <- test$y*0.9
test$y2 <- test$y + (0.025*test$y)
test$x <- rep(1.2, 6)
test$xend <- rep(1.8, 6)
test$xmid <- rep(1.5, 6)
test <- test %>% group_by(year_rcp) %>% group_split()
test[[1]]

p2 <- lapply(1:length(unique(mean_dist$year_rcp)), function(x){
  yr_rcp <- unique(mean_dist$year_rcp)[x]
  p_sub <- ggstatsplot::ggbetweenstats(data=mean_dist %>% filter(year_rcp == yr_rcp), 
                                       x = modeled, y = mean_dist, 
                                       results.subtitle = F, bf.message = F, 
                                       centrality.plotting = F, pairwise.comparisons = F,
                                       ggplot.component = list(scale_y_continuous(limits=c(0,NA), 
                                                                                  expand=expansion(
                                                                                    mult = c(0, 0.035))),
                                                               scale_colour_manual(name="",  values=c("#000000", "#BABABA")))) + 
    ggplot2::geom_point(aes(x=modeled, y=mean_dist2), 
                        data = mean_dat %>% filter(year_rcp == yr_rcp), inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=x, xend=xend, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggrepel::geom_label_repel(aes(x=modeled, y=mean_dist2, label=mean_dist2),
                              data = mean_dat %>% filter(year_rcp == yr_rcp),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    ggtitle(yr_rcp) + theme_bw() + theme(legend.position = "none")
  if(x %in% c(1,4)){
    p_sub <- p_sub + labs(y="Mean climatic distance", caption=NULL) + 
      theme(axis.title.y = element_text(size=14, face="bold"),
            axis.title.x = element_blank())
  } else{
    p_sub <- p_sub + labs(caption=NULL) + 
      theme(axis.title = element_blank())
  }
  if(x >= 4){
    p_sub <- p_sub + labs(x="Species") + 
      theme(axis.title.x = element_text(size=14, face="bold"))
  }
  return(p_sub)
})
p2 <- ggstatsplot::combine_plots(p2)
p2
ggsave("figures/FigureS38.png", p2, width=9, height=6, dpi=600, bg="transparent")
