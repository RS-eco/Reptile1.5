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

#define climate change thresholds
summary(climatic_distance$dist)
hist(climatic_distance$dist)
(thres <- climatic_distance %>% group_by(year, rcp, gcm) %>% 
    summarise(thres = quantile(dist, probs=0.75)))

# Merge threshold with data
climatic_distance <- left_join(climatic_distance, thres); rm(thres)

# Subset data by threshold
climatic_distance_top25 <- climatic_distance %>% filter(dist > thres)
hist(climatic_distance$dist)

# Plot top climatic distance
ggplot() + geom_tile(data=climatic_distance_top25 %>% filter(gcm == "MIROC5", rcp=="rcp60"), 
                     aes(x=x, y=y, fill=dist)) + 
  facet_grid(year~rcp) + scico::scale_fill_scico(palette = "roma") + 
  theme_classic() + theme(strip.background = element_blank())

# Save subset to file
#saveRDS(climatic_distance_top25, "data/climate_distance_top25.rds", compress="xz")
rm(climatic_distance_top25); gc()

# Calculate mean climatic distance
mean_dist <- climatic_distance %>% group_by(x,y,year,rcp) %>% 
  summarise(dist = mean(dist, na.rm=T)) %>% unite("year_rcp", year:rcp) %>%
  pivot_wider(names_from="year_rcp", values_from="dist")
rm(climatic_distance); gc()

# Split climatic distance into quantiles
#quantiles <- (0:8)/8 # how many quantiles we want to map 
#colours <- rev(scico(8, palette = 'roma')) # 8 evenly interpolated colors 
#quantile.vals <- quantile(mean_dist$dist, quantiles, names=F, na.rm=T) # the values for each quantile
#quantile.vals <- c(floor(quantile.vals[1]*1000)/1000, ceiling(quantile.vals[2:9]*1000)/1000)

#cut(dist, breaks=quantile.vals, include.lowest=T)

##################################################

#-#-# Overlap small range species and highest change cells #-#-#

## Get species data
library(rasterSp)
data(gard_reptiles)
gard_reptiles <- gard_reptiles %>% 
  mutate(Group = factor(Group, 
                        levels = c("croc", "lizard", "Rhynchocephalia", "snake", 
                                   "turtle", "worm lizard"),
                        labels = c("Lizard", "Lizard", "Lizard", 
                                   "Snake", "Turtle", "Lizard")))

# Row 2
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
  left_join(gard_reptiles, by=c("Species"="Binomial")) %>% 
  group_by(Species, Group, model_type) %>% 
  summarise(mean = mean(AUC, na.rm=T)) %>% filter(mean >= 0.7) %>% ungroup() %>% 
  group_by(Species, Group) %>% summarise(n = n()) %>% filter(n == 2)
rm(AUC_data); gc()
AUC_high <- AUC_high %>% mutate(modeled=1) %>% select(-c(Group, n))

Reptile_dist <- gard_reptiles_dist %>% 
  left_join(gard_reptiles, by=c("species"="Binomial")) %>%
  left_join(AUC_high, by=c("species"="Species")) %>% group_by(species, Group) %>% 
  left_join(mean_dist)
rm(gard_reptiles_dist, mean_dist, gard_reptiles, AUC_high); gc()

# Calculate mean and sd climatic distance for each species
mean_dist <- Reptile_dist %>% select(-c(x,y,Area,FID_2, TID, Value)) %>%
  pivot_longer(names_to="year_rcp", values_to="dist", -c(species, Group, modeled)) %>%
  group_by(species, Group, year_rcp, modeled) %>% 
  summarise(mean_dist = mean(dist, na.rm=T), sd_dist = sd(dist, na.rm=T)) %>%
  mutate(modeled = replace_na(modeled,0)) %>%
  mutate(modeled = factor(modeled, levels=c(1,0), labels=c("modeled", "non-modeled")))

#-#-# Small range vs. large range species

# Plot for 2080, RCP6.0
sub_dat <- mean_dist %>% filter(year_rcp == "2080_rcp60") %>% ungroup() %>% 
  group_by(Group) %>% arrange(modeled, desc(mean_dist), desc(sd_dist)) %>%
  group_by(Group, modeled) %>% mutate(spec = 1) %>% mutate(spec = cumsum(spec))

mean_dat <- sub_dat %>% group_by(Group, modeled) %>%
  dplyr::summarise(median_dist2 = median(mean_dist, na.rm = T), 
                   mean_dist2 = round(mean(mean_dist, na.rm = T),2),
                   sd = sd(mean_dist, na.rm = T),
                   se = sd(mean_dist, na.rm = T)/
                     sqrt(length(mean_dist)))

ggplot() + geom_ribbon(data=sub_dat, aes(x=spec, ymin=mean_dist-sd_dist, 
                                         ymax=mean_dist+sd_dist, fill=modeled), alpha=.75) + 
  geom_line(data=sub_dat, aes(x=spec, y=mean_dist, lty=modeled)) + 
  geom_hline(data=mean_dat, aes(yintercept=mean_dist2, lty=modeled), 
             size=1/.pt, col="grey50") + 
  facet_wrap(Group~., scales="free", ncol=1, strip.position="right") + 
  scale_fill_manual(name="", values=c("#f1a340", "#998ec3")) + scale_linetype(name="") + 
  scale_y_continuous(name="Mean climatic distance", limits=c(0,NA),
                     expand=expansion(mult=c(0,0.001)),
                     breaks=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1)) + 
  tag_facets(position = list(x = 0.03, y = 0.9)) +
  scale_x_continuous(name="Species", expand=expansion(mult=c(0.005,0.005))) + theme_bw() + 
  theme(strip.text = element_text(size=12, face="bold"), strip.background= element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
        legend.position=c(0.85,0.925), legend.direction = "horizontal", 
        legend.background = element_blank())
#ggsave("figures/Figure6_v1.png", width=8, height=6, dpi=600, bg="transparent")

total_dat <- mean_dist %>% filter(year_rcp == "2080_rcp60") %>% ungroup() %>% 
  arrange(modeled, desc(mean_dist), desc(sd_dist)) %>%
  group_by(modeled) %>% mutate(spec = 1) %>% mutate(spec = cumsum(spec))
total_dat$Group <- "Total"
ggpubr::compare_means(mean_dist ~ modeled, total_dat,
                      method="t.test", p.adjust.method = "holm",
                      paired=F) %>% as.data.frame()


all_dat <- bind_rows(sub_dat, total_dat) %>%
  mutate(Group = factor(Group, levels=c("Lizard", "Snake", "Turtle", "Total")))
all_dat$Group <- factor(all_dat$Group, 
                        labels = paste0(letters[1:4], ")\t \t", levels(all_dat$Group)))

mean_dat <- all_dat %>% group_by(Group, modeled) %>%
  dplyr::summarise(median_dist2 = median(mean_dist, na.rm = T), 
                   mean_dist2 = round(mean(mean_dist, na.rm = T),2),
                   sd = sd(mean_dist, na.rm = T),
                   se = sd(mean_dist, na.rm = T)/
                     sqrt(length(mean_dist)))
test <- ggpubr::compare_means(mean_dist ~ modeled, all_dat, group.by = c("Group"),
                              method="t.test", p.adjust.method = "holm",
                              paired=F) %>% as.data.frame() %>% group_by(Group)
test_y <- all_dat %>% group_by(Group) %>% summarise(y=max(mean_dist, na.rm=T))
test <- left_join(test, test_y)
test$y <- test$y*0.9
test$y2 <- test$y + (0.025*test$y)
test$x <- c(1.2, 1.2, 1.2, 1.2)
test$xend <- c(1.8, 1.8, 1.8, 1.8)
test$xmid <- c(1.5, 1.5, 1.5, 1.5)
test <- test %>% group_by(Group) %>% group_split()
test[[1]]

p2 <- lapply(1:length(levels(all_dat$Group)), function(x){
  rea <- levels(all_dat$Group)[x]
  p_sub <- ggstatsplot::ggbetweenstats(data=all_dat %>% filter(Group == rea), 
                              x = modeled, y = mean_dist, 
                              results.subtitle = F, bf.message = F, 
                              centrality.plotting = F, pairwise.comparisons = F,
                              ggplot.component = list(scale_y_continuous(limits=c(0,NA), 
                                                                         expand=expansion(
                                                                           mult = c(0, 0.035))),
                                                      scale_colour_manual(name="",  values=c("#000000", "#BABABA")))) + 
    ggplot2::geom_point(aes(x=modeled, y=mean_dist2), 
                        data = mean_dat %>% filter(Group == rea), inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=x, xend=xend, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggrepel::geom_label_repel(aes(x=modeled, y=mean_dist2, label=mean_dist2),
                              data = mean_dat %>% filter(Group == rea),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    ggtitle(rea) + theme_bw() + theme(legend.position = "none")
  if((x %% 2) == 1){
    p_sub <- p_sub + labs(y="Mean climatic distance", caption=NULL) + 
      theme(axis.title.y = element_text(size=14, face="bold"),
            axis.title.x = element_blank())
  } else {
    p_sub <- p_sub + labs(caption=NULL) + 
      theme(axis.title = element_blank())
  }
  if (x %in% c(3,4)){
    p_sub <- p_sub + labs(x="Species") + 
      theme(axis.title.x = element_text(size=14, face="bold"))
  } 
  return(p_sub)
})
p2 <- ggstatsplot::combine_plots(p2)
p2
ggsave("figures/Figure6.png", p2, width=6, height=6, dpi=600, bg="transparent")
