#' ---
#' title: "Reptile1.5 - Check model fit"
#' author: "RS-eco"
#' ---

#' Load packages
library(dplyr); library(magrittr); library(ggplot2); 
library(patchwork); library(scico); library(ggpmisc)

####################

#' Create ggplot2 theme
theme_plot <- function(base_size = 10, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid = element_blank(),
          plot.title = element_text(size=12, face="bold", vjust=1, hjust=0.5, 
                                    margin = margin(t = 0, r = 0, b = 2, l = 0)),
          axis.title.x = element_text(size=12, face="bold", angle=0, vjust=1, 
                                    hjust=0.5, margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size=12, face="bold", angle=90, vjust=1, 
                                    hjust=0.5, margin = margin(t = 0, r = 10, b = 0, l = 0)),
          panel.spacing.y = unit(0., "lines"))}

# Specify colour scheme
bluered <- rev(scico(255, palette = 'roma'))
bluewhitered <- scico(n=255, palette="vik")

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

####################

### Figure S3 (Scatterplot observed vs. richness)

# Merge predicted and observed richness

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
  left_join(AUC_high, by=c("species"="Species")) %>% group_by(species, Group)
rm(gard_reptiles_dist, gard_reptiles, AUC_high); gc()

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

# Merge data
sr1$algorithm <- "GAM"
sr2$algorithm <- "GBM"
dat <- bind_rows(sr1, sr2) %>% 
  mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard")))
rm(sr1, sr2)

sub <- "prob"

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

# Merge data
sr1$algorithm <- "GAM"
sr2$algorithm <- "GBM"
dat_prob <- bind_rows(sr1, sr2) %>% 
  mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard")))
rm(sr1, sr2)

#########################

# Plot richness for 1995

# Calculate ensemble mean of 1995 richness
tot_1995 <- dat %>% ungroup() %>% dplyr::select(x,y,algorithm,Group,EWEMBI_1995) %>% 
  group_by(x,y,algorithm,Group) %>% summarise_at(vars(-group_cols()), sum) %>%
  group_by(x,y,Group) %>% summarise(mean = mean(EWEMBI_1995, na.rm=T))

prob_1995 <- dat_prob %>% ungroup() %>% dplyr::select(x,y,algorithm,Group,EWEMBI_1995) %>% 
  group_by(x,y,algorithm,Group) %>% summarise_at(vars(-group_cols()), sum) %>%
  group_by(x,y,Group) %>% summarise(prob = mean(EWEMBI_1995, na.rm=T))

sr_obs <- Reptile_dist %>% group_by(x,y,Group) %>% summarise(sr = sum(modeled, na.rm=T))
sr_obs_pred <- full_join(tot_1995, sr_obs) %>% full_join(prob_1995)
sr_obs_pred <- sr_obs_pred %>% tidyr::drop_na(sr, mean, prob) 

# Drop richness values for which one of them is NA
# => Not good!!! Why?

#sr_obs_pred$sr <- sr_obs_pred$sr %>% tidyr::replace_na(0)
#sr_obs_pred$mean <- sr_obs_pred$mean %>% tidyr::replace_na(0)
#sr_obs_pred$prob <- sr_obs_pred$prob %>% tidyr::replace_na(0)

dat_plot <- sr_obs_pred %>% filter(Group=="Lizard")
p1 <- ggplot() + geom_point(data=dat_plot, aes(x=sr, y=mean), alpha=0.1, pch=1, colour="#ef8a62") + 
  geom_point(data=dat_plot, aes(x=sr, y=prob), alpha=0.1, pch=1, colour="#67a9cf") + 
  geom_smooth(data=dat_plot, aes(x=sr, y=mean), method = "lm", colour="#b2182b", size=0.75) + 
  geom_smooth(data=dat_plot, aes(x=sr, y=prob), method = "lm", colour="#2166ac", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  labs(tag="a)", title="Lizard") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=mean, label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.9, parse=TRUE, colour="#b2182b") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=mean, label = ..adj.rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#b2182b") +
  stat_poly_eq(data=dat_plot, aes(x=sr, y=prob, label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.2, parse=TRUE, colour="#2166ac") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=prob, label = ..adj.rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.1, parse=TRUE, colour="#2166ac") + 
  scale_x_continuous("", limits=c(0,112), expand=c(0,0)) + 
  scale_y_continuous("Projected current richness", limits=c(0,112), 
                     expand=c(0,0)) + theme_plot() + coord_equal() + 
  theme(plot.tag.position = c(0.185,0.91))
dat_plot <- sr_obs_pred %>% filter(Group=="Snake")
p2 <- ggplot() + geom_point(data=dat_plot, aes(x=sr, y=mean), alpha=0.1, pch=1, colour="#ef8a62") + 
  geom_point(data=dat_plot, aes(x=sr, y=prob), alpha=0.1, pch=1, colour="#67a9cf") + 
  geom_smooth(data=dat_plot, aes(x=sr, y=mean), method = "lm", colour="#b2182b", size=0.75) + 
  geom_smooth(data=dat_plot, aes(x=sr, y=prob), method = "lm", colour="#2166ac", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  labs(tag="b)", title="Snake") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=mean, label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.9, parse=TRUE, colour="#b2182b") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=mean, label = ..adj.rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#b2182b") +
  stat_poly_eq(data=dat_plot, aes(x=sr, y=prob, label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.2, parse=TRUE, colour="#2166ac") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=prob, label = ..adj.rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.1, parse=TRUE, colour="#2166ac") + 
  scale_x_continuous("", limits=c(0,150), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,150), expand=c(0,0)) + theme_plot() + 
  theme(plot.tag.position = c(0.2,0.91)) + coord_equal()
dat_plot <- sr_obs_pred %>% filter(Group=="Turtle")
p3 <- ggplot() + geom_point(data=dat_plot, aes(x=sr, y=mean), alpha=0.1, pch=1, colour="#ef8a62") + 
  geom_point(data=dat_plot, aes(x=sr, y=prob), alpha=0.1, pch=1, colour="#67a9cf") + 
  geom_smooth(data=dat_plot, aes(x=sr, y=mean), method = "lm", colour="#b2182b", size=0.75) + 
  geom_smooth(data=dat_plot, aes(x=sr, y=prob), method = "lm", colour="#2166ac", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  labs(tag="c)", title="Turtle") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=mean, label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.9, parse=TRUE, colour="#b2182b") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=mean, label = ..adj.rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#b2182b") +
  stat_poly_eq(data=dat_plot, aes(x=sr, y=prob, label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.2, parse=TRUE, colour="#2166ac") + 
  stat_poly_eq(data=dat_plot, aes(x=sr, y=prob, label = ..adj.rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.1, parse=TRUE, colour="#2166ac") + 
  scale_x_continuous("Range-based observed richness", limits=c(0,30), expand=c(0,0)) + 
  scale_y_continuous("Projected current richness", limits=c(0,30), expand=c(0,0)) + theme_plot() + 
  theme(plot.tag.position = c(0.185,0.91)) + coord_equal()

# Calculate ensemble mean of 1995 richness
tot_1995 <- dat %>% ungroup() %>% dplyr::select(x,y,algorithm,EWEMBI_1995) %>% 
  group_by(x,y,algorithm) %>% summarise_at(vars(-group_cols()), sum) %>%
  group_by(x,y) %>% summarise(mean = mean(EWEMBI_1995, na.rm=T))
rm(dat)

prob_1995 <- dat_prob %>% ungroup() %>% dplyr::select(x,y,algorithm,EWEMBI_1995) %>% 
  group_by(x,y,algorithm) %>% summarise_at(vars(-group_cols()), sum) %>%
  group_by(x,y) %>% summarise(prob = mean(EWEMBI_1995, na.rm=T))

sr_obs <- Reptile_dist %>% group_by(x,y) %>% summarise(sr = sum(modeled, na.rm=T))
sr_obs_pred <- full_join(tot_1995, sr_obs) %>% full_join(prob_1995)
sr_obs_pred <- sr_obs_pred %>% tidyr::drop_na(sr, mean, prob)

# Drop richness values for which one of them is NA
# => Not good!!! Why?

#sr_obs_pred$sr <- sr_obs_pred$sr %>% tidyr::replace_na(0)
#sr_obs_pred$mean <- sr_obs_pred$mean %>% tidyr::replace_na(0)
#sr_obs_pred$prob <- sr_obs_pred$prob %>% tidyr::replace_na(0)

# Plot observed vs. predicted species richness
p4 <- ggplot() + geom_point(data=sr_obs_pred, aes(x=sr, y=mean), alpha=0.1, pch=1, colour="#ef8a62") + 
  geom_point(data=sr_obs_pred, aes(x=sr, y=prob), alpha=0.1, pch=1, colour="#67a9cf") + 
  geom_smooth(data=sr_obs_pred, aes(x=sr, y=mean), method = "lm", colour="#b2182b", size=0.75) + 
  geom_smooth(data=sr_obs_pred, aes(x=sr, y=prob), method = "lm", colour="#2166ac", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  labs(tag="d)", title="Total") + 
  stat_poly_eq(data=sr_obs_pred, aes(x=sr, y=mean, label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.9, parse=TRUE, colour="#b2182b") + 
  stat_poly_eq(data=sr_obs_pred, aes(x=sr, y=mean, label = ..adj.rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#b2182b") +
  stat_poly_eq(data=sr_obs_pred, aes(x=sr, y=prob, label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.2, parse=TRUE, colour="#2166ac") + 
  stat_poly_eq(data=sr_obs_pred, aes(x=sr, y=prob, label = ..adj.rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.1, parse=TRUE, colour="#2166ac") + 
  scale_x_continuous("Range-based observed richness", limits=c(0,260), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,260), 
                     expand=c(0,0)) + coord_equal() + theme_plot() + 
  theme(plot.tag.position = c(0.2,0.91))

p <- p1 + p2 + p3 + p4 + plot_layout(ncol=2)
ggsave("figures/FigureS3.png", p, width=9, height=9, dpi=600)

####################
