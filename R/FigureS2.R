########################################

# Create boxplot of AUC for each species and model algorithm

library(ggplot2)

# Read data
taxa <- "Reptile"
AUC_data <- lapply(c("GAM", "GBM"), function(model_type){
  readRDS(paste0("data/AUCvalues_All_", model_type, "_", taxa, ".rds"))
})
AUC_data <- do.call(rbind, AUC_data)

# Load rasterSp package
#remotes::install_github("RS-eco/rasterSp)
library(rasterSp)
data(gard_reptiles)

# Aggregate the different AUC values from the 10 iterations per species
library(dplyr)
AUC_sum1 <- AUC_data %>% group_by(Species, taxa, model_type) %>%
  summarise(mean = mean(AUC, na.rm=T)) %>% 
  mutate(Species =sub("_", " ", Species)) %>% 
  left_join(gard_reptiles, by=c("Species" = "Binomial"))  %>% 
  mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard"))) 
AUC_sum2 <- AUC_data %>% group_by(Species, taxa, model_type) %>%
  summarise(mean = mean(AUC, na.rm=T)) %>% 
  mutate(Species = sub("_", " ", Species))
AUC_sum2$Group <- "Total"

AUC_sum <- bind_rows(AUC_sum1, AUC_sum2)%>%
  mutate(Group = factor(Group, levels=c("Lizard", "Snake", "Turtle", "Total")))
AUC_sum$Group <- factor(AUC_sum$Group, labels = paste0(letters[1:4], ")\t \t", levels(AUC_sum$Group)))
head(AUC_sum)

AUC_sum %>% pivot_wider(names_from = "model_type", values_from="mean") %>%
  filter(GAM < 0.7 | GBM < 0.7) %>% group_by(taxa, Group) %>%
  summarise(n_distinct(Species))

#Subset by AUC > 0.7 for both GAM & GBM
library(tidyr)
AUC_sum_high <- AUC_sum %>% pivot_wider(names_from = "model_type", values_from="mean") %>%
  filter(GAM >= 0.7 & GBM >= 0.7) %>% 
  pivot_longer(cols=c("GAM", "GBM"), names_to="model_type", values_to="mean")

# Make plot
AUC_sum_high %>% 
  ggstatsplot::grouped_ggwithinstats(x = model_type, y = mean, grouping.var = Group,
                                      results.subtitle = FALSE, bf.message = F,
                                      plotgrid.args = list(ncol=2), point.path=F,
                                      ggplot.component =
                                        list(theme_bw() + theme(axis.line = element_line(size = 0.5, colour = "black"),
                                                                panel.background = element_blank(), strip.background = element_blank(),
                                                                text=element_text(size = 16), strip.text = element_text(face="bold", size=16),
                                                                axis.text=element_text(colour="black", size = 12), legend.position="none"),
                                             labs(x="", y="AUC", caption=NULL),
                                             ggplot2::scale_y_continuous(breaks = seq(0.7, 1, by=0.1), limits = c(0.7, 1),
                                                                         expand=c(0.02,0)),
                                               scale_colour_manual(name="",  values=c("#000000", "#BABABA"))
                                             ))
ggsave("figures/FigureS2.png", width = 8, height = 9, dpi = 300, bg="transparent") 
