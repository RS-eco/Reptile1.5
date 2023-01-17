#' ---
#' title: "Reptile1.5 - Create figures for method scheme"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#Automatically install required packages, which are not yet installed
packages <- c("dplyr", "ggplot2", "sf", "scico")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE)
rm(packages, l)

# Specify colour scheme
bluered <- rev(scico(255, palette = 'roma'))

########################################

df_grid <- expand.grid(x=c(1:5), y=c(1:5))
df_grid$presence <- c(0, 1, 1, 0, 0,
                      1, 1, 1, 0, 0,
                      1, 1, 0, 0, 0,
                      1, 1, 0, 0, 0,
                      0, 0, 0, 0, 0)
df_grid$presence2 <- factor(df_grid$presence, labels=c("Absent", "Present"))

ggplot() + geom_tile(data=df_grid, aes(x, y, fill=presence2),
                     colour="white", size=1) + 
  coord_equal() + theme_bw() + 
  scale_fill_manual(name="", values=c("#a6cee3", "#d95f02"))+ 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/current_dist.png", dpi=1000, height=2, width=3.24)

df_grid$presence <- c(0, 1, 1, 0, 0,
                      1, 1, 1, 0, 0,
                      1, 1, 0, 0, 0,
                      0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0)
df_grid$presence2 <- factor(df_grid$presence, labels=c("Absent", "Present"))
ggplot() + geom_tile(data=df_grid, aes(x, y, fill=presence2),
                     colour="white", size=1) + 
  coord_equal() + theme_bw() + 
  scale_fill_manual(name="", values=c("#a6cee3", "#d95f02"))+ 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/current_dist2.png", dpi=1000, height=2, width=3.24)

set.seed(123)
df_grid$curclim <- round((df_grid$presence+0.75)*rnorm(25, mean = 10, sd = 2),2)
summary(df_grid$curclim)

df_grid$fut_presence <-  c(0, 0, 0, 0, 0,
                           0, 0, 1, 0, 0,
                           0, 1, 1, 1, 0,
                           0, 0, 1, 1, 1,
                           0, 0, 0, 0, 1)
df_grid$futclim <- round((df_grid$fut_presence+0.75)*rnorm(25, mean = 10, sd = 2),2)
summary(df_grid$futclim)
df_grid$fut_presence2 <- factor(df_grid$fut_presence, labels=c("Absent", "Present"))

ggplot() + 
  geom_tile(data=df_grid, aes(x, y, fill=curclim), colour="white", size=1) + 
  coord_equal() + theme_bw() + 
  scale_fill_gradientn(name="", colors=bluered, limits=c(4.5,25.1), expand=c(0,0)) + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(0.9, "cm"),
        legend.title = element_blank(), legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/current_climate.png", bg="transparent",
       dpi=1000, height=2, width=2.95)

ggplot() + 
  geom_tile(data=df_grid, aes(x, y, fill=futclim), 
            colour="white", size=1) + 
  coord_equal() + theme_bw() + 
  scale_fill_gradientn(name="", colors=bluered, limits=c(4.5,25.1), expand=c(0,0)) + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(0.9, "cm"),
        legend.title=element_blank(), legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/future_climate.png", bg="transparent",
       dpi=1000, height=2, width=2.95)

ggplot() + 
  geom_tile(data=df_grid, aes(x, y, fill=fut_presence2), colour="white", size=1) + 
  scale_fill_manual(name="", values=c("#a6cee3", "#d95f02"))+ 
  coord_equal() + theme_bw() + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(1, "cm"),
        legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/future_dist.png", dpi=1000, height=2, width=3.24)

set.seed(567)
df_grid$prob <- (df_grid$curclim/max(df_grid$curclim))-abs(rnorm(n=25, mean=0, sd=0.1))
ggplot() + 
  geom_tile(data=df_grid, aes(x, y, fill=prob), colour="white", size=1) + 
  scale_fill_gradientn(name="", colors=bluered, 
                       limits=c(0,1), expand=c(0,0)) + 
  coord_equal() + theme_bw() + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(0.925, "cm"),
        legend.title = element_blank(), legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/current_prob.png", bg="transparent",
       dpi=1000, height=2, width=3.115)

df_grid$fut_prob <- (df_grid$futclim/max(df_grid$futclim))-abs(rnorm(n=25, mean=0, sd=0.1))
ggplot() + 
  geom_tile(data=df_grid, aes(x, y, fill=fut_prob), colour="white", size=1) + 
  scale_fill_gradientn(name="", colors=bluered, 
                       limits=c(0,1), expand=c(0,0)) + 
  coord_equal() + theme_bw() + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(0.925, "cm"),
        legend.title = element_blank(), legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/future_prob.png", bg="transparent",
       dpi=1000, height=2, width=3.115)

########################################

## Change in range extent

head(df_grid)
df_grid$range_change <- paste0(df_grid$fut_presence, df_grid$presence)
df_grid$range_change2 <- factor(df_grid$range_change, 
                               labels=c("Absent", "Range loss", 
                                        "Range gain", "Stable"))

ggplot() + 
  geom_tile(data=df_grid, aes(x, y, fill=range_change2), colour="white", size=1) + 
  scale_fill_manual(name="", values=c("#a6cee3", "#1f78b4","#33a02c", "#b2df8a"))+ 
  coord_equal() + theme_bw() + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/range_change.png", dpi=1000, height=2, width=3.64,
       bg="transparent")

## Range overlap
df_grid$range_overlap <- factor(df_grid$range_change, 
                               labels=c("Absent", "Current", 
                                        "Future", "Overlap"))

ggplot() + 
  geom_tile(data=df_grid, aes(x, y, fill=range_overlap), colour="white", size=1) + 
  scale_fill_manual(name="", values=c("#a6cee3", "#0571b0",  "#f4a582", "#ca0020"))+ 
  coord_equal() + theme_bw() + 
  theme(axis.title=element_blank(), axis.text = element_blank(),
        axis.ticks=element_blank(), axis.line = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        legend.spacing = unit(0.05, "cm"), legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size=12),
        plot.margin = unit(c(0,0,0,0), "cm"))
ggsave(filename="figures/range_overlap.png", dpi=1000, height=2, width=3.35,
       bg="transparent")

## Change in range position

###
