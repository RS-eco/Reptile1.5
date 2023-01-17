#' ---
#' title: "PCA of current and future climate and
#' the resulting climatic distance"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

library(ggplot2); library(ggrepel); library(scico)
library(dplyr); library(tidyr)

# Load PCA data
clim_pca_current <- readRDS("data/clim_pca_current.rds")
clim_pca_future <- readRDS("data/clim_pca_future.rds")
clim_pca_future$rcp <- factor(clim_pca_future$rcp, levels=c("rcp26", "rcp60", "rcp85"),
                                  labels=c("RCP2.6", "RCP6.0", "RCP8.5"))

# Load PCA loadings
current_pca_loadings <- readRDS("data/current_pca_loadings.rds") %>%
  pivot_wider(names_from="PC", values_from="value", names_prefix="PC")
future_pca_loadings <- readRDS("data/future_pca_loadings.rds") %>%
  pivot_wider(names_from="PC", values_from="value", names_prefix="PC")
future_pca_loadings$rcp <- factor(future_pca_loadings$rcp, levels=c("rcp26", "rcp60", "rcp85"),
                                labels=c("RCP2.6", "RCP6.0", "RCP8.5"))

# Load PCA eigenvalues
current_pca_eigenvalues <- readRDS("data/current_pca_eigenvalues.rds") %>%
  select(-c(std.dev, cumulative)) %>% pivot_wider(names_from="PC", values_from="percent", names_prefix="PC")
future_pca_eigenvalues <- readRDS("data/future_pca_eigenvalues.rds") %>%
  select(-c(std.dev, cumulative)) %>% pivot_wider(names_from="PC", values_from="percent", names_prefix="PC")
future_pca_eigenvalues$rcp <- factor(future_pca_eigenvalues$rcp, levels=c("rcp26", "rcp60", "rcp85"),
                                  labels=c("RCP2.6", "RCP6.0", "RCP8.5"))

print(current_pca_eigenvalues)

##################################################

# Calculate mean clim_pca_future
mean_clim_pca_future <- clim_pca_future %>% group_by(x,y,year,rcp) %>%
  summarise(PC1 = mean(.fittedPC1), PC2 = mean(.fittedPC2)); rm(clim_pca_future); gc()
mean_future_pca_loadings <- future_pca_loadings %>% group_by(year,rcp,column) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2))
mean_future_pca_eigenvalues <- future_pca_eigenvalues %>% 
  group_by(year,rcp) %>% summarise(PC1 = mean(PC1), PC2 = mean(PC2))

## Plot current & future PCA data
p1 <- lapply(c("RCP2.6", "RCP6.0", "RCP8.5"), function(x){
  ggplot() + geom_point(data = mean_clim_pca_future %>% 
                          filter(year == 2050, rcp == x), 
                        aes(x=PC1,y=PC2), alpha=0.25, fill=NA, 
                        shape=1, colour="#FF7F0EFF") + 
    geom_point(data = clim_pca_current %>% select(-year), 
               aes(x=.fittedPC1,y=.fittedPC2), 
               alpha=0.25, fill=NA, shape=1, colour="#2CA02CFF") + 
    geom_vline(xintercept=0, linetype="dashed", col="grey") + 
    geom_hline(yintercept=0, linetype="dashed", col="grey") + 
    geom_segment(data = current_pca_loadings, 
                 aes(x = 0, y = 0, xend = (PC1*2), yend = (PC2*2)), 
                 arrow = arrow(length = unit(1/2, "picas")), color="#1F77B4FF") + 
    geom_text_repel(data = current_pca_loadings, 
                    aes(x = PC1*2, y = PC2*2, label = column), color="#1F77B4FF") + 
    geom_segment(data = mean_future_pca_loadings %>% 
                   filter(year == 2050, rcp == x), 
                 aes(x = 0, y = 0, xend = (PC1*2), yend = (PC2*2)), 
                 arrow = arrow(length = unit(1/2, "picas")), color="#D62728FF") + 
    geom_text_repel(data = mean_future_pca_loadings  %>% 
                      filter(year == 2050, rcp == x), 
                    aes(x = PC1*2, y = PC2*2, label = column), color="#D62728FF") + 
    scale_x_continuous(expand=expansion(mult=c(0.025,0.025))) + 
    scale_y_continuous(expand=expansion(mult=c(0.025,0.025))) + 
    theme_bw() + labs(x=paste0("PC1 (", round(mean_future_pca_eigenvalues[mean_future_pca_eigenvalues$year == 2050 & 
                                                                            mean_future_pca_eigenvalues$rcp == x,]$PC1*100,2), "%)"), 
                      y=paste0("PC2 (", round(mean_future_pca_eigenvalues[mean_future_pca_eigenvalues$year == 2050 & 
                                                                            mean_future_pca_eigenvalues$rcp == x,]$PC2*100,2), "%)")) +
    theme(strip.background = element_blank(), 
          strip.text = element_text(size=12, face="bold"))
})
p2 <- lapply(c("RCP2.6", "RCP6.0", "RCP8.5"), function(x){
  ggplot() + geom_point(data = mean_clim_pca_future %>% 
                          filter(year == 2080, rcp == x), 
                        aes(x=PC1,y=PC2), alpha=0.25, fill=NA, 
                        shape=1, colour="#FF7F0EFF") + 
    facet_grid(rcp~., scales="free") + 
    geom_point(data = clim_pca_current %>% select(-year), 
               aes(x=.fittedPC1,y=.fittedPC2), 
               alpha=0.25, fill=NA, shape=1, colour="#2CA02CFF") + 
    geom_vline(xintercept=0, linetype="dashed", col="grey") + 
    geom_hline(yintercept=0, linetype="dashed", col="grey") + 
    geom_segment(data = current_pca_loadings, 
                 aes(x = 0, y = 0, xend = (PC1*2), yend = (PC2*2)), 
                 arrow = arrow(length = unit(1/2, "picas")), color="#1F77B4FF") + 
    geom_text_repel(data = current_pca_loadings, 
                    aes(x = PC1*2, y = PC2*2, label = column), color="#1F77B4FF") + 
    geom_segment(data = mean_future_pca_loadings %>% 
                   filter(year == 2080, rcp == x), 
                 aes(x = 0, y = 0, xend = (PC1*2), yend = (PC2*2)), 
                 arrow = arrow(length = unit(1/2, "picas")), color="#D62728FF") + 
    geom_text_repel(data = mean_future_pca_loadings  %>% 
                      filter(year == 2080, rcp == x), 
                    aes(x = PC1*2, y = PC2*2, label = column), color="#D62728FF") + 
    scale_x_continuous(expand=expansion(mult=c(0.025,0.025))) + 
    scale_y_continuous(expand=expansion(mult=c(0.025,0.025))) + 
    theme_bw() + labs(x=paste0("PC1 (", round(mean_future_pca_eigenvalues[mean_future_pca_eigenvalues$year == 2080 & 
                                                                            mean_future_pca_eigenvalues$rcp == x,]$PC1*100,2), "%)"), 
                      y=paste0("PC2 (", round(mean_future_pca_eigenvalues[mean_future_pca_eigenvalues$year == 2080 & 
                                                                            mean_future_pca_eigenvalues$rcp == x,]$PC2*100,2), "%)")) +
    theme(strip.background = element_blank(), 
          strip.text = element_text(size=12, face="bold"))
})
p <- ((p1[[1]] + ggtitle("2050") + 
  theme(plot.title = element_text(face="bold", hjust=0.5))) | 
  (p2[[1]] + ggtitle("2080") + 
     theme(plot.title = element_text(face="bold", hjust=0.5)))) /
  (p1[[2]] | p2[[2]]) / (p1[[3]] | p2[[3]])
p <- p + plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag.position = c(0.1, 1))
ggsave("figures/FigureS6.png", p, width=6, height=9, dpi=300, bg="transparent")

##################################################

rm(list=ls()); gc()

## Load climatic distance
climatic_distance <- readRDS("data/climatic_distance.rds") %>% ungroup()
climatic_distance$dist <- as.numeric(climatic_distance$dist)
climatic_distance$rcp <- factor(climatic_distance$rcp, levels=c("rcp26", "rcp60", "rcp85"),
                                labels=c("RCP2.6", "RCP6.0", "RCP8.5"))

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

# Project outline
outline_moll <- sf::st_transform(outline, 
                                 crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Calculate mean climatic distance
mean_dist <- climatic_distance %>% group_by(x,y,year,rcp) %>% 
  summarise(dist = mean(dist, na.rm=T)) %>% unite("year_rcp", year:rcp) %>%
  pivot_wider(names_from="year_rcp", values_from="dist")

# Project data to Mollweide
mean_dist <- mean_dist %>% raster::rasterFromXYZ()
raster::projection(mean_dist) <- "+proj=longlat + datum=WGS84"

# Create empty grid
r_grid <- raster::raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90, 
                         crs="+proj=longlat + datum=WGS84", vals=1)
r_grid <- raster::crop(r_grid, mean_dist)
r_grid <- raster::mask(r_grid, outline)
mean_dist <- raster::extend(mean_dist, r_grid)
mean_dist <- raster::stack(mean_dist, r_grid)
mean_dist <- raster::projectRaster(mean_dist, method="ngb",
                              crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
mean_dist <- data.frame(raster::rasterToPoints(mean_dist)) %>% select(-layer) %>%
  pivot_longer(names_to="year_rcp", values_to="dist", -c(x,y))
mean_dist$year_rcp <- factor(mean_dist$year_rcp, 
                        levels=c("X2050_RCP2.6", "X2050_RCP6.0", "X2050_RCP8.5",
                                 "X2080_RCP2.6", "X2080_RCP6.0", "X2080_RCP8.5"), 
                        labels=c("2050 RCP2.6", "2050 RCP6.0", "2050 RCP8.5",
                                 "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5"))
mean_dist <- mean_dist %>% separate(year_rcp, c("year", "rcp"), sep=" ")
summary(mean_dist)

## Plot map of climatic distance
ggplot() + geom_tile(data=mean_dist, aes(x=x, y=y, fill=dist)) + facet_grid(year ~ rcp) + 
  geom_sf(data=outline, fill="transparent", colour="black") + 
  scico::scale_fill_scico(name="Distance", palette="roma") + 
  coord_sf(expand=F, xlim=c(min(mean_dist$x), max(mean_dist$x)), 
           ylim=c(min(mean_dist$y),max(mean_dist$y))) + 
  labs(x="", y="") + theme_minimal() + 
  theme(strip.background = element_blank(), axis.text = element_blank())

# Quantile mapping
quantiles <- (0:8)/8 # how many quantiles we want to map 
colours <- rev(scico(8, palette = 'roma')) # 8 evenly interpolated colors 
quantile.vals <- quantile(mean_dist$dist, quantiles, names=F, na.rm=T) # the values for each quantile
quantile.vals <- c(floor(quantile.vals[1]*1000)/1000, ceiling(quantile.vals[2:9]*1000)/1000) 
col_val <- (quantile.vals - min(mean_dist$dist)) / diff(range(mean_dist$dist))  
(lim_map <- c(min(mean_dist$dist, na.rm=T), max(mean_dist$dist, na.rm=T)))
p <- ggplot() + geom_tile(data=mean_dist, 
                     aes(x=x, y=y, fill=cut(dist, breaks=quantile.vals, include.lowest=T))) + 
  facet_grid(rcp~year, switch="y") + tag_facets(position = list(x = 0.025, y = 0.95)) + 
  geom_sf(data=outline_moll, fill=NA, colour="black", size = 1/.pt) + 
  scale_fill_manual(name="Climatic distance", values=colours, na.translate=FALSE) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), ylim=c(-6431255, 9623945)) + theme_classic() + 
  theme(axis.title = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_blank(), legend.position="bottom")
ggsave("figures/FigureS7.png", p, width=9, height=7.71, dpi=500)
