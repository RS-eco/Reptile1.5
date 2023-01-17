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

#' ###  Create maps with modelled species data ###

# Set dispersal
disp <- "dispersal2"

# Load data files
sumProb1 <- read.csv(paste0("data/Reptile_prob_GAM_", disp, ".csv.xz"))
sumProb2 <- read.csv(paste0("data/Reptile_prob_GBM_", disp ,".csv.xz"))

# Create time_rcp combination
time_rcp <- expand.grid(time=c(2050, 2080), rcp= c("rcp26", "rcp60", "rcp85")) %>% 
  tidyr::unite("time_rcp", c(rcp, time))
time_rcp <- as.vector(time_rcp$time_rcp)
time_rcp <- c("1995", time_rcp)
time_rcp_lab <- c("1995", "2050 RCP2.6", "2080 RCP2.6", "2050 RCP6.0", 
                  "2080 RCP6.0", "2050 RCP8.5", "2080 RCP8.5")

# Calculate ensemble mean for each time_rcp combination
mean_taxa <- lapply(time_rcp, function(x){
  sumData <- left_join(sumProb1, sumProb2, by=c("x","y")) %>% select(x,y,matches(x))
  sumData$sum <- apply(sumData[,-c(1,2)], 1, mean, na.rm=TRUE)
  sumData <- sumData %>% dplyr::select(c(x,y,sum))
  sumData$time_rcp <- x
  return(sumData)
}); rm(sumProb1, sumProb2); gc()
mean_taxa <- do.call("rbind", mean_taxa)
mean_taxa$time_rcp <- factor(mean_taxa$time_rcp, labels=time_rcp_lab)
mean_taxa$disp <- disp

delta_sum <- mean_taxa %>% tidyr::spread(time_rcp, sum) %>% 
  tidyr::drop_na() %>% mutate_at(time_rcp_lab[-1], funs((. - `1995`)/`1995`*100)) %>% 
  dplyr::select(-`1995`) %>% tidyr::gather(time_rcp, sum, -c(x,y,disp))

data(outline, package="ggmap2")
library(sf)
outline <- sf::st_as_sf(outline)

lim_map <- c(min(delta_sum$sum), max(delta_sum$sum))
lim_histx <- unlist(c(delta_x %>% ungroup() %>% summarise(min=min(mean, na.rm=T)),
                      delta_x %>% ungroup() %>% summarise(max=max(mean, na.rm=T))))
lim_histy <- unlist(c(delta_y %>% ungroup() %>% summarise(min=min(mean, na.rm=T)), 
                      delta_y %>% ungroup() %>% summarise(max=max(mean, na.rm=T))))
col_val <- scales::rescale(unique(c(seq(min(delta_sum$sum), 0, length=5), 
                                    seq(0, max(delta_sum$sum), length=5))))
p <-  ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank() + theme_classic() + 
  theme(legend.position="none", axis.title = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(), 
        axis.text = element_blank())
p2 <- lapply(c("RCP2.6", "RCP6.0", "RCP8.5"), function(rcp){
  ggplot() + geom_histogram(data=delta_x[delta_x$time_rcp == paste(year, rcp) & 
                                           delta_x$disp == disp,], 
                            aes(x=x, y=mean), width=1,
                            stat="identity", position="stack", colour=NA) + 
    scale_y_continuous(limits=lim_histx, position = "right", expand=c(0,0)) + 
    scale_x_continuous(expand=c(0,0), breaks=c(15,0,-15,-30)) + 
    scale_fill_grey() + theme_classic() + 
    theme(legend.position="none", axis.title = element_blank(),
          axis.line.x = element_blank(), axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(), axis.text.y = element_text(size=rel(0.8)))
})
p3 <- lapply(c("RCP2.6", "RCP6.0", "RCP8.5"), function(rcp){
  ggplot() + geom_histogram(data=delta_y[delta_y$time_rcp == paste(year, rcp) & 
                                           delta_y$disp == disp,], 
                            aes(x=y, y=mean), width=1,
                            stat="identity", position="stack", colour=NA) + 
    scale_y_reverse(limits=rev(lim_histy)) + 
    scale_fill_grey(name="", labels=c("Amphibians", "Birds", "Mammals")) + 
    coord_flip(expand=FALSE) + theme_classic() + 
    theme(legend.position="bottom", axis.title = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(), axis.text.x = element_text(size=rel(0.8))) + 
    guides(fill = guide_legend(direction = "vertical"))})
#outline <- sf::st_transform(outline, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
p4 <- lapply(c("RCP2.6", "RCP6.0", "RCP8.5"), function(rcp){
  # Transform data to Robinson projection
  data <- delta_sum[delta_sum$time_rcp == paste(year, rcp) & 
                      delta_sum$disp == disp,]
  #data <- data[,c("x","y", "sum")]
  #data <- raster::rasterFromXYZ(data)
  #raster::projection(data) <- "+proj=longlat + datum=WGS84"
  #data <- raster::projectRaster(data, crs="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  #data <- data.frame(raster::rasterToPoints(data))
  # Make map
  ggplot() +
    geom_tile(data=data, aes(x=x, y=y, fill=sum)) + 
    geom_sf(data=outline, fill="transparent", colour="black") + 
    scale_fill_gradientn(name="", colours=rev(colorRampPalette(
      c("#00007F", "blue", "#007FFF", "cyan", 
        "white", "yellow", "#FF7F00", "red", "#7F0000"))(255)),
      na.value="transparent", values=col_val, limits=lim_map) + 
    coord_sf(expand=F, xlim=c(min(data$x), max(data$x)), ylim=c(min(data$y),max(data$y)), 
             ndiscr=0) + theme_classic() + 
    theme(axis.title = element_blank(), axis.line = element_blank(),
          axis.ticks = element_blank(), axis.text = element_blank(),
          plot.background = element_rect(fill = "transparent"), 
          legend.background = element_rect(fill = "transparent"), 
          legend.key.width=unit(2.5, "cm"), legend.position="bottom",
          legend.box.background = element_rect(fill = "transparent", colour=NA))
})
pall <- {{p + ggtitle("a)") + theme(plot.title=element_text(size=20, vjust=-10)) + p2[[1]] + 
    p3[[1]] + p4[[1]] + plot_layout(ncol=2, widths=c(1,8), heights=c(1,4)) & 
    theme(legend.position="none")} - {p + ggtitle("b)") + 
        theme(plot.title=element_text(size=20, vjust=-10)) + p2[[2]] + 
        p3[[2]] + p4[[2]] + plot_layout(ncol=2, widths=c(1,8), heights=c(1,4)) & 
        theme(legend.position="none")} + {p + ggtitle("c)") + 
            theme(plot.title=element_text(size=20, vjust=-10)) + p2[[3]] + 
            p3[[3]] + p4[[3]] + plot_layout(ncol=2, widths=c(1,8), heights=c(1,4))} + 
    plot_layout(ncol=1)}
ggsave("figures/FigureS4.png", pall, width=7.5, height=12, unit="in", dpi=600, bg="transparent")
