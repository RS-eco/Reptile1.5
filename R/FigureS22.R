#' ---
#' title: "Richness changes among different RCPs and realms"
#' author: "RS-eco"
#' ---

#' ### Load packages ###

# Clean environment
rm(list=ls()); invisible(gc())

# Load packages
library(tidyr); library(dplyr); library(ggplot2); library(patchwork); 
library(forcats); library(magrittr); library(exactextractr); 
library(data.table); library(scico)

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

#########################

#' ### Load modelled richness data

# Specify dispersal
dispersal <- c("disp_quarter", "disp_eigth", "disp_sixteenth")

# Create data files
lapply(dispersal, function(disp){
  if(!file.exists(paste0("data/sr_groups_", disp, ".rds"))){
    sr1 <- readRDS(paste0("data/Reptile_thresh_GAM_", disp, "_groups.rds"))
    sr1 %<>% gather(gcm_rcp_year, value, -c(x, y, Group)) %>% 
      separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
      separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
    sr1$model_type <- "GAM"
    
    sr2 <- readRDS(paste0("data/Reptile_thresh_GBM_", disp ,"_groups.rds"))
    sr2 %<>% gather(gcm_rcp_year, value, -c(x, y, Group)) %>% 
      separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
      separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
    sr2$model_type <- "GBM"
    
    sr_groups <- bind_rows(sr1, sr2); rm(sr1, sr2); gc()
    saveRDS(sr_groups, file=paste0("data/sr_groups_", disp, ".rds"), compress="xz")
    rm(sr_groups); gc()
  }  
})

# Load & summarise created files
sr_groups <- lapply(dispersal, function(disp){
  dat <- readRDS(paste0("data/sr_groups_", disp, ".rds")) %>% 
    mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
    mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>% filter(rcp %in% c("EWEMBI", "RCP6.0")) %>% 
    mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                          labels=c("lizard", "lizard", "snake", "turtle", "lizard"))) %>%
    group_by(x,y,Group,gcm,model_type,rcp,year) %>% summarise(value=sum(value,na.rm=T))
  dat$disp <- disp
  return(dat)
})
sr_groups <- data.table::rbindlist(sr_groups); invisible(gc())

# Summarise data by groups
r_groups <- sr_groups %>% ungroup() %>% group_by(x,y,disp,Group,year) %>% 
  summarise(value=mean(value, na.rm=T)); gc()
r_total <- sr_groups %>% ungroup %>% group_by(x,y,disp,gcm,model_type,year) %>% 
  summarise(value=sum(value,na.rm=T)) %>% ungroup() %>% group_by(x,y,disp,year) %>% 
  summarise(value=mean(value, na.rm=T)) %>% mutate(Group = "total"); rm(sr_groups); gc()
r_dat <- r_groups %>% bind_rows(r_total) %>%
  mutate(Group = factor(Group, levels=c("lizard", "snake", "turtle", "total"))) %>%
  mutate(disp = factor(disp, levels=c("disp_quarter", "disp_eigth", "disp_sixteenth"),
                       labels=c("d/4", "d/8", "d/16"))) %>% 
  mutate(year = as.numeric(year)) %>% ungroup() %>% 
  pivot_wider(names_from="year", values_from="value", values_fill=0); gc()
rm(r_groups, r_total); gc()
head(r_dat)

# Project data in Mollweide projection
keys <- r_dat %>% group_by(disp, Group) %>% group_keys()
r_dat <- r_dat %>% group_by(disp, Group) %>% group_split()
r_dat <- lapply(1:length(r_dat), function(x){
  dat <- r_dat[[x]] %>% as.data.frame() %>% 
    dplyr::select(-c(disp,Group)) %>% raster::rasterFromXYZ()
  raster::projection(dat) <- "+proj=longlat + datum=WGS84"
  dat <- raster::projectRaster(dat, method="ngb",
                               crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  dat <- dat %>% raster::rasterToPoints() %>% as.data.frame()
  dat$disp <- keys$disp[x]
  dat$Group <- keys$Group[x]
  return(dat)
})
r_dat <- data.table::rbindlist(r_dat) %>% 
  pivot_longer(-c(x,y,disp,Group), names_to="year", values_to="value") %>%
  arrange(Group, disp, year)
r_dat$year <- sub("X", "", r_dat$year)

y_max <- r_dat %>% as.data.frame() %>% filter(Group == "lizard") %>% ungroup() %>% 
  dplyr::select(value) %>% unlist() %>% max()

mean_dat <- r_dat %>% filter(Group == "lizard") %>%
  mutate(year = as.factor(year)) %>% group_by(year,disp) %>%
  summarise(mean_sr = round(mean(value),2),
            no_rec = n())

test <- ggpubr::compare_means(value ~ year, 
                              data=r_dat %>% filter(Group == "lizard"), group.by = c("disp"),
                              method="t.test", paired=T, p.adjust.method = "holm")
test$y <- rep(r_dat %>% filter(Group == "lizard") %>% group_by(disp) %>% 
                summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.017,0.08,-0.04)*test$y
test$y2 <- test$y + (0.025*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25), 3)
test$xmid <- rep(c(1.5, 2, 2.5), 3)
test <- test %>% group_by(disp) %>% group_split()
test[[1]]

p1 <- lapply(1:length(unique(mean_dat$disp)), function(x){
  dispers <- unique(mean_dat$disp)[x]
  p <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(disp == dispers, Group == "lizard"), 
                                   x = year, y = value, results.subtitle = F, bf.message = F,
                                   centrality.plotting = F, pairwise.comparisons = F,
                                   ggplot.component = list(scale_y_continuous(limits=c(0,y_max*1.14), 
                                                                              expand=c(0,0)))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr), 
                        data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]), 
                        inherit.aes = FALSE, size=3, col="red4") + ggtitle(dispers) + 
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    labs(tag=c("a)", "b)", "c)")[x]) + theme_bw() + 
    theme(plot.title=element_text(size=12, face="bold", hjust=0.5),
          legend.position = "none", axis.title.x = element_blank(), 
          axis.text.x = element_blank())
  if(x == 1){
    p + labs(y="Lizard richness", caption=NULL) + 
      theme(axis.title.y = element_text(size=14, face="bold"),
            plot.tag.position = c(0.22,0.86))
  } else{
    p + theme(axis.title.y=element_blank(),
              plot.tag.position = c(0.16,0.86)) + labs(caption=NULL)
  }
})
p1 <- ggstatsplot::combine_plots(p1, plotgrid.args=list(nrow=1))

y_max <- r_dat %>% as.data.frame() %>% filter(Group == "snake") %>% ungroup() %>% 
  dplyr::select(value) %>% unlist() %>% max()

mean_dat <- r_dat %>% filter(Group == "snake") %>%
  mutate(year = as.factor(year)) %>% group_by(year,disp) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, 
                              r_dat %>% filter(Group == "snake"), group.by = c("disp"),
                              method="t.test", paired=T, p.adjust.method = "holm")
test$y <- rep(r_dat %>% filter(Group == "snake") %>% group_by(disp) %>% summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.018,0.08,-0.04)*test$y
test$y2 <- test$y + (0.024*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25), 3)
test$xmid <- rep(c(1.5, 2, 2.5), 3)
test <- test %>% group_by(disp) %>% group_split()
test[[1]]

p2 <- lapply(1:length(unique(mean_dat$disp)), function(x){
  dispers <- unique(mean_dat$disp)[x]
  p <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(disp == dispers, Group == "snake"),
                                   x = year, y = value, grouping.var = disp,
                                   results.subtitle = F, bf.message = F,
                                   centrality.plotting = F, pairwise.comparisons = F,
                                   ggplot.component = list(scale_y_continuous(limits=c(0,y_max*1.14), 
                                                                              expand=c(0,0)))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr),
                        data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]), 
                        inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    labs(tag=c("d)", "e)", "f)")[x]) + theme_bw() + 
    theme(plot.title=element_blank(), legend.position = "none", 
          axis.title.x = element_blank(), axis.text.x = element_blank())
  if(x == 1){
    p + labs(y="Snake richness", caption=NULL) + 
      theme(axis.title.y = element_text(size=14, face="bold"),
            plot.tag.position = c(0.22,0.945))
  } else{
    p + theme(axis.title.y=element_blank(), plot.tag.position = c(0.16,0.945)) + 
      labs(caption=NULL)
  }
})
p2 <- ggstatsplot::combine_plots(p2, plotgrid.args=list(nrow=1))

y_max <- r_dat %>% as.data.frame() %>% filter(Group == "turtle") %>% ungroup() %>% 
  dplyr::select(value) %>% unlist() %>% max()

mean_dat <- r_dat %>% filter(Group == "turtle") %>%
  mutate(year = as.factor(year)) %>% group_by(year,disp) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, 
                              r_dat %>% filter(Group == "turtle"), group.by = c("disp"),
                              method="t.test", paired=T, p.adjust.method = "holm")
test$y <- rep(r_dat %>% filter(Group == "turtle") %>% group_by(disp) %>% summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.018,0.08,-0.04)*test$y
test$y2 <- test$y + (0.024*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25), 3)
test$xmid <- rep(c(1.5, 2, 2.5), 3)
test <- test %>% group_by(disp) %>% group_split()
test[[1]]

p3 <- lapply(1:length(unique(mean_dat$disp)), function(x){
  dispers <- unique(mean_dat$disp)[x]
  p <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(disp == dispers, Group == "turtle"),
                                   x = year, y = value, grouping.var = disp,
                                   results.subtitle = F, bf.message = F,
                                   centrality.plotting = F, pairwise.comparisons = F,
                                   ggplot.component = list(scale_y_continuous(limits=c(0,y_max*1.14), 
                                                                              expand=c(0,0)))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr),
                        data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]), 
                        inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    labs(tag=c("g)", "h)", "i)")[x]) + theme_bw() + 
    theme(plot.title=element_blank(), legend.position = "none", 
          axis.title.x = element_blank(), axis.text.x = element_blank())
  if(x == 1){
    p + labs(y="Turtle richness", caption=NULL) +
      theme(plot.tag.position = c(0.22,0.945),
            axis.title.y = element_text(size=12, face="bold"))
  } else{
    p + theme(axis.title.y=element_blank(),
              plot.tag.position = c(0.16,0.945)) + labs(caption=NULL)
  }
})
p3 <- ggstatsplot::combine_plots(p3, plotgrid.args=list(nrow=1))

y_max <- r_dat %>% filter(Group == "total") %>% ungroup() %>% 
  select(value) %>% unlist() %>% max()

mean_dat <- r_dat %>% filter(Group == "total") %>%
  mutate(year = as.factor(year)) %>% group_by(year,disp) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, 
                              r_dat %>% filter(Group == "total"), group.by = c("disp"),
                              method="t.test", paired=T, p.adjust.method = "holm")
test$y <- rep(r_dat %>% filter(Group == "total") %>% group_by(disp) %>% summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.018,0.08,-0.04)*test$y
test$y2 <- test$y + (0.024*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25), 3)
test$xmid <- rep(c(1.5, 2, 2.5), 3)
test <- test %>% group_by(disp) %>% group_split()
test[[1]]

p4 <- lapply(1:length(unique(mean_dat$disp)), function(x){
  dispers <- unique(mean_dat$disp)[x]
  p <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(disp == dispers, Group == "total"),
                                   x = year, y = value, grouping.var = disp,
                                   results.subtitle = F, bf.message = F,
                                   centrality.plotting = F, pairwise.comparisons = F,
                                   ggplot.component = list(scale_y_continuous(limits=c(0,y_max*1.14), 
                                                                              expand=c(0,0)))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr),
                        data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]), 
                        inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(disp == unique(mean_dat$disp)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    labs(tag=c("j)", "k)", "l)")[x]) + theme_bw() + 
    theme(plot.title=element_blank(), 
          legend.position = "none")
  if(x == 1){
    p + labs(x="Year", y="Total richness", caption=NULL) + 
      theme(axis.title.y = element_text(size=14, face="bold"),
            axis.title.x=element_text(size=14, face="bold"),
            plot.tag.position = c(0.22,0.945),
            axis.text.x =element_text(size=12))
  } else{
    p + theme(axis.title.y=element_blank(),
              axis.title.x=element_text(size=14, face="bold"),
              plot.tag.position = c(0.16,0.945),
              axis.text.x =element_text(size=12)) + 
      labs(x="Year", caption=NULL)
  }
})
p4 <- ggstatsplot::combine_plots(p4, plotgrid.args=list(nrow=1))

p <- p1 / p2 / p3 / p4
ggsave("figures/FigureS22.png", p, dpi=300, width=9, height=11.6, bg="transparent")
