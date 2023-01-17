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
disp <- "disp_eigth"

# Load file created under Figure2.R
sr_groups <- readRDS(paste0("data/sr_groups_", disp, ".rds"))

#' ###  Split data by groups ###

# Turn sr into raster
r_sr <- sr_groups %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>% filter(year != 1995) %>%
  mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("lizard", "lizard", "snake", "turtle", "lizard"))) %>%
  group_by(x,y,Group,gcm,model_type,rcp,year) %>% summarise(value=sum(value,na.rm=T)) %>%
  ungroup() %>% group_by(x,y,Group,rcp,year) %>% 
  summarise(value=mean(value, na.rm=T)) %>% unite("rcp_year", rcp:year)
r_total <- sr_groups %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>% filter(year != 1995) %>%
  group_by(x,y,gcm,model_type,rcp,year) %>% summarise(value=sum(value,na.rm=T)) %>%
  ungroup() %>% group_by(x,y,rcp,year) %>% 
  summarise(value=mean(value, na.rm=T)) %>% unite("rcp_year", rcp:year) %>% mutate(Group = "total")
r_dat <- r_sr %>% bind_rows(r_total) %>%
  mutate(Group = factor(Group, levels=c("lizard", "snake", "turtle", "total"))) %>%
  separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop") %>% 
  mutate(year = as.numeric(year)) %>% ungroup() %>% 
  pivot_wider(names_from="year", values_from="value", values_fill=0); gc()
r_sr2 <- sr_groups %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>% filter(year == 1995) %>%
  mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("lizard", "lizard", "snake", "turtle", "lizard"))) %>%
  group_by(x,y,Group,gcm,model_type,rcp,year) %>% summarise(value=sum(value,na.rm=T)) %>%
  ungroup() %>% group_by(x,y,Group,rcp,year) %>% 
  summarise(value=mean(value, na.rm=T)) %>% unite("rcp_year", rcp:year)
r_total2 <- sr_groups %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>% filter(year == 1995) %>%
  group_by(x,y,gcm,model_type,rcp,year) %>% summarise(value=sum(value,na.rm=T)) %>%
  ungroup() %>% group_by(x,y,rcp,year) %>% 
  summarise(value=mean(value, na.rm=T)) %>% unite("rcp_year", rcp:year) %>% mutate(Group = "total")
r_dat2 <- r_sr2 %>% bind_rows(r_total2) %>% mutate(rcp = "RCP2.6")
r_dat3 <- r_sr2 %>% bind_rows(r_total2) %>% mutate(rcp = "RCP6.0")
r_dat4 <- r_sr2 %>% bind_rows(r_total2) %>% mutate(rcp = "RCP8.5"); rm(r_sr2, r_total2)
r_dat_1995 <- bind_rows(list(r_dat2, r_dat3, r_dat4)) %>% select(-rcp_year) %>%
  mutate(year = 1995) %>% ungroup() %>% 
  pivot_wider(names_from="year", values_from="value", values_fill=0); rm(r_dat2, r_dat3, r_dat4); gc()
r_dat <- left_join(r_dat_1995, r_dat) %>%
  mutate(Group = factor(Group, levels=c("lizard", "snake", "turtle", "total"))); rm(r_dat_1995); gc()

# Project data in Mollweide projection
keys <- r_dat %>% group_by(rcp, Group) %>% group_keys()
r_dat <- r_dat %>% group_by(rcp, Group) %>% group_split()
r_dat <- lapply(1:length(r_dat), function(x){
  dat <- r_dat[[x]] %>% as.data.frame() %>% 
    dplyr::select(-c(rcp,Group)) %>% raster::rasterFromXYZ()
  raster::projection(dat) <- "+proj=longlat + datum=WGS84"
  dat <- raster::projectRaster(dat, method="ngb",
                               crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  dat <- dat %>% raster::rasterToPoints() %>% as.data.frame()
  dat$rcp <- keys$rcp[x]
  dat$Group <- keys$Group[x]
  return(dat)
})
r_dat <- r_dat %>% data.table::rbindlist() %>% 
  pivot_longer(-c(x,y,Group,rcp), names_to="year", values_to="value") %>%
  arrange(Group, rcp, year)
r_dat$year <- sub("X", "", r_dat$year)

y_max <- r_dat %>% as.data.frame() %>% filter(Group == "lizard") %>% ungroup() %>% 
  dplyr::select(value) %>% unlist() %>% max()
mean_dat <- r_dat %>% filter(Group == "lizard") %>%
  mutate(year = as.factor(year)) %>% group_by(year,rcp) %>%
  summarise(mean_sr = round(mean(value),2),
            no_rec = n())
head(mean_dat,9)

test <- ggpubr::compare_means(value ~ year, 
                              r_dat %>% filter(Group == "lizard"), group.by = c("rcp"),
                              method="t.test", paired=T, p.adjust.method = "holm")
test$y <- rep(r_dat %>% filter(Group == "lizard") %>% group_by(rcp) %>% summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.016,0.08,-0.04)*test$y
test$y2 <- test$y + (c(0.024, 0.024, 0.04)*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25), 3)
test$xmid <- rep(c(1.5, 2, 2.5), 3)
test <- test %>% group_by(rcp) %>% group_split()
test[[1]]

p1 <- lapply(1:length(unique(mean_dat$rcp)), function(x){
  repcp <- unique(mean_dat$rcp)[x]
  p <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(rcp == repcp, Group == "lizard"),
                                   x = year, y = value, results.subtitle = F, bf.message = F,
                                   centrality.plotting = F, pairwise.comparisons = F,
                                   ggplot.component = list(scale_y_continuous(limits=c(0,y_max*1.2), 
                                                                              expand=c(0,0)))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr),
                        data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]), 
                        inherit.aes = FALSE, size=3, col="red4") + ggtitle(repcp) + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]),
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
    p + theme(axis.title.y=element_blank(), plot.tag.position = c(0.16,0.86)) + 
      labs(caption=NULL)
  }
})
p1 <- ggstatsplot::combine_plots(p1, plotgrid.args=list(nrow=1))

y_max <- r_dat %>% as.data.frame() %>% filter(Group == "snake") %>% ungroup() %>% 
  dplyr::select(value) %>% unlist() %>% max()

mean_dat <- r_dat %>% filter(Group == "snake") %>%
  mutate(year = as.factor(year)) %>% group_by(year,rcp) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, 
                              r_dat %>% filter(Group == "snake"), group.by = c("rcp"),
                              method="t.test", paired=T, p.adjust.method = "holm")
test$y <- rep(r_dat %>% filter(Group == "snake") %>% group_by(rcp) %>% summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.016,0.08,-0.04)*test$y
test$y2 <- test$y + (c(0.024, 0.024, 0.04)*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25), 3)
test$xmid <- rep(c(1.5, 2, 2.5), 3)
test <- test %>% group_by(rcp) %>% group_split()
test[[1]]

p2 <- lapply(1:length(unique(mean_dat$rcp)), function(x){
  repcp <- unique(mean_dat$rcp)[x]
  p <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(rcp == repcp, Group == "snake"),
                                   x = year, y = value, results.subtitle = F, bf.message = F,
                                   centrality.plotting = F, pairwise.comparisons = F,
                                   ggplot.component = list(scale_y_continuous(limits=c(0,y_max*1.14), 
                                                                              expand=c(0,0)))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr), data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]), 
                        inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]),
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
  mutate(year = as.factor(year)) %>% group_by(year,rcp) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, 
                              r_dat %>% filter(Group == "turtle"), group.by = c("rcp"),
                              method="t.test", paired=T, p.adjust.method = "holm")
test$y <- rep(r_dat %>% filter(Group == "turtle") %>% group_by(rcp) %>% summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.016, 0.08,-0.04)*test$y
test$y2 <- test$y + (c(0.024, 0.024, 0.04)*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25), 3)
test$xmid <- rep(c(1.5, 2, 2.5), 3)
test <- test %>% group_by(rcp) %>% group_split()
test[[1]]

p3 <- lapply(1:length(unique(mean_dat$rcp)), function(x){
  repcp <- unique(mean_dat$rcp)[x]
  p <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(rcp == repcp, Group=="turtle"),
                                        x = year, y = value, results.subtitle = F, bf.message = F,
                                        centrality.plotting = F, pairwise.comparisons = F,
                                        ggplot.component = list(scale_y_continuous(limits=c(0,y_max*1.14), 
                                                                                   expand=c(0,0)))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr), data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]), 
                                     inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    labs(tag=c("g)", "h)", "i)")[x]) + theme_bw() + 
    theme(plot.title=element_blank(), legend.position = "none", 
          axis.title.x = element_blank(), axis.text.x = element_blank())
  if(x == 1){
    p + labs(y="Turtle richness", caption=NULL) + 
      theme(axis.title.y = element_text(size=14, face="bold"),
            plot.tag.position = c(0.22,0.945))
  } else{
    p + theme(axis.title.y=element_blank(),
              plot.tag.position = c(0.16,0.945)) + labs(caption=NULL)
  }
})
p3 <- ggstatsplot::combine_plots(p3, plotgrid.args=list(nrow=1))

y_max <- r_dat %>% as.data.frame() %>% filter(Group == "total") %>% ungroup() %>% 
  dplyr::select(value) %>% unlist() %>% max()

mean_dat <- r_dat %>% filter(Group == "total") %>%
  mutate(year = as.factor(year)) %>% group_by(year,rcp) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, 
                              r_dat %>% filter(Group == "total"), group.by = c("rcp"),
                              method="t.test", paired=T, p.adjust.method = "holm")
test$y <- rep(r_dat %>% filter(Group == "total") %>% group_by(rcp) %>% summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.016,0.08,-0.04)*test$y
test$y2 <- test$y + (c(0.024, 0.024, 0.04)*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25), 3)
test$xmid <- rep(c(1.5, 2, 2.5), 3)
test <- test %>% group_by(rcp) %>% group_split()
test[[1]]

p4 <- lapply(1:length(unique(mean_dat$rcp)), function(x){
  repcp <- unique(mean_dat$rcp)[x]
  p <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(rcp == repcp, Group=="total"),
                                        x = year, y = value, results.subtitle = F, bf.message = F,
                                        centrality.plotting = F, pairwise.comparisons = F,
                                        ggplot.component = list(scale_y_continuous(limits=c(0,y_max*1.14), 
                                                                                   expand=c(0,0)))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr), data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]), 
                                     inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(rcp == unique(mean_dat$rcp)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    labs(tag=c("j)", "k)", "l)")[x]) + theme_bw() + 
    theme(plot.title=element_blank(), legend.position = "none")
  if(x == 1){
    p + labs(x="Year", y="Total richness", caption=NULL) + 
      theme(axis.title.y = element_text(size=14, face="bold"),
            axis.title.x=element_text(size=14, face="bold"),
            axis.text.x =element_text(size=12),
            plot.tag.position = c(0.22,0.945))
  } else{
    p + theme(axis.title.y=element_blank(),
              axis.title.x=element_text(size=14, face="bold"),
              axis.text.x =element_text(size=12),
              plot.tag.position = c(0.18,0.945)) + 
      labs(x="Year", caption=NULL)
  }
})
p4 <- ggstatsplot::combine_plots(p4, plotgrid.args=list(nrow=1))

p <- p1 / p2 / p3 / p4
ggsave("figures/FigureS32.png", p, dpi=300, width=9, height=11.6, bg="transparent")