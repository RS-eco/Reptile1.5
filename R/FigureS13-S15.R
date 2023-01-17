#' ---
#' title: "Richness changes among different reptile groups and realms"
#' author: "RS-eco"
#' ---

#' ### Load packages ###

# Clean environment
rm(list=ls()); invisible(gc())

# Load packages
library(tidyr); library(dplyr); library(magrittr)
library(ggplot2); library(patchwork)
library(forcats); library(sf)

# Load realm data
#remotes::install_github("RS-eco/geodat")
data(zoorealms, package="geodat")

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
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>% filter(rcp %in% c("EWEMBI", "RCP6.0")) %>%   
  mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("lizard", "lizard", "snake", "turtle", "lizard"))) %>%
  group_by(x,y,Group,gcm,model_type,rcp,year) %>% summarise(value=sum(value,na.rm=T)) %>%
  ungroup() %>% group_by(x,y,Group,year) %>% 
  summarise(value=mean(value, na.rm=T)); rm(sr_groups); invisible(gc())

#' ###  Split data by realms & groups ###
r_dat <- r_sr %>% spread(year, value) %>% group_by(Group) %>% group_split()
keys <- r_sr %>% group_by(Group) %>% group_keys() %>% unlist()
r_dat <- lapply(r_dat, function(x){
  dat <- x %>% as.data.frame() %>% dplyr::select(-Group) %>% raster::rasterFromXYZ()
  raster::projection(dat) <- "+proj=longlat + datum=WGS84"
  dat <- raster::projectRaster(dat, method="ngb",
                                crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  return(dat)
})

zoorealms <- sf::st_transform(zoorealms, "+proj=moll")
r_dat <- lapply(1:length(r_dat), function(x){
  df <- raster::stack(r_dat[[x]], raster::rasterize(zoorealms, r_dat[[x]][[1]])) %>% 
    raster::rasterToPoints() %>% as.data.frame()
  df$group <- keys[[x]]
  return(df)
})
r_dat <- r_dat %>% bind_rows()
r_dat <- r_dat[which(unlist(apply(r_dat[,-c(1,2,6,7)], MARGIN=1, FUN=function(x)!all(is.na(x))))),]; gc()
r_dat <- r_dat %>% as_tibble() %>% rename(realm = layer) %>% 
  mutate_at(vars(-c(x,y,realm,group)), replace_na, 0) %>% 
  mutate(realm = factor(realm, levels=c(1:11), labels=zoorealms$Realm)); rm(zoorealms)

r_dat2 <- r_dat %>% select(-realm); gc()
r_dat2$realm <- "Global"
summary(r_dat2)

r_dat <- bind_rows(r_dat, r_dat2) %>% tidyr::drop_na(realm) %>% 
  gather(year, value, -c(x, y, realm, group)) %>% mutate(year = as.numeric(sub("X", "", year))); gc()
r_dat$realm <- factor(r_dat$realm)
r_dat$realm <- factor(r_dat$realm, levels = levels(r_dat$realm)[c(3,1,2,4:12)], 
                      labels = paste0(letters[1:12], ") \t \t", levels(r_dat$realm)[c(3,1,2,4:12)]))
r_dat <- r_dat %>% arrange(group, realm, year)

mean_dat <- r_dat %>% filter(group == "lizard") %>% 
  mutate(year = as.factor(year)) %>% group_by(year,realm) %>% 
  summarise(mean_sr = round(mean(value),2),
            n_rec = n())
head(mean_dat, 15)

test <- ggpubr::compare_means(value ~ year, r_dat %>% filter(group == "lizard"), 
                              group.by = c("realm"), paired=T,
                              method="t.test", p.adjust.method = "holm") 
test$y <- rep(r_dat %>% filter(group == "lizard") %>% group_by(realm) %>% 
                summarise(ymax=max(value)) %>% ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.015,0.075,-0.03)*test$y
test$y2 <- test$y + (0.025*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 12)
test$xend <- rep(c(1.25, 2.25, 3.25), 12)
test$xmid <- rep(c(1.5, 2, 2.5), 12)
test <- test %>% group_by(realm) %>% group_split()
test[[1]]

p1 <- lapply(1:length(unique(mean_dat$realm)), function(x){
  rea <- unique(r_dat$realm)[x]
  p_sub <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(realm == rea, group == "lizard"),
                                       x = year, y = value, results.subtitle = F, bf.message = F,
                                       centrality.plotting = F, pairwise.comparisons = F,
                                       ggplot.component =list(scale_y_continuous(limits=c(0,NA), 
                                                                                 expand=expansion(
                                                                                   mult = c(0,0.035))))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr), data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]), 
                        inherit.aes = FALSE, size=3, col="red4") + labs(title=rea) + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    theme_bw() + theme(legend.position = "none")
  if(((x+2)/3) %% 1 == 0){
    p_sub <- p_sub + labs(y="Lizard Richness", caption=NULL) + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_text(size=14, face="bold"))
  } else{
    p_sub <- p_sub + labs(caption=NULL) + 
      theme(axis.title = element_blank(), axis.text.x = element_blank())
  }
  if(x >= 10){
    p_sub <- p_sub + theme(axis.title.x=element_text(size=14, face="bold"),
                           axis.text.x =element_text(size=12)) + 
      xlab("Year")
  }
  return(p_sub)
})
p1 <- ggstatsplot::combine_plots(p1, plotgrid.args=list(ncol=3))
ggsave("figures/FigureS16.png", p1, dpi=300, width=9, height=11.6, bg="transparent")

mean_dat <- r_dat %>% filter(group == "snake") %>% 
  mutate(year = as.factor(year)) %>% group_by(year,realm) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, r_dat %>% filter(group == "snake"), 
                              group.by = c("realm"), paired=T,
                              method="t.test", p.adjust.method = "holm") 
test$y <- rep(r_dat %>% filter(group == "snake") %>% group_by(realm) %>% 
                summarise(ymax=max(value)) %>% ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.015,0.075,-0.03)*test$y
test$y2 <- test$y + (0.025*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 12)
test$xend <- rep(c(1.25, 2.25, 3.25), 12)
test$xmid <- rep(c(1.5, 2, 2.5), 12)
test <- test %>% group_by(realm) %>% group_split()
test[[1]]

p2 <- lapply(1:length(unique(mean_dat$realm)), function(x){
  rea <- unique(r_dat$realm)[x]
  p_sub <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(realm == rea, group == "snake"),
                                       x = year, y = value, results.subtitle = F, bf.message = F,
                                       centrality.plotting = F, pairwise.comparisons = F,
                                       ggplot.component =list(scale_y_continuous(limits=c(0,NA), 
                                                                                 expand=expansion(
                                                                                   mult = c(0,0.035))))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr), data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]), 
                        inherit.aes = FALSE, size=3, col="red4") + labs(title=rea) + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    theme_bw() + theme(legend.position = "none")
  if(((x+2)/3) %% 1 == 0){
    p_sub <- p_sub + labs(y="Snake Richness", caption=NULL) + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_text(size=14, face="bold"))
  } else{
    p_sub <- p_sub + labs(caption=NULL) + 
      theme(axis.title = element_blank(), axis.text.x = element_blank())
  }
  if(x >= 10){
    p_sub <- p_sub + theme(axis.title.x=element_text(size=14, face="bold"),
                           axis.text.x =element_text(size=12)) + 
      xlab("Year")
  }
  return(p_sub)
})
p2 <- ggstatsplot::combine_plots(p2, plotgrid.args=list(ncol=3))
ggsave("figures/FigureS17.png", p2, dpi=300, width=9, height=11.6, bg="transparent")

mean_dat <- r_dat %>% filter(group == "turtle") %>% 
  mutate(year = as.factor(year)) %>% group_by(year,realm) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, r_dat %>% filter(group == "turtle"), 
                              group.by = c("realm"), paired=T,
                              method="t.test", p.adjust.method = "holm") 
test$y <- rep(r_dat %>% filter(group == "turtle") %>% group_by(realm) %>% 
                summarise(ymax=max(value)) %>% ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.015,0.075,-0.03)*test$y
test$y2 <- test$y + (0.025*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 12)
test$xend <- rep(c(1.25, 2.25, 3.25), 12)
test$xmid <- rep(c(1.5, 2, 2.5), 12)
test <- test %>% group_by(realm) %>% group_split()
test[[1]]

p3 <- lapply(1:length(unique(mean_dat$realm)), function(x){
  rea <- unique(r_dat$realm)[x]
  p_sub <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(realm == rea, group == "turtle"),
                                       x = year, y = value, results.subtitle = F, bf.message = F,
                                       centrality.plotting = F, pairwise.comparisons = F,
                                       ggplot.component =list(scale_y_continuous(limits=c(0,NA), 
                                                                                 expand=expansion(
                                                                                   mult = c(0,0.035))))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr), data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]), 
                                         inherit.aes = FALSE, size=3, col="red4") + 
    geom_segment(data=test[[x]], aes(x=group1, xend=group2, y=y, yend=y), inherit.aes=F) + 
    geom_text(data=test[[x]], aes(x=xmid, y=y2, label=p.signif), size=3, inherit.aes = FALSE) +
    ggplot2::geom_line(aes(x=year, y=mean_sr), group=1, size=0.5,
                       data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]), 
                       inherit.aes = FALSE, col="red3", alpha=0.5) + labs(title=rea) + 
    ggrepel::geom_label_repel(aes(x=year, y=mean_sr, label=mean_sr),
                              data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]),
                              min.segment.length = 0, size=3, inherit.aes = FALSE) + 
    theme_bw() + theme(legend.position = "none")
  if(((x+2)/3) %% 1 == 0){
    p_sub <- p_sub + labs(y="Turtle Richness", caption=NULL) + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_text(size=14, face="bold"))
  } else{
    p_sub <- p_sub + labs(caption=NULL) + 
      theme(axis.title = element_blank(), axis.text.x = element_blank())
  }
  if(x >= 10){
    p_sub <- p_sub + theme(axis.title.x=element_text(size=14, face="bold"),
                           axis.text.x =element_text(size=12)) + 
      xlab("Year")
  }
  return(p_sub)
})
p3 <- ggstatsplot::combine_plots(p3, plotgrid.args=list(ncol=3))
ggsave("figures/FigureS18.png", p3, dpi=300, width=9, height=11.6, bg="transparent")
