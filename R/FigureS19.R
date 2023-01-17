#' ---
#' title: "Richness changes among different realms"
#' author: "RS-eco"
#' ---

#' ### Load packages ###

# Clean environment
rm(list=ls()); invisible(gc())

# Load packages
library(tidyr); library(dplyr); library(ggplot2); library(patchwork); 
library(forcats); library(magrittr); library(exactextractr); 
library(data.table); library(scico)

# Load realm data
#remotes::install_github("RS-eco/geodat")
data(zoorealms, package="geodat")

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

#########################

#' ### Load modelled prob data

# Specify dispersal
disp <- "disp_eigth"

# Load data files
if(!file.exists(paste0("data/sumProb_groups_", disp, ".rds"))){
  sr1 <- readRDS(paste0("data/Reptile_prob_GAM_", disp, "_groups.rds"))
  sr1 %<>% gather(gcm_rcp_year, value, -c(x, y, Group)) %>% 
    separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
    separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
  sr1$model_type <- "GAM"
  
  sr2 <- readRDS(paste0("data/Reptile_prob_GBM_", disp ,"_groups.rds"))
  sr2 %<>% gather(gcm_rcp_year, value, -c(x, y, Group)) %>% 
    separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
    separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
  sr2$model_type <- "GBM"
  
  sr_groups <- bind_rows(sr1, sr2); rm(sr1, sr2); gc()
  saveRDS(sr_groups, file=paste0("data/sumProb_groups_", disp, ".rds"), compress="xz")
  rm(sr_groups); gc()
}

print(disp)
sr_groups <- readRDS(paste0("data/sumProb_groups_", disp, ".rds")); invisible(gc())

#' ###  Split data by realms ###

# Turn sr into raster
r_sr <- sr_groups %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>% 
  group_by(x,y,gcm,model_type,rcp,year) %>% summarise(value=sum(value,na.rm=T)) %>%
  ungroup() %>% group_by(x,y,rcp,year) %>% filter(rcp %in% c("EWEMBI", "RCP6.0")) %>% 
  summarise(value=mean(value, na.rm=T)) %>% unite("rcp_year", rcp:year)
rm(sr_groups); invisible(gc())

r_dat <- r_sr %>% spread(rcp_year, value) %>% raster::rasterFromXYZ()
raster::projection(r_dat) <- "+proj=longlat + datum=WGS84"

# Create empty grid
r_grid <- raster::raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90, 
                         crs="+proj=longlat + datum=WGS84", vals=1)
r_grid <- raster::crop(r_grid, r_dat)
r_grid <- raster::mask(r_grid, outline)

# Project data to Mollweide
r_dat <- raster::stack(r_dat, r_grid)
data <- raster::projectRaster(r_dat, method="ngb",
                              crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

zoorealms <- sf::st_transform(zoorealms, "+proj=moll")
r_dat <- raster::stack(data[[1:3]], raster::rasterize(zoorealms, data[[1]])) %>% 
  raster::rasterToPoints() %>% as.data.frame()
r_dat <- r_dat[which(unlist(apply(r_dat[,-c(1,2,6)], MARGIN=1, FUN=function(x)!all(is.na(x))))),]; gc()
r_dat <- r_dat %>% as_tibble() %>% rename(realm = layer) %>% 
  mutate(realm = factor(realm, labels=zoorealms$Realm)); rm(zoorealms)
summary(r_dat)
r_dat2 <- r_dat %>% select(-realm); gc()
r_dat2$realm <- "Global"
summary(r_dat2)

r_dat <- bind_rows(r_dat, r_dat2) %>% drop_na(realm) %>% gather(rcp_year, value, -c(x, y, realm)) %>% 
  separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")  %>% 
  filter(rcp %in% c("EWEMBI", "RCP6.0")) %>% mutate(year = as.numeric(year)); gc()
r_dat$realm <- factor(r_dat$realm)
r_dat$realm <- factor(r_dat$realm, levels = levels(r_dat$realm)[c(3,1,2,4:12)], 
                      labels = paste0(letters[1:12], ") \t \t", levels(r_dat$realm)[c(3,1,2,4:12)]))
r_dat <- r_dat %>% arrange(realm)

mean_dat <- r_dat %>% mutate(year = as.factor(year)) %>%
  group_by(year,realm) %>%
  summarise(mean_sr = round(mean(value),2))

test <- ggpubr::compare_means(value ~ year, r_dat, group.by = c("realm"),
                              method="t.test", p.adjust.method = "holm",
                              paired=T)
test$y <- rep(r_dat %>% group_by(realm) %>% summarise(ymax=max(value)) %>% 
                ungroup() %>% dplyr::select(ymax) %>% unlist(), each=3)
test$y <- test$y + c(0.015,0.075,-0.03)*test$y
test$y2 <- test$y + (0.025*test$y)
test$x <- rep(c(0.75, 1.75, 2.725), 12)
test$xend <- rep(c(1.25, 2.25, 3.25), 12)
test$xmid <- rep(c(1.5, 2, 2.5), 12)
test <- test %>% group_by(realm) %>% group_split()
test[[1]]

p2 <- lapply(1:length(unique(mean_dat$realm)), function(x){
  rea <- unique(r_dat$realm)[x]
  p_sub <- ggstatsplot::ggwithinstats(data=r_dat %>% filter(realm == rea), x = year, y = value, 
                                      results.subtitle = F, bf.message = F,
                                      centrality.plotting = F, pairwise.comparisons = F,
                                      ggplot.component = list(scale_y_continuous(limits=c(0,NA), 
                                                                                 expand=expansion(
                                                                                   mult = c(0,0.035))))) + 
    ggplot2::geom_point(aes(x=year, y=mean_sr),
                        data = mean_dat %>% filter(realm == unique(mean_dat$realm)[x]), 
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
    p_sub <- p_sub + labs(y="Summed Probability", caption=NULL) + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_text(size=12, face="bold"))
  } else{
    p_sub <- p_sub + labs(caption=NULL) + 
      theme(axis.title = element_blank(), axis.text.x = element_blank())
  }
  if(x >= 10){
    p_sub <- p_sub + theme(axis.title.x=element_text(size=14, face="bold"),
                           axis.text.x =element_text(size=12)) + xlab("Year")
  }
  return(p_sub)
})
p2 <- ggstatsplot::combine_plots(p2, plotgrid.args=list(ncol=3))
ggsave("figures/FigureS19.png", p2, dpi=300, width=9, height=11.6, bg="transparent")
