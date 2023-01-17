#' ---
#' title: "Spatial patterns in current and future reptile summed probability"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

#' ### Load packages ###

library(dplyr); library(tidyr); library(ggplot2)
library(patchwork); library(magrittr); library(scales)
library(scico); library(ggsci)

#' ### Load general data ###

# Specify colour schemes
bluered <- rev(scico(n=255, palette="roma"))
whiteblue <- rev(scico(n=255, palette="davos"))
redwhite <- rev(scico(n=255, palette="lajolla"))
redwhiteblue <- rev(scico(n=255, palette="vik"))

# Obtain world map
outline <- rgeos::gPolygonize(rgeos::gNode(as(rworldmap::getMap(resolution = "high"), "SpatialLines")))
outline <- rgeos::gUnaryUnion(outline)
outline <- sf::st_as_sf(outline)

#########################

#' ###  Create richness maps with modelled species data ###

# Specify dispersal
disp <- "disp_eigth"
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
dat <- bind_rows(sr1, sr2) %>% 
  mutate(Group = factor(Group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard")))

#########################

# Plot richness for 1995

# Calculate ensemble mean of 1995 richness
tot_1995 <- dat %>% ungroup() %>% dplyr::select(x,y,algorithm,EWEMBI_1995) %>% 
  group_by(x,y,algorithm) %>% summarise_at(vars(-group_cols()), sum) %>%
  group_by(x,y) %>% summarise(mean = mean(EWEMBI_1995))

# Turn data to raster
r_dat <- tot_1995 %>% raster::rasterFromXYZ()
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
dat_1995 <- data.frame(raster::rasterToPoints(data))

# Project outline
outline_moll <- sf::st_transform(outline, 
                                 crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

####################

lim_map <- c(0, max(dat_1995$mean, na.rm=T))
col_val <- scales::rescale(seq(0, max(dat_1995$mean, na.rm=T), length=9))

dat_1995 %>% filter(mean == 0) %>% summarise(mn_y = mean(y))
dat_1995 %>% filter(mean > 251) %>% summarise(mn_y = mean(y))

p1 <- ggplot() + geom_tile(data=dat_1995, aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Summed probability", colours=bluered, 
                       values=col_val, limits=lim_map) + ggtitle("a)") + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + theme_classic() + labs(y="1995") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=12),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent"), 
        plot.title = element_text(vjust = - 5, hjust=0.03),
        legend.title = element_text(vjust=0.8),
        legend.background = element_rect(fill = "transparent"), 
        legend.key.width=unit(1.25, "cm"), legend.position="bottom",
        legend.box.background = element_rect(fill = "transparent", colour=NA),
        plot.margin=unit(c(0.2,0.2,0.4,0.4),"mm"))

#########################

# Plot histogram of richness per group

data <- dat %>% dplyr::select(x,y,algorithm,Group,EWEMBI_1995) %>% 
  group_by(x,y,Group,algorithm) %>% summarise_at(vars(-group_cols()), sum) %>%
  group_by(x,y,Group) %>% summarise(mean = mean(EWEMBI_1995)) %>%
  pivot_wider(names_from="Group", values_from="mean", values_fill=0)

# Turn data to raster
r_dat <- data %>% raster::rasterFromXYZ()
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
data <- data.frame(raster::rasterToPoints(data)) %>% select(-layer) %>% 
  pivot_longer(names_to="Group", values_to="mean", -c(x,y))
unique(data$Group)
dat_1995 <- dat_1995 %>% mutate(Group = "Total")
unique(dat_1995$Group)
data <- data %>% bind_rows(dat_1995) %>% 
  mutate(Group = factor(Group, levels=c("Lizard", "Snake", "Turtle", "Total")))

(mean_data <- data %>% group_by(Group) %>%
    dplyr::summarise(mean_sp = mean(mean, na.rm=T),
                     sd_sp = sd(mean, na.rm=T),
                     se = sd(mean, na.rm=T)/sqrt(length(mean))))
mean_data$x <- c(15,29,4.3,52)

p2 <- ggplot() + 
  geom_freqpoly(data=data, aes(x=mean, colour=Group, lty=Group), binwidth=1) + 
  geom_vline(data=mean_data, aes(xintercept=mean_sp, colour=Group, lty=Group)) + 
  scale_x_sqrt(limits=c(0,NA), expand=c(0.01,0), 
               breaks=c(0,4,16,36,64,100,144,196), 
               labels=c(0,4,16,36,64,100,144,196)) + 
  geom_text(data=mean_data, aes(x=x, y=350, label=round(mean_sp,1)), size=6/.pt,
            col=pal_d3("category10")(4)) + 
  scale_y_continuous(name="Number of cells", limits=c(0,15500), 
                     breaks=c(0,5000,10000,15000), expand=c(0,0)) + 
  scale_colour_manual(values=pal_d3("category10")(4)) + 
  ggtitle("b)") + theme_bw() + labs(x="Summed probability (sqrt-scale)") + 
  theme(legend.position = c(0.8,0.6), legend.background = element_rect(fill="transparent"),
        axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(vjust = - 7, hjust=0.03),
        plot.margin=unit(c(0.2,0.2,0.4,0.4),"mm"))
p2

#########################

# See 07_EnsInd_ProbThresh_Reptile_Groups.R file

# Calculate change in individual species
group_names <- c("croc", "lizard", "snake", "turtle", "worm lizard")
dat <- lapply(group_names, function(x){
  print(x)
  readRDS(paste0("data/Reptile_", x, "_prob_", disp, "_sp_locs.rds"))
})
dat <- data.table::rbindlist(dat, use.names=T); invisible(gc())

#########################

# Calculate mean total decrease in richness
dat_dec <- dat %>% select(c(x,y,species,algorithm,matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ . - `EWEMBI_1995`)) %>% 
  dplyr::select(-c(`EWEMBI_1995`)) %>% group_by(x,y,species,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm", values_to="change") %>%
  mutate(change = if_else(change > 0, 0, change)) %>% ungroup() %>% select(-species) %>% 
  group_by(x,y,algorithm,gcm) %>% summarise(decrease=sum(change)) %>%
  group_by(x,y) %>% summarise(mean=mean(decrease)); gc()
head(dat_dec)

# Project data to Mollweide
dat_dec <- dat_dec %>% raster::rasterFromXYZ()
raster::projection(dat_dec) <- "+proj=longlat + datum=WGS84"
dat_dec <- raster::extend(dat_dec, r_grid, value=0)
dat_dec <- raster::stack(dat_dec, r_grid)
dat_dec <- raster::projectRaster(dat_dec, method="ngb",
                                 crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dat_dec <- data.frame(raster::rasterToPoints(dat_dec)); gc()

lim_map <- c(min(dat_dec$mean, na.rm=T),0)
col_val <- scales::rescale(seq(min(dat_dec$mean, na.rm=T), 0, length=9))

ggplot() + geom_tile(data=dat_dec %>% filter(mean < -40), aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Species decrease", colours=redwhite, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945))

p3 <- ggplot() + geom_tile(data=dat_dec, aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Species decrease", colours=redwhite, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + 
  theme_classic() + labs(y="2080 RCP6.0") + ggtitle("c)") +  
  theme(axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=12),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent"), 
        legend.title = element_text(vjust=0.8), plot.title = element_text(vjust = - 5, hjust=0.03),
        legend.background = element_rect(fill = "transparent"), 
        legend.key.width=unit(1.25, "cm"), legend.position="bottom",
        legend.box.background = element_rect(fill = "transparent", colour=NA),
        plot.margin=unit(c(0.2,0.2,0.4,0.4),"mm"))
rm(dat_dec); gc()

#########################

# Plot increase in richness

# Calculate mean total increase in richness
dat_inc <- dat %>% select(c(x,y,species,algorithm,matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ . - `EWEMBI_1995`)) %>% 
  dplyr::select(-c(`EWEMBI_1995`)) %>% group_by(x,y,species,algorithm) %>% 
  pivot_longer(cols=!group_cols(), names_to="gcm", values_to="change") %>%
  mutate(change = if_else(change < 0, 0, change)) %>% ungroup() %>% select(-species) %>% 
  group_by(x,y,algorithm,gcm) %>% summarise(decrease=sum(change)) %>%
  group_by(x,y) %>% summarise(mean=mean(decrease)); gc()

# Project data to Mollweide
dat_inc <- dat_inc %>% raster::rasterFromXYZ()
raster::projection(dat_inc) <- "+proj=longlat + datum=WGS84"
dat_inc <- raster::extend(dat_inc, r_grid, value=0)
dat_inc <- raster::stack(dat_inc, r_grid)
dat_inc <- raster::projectRaster(dat_inc, method="ngb",
                                 crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dat_inc <- data.frame(raster::rasterToPoints(dat_inc))

lim_map <- c(0, max(dat_inc$mean, na.rm=T))
col_val <- scales::rescale(seq(0, max(dat_inc$mean, na.rm=T), length=9))

ggplot() + geom_tile(data=dat_inc %>% filter(mean > 20), aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Species increase", colours=whiteblue, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945))

p4 <- ggplot() + geom_tile(data=dat_inc, aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Species increase", colours=whiteblue, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + 
  theme_classic() + ggtitle("d)")+ 
  theme(axis.title = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent"), 
        legend.title = element_text(vjust=0.8), 
        plot.title = element_text(vjust = - 5, hjust=0.03),
        legend.background = element_rect(fill = "transparent"), 
        legend.key.width=unit(1.25, "cm"), legend.position="bottom",
        legend.box.background = element_rect(fill = "transparent", colour=NA),
        plot.margin=unit(c(0.2,0.2,0.4,0.4),"mm"))
rm(dat_inc); gc()

#########################

# Plot net change in richness

# Calculate change in richness across all groups
delta_sum <- dat %>% select(c(x,y,algorithm,matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  group_by(x,y,algorithm) %>% summarise_at(vars(-group_cols()), sum) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ . - `EWEMBI_1995`)) %>%
  dplyr::select(-EWEMBI_1995) %>% tidyr::gather(gcm_rcp_year, sum, -c(x,y,algorithm))

data <- delta_sum %>% group_by(x,y) %>% summarise(mean=mean(sum))
summary(data$mean)

# Project data to Mollweide
data <- data %>% raster::rasterFromXYZ()
raster::projection(data) <- "+proj=longlat + datum=WGS84"
data <- raster::extend(data, r_grid)
data <- raster::stack(data, r_grid)
data <- raster::projectRaster(data, method="ngb",
                              crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
data <- data.frame(raster::rasterToPoints(data))

(lim_map <- c(min(data$mean, na.rm=T), max(data$mean, na.rm=T)))
col_val <- scales::rescale(unique(c(seq(min(data$mean, na.rm=T), 0, length=5), 
                                    seq(0, max(data$mean, na.rm=T), length=5))))

ggplot() + geom_tile(data=data %>% filter(mean < -30), aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Net change to 1995", colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945))
ggplot() + geom_tile(data=data %>% filter(mean > 10), aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Net change to 1995", colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945))

p5 <- ggplot() + geom_tile(data=data, aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="Net change to 1995", colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + 
  theme_classic() + labs(y="2080 RCP6.0") + ggtitle("e)") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_text(face="bold", size=12),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent"), 
        legend.title = element_text(vjust=0.8), plot.title = element_text(vjust = - 5, hjust=0.03),
        legend.background = element_rect(fill = "transparent"), 
        legend.key.width=unit(1.25, "cm"), legend.position="bottom",
        legend.box.background = element_rect(fill = "transparent", colour=NA),
        plot.margin=unit(c(0.2,0.2,0.4,0.4),"mm"))

#########################

# Plot percentage change in richness

# Calculate change in richness across all groups
delta_sum <- dat %>% select(c(x,y,algorithm,matches("rcp60_2080"),`EWEMBI_1995`)) %>% 
  group_by(x,y,algorithm) %>% summarise_at(vars(-group_cols()), sum) %>% 
  mutate_at(vars(matches("rcp60_2080")), list(~ (. - EWEMBI_1995)/EWEMBI_1995*100)) %>%
  dplyr::select(-EWEMBI_1995) %>% tidyr::gather(gcm_rcp_year, sum, -c(x,y,algorithm))

data <- delta_sum %>% group_by(x,y) %>% summarise(mean=mean(sum))
summary(data$mean)

# Project data to Mollweide
data <- data %>% raster::rasterFromXYZ()
raster::projection(data) <- "+proj=longlat + datum=WGS84"
data <- raster::extend(data, r_grid)
data <- raster::stack(data, r_grid)
data <- raster::projectRaster(data, method="ngb",
                              crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
data <- data.frame(raster::rasterToPoints(data))

# Manually set data limit to -100 - 100
data$mean[data$mean == Inf] <- 100
data$mean[data$mean >= 100] <- 100
#data$mean[data$mean <= -100] <- -100
summary(data$mean)

(lim_map <- c(min(data$mean, na.rm=T), max(data$mean, na.rm=T)))
col_val <- scales::rescale(unique(c(seq(min(data$mean, na.rm=T), 0, length=5), 
                                    seq(0, max(data$mean, na.rm=T), length=5))))

p6 <- ggplot() + geom_tile(data=data, aes(x=x, y=y, fill=mean)) + 
  geom_sf(data=outline_moll, fill=NA, color = "black", size = 1/.pt) +
  scale_fill_gradientn(name="% change to 1995", colours=redwhiteblue, 
                       values=col_val, limits=lim_map,
                       breaks=c(-100, -50, 0, 50, 100), 
                       labels=c("-100", "-50", "0", 
                                "50", parse(text=paste("''",">= 100",sep="")))) + 
  coord_sf(expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + 
  theme_classic() + ggtitle("f)") + 
  theme(axis.title = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent"), 
        legend.title = element_text(vjust=0.8), plot.title = element_text(vjust = - 5, hjust=0.03),
        legend.background = element_rect(fill = "transparent"), 
        legend.key.width=unit(1.25, "cm"), legend.position="bottom",
        legend.box.background = element_rect(fill = "transparent", colour=NA),
        plot.margin=unit(c(0.2,0.2,0.4,0.4),"mm"))

p <- (p1 + plot_spacer() + inset_element(p2, 0,0.1,1,1, align_to = "full")) / 
  (p3 + p4) / (p5 + p6) + plot_layout(heights=c(1,1,1))
ggsave("figures/FigureS18.png", p, width=9, height=10, dpi=600, bg="transparent")
