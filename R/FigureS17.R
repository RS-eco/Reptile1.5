#' ---
#' title: "Number of species with changes in range position"
#' author: "RS-eco"
#' ---

# Clear working environment
rm(list=ls()); invisible(gc())

# Load packages
library(dplyr); library(tidyr); library(sf)
library(ggplot2); library(patchwork); library(forcats)
library(ggpp); library(scico)

#########################

# range_sizes.rds was created by 08_RangeSummary_IndReptile.R

dat <- readRDS("data/range_sizes.rds")
dat$gcm_rcp <- gsub("[.]", "-", dat$gcm_rcp)
unique(dat$gcm_rcp)

# Divide gcm_rcp
dat <- separate(dat, gcm_rcp, c("gcm", "rcp"), sep="_", fill="right")
unique(dat$gcm)
unique(dat$rcp)
unique(dat$year)
dat <- unite(dat, "year_rcp", c(year, rcp), na.rm=T)
head(dat)
tail(dat)

dat <- dat %>% filter(disp == "disp_eigth") %>% 
  mutate(year_rcp = factor(year_rcp, levels=c(1995, "2050_rcp26", "2050_rcp60", "2050_rcp85",
                                              "2080_rcp26", "2080_rcp60", "2080_rcp85"), 
                           labels=c(1995, "2050 RCP2.6", "2050 RCP6.0", "2050 RCP8.5",
                                    "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5"))) %>% 
  mutate(group = factor(group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("lizard", "lizard", "snake", "turtle", "lizard"))) %>%
  mutate(group = factor(group, levels=c("lizard", "snake", "turtle", "total"))); gc()

# Change in range centroid
cur_dat <- dat %>% filter(year_rcp == 1995) %>% select(-gcm)
fut_dat <- dat %>% filter(year_rcp != 1995)
all_dat <- full_join(fut_dat, cur_dat, by=c("species", "group", "algorithm"))

# Check number of species
length(unique(all_dat$species))

# Identify species with no range for the future
mis_sp1 <- all_dat %>% filter(year_rcp.x == "2080 RCP6.0", is.na(mean_lat.x))
length(unique(mis_sp1$species))
# But only for some GCM & Algorithm combinations

all_dat <- all_dat %>% drop_na() %>% rowwise() %>%
  mutate(dist = round(sp::spDistsN1(matrix(c(mean_long.x, mean_lat.x), ncol=2), 
                                    matrix(c(mean_long.y, mean_lat.y), ncol=2), longlat=T), 2),
         bear = geosphere::bearing(c(mean_long.y, mean_lat.y),c(mean_long.x, mean_lat.x)),
         delta_long =  mean_long.x - mean_long.y, delta_lat = mean_lat.x - mean_lat.y)
length(unique(all_dat$species))
unique(all_dat$year_rcp.x)

## Summarize bearings into direction categories
all_dat$dir <- 1
all_dat$dir[all_dat$bear > -11.25 & all_dat$bear <= 11.25] <- "N"
all_dat$dir[all_dat$bear > 11.25 & all_dat$bear <= 33.75] <- "NNE"
all_dat$dir[all_dat$bear > 33.75 & all_dat$bear <= 56.25] <- "NE"
all_dat$dir[all_dat$bear > 56.25 & all_dat$bear <= 78.75] <- "ENE"
all_dat$dir[all_dat$bear > 78.75 & all_dat$bear <= 101.25] <- "E"
all_dat$dir[all_dat$bear > 101.25 & all_dat$bear <= 123.75] <- "ESE"
all_dat$dir[all_dat$bear > 123.75 & all_dat$bear <= 146.25] <- "SE"
all_dat$dir[all_dat$bear > 146.25 & all_dat$bear <= 168.75] <- "SSE"
all_dat$dir[all_dat$bear > 168.75 & all_dat$bear <= 180] <- "S"
all_dat$dir[all_dat$bear > -180 & all_dat$bear <= -168.75] <- "S"
all_dat$dir[all_dat$bear > -168.75 & all_dat$bear <= -146.25] <- "SSW"
all_dat$dir[all_dat$bear > -146.25 & all_dat$bear <= -123.75] <- "SW"
all_dat$dir[all_dat$bear > -123.75 & all_dat$bear <= -101.25] <- "WSW"
all_dat$dir[all_dat$bear > -101.25 & all_dat$bear <= -78.75] <- "W"
all_dat$dir[all_dat$bear > -78.75 & all_dat$bear <= -56.25] <- "WNW"
all_dat$dir[all_dat$bear > -56.25 & all_dat$bear <= -33.75] <- "NW"
all_dat$dir[all_dat$bear > -33.75 & all_dat$bear <= -11.25] <- "NNW"
table(all_dat$dir)
all_dat <- all_dat %>% filter(year_rcp.x == "2080 RCP6.0") %>% 
  mutate(dir = factor(dir, levels=c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                                    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))) %>%
  ungroup()
unique(all_dat$dir)

summary(all_dat$dist)

# Load realm data
#remotes::install_github("RS-eco/geodat")
data(zoorealms, package="geodat")

# Add zoorealm info to change data
zoorealms2 <- sf::st_make_valid(zoorealms)
all_dat <- sf::st_as_sf(all_dat, coords=c("mean_long.y", "mean_lat.y"), crs=4326)
all_zoo <- sf::st_within(all_dat, zoorealms2)
all_dat$realm <- as.numeric(all_zoo); rm(all_zoo)
all_dat <- all_dat %>% mutate(realm = factor(realm, levels=c(1:11, NA), 
                                             labels=c(levels(zoorealms2$Realm))))
all_dat <- sf::st_transform(all_dat, "+proj=moll")
all_dat$mean_long.y <-sapply(as.character(all_dat$geometry), function(x) as.numeric(strsplit(substring(as.character(x), 3), split=", ")[[1]][1]))
all_dat$mean_lat.y <- sapply(as.character(all_dat$geometry), function(x) as.numeric(strsplit(sub(")", "", x), split=", ")[[1]][2]))
all_dat <- all_dat %>% dplyr::select(-geometry)

# Create inset plots
sum_dat <- all_dat %>% ungroup() %>% 
  mutate(dist = cut(dist, breaks=c(0,25,50,100,250,500,20000), include.lowest=T)) %>%
  group_by(dist, dir, realm, algorithm, gcm) %>% 
  summarise(no_sp = n_distinct(species))
sum_dat <- sum_dat %>% mutate(dist = factor(dist, levels=levels(sum_dat$dist),
                                            labels = c(levels(sum_dat$dist)[1:5], "> 500")))

# Calculate ensemble mean
sum_dat <- sum_dat %>% group_by(dist, dir, realm) %>% 
  summarise(no_sp=mean(no_sp, na.rm=T))

p <- lapply(1:length(zoorealms$Realm), function(x){
  sub_dat <- sum_dat %>% filter(realm == levels(zoorealms$Realm)[x])
  max_dat <- sum_dat %>% as.data.frame() %>% filter(realm == levels(zoorealms$Realm)[x]) %>% 
    select(-geometry) %>% group_by(dir) %>% summarise(total_sp = sum(no_sp)) %>% 
    summarise(max_sp=ceiling(max(total_sp)/15)*15)
  breaks <- c(max_dat$max_sp/3,max_dat$max_sp*(2/3))
  axis1 <- data.frame(x = c(2,2,2), y = c(breaks,max_dat$max_sp), 
                      label = c(breaks,max_dat$max_sp))
  ggplot() + geom_col(data=sub_dat, aes(x=dir, y=no_sp, fill=dist), 
                      position = position_stack(reverse = TRUE)) + 
    geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
    ggtitle(paste0(letters[x], ") \t \t", zoorealms$Realm[x])) + 
    coord_polar(start=circular::rad(348.75)) + 
    scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
    scale_y_continuous(expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
    scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma"), drop=F) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                            plot.background = element_rect(fill=NA, colour=NA),
                            panel.ontop=T, axis.title = element_blank(),
                            panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                            linetype="dashed"))
})
leg <- ggpubr::as_ggplot(ggpubr::get_legend(p[[1]]))
p2 <- (wrap_plots(p) & theme(plot.title = element_text(hjust=0.12), legend.position="none")) +
  plot_spacer() + inset_element(leg, 0.5, 0.5, 0.6, 0.6, align_to="plot") 
ggsave("figures/FigureS17.png", p2, dpi=300, width=12, height=9.97)
