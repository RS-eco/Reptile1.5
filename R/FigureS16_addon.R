#' ---
#' title: "Number of species with changes in range position divided by Northern & Southern hemisphere"
#' author: "RS-eco"
#' ---

# Clear working environment
rm(list=ls()); invisible(gc())

# Load packages
library(dplyr); library(tidyr); library(sf); library(dtplyr);
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
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard"))) %>%
  mutate(group = factor(group, levels=c("Lizard", "Snake", "Turtle"))); gc()

# Change in range centroid
cur_dat <- dat %>% filter(year_rcp == "1995") %>% select(-gcm)
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

#' Separate data between northern and souther hemisphere 
#' according to range centroid of current range
all_dat$hemisphere <- NA
all_dat$hemisphere[all_dat$mean_lat.x > 0] <- "Northern"
all_dat$hemisphere[all_dat$mean_lat.x < 0] <- "Southern"

# Number of species for each scenario
all_dat %>% group_by(year_rcp.x) %>% summarise(n_sp = n_distinct(species))

# Display overall mean
all_dat %>% filter(year_rcp.x == "2080 RCP6.0") %>% 
  ungroup() %>% summarise(mean_dist = mean(dist), sd_dist = sd(dist),
                          se_dist = sd(dist)/sqrt(length(dist)),
                          mean_bear = mean(bear), median_bear = median(bear))

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
all_dat <- all_dat  %>% filter(year_rcp.x == "2080 RCP6.0") %>% 
  mutate(dir = factor(dir, levels=c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                                    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))) %>%
  ungroup()
length(unique(all_dat$species))

summary(all_dat$dist)

# Load realm data
#remotes::install_github("RS-eco/geodat")
data(zoorealms, package="geodat")

# Add zoorealm info to change data
zoorealms2 <- sf::st_make_valid(zoorealms)
all_dat <- sf::st_as_sf(all_dat, coords=c("mean_long.y", "mean_lat.y"), crs=4326)
all_zoo <- sf::st_within(all_dat, zoorealms2)
all_dat$realm <- as.numeric(all_zoo); rm(all_zoo)
all_dat <- all_dat %>% mutate(realm = factor(realm, levels=c(1:12, NA), 
                                             labels=c(levels(zoorealms2$Realm), NA)))
all_dat <- sf::st_transform(all_dat, "+proj=moll")
all_dat$mean_long.y <-sapply(as.character(all_dat$geometry), function(x) as.numeric(strsplit(substring(as.character(x), 3), split=", ")[[1]][1]))
all_dat$mean_lat.y <- sapply(as.character(all_dat$geometry), function(x) as.numeric(strsplit(sub(")", "", x), split=", ")[[1]][2]))
all_dat <- all_dat %>% dplyr::select(-geometry)

# Calculate ensemble mean
sub_dat <- all_dat %>% filter(year_rcp.x == "2080 RCP6.0") %>%
  group_by(realm, species, hemisphere) %>% 
  summarise(mean_long.y=mean(mean_long.y), mean_lat.y=mean(mean_lat.y))
# Wrap dateline
zoorealms <- st_wrap_dateline(zoorealms, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"))
# Re-project realm data
zoorealms <- sf::st_transform(zoorealms, "+proj=moll")

# Plot map of realm data
sub_dat <- sub_dat %>% drop_na(hemisphere)
p_map <- zoorealms %>% ggplot() + geom_sf(aes(fill=as.character(Realm))) + 
  geom_sf(data=zoorealms, fill=NA) + 
  coord_sf(datum=NA, expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + 
  geom_point(data=sub_dat, aes(x=mean_long.y, y=mean_lat.y, 
                               colour=hemisphere), 
             size=3/.pt, alpha=0.3) + 
  geom_text_npc(npcx=0.05, npcy=0.9, label="a)") + 
  geom_hline(yintercept=0, lty="dashed") + 
  scale_colour_manual(name="Hemisphere", values=c("black", "grey50")) + 
  scale_fill_manual(name="Realm", values=ggsci::pal_d3("category20")(12)[c(1:7,9:12)]) + 
  theme_classic() + guides(color="none") + 
  theme(axis.text = element_blank(), axis.line = element_blank(),
        plot.title = element_blank(), legend.position="right",
        axis.ticks = element_blank(), axis.title = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 10, l = 10, unit = "pt"))
#rm(sub_dat)
gc()

# Calculate ensemble mean
sub_dat2 <- all_dat %>% filter(year_rcp.x == "2080 RCP6.0") %>%
  mutate(realm = factor(realm, labels=zoorealms$Realm)) %>%
  mutate(realm = fct_explicit_na(realm, na_level = "No realm")) %>%
  #mutate(realm = fct_reorder()) %>% 
  group_by(realm, group, algorithm, gcm, hemisphere) %>% summarise(no_sp = n_distinct(species)) %>%
  ungroup() %>% group_by(realm, group, hemisphere) %>% summarise(no_sp = mean(no_sp)) %>% 
  drop_na(hemisphere)
# Hemisphere NAs are dropped from bar-chart
sub_dat3 <- all_dat %>% filter(year_rcp.x == "2080 RCP6.0") %>%
  mutate(realm = factor(realm, labels=zoorealms$Realm)) %>%
  mutate(realm = fct_explicit_na(realm, na_level = "No realm")) %>%
  #mutate(realm = fct_reorder()) %>% 
  group_by(realm, algorithm, gcm, hemisphere) %>% summarise(no_sp = n_distinct(species)) %>%
  ungroup() %>% group_by(realm, hemisphere) %>% summarise(no_sp = mean(no_sp)) %>% 
  drop_na(hemisphere)
# Hemisphere NAs are dropped from bar-chart

sub_dat2$hemisphere <- factor(sub_dat2$hemisphere, 
                              labels=c("Northern hemisphere", "Southern hemisphere"))
sub_dat3$hemisphere <- factor(sub_dat3$hemisphere, 
                              labels=c("Northern hemisphere", "Southern hemisphere"))
p_bar <- sub_dat2 %>% ungroup() %>% 
  ggplot() + geom_bar(aes(x=realm, y=no_sp, fill=group), 
                      position = position_stack(reverse = TRUE), stat="identity") + 
  geom_text(data=sub_dat3, aes(x=realm, y=no_sp, label=round(no_sp,0)), hjust=-0.15, size=2) + 
  facet_wrap(.~hemisphere, scales = "free_x") + coord_flip() + labs(tag="b)") + 
  # scales free_x drops unused factor levels, but only if coord_flip() is removed!
  scale_fill_grey(name="", start=0.75, end=0) + 
  scale_y_continuous(name="Number of species", limits=c(0,NA), 
                     breaks=c(0, 200, 400, 600, 800, 1000, 1200),
                     expand=expansion(add=c(0,80))) + 
  theme_bw() + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), 
                     axis.title.y = element_blank(), legend.position=c(0.88,0.88),
                     legend.key.size = unit(0.4, 'cm'), legend.key.height = unit(0.4, 'cm'),
                     legend.key.width = unit(0.4, 'cm'), legend.title = element_text(size=9),
                     legend.text = element_text(size=9), legend.background = element_blank(), 
                     plot.background=element_blank(), strip.background = element_blank(),
                     strip.text = element_text(size=10, face="bold"), plot.tag.position=c(0.1,0.95),
                     plot.margin = margin(t = 5, r = 10, b = 10, l = 5, unit = "pt"))
p_bar
rm(sub_dat2, sub_dat3); gc()

# Calculate ensemble mean
sum_dat <- all_dat %>% ungroup() %>% filter(year_rcp.x == "2080 RCP6.0") %>% 
  filter(realm %in% levels(zoorealms$Realm)[c(1,3,5,6)]) %>% 
  mutate(dist = cut(dist, breaks=c(0,25,50,100,250,500,20000), include.lowest=T)) %>%
  group_by(dist, dir, realm, algorithm, gcm, hemisphere) %>% 
  summarise(no_sp = n_distinct(species)) %>% ungroup() %>%
  group_by(dist, dir, realm, hemisphere) %>% summarise(no_sp = mean(no_sp))
sum_dat <- sum_dat %>% mutate(dist = factor(dist, levels=levels(sum_dat$dist),
                                            labels = c(levels(sum_dat$dist)[1:5], "> 500")))

dir_df <- data.frame(dir=factor(c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"),
                                levels=c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                                         "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")))

# p_realms_north
levels(zoorealms$Realm)
p_realms_north <- lapply(c(1,3,5,6), function(x){
  sub_dat <- sum_dat %>% filter(realm == levels(zoorealms$Realm)[x], 
                                hemisphere == "Northern") %>% right_join(dir_df)
  max_dat <- sub_dat %>% select(-geometry) %>% group_by(dir) %>% 
    summarise(total_sp = sum(no_sp)) %>% 
    summarise(max_sp=ceiling(max(total_sp, na.rm=T)/3)*3)
  breaks <- c(max_dat$max_sp/3,max_dat$max_sp*(2/3))
  axis1 <- data.frame(x = c(2,2,2), y = c(breaks,max_dat$max_sp), 
                      label = c(breaks,max_dat$max_sp))
  ggplot() + geom_col(data=sub_dat, aes(x=dir, y=no_sp, fill=dist), 
                      position = position_stack(reverse = TRUE)) + 
    geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
    ggtitle(zoorealms$Realm[x]) + 
    coord_polar(start=circular::rad(348.75)) + 
    scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
    scale_y_continuous(name="", expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
    scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma"), drop=F) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                            plot.background = element_rect(fill=NA, colour=NA),
                            panel.ontop=T, axis.title.x = element_blank(), 
                            axis.title.y = element_text(size=10, face="bold"),
                            panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                            linetype="dashed"))
})
p_realms_north[[3]]

# p_realms_south
p_realms_south <- lapply(c(1,3,5,6), function(x){
  sub_dat <- sum_dat %>% filter(realm == levels(zoorealms$Realm)[x], 
                                hemisphere == "Southern") %>% right_join(dir_df)
  max_dat <- sub_dat %>% select(-geometry) %>% group_by(dir) %>% 
    summarise(total_sp = sum(no_sp)) %>% 
    summarise(max_sp=ceiling(max(total_sp, na.rm=T)/3)*3)
  breaks <- c(max_dat$max_sp/3,max_dat$max_sp*(2/3))
  axis1 <- data.frame(x = c(2,2,2), y = c(breaks,max_dat$max_sp), 
                      label = c(breaks,max_dat$max_sp))
  ggplot() + geom_col(data=sub_dat, aes(x=dir, y=no_sp, fill=dist), 
                      position = position_stack(reverse = TRUE)) + 
    geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
    coord_polar(start=circular::rad(348.75)) + 
    scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
    scale_y_continuous(name="", expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
    scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma"), drop=F) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                            plot.background = element_rect(fill=NA, colour=NA),
                            panel.ontop=T, axis.title.x = element_blank(), 
                            axis.title.y = element_text(size=10, face="bold"), 
                            plot.title = element_blank(), 
                            panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                            linetype="dashed"))
})
p_realms_south[[1]]

zoorealms$Realm[c(1,3,5,6)]

p_map <- p_map + expand_limits(x=0, y=0)
p_realms_north[[1]] <- p_realms_north[[1]] + scale_y_continuous(name="North") + 
  labs(tag="c)") + theme(plot.tag.position=c(0.3,0.95))
p_realms_south[[1]] <- p_realms_south[[1]] + scale_y_continuous(name="South") + 
  labs(tag="d)") + theme(plot.tag.position=c(0.3,0.95))
p_realms <- c(p_realms_north,p_realms_south) 
p_realms <- wrap_plots(p_realms, ncol=4) + plot_layout(guides = "collect")
p <- p_map / p_bar / p_realms + plot_layout(heights=c(3,2,3)) + 
  theme(plot.background = element_blank())
ggsave("figures/FigureS16_addon.png", p, dpi=600, width=9, height=11, 
       bg="transparent")
