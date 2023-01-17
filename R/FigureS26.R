#' ---
#' title: "Number of species with changes in range position per dispersal scenario"
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

dat <- dat %>% 
  mutate(year_rcp = factor(year_rcp, levels=c(1995, "2050_rcp26", "2050_rcp60", "2050_rcp85",
                                              "2080_rcp26", "2080_rcp60", "2080_rcp85"), 
                           labels=c(1995, "2050 RCP2.6", "2050 RCP6.0", "2050 RCP8.5",
                                    "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5"))) %>% 
  mutate(group = factor(group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("lizard", "lizard", "snake", "turtle", "lizard"))) %>%
  mutate(group = factor(group, levels=c("lizard", "snake", "turtle")))

# Change in range centroid
cur_dat <- dat %>% filter(year_rcp == 1995) %>% select(-gcm)
fut_dat <- dat %>% filter(year_rcp == "2080 RCP6.0")
all_dat <- full_join(fut_dat, cur_dat, by=c("species", "group", "disp", "algorithm")); rm(dat); gc()

all_dat <- all_dat %>% drop_na() %>% rowwise() %>%
  mutate(dist = round(sp::spDistsN1(matrix(c(mean_long.x, mean_lat.x), ncol=2), 
                                    matrix(c(mean_long.y, mean_lat.y), ncol=2), longlat=T), 2),
         bear = geosphere::bearing(c(mean_long.y, mean_lat.y),c(mean_long.x, mean_lat.x)),
         delta_long =  mean_long.x - mean_long.y, delta_lat = mean_lat.x - mean_lat.y)

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
all_dat <- all_dat %>% 
  mutate(dir = factor(dir, levels=c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                                    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))) %>% ungroup()
summary(all_dat$dist)

all_dat %>% 
  mutate(dir = factor(dir, levels=c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", 
                                    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"),
                      labels=c("N", "N", "N", "E", "E", "E", "E", "S", 
                               "S", "S", "S", "W", "W", "W", "W", "N"))) %>% ungroup() %>%
  group_by(disp, dir) %>% summarise(n())


all_dat %>% group_by(disp) %>% summarise(mn_dist=mean(dist), sd_dist= sd(dist), 
                                         se = sd(dist)/sqrt(length(dist)))

sum_dat <- all_dat %>% mutate(dist = cut(dist, breaks=c(0,25,50,100,250,500,20000), include.lowest=T)) %>%
  group_by(year_rcp.x, disp, dist, dir, algorithm, gcm) %>% 
  summarise(no_sp = n_distinct(species))
sum_dat <- sum_dat %>% mutate(dist = factor(dist, levels=levels(sum_dat$dist),
                                            labels = c(levels(sum_dat$dist)[1:5], "> 500")))

# Calculate ensemble mean
sum_dat <- sum_dat %>% group_by(disp, dist, dir) %>% summarise(no_sp=mean(no_sp, na.rm=T))

y_max <- sum_dat %>% group_by(disp, dir) %>% summarise(no_sp=sum(no_sp)) %>% 
  ungroup() %>% select(no_sp) %>% unlist() %>% max()
max_dat <- sum_dat %>% group_by(disp, dir) %>% 
  summarise(total_sp = sum(no_sp)) %>% ungroup() %>%
  summarise(max_sp=ceiling(max(total_sp)/15)*15)
breaks <- c(max_dat$max_sp/3,max_dat$max_sp*(2/3))
axis1 <- data.frame(x = c(2,2,2), y = c(breaks,max_dat$max_sp), 
                    label = c(breaks,max_dat$max_sp))
p2_1 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(disp=="disp_quarter"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + ggtitle("a) \t \t d/4") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))
p2_2 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(disp=="disp_eigth"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + ggtitle("b) \t \t d/8") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))
p2_3 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(disp=="disp_sixteenth"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + ggtitle("c) \t \t d/16") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))

p <- (p2_1 / p2_2 / p2_3) + plot_layout(guides="collect") & 
  theme(plot.title = element_text(hjust=0.12))
ggsave("figures/FigureS26.png", p, dpi=300, width=4, height=8.88)
