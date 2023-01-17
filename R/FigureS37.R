#' ---
#' title: "Number of species with changes in range position per Year / RCP"
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
  mutate(group = factor(group, levels=c("lizard", "snake", "turtle", "total")))

# Change in range centroid
cur_dat <- dat %>% filter(year_rcp == 1995) %>% select(-gcm)
fut_dat <- dat %>% filter(year_rcp != "1995")
all_dat <- full_join(fut_dat, cur_dat, by=c("species", "algorithm"))

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
                                    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))) %>% 
  ungroup()

summary(all_dat$dist)

sum_dat <- all_dat %>% mutate(dist = cut(dist, breaks=c(0,25,50,100,250,500,20000), include.lowest=T)) %>%
  group_by(year_rcp.x, dist, dir, algorithm, gcm) %>% summarise(no_sp = n_distinct(species))
sum_dat <- sum_dat %>% mutate(dist = factor(dist, levels=levels(sum_dat$dist),
                                            labels = c(levels(sum_dat$dist)[1:5], "> 500")))

# Calculate ensemble mean
sum_dat <- sum_dat %>% group_by(year_rcp.x, dist, dir) %>% summarise(no_sp=mean(no_sp, na.rm=T))

y_max <- sum_dat %>% group_by(year_rcp.x, dir) %>% summarise(no_sp=sum(no_sp)) %>% 
  ungroup() %>% select(no_sp) %>% unlist() %>% max()
max_dat <- sum_dat %>% group_by(year_rcp.x, dir) %>% 
  summarise(total_sp = sum(no_sp)) %>% ungroup() %>%
  summarise(max_sp=ceiling(max(total_sp)/15)*15)
breaks <- c(max_dat$max_sp/3,max_dat$max_sp*(2/3))
axis1 <- data.frame(x = c(2,2,2), y = c(breaks,max_dat$max_sp), 
                    label = c(breaks,max_dat$max_sp))

p1_1 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(year_rcp.x == "2050 RCP2.6"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + ggtitle("2050") + labs(tag="a)") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(name="RCP2.6", expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title.x = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt, linetype="dashed"),
                          plot.tag.position = c(0.15,0.8),
                          axis.title.y = element_text(size=12, face="bold"),
                          plot.title = element_text(size=12, face="bold"))
p1_2 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(year_rcp.x == "2050 RCP6.0"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="c)") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(name="RCP6.0", expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          axis.title.y = element_text(size=12, face="bold"),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title.x = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt, linetype="dashed"),
                          plot.tag.position = c(0.15,0.875),
                          plot.title = element_text(size=13, face="bold"))
p1_3 <- ggplot() +
  geom_col(data=sum_dat %>% filter(year_rcp.x == "2050 RCP8.5"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="e)") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(name="RCP8.5", expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title.x = element_blank(),
                          axis.title.y = element_text(size=12, face="bold"),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt, linetype="dashed"),
                          plot.tag.position = c(0.15,0.875), plot.title = element_text(size=13, face="bold"))
p2_1 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(year_rcp.x == "2080 RCP2.6"), aes(x=dir, y=no_sp, fill=dist), 
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + ggtitle("2080") + labs(tag="b)") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(name="", expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA), panel.ontop=T, 
                          panel.grid.major = element_line(colour="gray60", size=1/.pt, linetype="dashed"),
                          plot.tag.position = c(0.0675,0.8), plot.title = element_text(size=12, face="bold"),
                          axis.title = element_blank())
p2_2 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(year_rcp.x == "2080 RCP6.0"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="d)") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(name="", expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt, linetype="dashed"),
                          plot.tag.position = c(0.0675,0.875),
                          plot.title = element_blank())
p2_3 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(year_rcp.x == "2080 RCP8.5"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="f)") + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_y_continuous(name="", expand=expansion(add=c(0,-max_dat$max_sp/10)), breaks = breaks) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt, linetype="dashed"),
                          plot.tag.position = c(0.0675,0.875), plot.title = element_blank())
p <- ((p1_1 + p2_1) / (p1_2 + p2_2) / (p1_3 + p2_3)) + plot_layout(guides = "collect") & 
  theme(plot.title = element_text(hjust=0.5))
ggsave("figures/FigureS33.png", p, dpi=300, width=7.8, height=9, bg="transparent")
