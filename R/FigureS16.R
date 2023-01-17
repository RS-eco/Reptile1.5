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

sum_dat <- all_dat %>% mutate(dist = cut(dist, breaks=c(0,25,50,100,250,500,20000), 
                                         include.lowest=T)) %>%
  group_by(group, dist, dir, algorithm, gcm, hemisphere) %>% 
  summarise(no_sp = n_distinct(species))
sum_dat <- sum_dat %>% mutate(dist = factor(dist, levels=levels(sum_dat$dist),
                                            labels = c(levels(sum_dat$dist)[1:5], "> 500")))

all_dat %>% group_by(group) %>% 
  summarise(mn_dist = mean(dist), median_dist = median(dist))

# Calculate ensemble mean
sum_dat <- sum_dat %>% group_by(group, hemisphere, dist, dir) %>% 
  summarise(no_sp=mean(no_sp, na.rm=T))

sum_dat %>% group_by(group, dir, hemisphere) %>%
  summarise(total_sp=sum(no_sp)) %>%
  group_by(group) %>% summarise(max_sp=max(total_sp))
# Need those values for manually adjusting the axis to the maximum value!!!

tot_sp <- sum_dat %>% group_by(group, hemisphere) %>% summarise(all_sp=sum(no_sp))
sum_dist <- sum_dat %>% group_by(group, dist, hemisphere) %>%
  summarise(total_sp=sum(no_sp)) %>%
  left_join(tot_sp) %>% mutate(perc_sp = total_sp/all_sp*100)
sum_dist

axis1 <- data.frame(x = c(2,2,2), y = c(80, 150, 210), label = c(70, 140, NA))
# Add third label to check if negative value for expansion of y-axis is correct!

p1_1 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Lizard", hemisphere=="Northern"), 
           aes(x=dir, y=no_sp, fill=dist), 
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="a)") + ggtitle("Northern hemisphere") + 
  scale_y_continuous(name="Lizard", expand=expansion(add=c(0,-23), mult=c(0,0)), 
                     breaks = c(70,140)) + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.tag.position = c(0.15,0.8),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title.x = element_blank(),
                          axis.title.y = element_text(size=12, face="bold"),
                          plot.title = element_text(size=12, face="bold", hjust=0.5),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))
p1_2 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Lizard", hemisphere=="Southern"), 
           aes(x=dir, y=no_sp, fill=dist), 
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="b)") + ggtitle("Southern hemisphere") +  
  scale_y_continuous(expand=expansion(add=c(0,-23), mult=c(0,0)), breaks = c(70,140)) + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                          plot.tag.position = c(0.0675,0.8),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          plot.title = element_text(size=12, face="bold", hjust=0.5),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))

axis2 <- data.frame(x = c(2,2,2), y = c(52, 96, 132), label = c(44, 88, NA))
# Add third label to check if negative value for expansion of y-axis is correct!

p2_1 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Snake", hemisphere=="Northern"), 
           aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis2, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="c)") + 
  scale_y_continuous(name="Snake", expand=expansion(add=c(0,-14)), breaks = c(0, 44, 88)) +
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                          legend.position="none", plot.tag.position = c(0.15,0.875),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title.x = element_blank(),
                          axis.title.y = element_text(size=12, face="bold"),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))
p2_2 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Snake", hemisphere=="Southern"), 
           aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis2, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="d)") + 
  scale_y_continuous(expand=expansion(add=c(0,-18)), breaks = c(0, 44, 88)) +
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), plot.tag.position = c(0.0675,0.875),
                          axis.ticks.y = element_blank(), legend.position="none",
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))

axis3 <- data.frame(x = c(2,2,2), y = c(10, 18, 24), label = c(8, 16, NA))
# Add third label to check if negative value for expansion of y-axis is correct!

p3_1 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Turtle", hemisphere=="Northern"), 
           aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis3, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="e)") + 
  scale_y_continuous(name="Turtle", expand=expansion(add=c(0,-2.5)), breaks=c(8, 16)) +
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), plot.tag.position = c(0.15,0.875),
                          axis.ticks.y = element_blank(), legend.position="none",
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title.x = element_blank(),
                          axis.title.y = element_text(size=12, face="bold"),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))
p3_2 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Turtle", hemisphere=="Southern"), 
           aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis3, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + labs(tag="f)") + 
  scale_y_continuous(expand=expansion(add=c(0,-2.5)), breaks=c(8, 16)) +
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), plot.tag.position = c(0.0675,0.875),
                          axis.ticks.y = element_blank(), legend.position="none",
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))

p <- (p1_1 + p1_2) / (p2_1 + p2_2) / (p3_1 + p3_2) + plot_layout(guides="collect")
ggsave("figures/FigureS16.png", p, dpi=600, width=8, height=10, 
       bg="transparent")
rm(p1_1, p1_2, p2_1, p2_2, p3_1, p3_2, p, 
   axis1, axis2, axis3, cur_dat, dat, fut_dat, 
   mis_sp1, sum_dat, sum_dist, tot_sp); gc()