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
  group_by(group, dist, dir, algorithm, gcm) %>% summarise(no_sp = n_distinct(species))
sum_dat <- sum_dat %>% mutate(dist = factor(dist, levels=levels(sum_dat$dist),
                                            labels = c(levels(sum_dat$dist)[1:5], "> 500")))

all_dat %>% group_by(group) %>% summarise(mn_dist = mean(dist), median_dist = median(dist))

# Calculate ensemble mean
sum_dat <- sum_dat %>% group_by(group, dist, dir) %>% 
  summarise(no_sp=mean(no_sp, na.rm=T))

sum_dat %>% group_by(group, dir) %>%
  summarise(total_sp=sum(no_sp)) %>%
  group_by(group) %>% summarise(max_sp=max(total_sp))

tot_sp <- sum_dat %>% group_by(group) %>% summarise(all_sp=sum(no_sp))
sum_dist <- sum_dat %>% group_by(group, dist) %>%
  summarise(total_sp=sum(no_sp)) %>%
  left_join(tot_sp) %>% mutate(perc_sp = total_sp/all_sp*100)
sum_dist

axis1 <- data.frame(x = c(2,2,2), y = c(120, 220, 303), label = c(100, 200, NA))
p1_1 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Lizard"), aes(x=dir, y=no_sp, fill=dist), 
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis1, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + ggtitle("a) \t \t \t Lizard") + 
  scale_y_continuous(expand=expansion(add=c(0,-30.3), mult=c(0,0)), breaks = c(100,200)) + 
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), 
                          axis.ticks.y = element_blank(),
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))
p1_1

axis2 <- data.frame(x = c(2,2,2), y = c(75, 140, 180), label = c(60, 120, NA))
p1_2 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Snake"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis2, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + ggtitle("b) \t \t \t Snake") + 
  scale_y_continuous(expand=expansion(add=c(0,-18)), breaks = c(0, 60, 120)) +
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), 
                          axis.ticks.y = element_blank(), legend.position="none",
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))
p1_2

axis3 <- data.frame(x = c(2,2,2), y = c(11.5, 22, 30), label = c(10, 20, NA))
p1_3 <- ggplot() + 
  geom_col(data=sum_dat %>% filter(group=="Turtle"), aes(x=dir, y=no_sp, fill=dist),
           position = position_stack(reverse = TRUE)) + 
  geom_text(data=axis3, aes(x=x, y=y, label=label), size=9/.pt, colour="gray30") +
  coord_polar(start=circular::rad(348.75)) + ggtitle("c) \t \t \t Turtle") + 
  scale_y_continuous(expand=expansion(add=c(0,-3)), breaks=c(10, 20)) +
  scale_x_discrete(breaks=c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) + 
  scale_fill_manual(name="Distance (km)", values=scico(n=6, palette="roma")) + 
  theme_minimal() + theme(axis.text.y = element_blank(), 
                          axis.ticks.y = element_blank(), legend.position="none",
                          plot.background = element_rect(fill=NA, colour=NA),
                          panel.ontop=T, axis.title = element_blank(),
                          panel.grid.major = element_line(colour="gray60", size=1/.pt,
                                                          linetype="dashed"))
p1_3
p1 <- (p1_1 + p1_2 + p1_3) + plot_layout(guides="collect")

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
  group_by(realm, species) %>% summarise(mean_long.y=mean(mean_long.y),
                                         mean_lat.y=mean(mean_lat.y))
# Wrap dateline
zoorealms <- st_wrap_dateline(zoorealms, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"))
# Re-project realm data
zoorealms <- sf::st_transform(zoorealms, "+proj=moll")

# Plot map of realm data
p_map <- zoorealms %>% ggplot() + geom_sf(aes(fill=as.character(Realm))) + 
  geom_sf(data=zoorealms, fill=NA) + 
  coord_sf(datum=NA, expand=F, xlim=c(-14269066, 17829034), 
           ylim=c(-6431255, 9623945)) + 
  geom_point(data=sub_dat, aes(x=mean_long.y, y=mean_lat.y), size=2/.pt, alpha=0.3, colour="black") + 
  geom_text_npc(npcx=0.05, npcy=0.9, label="d)") + 
  scale_fill_manual(name="Realm", values=ggsci::pal_d3("category20")(12)[c(1:7,9:12)]) + 
  theme_classic() + theme(axis.text = element_blank(), axis.line = element_blank(),
                          plot.title = element_blank(),
                          axis.ticks = element_blank(), axis.title = element_blank(),
                          plot.margin = margin(t = 0, r = 0, b = 10, l = 10, unit = "pt"))

# Calculate ensemble mean
sub_dat2 <- all_dat %>% filter(year_rcp.x == "2080 RCP6.0") %>%
  mutate(realm = factor(realm, labels=zoorealms$Realm)) %>%
  mutate(realm = fct_explicit_na(realm, na_level = "No realm")) %>%
  #mutate(realm = fct_reorder()) %>% 
  group_by(realm, group, algorithm, gcm) %>% summarise(no_sp = n_distinct(species)) %>%
  ungroup() %>% group_by(realm, group) %>% summarise(no_sp = mean(no_sp))
sub_dat3 <- all_dat %>% filter(year_rcp.x == "2080 RCP6.0") %>%
  mutate(realm = factor(realm, labels=zoorealms$Realm)) %>%
  mutate(realm = fct_explicit_na(realm, na_level = "No realm")) %>%
  #mutate(realm = fct_reorder()) %>% 
  group_by(realm, algorithm, gcm) %>% summarise(no_sp = n_distinct(species)) %>%
  ungroup() %>% group_by(realm) %>% summarise(no_sp = mean(no_sp))

p_bar <- sub_dat2 %>% ungroup() %>% 
  ggplot() + geom_bar(aes(x=realm, y=no_sp, fill=group), 
                      position = position_stack(reverse = TRUE), stat="identity") + 
  geom_text(data=sub_dat3, aes(x=realm, y=no_sp, label=round(no_sp,0)),
            vjust=-0.15, size=2) + 
  scale_fill_grey(name="", start=0.75, end=0) +
  scale_y_continuous(name="Number of species", limits=c(0,NA), 
                     breaks=c(0, 200, 400, 600, 800, 1000, 1200),
                     expand=expansion(add=c(0,80))) + 
  theme_bw() + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), 
                     axis.title.x = element_blank(), legend.position=c(0.8,0.88),
                     legend.key.size = unit(0.4, 'cm'), legend.key.height = unit(0.4, 'cm'),
                     legend.key.width = unit(0.4, 'cm'), legend.title = element_text(size=9),
                     legend.text = element_text(size=9), legend.background = element_blank(), 
                     plot.background=element_blank(),
                     plot.margin = margin(t = 5, r = 10, b = 10, l = 5, unit = "pt"))

# Calculate ensemble mean
sum_dat <- all_dat %>% ungroup() %>% filter(year_rcp.x == "2080 RCP6.0") %>% 
  group_by(dist, dir, group, realm, algorithm, gcm) %>% 
  summarise(no_sp = n_distinct(species)) %>% ungroup() %>%
  group_by(dist, dir, group, realm) %>% summarise(no_sp = mean(no_sp))

# Create inset plots  
lab_positions <- tibble(x1=c(1,1,1,9,2,
                             15,9,1,2,9,1),
                        y1=c(1.2,1.7,1.4,1.5,4.8,
                             1.3,2,1.2,1.2,0.5,1.2),
                        realm = zoorealms$Realm)

# One-colour shading extracted from: https://mdigi.tools/color-shades/
p_inset <- lapply(1:nlevels(zoorealms$Realm), function(x){
  sub_dat <- sum_dat %>% filter(realm == levels(zoorealms$Realm)[x]) %>% group_by(dir, group) %>%
    summarise(no_sp = sum(no_sp))
  max_dat <- sum_dat %>% filter(realm == levels(zoorealms$Realm)[x], dir == "N") %>% group_by(dir) %>%
    summarise(no_sp = sum(no_sp))
  lab_pos <- lab_positions %>% filter(realm == levels(zoorealms$Realm)[x])
  col <- list(c("#d5c4e5", "#824db2", "#2b1a3b"), c("#ffd2aa", "#ff7800", "#552800"), 
              c("#bdd1ec", "#3876c7", "#132742"), c("#bcedbc", "#37c837", "#124312"),
              c("#e1ccc8", "#a66659", "#37221e"), c("#f0b8df", "#d32ca0", "#460f35"),
              c("#b2f0f6", "#19d3e6", "#08464c"), c("#b6daf3", "#2590da", "#0c3048"),
              c("#f2b7b7", "#d82728", "#480d0d"), c("#ffd4aa", "#ff7f00", "#552a00"),
              c("#f2f2b7", "#d7d827", "#48480d"))[[x]]
  sub_dat %>% 
    ggplot(aes(x=dir, y=no_sp, fill=group)) + 
    geom_col(position = position_stack(reverse = TRUE)) + 
    geom_text(x=lab_pos$x1, y=max(max_dat$no_sp)*lab_pos$y1, 
              label=levels(zoorealms$Realm)[x], size=9/.pt) + 
    coord_polar(start=circular::rad(348.75)) + 
    scale_y_continuous(limits=c(0,NA), expand=expansion(add=c(0,1))) + 
    scale_fill_manual(name="", values=col) + theme_minimal() + 
    theme(axis.title = element_blank(), legend.position = "none",
          panel.grid = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank())
})
p_inset[[8]]
p_inset[[11]]

zoorealms$Realm[1:5]
zoorealms$Realm[6:11]
df_inset <- tibble(x = c(0.355,0.72,0.43,0.602,0.93, # 1 - 5
                         0.715,0.3,0.37,0.07,0.9,0.68), # 6 - 11
                   y = c(0.094,0.13,0.319,0.18,0.46, # 1 -5
                         0.3,0.56,0.655,0.61,0.68,0.93), # 6 - 11
                   plot = p_inset, vjust=c(0.5,0.5,0.5,0.5,0.5,
                                           0.5,0.5,0.5,0.5,0.5,0.5),
                   hjust=c(0.5,0.5,0.5,0.5,0.5,
                           0.5,0.5,0.5,0.5,0.5,0.5),
                   text = zoorealms$Realm)
p2 <- p_map + expand_limits(x=0, y=0)  + 
  geom_plot_npc(data = df_inset, aes(npcx = x, npcy = y, label = plot, vjust=vjust, hjust=hjust))
p <- p1 / (p2 + inset_element(p_bar, -0.05, -.05, 0.2, 0.46)) + 
  plot_layout(heights=c(1,1.4)) + theme(plot.background = element_blank())
ggsave("figures/Figure5.png", p, dpi=600, width=12, height=9, bg="transparent")
