#' ---
#' title: "Number of species with changes in range extent and range overlap with RCP"
#' author: "RS-eco"
#' ---

# Clear working environment
rm(list=ls()); invisible(gc())

# Load packages
library(dplyr); library(tidyr)
library(ggplot2); library(patchwork)
library(ggpmisc); library(scico)
library(tagger); library(ggpattern)
library(ggpp); library(ggpubr)
library(moments); library(ggrepel)

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
                                    "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5")))

# Calculate change in range size
dat_1995 <- dat %>% filter(disp == "disp_eigth") %>% 
  filter(year_rcp == "1995") %>%
  dplyr::select(species, year_rcp, gcm, algorithm, range_size) %>%
  pivot_wider(names_from=year_rcp, values_from=range_size) %>%
  dplyr::select(-gcm)
fut_dat <- dat %>% filter(disp == "disp_eigth") %>% 
  filter(year_rcp != "1995") %>%  
  dplyr::select(species, year_rcp, gcm, algorithm, range_size) %>%
  pivot_wider(names_from=year_rcp, values_from=range_size)

change_dat <- fut_dat %>% left_join(dat_1995) %>% 
  mutate_at(vars(starts_with("20")), list(~ (. -`1995`)/`1995`)) %>% dplyr::select(-`1995`) %>%
  pivot_longer(cols=starts_with("20"), names_to="year_rcp", values_to="range_change")
summary(change_dat)  

# Calculate number of species per bin
sum_dat <- change_dat %>% 
  mutate(range_bin = cut(range_change*100, breaks=seq(-102,154, by=4))) %>%
  group_by(gcm, algorithm, year_rcp, range_bin) %>%
  summarise(no_sp = n_distinct(species)) %>%
  mutate(lower =  as.numeric(sub("\\((.+),.*", "\\1", range_bin)),
         upper = as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", range_bin))) %>%
  mutate(mid = (lower+upper)/2) %>% ungroup() %>%
  mutate(mid = if_else(mid > 100, 100, mid)) %>%
  group_by(year_rcp, range_bin, mid, lower, upper) %>%
  summarise(mean_sp=mean(no_sp), sd_sp=sd(no_sp)) %>% 
  separate(year_rcp, c("year", "rcp"), sep=" ")

mean_dat <- change_dat %>% separate(year_rcp, c("year", "rcp"), sep=" ")  %>% 
  group_by(year, rcp) %>% summarise(mean_change=mean(range_change*100))

#########################

# A data frame with labels for each facet
f_labels <- data.frame(x=0.05, y=0.95, disp = c("RCP2.6", "RCP6.0", "RCP8.5"), 
                       label = c("a)", "c)", "e)"))

test_dat <- change_dat %>% separate(year_rcp, c("year", "rcp"), sep=" ")  %>% 
  group_by(year, rcp, gcm, algorithm) %>%
  do(data.frame(skew = skewness(.$range_change),
                kurt = kurtosis(.$range_change)
                #, p.value = jarque.test(.$range_change)$p.value
  ))
test_dat <- test_dat %>% ungroup() %>% group_by(year, rcp) %>%
  summarise(Skewness = round(mean(skew),2), Kurtosis = round(mean(kurt),2)
            #, `p-value` = round(mean(p.value),2)
  )
test_dat <- test_dat %>% 
  tidyr::pivot_longer(Skewness:Kurtosis, names_to="var", values_to="value") %>%
  tidyr::pivot_wider(names_from="year", values_from="value")
colnames(test_dat)[2:4] <- c("2050/2080", "──", "- - -")

test_dat <- test_dat %>% ungroup() %>% nest_by(rcp)

# Change in range sizes
max_sp <- sum_dat %>% group_by(rcp) %>% summarise(max_sp = max(mean_sp))
mean_dat <- left_join(mean_dat, max_sp)


p1 <- sum_dat %>% arrange(range_bin) %>%
  ggplot(aes(x=mid, y=mean_sp, lty=year)) + 
  geom_ribbon(aes(ymin=mean_sp-sd_sp, ymax=mean_sp+sd_sp, fill=year), alpha=0.5) + 
  geom_line(colour="black") + geom_vline(xintercept=0) + 
  geom_vline(data=mean_dat, aes(xintercept=mean_change, lty=year), show.legend=F) + 
  geom_text_repel(data=mean_dat, aes(x=mean_change, label=round(mean_change,1), y=0.0075*max_sp),
                  box.padding=0.35, size=3) + 
  geom_table_npc(data = test_dat, aes(label=data), 
                 table.theme = ttheme(colnames.style=
                                        colnames_style(fill=c("grey80", "#D95F02", "#7570B3"))),
                 npcx = 0.63, npcy = 0.93, hjust = 0, vjust = 1) + 
  facet_grid(rcp~., scales="free_y", switch="y") + 
  tag_facets(tag_pool = c("a", "c", "e"), position = list(x = 0.05, y = 0.925)) + 
  scale_linetype(name="") + scale_fill_manual(name="", values=c("#D95F02", "#7570B3")) + 
  scale_x_continuous(expand = c(0,0), breaks=c(-100,-75,-50,-25,0,25,50,75,100), 
                     labels=c("-100", "-75", "-50", "-25", "0", 
                              "25", "50", "75", parse(text=paste("''",">= 100",sep="")))) + 
  scale_y_continuous(expand=expansion(mult=c(0,.025))) + theme_bw() + 
  labs(x="% change in range extent", y="Number of species") + 
  theme(legend.position="none", strip.text=element_blank(), 
        axis.title = element_text(size=14, face="bold"),
        plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"),
        tagger.panel.tag.text = element_text(color = "black", face="plain", size = 10),
        tagger.panel.tag.background = element_blank()) + 
  guides(colour = guide_legend(ncol = 1))
p1

## Range overlap

# range_overlap.rds was created by 09_RangeOverlap_IndReptile.R
dat <- readRDS("data/range_overlap.rds")
head(dat)

# Split group into required columns
dat <- separate(dat, groups, c("taxon", "sub", "algorithm", "disp", "disp2", 
                               "gcm", "rcp", "year", "group"), 
                sep="_", fill="right") %>% dplyr::select(-c(taxon, sub)) %>% 
  unite("disp", c(disp, disp2), na.rm=T) %>% unite("year_rcp", c(year, rcp), na.rm=T) %>% 
  mutate(year_rcp = factor(year_rcp, levels=c(1995, "2050_rcp26", "2050_rcp60", "2050_rcp85",
                                              "2080_rcp26", "2080_rcp60", "2080_rcp85"), 
                           labels=c(1995, "2050 RCP2.6", "2050 RCP6.0", "2050 RCP8.5",
                                    "2080 RCP2.6", "2080 RCP6.0", "2080 RCP8.5"))) %>% 
  mutate(group = factor(group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard"))) %>%
  mutate(group = factor(group, levels=c("Lizard", "Snake", "Turtle")))
head(dat)

# Calculate total range size
tot_rangesize <- dat %>% group_by(species, year_rcp, disp, algorithm, gcm) %>%
  filter(dif != 0) %>% summarise(area_all = sum(tot_area), 
                                 cells_all = sum(tot_cells)) %>% ungroup()

# Calculate percentage range change
perc_dat <- dat %>% left_join(tot_rangesize) %>% 
  filter(disp == "disp_eigth") %>% filter(dif != 0) %>%
  mutate(perc_area=tot_area/area_all*100, perc_cells=tot_cells/cells_all*100) %>%
  separate(year_rcp, c("year", "rcp"), sep=" ") 
summary(perc_dat)
perc_dat %>% ungroup() %>% summarise(no_sp = n_distinct(species))

# Calculate number of species per % range overlap
sum_dat <- perc_dat %>% ungroup() %>% filter(dif == 3) %>%
  mutate(cut_area = cut(perc_area, c(0, 20, 40, 60, 80, 100))) %>%
  group_by(year, rcp, group, gcm, algorithm, cut_area) %>% 
  summarise(no_sp = n_distinct(species))

# Identify species with no overlap
`%!in%` <- Negate(`%in%`)
overlap_sp <- perc_dat %>% filter(dif == 3) %>% 
  ungroup() %>% select(year, rcp, gcm, algorithm, species)
no_overlap_sp <- perc_dat %>% anti_join(overlap_sp)
extinct_sp <- no_overlap_sp %>% filter(perc_area == 100)
unique(extinct_sp$dif)
no_fut_overlap_sp <- no_overlap_sp %>% filter(perc_area != 100)

# Calculate number of species with no overlap
extinct_sp$cut_area <- "Extinct"
no_fut_overlap_sp$cut_area <- "No overlap"
no_overlap_sum_dat <- bind_rows(extinct_sp, no_fut_overlap_sp) %>% 
  group_by(group, year, rcp, gcm, algorithm, cut_area) %>% 
  summarise(no_sp = n_distinct(species))

# Calculate ensemble mean/sd of range overlap
mean_dat <- bind_rows(sum_dat, no_overlap_sum_dat) %>% group_by(group, year, rcp, cut_area) %>%
  summarise(mean_sp = mean(no_sp), sd_sp = sd(no_sp)) %>% 
  mutate(cut_area = factor(cut_area, levels=c("Extinct", "No overlap", "(0,20]",
                                              "(20,40]", "(40,60]", "(60,80]", "(80,100]")))
sum(mean_dat$mean_sp)/6

(n_year <- mean_dat %>% group_by(cut_area) %>% 
    summarise(n_year = n_distinct(year)) %>% ungroup())
(n_count <- mean_dat %>% group_by(cut_area, year, rcp) %>% 
    mutate(mean_sp_start = cumsum(mean_sp)-mean_sp,
           mean_sp_end = cumsum(mean_sp)) %>% ungroup())
cum_year <- mean_dat %>% ungroup() %>% dplyr::select(group, rcp, cut_area, year) %>% 
  distinct() %>% mutate(year_no = 1) %>% group_by(rcp, group, cut_area) %>% 
  arrange(rcp, group, cut_area) %>% summarise(cum_year=cumsum(year_no)) %>% 
  ungroup() %>% dplyr::select(cum_year)
cum_year <- mean_dat %>% distinct(rcp, group, year, cut_area) %>% 
  arrange(rcp, group, cut_area, year) %>% bind_cols(cum_year)
dat <- mean_dat %>% dplyr::select(cut_area, year, group) %>% left_join(n_count) %>% 
  left_join(cum_year) %>% left_join(n_year) %>% distinct() %>% 
  arrange(rcp, cut_area, year, group) %>% 
  mutate(cut_area = as.numeric(factor(cut_area, labels=c(1:length(unique(mean_dat$cut_area)))))) %>% 
  mutate(x_start = (cut_area-0.45)+(0.9*((cum_year-1)/n_year))) %>%
  mutate(x_end = x_start + (0.9/n_year))

sum_dat2 <- dat %>% group_by(rcp, year, cut_area) %>%
  mutate(cut_area = factor(cut_area, levels=c(1:7), labels=c("Extinct", "No overlap", "(0,20]",
                                                             "(20,40]", "(40,60]", "(60,80]", "(80,100]"))) %>%
  summarise(sum_sp = sum(mean_sp), sd_sp = sum(sd_sp))

sum_dat3 <- bind_rows(sum_dat, no_overlap_sum_dat) %>% ungroup() %>% 
  group_by(year, rcp, gcm, algorithm, cut_area) %>%
  mutate(cut_area = factor(cut_area, levels=c("Extinct", "No overlap", "(0,20]",
                                              "(20,40]", "(40,60]", "(60,80]", "(80,100]"))) %>%
  summarise(no_sp=sum(no_sp)) %>% ungroup()
sum_dat_err <- sum_dat3 %>% group_by(year, rcp, cut_area) %>%
  summarise(mean_sp = mean(no_sp),
            sd_sp = sd(no_sp))

test <- compare_means(no_sp ~ year, sum_dat3, group.by = c("cut_area", "rcp"))
test$y <- c(400, 250, 500, 750, 1400, 2700, 1800, 
            400, 250, 750, 950, 1500, 2650, 1750, 
            600, 250, 1100, 1250, 1550, 2550, 1500)
test$x <- rep(c(0.75, 1.75, 2.725, 3.725, 4.725, 5.725, 6.725), 3)
test$xend <- rep(c(1.25, 2.25, 3.25, 4.25, 5.25, 6.25, 7.25), 3)
test$y2 <- test$y + c(100,100,100,100,100,100,100,
                      100,30,100,100,100,100,30,
                      100,30,30,100,100,30,30)

p2 <- ggplot() + 
  geom_bar(data=sum_dat2, aes(x = cut_area, y=sum_sp, fill = year), 
           stat="identity", position= position_dodge(width=0.9)) + 
  geom_errorbar(data=sum_dat_err, aes(x = cut_area, ymin=mean_sp, ymax=mean_sp+sd_sp, 
                                      group=year),
                position=position_dodge(width=0.9), width=0.2) + 
  geom_rect_pattern(data=dat,
                    aes(xmin=x_start, xmax=x_end, ymin=mean_sp_start, 
                        ymax=mean_sp_end, pattern = group),
                    fill = 'transparent', colour  = 'black', 
                    pattern_size=0.7, pattern_density = 0.15, pattern_spacing=0.03) +
  geom_segment(data=test, aes(x=x, xend=xend, y=y, yend=y)) + 
  geom_text(data=test, aes(x=x+0.25, y=y2, label=p.signif), size=3) +
  facet_grid(rcp~., scales="free") + 
  tag_facets(tag_pool = c("b", "d", "f"), position = list(x = 0.05, y = 0.925)) + 
  scale_fill_manual(name="", values=c("#D95F02", "#7570B3")) +
  scale_pattern_discrete(choices = c("stripe", "circle", "none")) + 
  scale_y_continuous(expand=expansion(mult=c(0,.05))) + 
  theme_bw() + labs(x="% range overlap", pattern="", ) + 
  theme(strip.text=element_text(size=14, face="bold"), strip.background = element_blank(), 
        axis.title = element_text(size=14, face="bold"), axis.title.y = element_blank(), 
        legend.background = element_blank(), legend.position=c(0.145,0.865), 
        legend.spacing.y = unit(-0.275, "cm"),
        tagger.panel.tag.text = element_text(color = "black", face="plain", size = 10),
        tagger.panel.tag.background = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 0, unit = "pt"))
p2

# Combine plots
p <- p1 + p2 + plot_layout(widths=c(1.75,1))
ggsave("figures/FigureS30.png", p, dpi=300, width=12, height=9)

####################