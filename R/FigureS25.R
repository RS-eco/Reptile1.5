#' ---
#' title: "Number of species with changes in range extent and range overlap with dispersal"
#' author: "RS-eco"
#' ---

# Clear working environment
rm(list=ls()); invisible(gc())

# Load packages
library(dplyr); library(tidyr)
library(ggplot2); library(patchwork)
library(ggpmisc); library(scico)
library(ggpp); library(tagger)
library(ggrepel); library(ggpattern)
library(moments); library(ggpubr)

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
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard"))) %>%
  mutate(group = factor(group, levels=c("Lizard", "Snake", "Turtle"))) %>% 
  mutate(disp = factor(disp, levels=c("disp_quarter", "disp_eigth", "disp_sixteenth"),
                       labels=c("d/4", "d/8", "d/16")))

# Calculate change in range size
dat_1995 <- dat %>% filter(year_rcp == "1995") %>%
  dplyr::select(species, disp, year_rcp, gcm, algorithm, range_size) %>%
  pivot_wider(names_from=year_rcp, values_from=range_size) %>%
  dplyr::select(-gcm)
fut_dat <- dat %>% filter(year_rcp == "2080 RCP6.0") %>% 
  mutate(year_rcp = factor(year_rcp, levels="2080 RCP6.0",
                           labels="2080")) %>%
  dplyr::select(species, disp, year_rcp, gcm, algorithm, range_size) %>%
  pivot_wider(names_from=year_rcp, values_from=range_size)

change_dat <- fut_dat %>% left_join(dat_1995) %>% 
  mutate_at(vars(starts_with("20")), list(~ (. -`1995`)/`1995`)) %>% dplyr::select(-`1995`) %>%
  pivot_longer(cols=starts_with("20"), names_to="year_rcp", values_to="range_change")
rm(fut_dat, dat_1995)
summary(change_dat)  

# Calculate number of species per bin
sum_dat <- change_dat %>% 
  mutate(range_bin = cut(range_change*100, breaks=seq(-102,154, by=4))) %>%
  group_by(gcm, algorithm, disp, year_rcp, range_bin) %>%
  summarise(no_sp = n_distinct(species)) %>%
  mutate(lower =  as.numeric(sub("\\((.+),.*", "\\1", range_bin)),
         upper = as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", range_bin))) %>%
  mutate(mid = (lower+upper)/2) %>% ungroup() %>%
  mutate(mid = if_else(mid > 100, 100, mid)) %>%
  group_by(year_rcp, range_bin, disp, mid, lower, upper) %>%
  summarise(mean_sp=mean(no_sp), sd_sp=sd(no_sp)); invisible(gc())

mean_dat <- change_dat %>% 
  group_by(disp, year_rcp) %>%
  summarise(mean_change=mean(range_change*100),
            sd_change = sd(range_change*100),
            se_change = sd(range_change*100)/sqrt(length(range_change*100)))
mean_dat

#########################

# A data frame with labels for each facet
f_labels <- data.frame(x=0.05, y=0.95, disp = c("d/4", "d/8", "d/16"), 
                       label = c("a)", "c)", "e)"))

test_dat <- change_dat %>% group_by(disp, year_rcp, gcm, algorithm) %>%
  do(data.frame(skew = skewness(.$range_change),
                kurt = kurtosis(.$range_change), 
                p.value = jarque.test(.$range_change)$p.value
  ))
test_dat <- test_dat %>% ungroup() %>% group_by(disp, year_rcp) %>%
  summarise(Skewness = round(mean(skew),2), Kurtosis = round(mean(kurt),2)
            #, `p-value` = round(mean(p.value),2)
  ) %>% select(-year_rcp)
test_dat <- test_dat %>% ungroup() %>% nest_by(disp)

# Change in range sizes
max_sp <- sum_dat %>% group_by(disp) %>% summarise(max_sp = max(mean_sp))
mean_dat <- left_join(mean_dat, max_sp)

p1 <- sum_dat %>% arrange(range_bin) %>%
  ggplot(aes(x=mid, y=mean_sp, lty=year_rcp)) + 
  geom_ribbon(aes(ymin=mean_sp-sd_sp, ymax=mean_sp+sd_sp, fill=year_rcp)) + 
  geom_line(colour="black") + geom_vline(xintercept=0) + 
  geom_vline(data=mean_dat, aes(xintercept=mean_change, lty=year_rcp), 
             show.legend=F, lty="dashed") + 
  geom_text_repel(data=mean_dat, aes(x=mean_change, label=round(mean_change,1), y=0.0075*max_sp),
                  box.padding=0.35, size=3, nudge_x=5) + 
  geom_table_npc(data = test_dat, aes(label=data), 
                 table.theme = ttheme(),
                 npcx = 0.63, npcy = 0.93, hjust = 0, vjust = 1) + 
  facet_grid(disp~., scales="free_y", switch="y") + 
  tag_facets(tag_pool = c("a", "c", "e"), position = list(x = 0.05, y = 0.925)) + 
  scale_linetype_manual(name="", values="dashed") + 
  scale_fill_manual(name="", values=c("#7570B3")) + 
  scale_x_continuous(expand = c(0,0), breaks=c(-100,-75,-50,-25,0,25,50,75,100), 
                     labels=c("-100", "-75", "-50", "-25", "0", 
                              "25", "50", "75", parse(text=paste("''",">= 100",sep="")))) + 
  scale_y_continuous(expand=expansion(mult=c(0,.025))) + theme_bw() + 
  labs(x="% change in range extent", y="Number of species") + 
  theme(legend.position= "none", strip.text=element_blank(), 
        axis.title = element_text(size=14, face="bold"),
        plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"),
        tagger.panel.tag.text = element_text(color = "black", face="plain", size = 10),
        tagger.panel.tag.background = element_blank()) + 
  guides(colour = guide_legend(ncol = 1))
p1

#########################

## Range overlap

# range_overlap.rds was created by 09_RangeOverlap_IndReptile.R
dat <- readRDS("data/range_overlap.rds")
head(dat)

# Split group into required columns
dat <- separate(dat, groups, c("taxon", "sub", "algorithm", "disp", 
                               "disp2", "gcm", "rcp", "year", "group"), 
                sep="_", fill="right") %>% dplyr::select(-c(taxon, sub)) %>% 
  unite("disp", c(disp, disp2), na.rm=T) %>% unite("year_rcp", c(year, rcp), na.rm=T) %>%
  filter(year_rcp == "2080_rcp60") %>% 
  mutate(disp = factor(disp, levels=c("disp_quarter", "disp_eigth", "disp_sixteenth"),
                       labels=c("d/4", "d/8", "d/16"))) %>%
  mutate(group = factor(group, levels=c("croc", "lizard", "snake", "turtle", "worm lizard"), 
                        labels=c("Lizard", "Lizard", "Snake", "Turtle", "Lizard"))) %>%
  mutate(group = factor(group, levels=c("Lizard", "Snake", "Turtle")))
head(dat)

# Calculate total range size
tot_rangesize <- dat %>% group_by(species, group, year_rcp, disp, algorithm, gcm) %>%
  filter(dif != 0) %>% summarise(area_all = sum(tot_area), cells_all = sum(tot_cells)) %>% 
  ungroup()

# Calculate percentage range change
perc_dat <- dat %>% left_join(tot_rangesize) %>% filter(dif != 0) %>%
  mutate(perc_area=tot_area/area_all*100, perc_cells=tot_cells/cells_all*100)
summary(perc_dat)
perc_dat %>% ungroup() %>% summarise(no_sp = n_distinct(species))

# Calculate number of species per % range overlap
sum_dat <- perc_dat %>% ungroup() %>% filter(dif == 3) %>% 
  mutate(cut_area = cut(perc_area, c(0, 20, 40, 60, 80, 100))) %>%
  group_by(disp, group, year_rcp, gcm, algorithm, cut_area) %>% 
  summarise(no_sp = n_distinct(species))
sum_dat %>% group_by(disp, year_rcp, gcm, algorithm) %>% 
  summarise(no_sp = sum(no_sp))

# Identify species with no overlap
`%!in%` <- Negate(`%in%`)
overlap_sp <- perc_dat %>% filter(dif == 3) %>% 
  ungroup() %>% select(disp, gcm, algorithm, species)
no_overlap_sp <- perc_dat %>% anti_join(overlap_sp)
extinct_sp <- no_overlap_sp %>% filter(perc_area == 100)
unique(extinct_sp$dif)
no_fut_overlap_sp <- no_overlap_sp %>% filter(perc_area != 100)

# Calculate number of species with no overlap
extinct_sp$cut_area <- "Extinct"
no_fut_overlap_sp$cut_area <- "No overlap"
no_overlap_sum_dat <- bind_rows(extinct_sp, no_fut_overlap_sp) %>% 
  group_by(disp, group, year_rcp, gcm, algorithm, cut_area) %>% 
  summarise(no_sp = n_distinct(species))

# Calculate ensemble mean/sd of range overlap
mean_dat <- bind_rows(sum_dat, no_overlap_sum_dat) %>% group_by(disp, group, cut_area) %>%
  summarise(mean_sp = mean(no_sp), sd_sp = sd(no_sp)) %>% arrange(disp, group, cut_area) %>% 
  mutate(cut_area = factor(cut_area, levels=c("Extinct", "No overlap", "(0,20]",
                                              "(20,40]", "(40,60]", "(60,80]", "(80,100]")))
sum(mean_dat$mean_sp)/3

(n_count <- mean_dat %>% group_by(cut_area, disp) %>% 
    mutate(mean_sp_start = cumsum(mean_sp)-mean_sp,
           mean_sp_end = cumsum(mean_sp)) %>% ungroup())
dat <- mean_dat %>% ungroup() %>% dplyr::select(disp, cut_area, group) %>% left_join(n_count) %>% 
  arrange(disp, cut_area, group) %>% 
  mutate(cut_area = as.numeric(factor(cut_area, labels=c(1:length(unique(mean_dat$cut_area)))))) %>% 
  mutate(x_start = (cut_area-0.45)) %>%
  mutate(x_end = x_start + 0.9)

sum_dat2 <- dat %>% group_by(disp, cut_area) %>% 
  mutate(cut_area = factor(cut_area, levels=c(1:7), labels=c("Extinct", "No overlap", "(0,20]",
                                                             "(20,40]", "(40,60]", "(60,80]", "(80,100]"))) %>%
  summarise(sum_sp = sum(mean_sp))

sum_dat3 <- bind_rows(sum_dat, no_overlap_sum_dat) %>% ungroup() %>% 
  group_by(gcm, algorithm, disp, cut_area) %>%
  mutate(cut_area = factor(cut_area, levels=c("Extinct", "No overlap", "(0,20]",
                                              "(20,40]", "(40,60]", "(60,80]", "(80,100]"))) %>%
  summarise(no_sp=sum(no_sp)) %>% ungroup()
sum_dat_err <- sum_dat3 %>% group_by(disp, cut_area) %>%
  summarise(mean_sp = mean(no_sp),
            sd_sp = sd(no_sp))

p2 <- ggplot() + 
  geom_bar(data=sum_dat2, aes(x = cut_area, y=sum_sp), 
           stat="identity", fill="#7570B3", position= position_dodge(width=0.9)) + 
  geom_errorbar(data=sum_dat_err, aes(x = cut_area, ymin=mean_sp, ymax=mean_sp+sd_sp), 
                position=position_dodge(width=0.9), width=0.2) + 
  geom_rect_pattern(data=dat, aes(xmin=x_start, xmax=x_end, ymin=mean_sp_start, 
                                  ymax=mean_sp_end, pattern = group),
                    fill = 'transparent', colour  = 'black', 
                    pattern_size=0.7, pattern_density = 0.15, pattern_spacing=0.03) +
  facet_grid(disp~., scales="free") + 
  tag_facets(tag_pool = c("b", "d", "f"), position = list(x = 0.05, y = 0.925)) + 
  scale_pattern_discrete(choices = c("stripe", "circle", "none")) + 
  scale_y_continuous(expand=expansion(mult=c(0,.15))) + 
  theme_bw() + labs(x="% range overlap", pattern="", ) + 
  theme(strip.text=element_text(size=14, face="bold"), strip.background = element_blank(), 
        axis.title = element_text(size=14, face="bold"), axis.title.y = element_blank(), 
        legend.background = element_blank(), legend.position=c(0.145,0.9), 
        legend.spacing.y = unit(-0.275, "cm"),
        tagger.panel.tag.text = element_text(color = "black", face="plain", size = 10),
        tagger.panel.tag.background = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 0, unit = "pt"))
p2

# Combine plots
p <- p1 + p2 + plot_layout(widths=c(1.75,1))
ggsave("figures/FigureS25.png", p, dpi=300, width=12, height=9, bg="transparent")

####################