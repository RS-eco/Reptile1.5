#' ---
#' title: "Relationship of species richness with temperature and precipitation"
#' author: "RS-eco"
#' ---

rm(list=ls()); invisible(gc())

#' ### Load packages ###

# Load packages
library(dtplyr); library(data.table); library(dplyr, warn.conflicts=F)
library(tidyr); library(ggplot2); library(patchwork); library(magrittr)
library(ggpp); library(tagger)

#' ### Load general data ###

# Specify colour scheme
library(scico)
bluered <- rev(scico(n=255, palette="roma"))
whiteblue <- rev(scico(n=255, palette="davos"))
redwhite <- rev(scico(n=255, palette="lajolla"))
redwhiteblue <- rev(scico(n=255, palette="vik"))

#' ### Load temperature and precipitation data ###

# Load rISIMIP package
if(!"rISIMIP" %in% installed.packages()[,"Package"]) 
  remotes::install_github("RS-eco/rISIMIP", build_vignettes = TRUE)
library(rISIMIP)

# Specify parameters
rcp <- "rcp60"
year <- 2080
disp <- "disp_eigth"
sub <- "thresh"

# Select climate data
gcm <- c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5")
climatefiles2 <- expand.grid(gcm,rcp,year) %>% rowwise() %>% 
  transmute(name = paste(Var1,Var2,Var3, sep="_")) %>% unlist()
(climatefiles2 <- paste0("bioclim_", tolower(climatefiles2), "_landonly"))

# Load climate data into list
climatedata <- get(data("bioclim_ewembi_1995_landonly")) %>% 
  dplyr::select("x", "y", "bio1", "bio12") %>% mutate(year = 1995)
climatedata2 <- lapply(1:length(climatefiles2), function(x){
  data <- get(data(list=climatefiles2[[x]])) %>% dplyr::select("x", "y", "bio1", "bio12")
  data$year <- rep(year, each=4)[x]
  return(data)
})  

climatedata <- c(list(climatedata), climatedata2)
rm(climatedata2); rm(list=climatefiles2); gc()
names(climatedata) <- c("EWEMBI_1995", sub("_landonly", "", sub("bioclim_", "", climatefiles2)))
climatedata <- data.table::rbindlist(climatedata, idcol="gcm_rcp_year") %>% as.data.frame()

#' ### Load species data ###
sr1 <- readRDS(paste0("data/Reptile_", sub, "_GAM_", disp, "_groups.rds"))
sr2 <- readRDS(paste0("data/Reptile_", sub, "_GBM_", disp ,"_groups.rds"))
sr1 <- bind_rows(sr1)
sr1$algorithm <- "GAM"
sr2 <- bind_rows(sr2)
sr2$algorithm <- "GBM"
sr_all <- bind_rows(sr1, sr2) %>% pivot_longer(names_to="gcm_rcp_year", values_to="sum", 
                                               -c(x,y,algorithm,Group)) %>%
  group_by(x,y,algorithm,gcm_rcp_year) %>% summarise(sum=sum(sum, na.rm=T)); rm(sr1, sr2); gc()
sr_all$gcm_rcp_year <- gsub("[.]", "-", tolower(sr_all$gcm_rcp_year))
climatedata$gcm_rcp_year <- tolower(climatedata$gcm_rcp_year)
max(sr_all$sum)

#' ### Merge climate and species data
dat1 <- inner_join(sr_all, climatedata) %>% mutate(value = round(2*bio1, digits= 0)/2) %>% 
  group_by(year, value) %>% summarise(sr = mean(sum), sd_sr = sd(sum)) %>% 
  mutate(var = "Annual mean temperature (°C)")
dat2 <- inner_join(sr_all, climatedata) %>% 
  mutate(value = round(bio12/100)*100) %>% group_by(year, value) %>% 
  summarise(sr = mean(sum), sd_sr = sd(sum)) %>% 
  mutate(var = "Annual precipitation (mm)")
dat <- bind_rows(dat1, dat2) %>% mutate(year = as.factor(year)) %>% drop_na()
rm(dat1, dat2); gc()
unique(dat$var)

dat %>% group_by(var, year) %>% 
  summarize(minSRvalue = value[which(sr == min(sr))], 
            maxSRvalue = value[which(sr == max(sr))])
dat %>% group_by(var, year) %>% 
  summarize(minSRvalue = value[which(sr %in% c(min(sr):(min(sr)+25)))], 
            maxSRvalue = value[which(sr %in% c((max(sr)-25):max(sr)))])

# Create plot
p1 <- dat %>%
  ggplot(aes(x=value, y=sr, lty=year)) + geom_line(colour="black") + 
  geom_ribbon(aes(ymin=sr-sd_sr, ymax=sr+sd_sr, fill=year), alpha=0.25) + 
  facet_wrap(var~., scales="free", strip.position="bottom") + 
  labs(x="", y="Species richness") + 
  tag_facets(tag_pool = c("a", "b"), position = list(x = 0.075, y = 0.9)) + 
  scale_y_continuous(expand=expansion(mult=0.02)) + 
  scale_x_continuous(expand=expansion(mult=0.02)) +
  scale_linetype(name="") + 
  scale_fill_manual(name="",values=c("#1B9E77", "#7570B3")) + 
  theme_bw() + theme(strip.background = element_blank(), strip.placement="outside",
                     strip.text = element_text(size=14), legend.background = element_blank(),
                     axis.text = element_text(size=12), axis.title.y = element_text(size=14), 
                     axis.title.x = element_blank(), legend.position=c(0.065,0.66),
                     plot.margin=unit(c(0,0.2,0,0.2),"mm"))

sr_all %>% filter(gcm_rcp_year == "ewembi_1995") %>%
  group_by(algorithm) %>% summarise(max(sum, na.rm=T))
dat <- inner_join(sr_all, climatedata) %>% 
  mutate(bio1 = round(2*bio1, digits= 0)/2) %>%
  mutate(bio12 = round(bio12/100)*100) %>%
  group_by(year, bio1, bio12) %>%
  summarise(sr = mean(sum))

dat %>% group_by(year) %>% 
  summarize(maxSRbio1 = bio1[which(sr == max(sr))], 
            maxSRbio12 = bio12[which(sr == max(sr))])
dat %>% group_by(year) %>% summarise(max(sr))

# A data frame with labels for each facet
f_labels <- data.frame(year = c(1995, 2080), label = c("c) 1995", "d) 2080 RCP6.0"))

p2 <- dat %>% ggplot() + geom_tile(aes(x=bio1, y=bio12/100, fill=sr)) + 
  facet_wrap(. ~ year) + coord_fixed(ratio=0.5) + 
  scale_fill_gradientn(name="Species\nRichness", colours=bluered) + 
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80), 
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  geom_text(x = -20, y = 87.5, aes(label = label), data = f_labels, size=6, hjust=0) + 
  labs(x="", y="Annual precipitation (mm)") + 
  theme_bw() + theme(strip.background = element_blank(), strip.text = element_blank(),
                     axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
                     axis.text.x = element_blank(), axis.title.x = element_blank(),
                     legend.background=element_blank(), legend.position = c(0.065,0.625),
                     plot.margin=unit(c(0.2,0.2,0.4,0.2),"mm"))

unique(dat$year)
pres <- dat %>% filter(year == "1995") %>% ungroup() %>% select(-c(year))
fut <- dat %>% filter(year == 2080) %>% ungroup() %>% select(-c(year))
colnames(pres)[3] <- "pres"
colnames(fut)[3] <- "fut"

dat_wide <- full_join(pres, fut) %>% mutate(change = fut - pres)
dat_wide$time_rcp <- paste0(year[2], " - 1995")
lost_clim <- dat_wide %>% filter(is.na(fut)) %>%
  mutate(lost = "Lost climate")
novel_clim <- dat_wide %>% filter(is.na(pres)) %>%
  mutate(lost = "Novel climate")
lost_novel_clim <- bind_rows(lost_clim, novel_clim)

(lim_map <- c(min(dat_wide$change, na.rm=T), max(dat_wide$change, na.rm=T)))
col_val <- scales::rescale(unique(c(seq(min(dat_wide$change, na.rm=T), 0, length=5), 
                                    seq(0, max(dat_wide$change, na.rm=T), length=5))))

p_sub <- lost_novel_clim %>% 
  ggplot() + geom_tile(aes(x=bio1, y=bio12/100, fill=lost)) + 
  coord_fixed(ratio=0.5) + 
  scale_fill_manual(name="", values= c("black","#E7298A")) +
  theme(legend.background = element_blank(),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
p_sub
leg <- ggpubr::as_ggplot(ggpubr::get_legend(p_sub))

dat_wide <- dat_wide %>% drop_na()

ggplot(data=dat_wide %>% filter(change > 0)) + geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=whiteblue) + 
  coord_fixed(ratio=0.5)

ggplot(data=dat_wide %>% filter(change < 0)) + geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=redwhite) + 
  coord_fixed(ratio=0.5)

p3 <- ggplot(data=dat_wide) + geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=lost_novel_clim$bio1, y=lost_novel_clim$bio12/100, 
           fill=factor(lost_novel_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80), labels=c(0, 2000, 4000, 6000, 8000), 
                     expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  annotate("text", x = -20, y = 87.5, label = "e) 2080 - 1995", size=6, hjust=0) + 
  coord_fixed(ratio=0.5) + labs(x="Annual mean temperature (°C)", y="Annual precipitation (mm)") + 
  theme_bw() + theme(legend.background = element_blank(), 
                     plot.title = element_text(size=14, face="bold"),
                     axis.text = element_text(size=12), axis.title = element_text(size=14),
                     legend.position = c(0.15,0.46),
                     plot.margin=unit(c(0.2,0.2,1,0.2),"mm"))

p3 <- p3 + inset_element(leg, left = unit(0.4, 'npc'), top = unit(0.86, 'npc'), 
                          bottom= unit(0.8, 'npc'), right=unit(0, 'npc'))

unique(dat$year)
pres <- dat %>% filter(year == "1995") %>% ungroup() %>% select(-c(year))
fut <- dat %>% filter(year == 2080) %>% ungroup() %>% select(-c(year))
colnames(pres)[3] <- "pres"
colnames(fut)[3] <- "fut"
dat_wide <- full_join(pres, fut) %>% mutate(change = (fut - pres)/pres*100)
dat_wide$time_rcp <- paste0(year[2], " - 1995")
lost_clim <- dat_wide %>% filter(is.na(fut)) %>%
  mutate(lost = "Lost climate")
novel_clim <- dat_wide %>% filter(is.na(pres)) %>%
  mutate(lost = "Novel climate")
lost_novel_clim <- bind_rows(lost_clim, novel_clim)

# Manually set data limit to -100 - 100
dat_wide$change[dat_wide$change >= 100] <- 100
summary(dat_wide$change)

(lim_map <- c(min(dat_wide$change, na.rm=T), max(dat_wide$change, na.rm=T)))
col_val <- scales::rescale(unique(c(seq(min(dat_wide$change, na.rm=T), 0, length=5), 
                                    seq(0, max(dat_wide$change, na.rm=T), length=5))))

ggplot(data=dat_wide %>% filter(change > 0)) + geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=whiteblue) + 
  coord_fixed(ratio=0.5)

ggplot(data=dat_wide %>% filter(change < 0)) + geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=redwhite) + 
  coord_fixed(ratio=0.5)

p4 <- dat_wide %>% ggplot() + geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=lost_novel_clim$bio1, y=lost_novel_clim$bio12/100, 
           fill=factor(lost_novel_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="% change", colours=redwhiteblue, 
                       values=col_val, limits=lim_map,
                       breaks=c(-100, -50, 0, 50, 100), 
                       labels=c("-100", "-50", "0", "50", 
                                parse(text=paste("''",">= 100",sep="")))) + 
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80), labels=c(0, 2000, 4000, 6000, 8000), 
                     expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  annotate("text", x = -20, y = 87.5, label = "f) 2080 - 1995", size=6, hjust=0) + 
  coord_fixed(ratio=0.5) + labs(x="Annual mean temperature (°C)", y="") + 
  theme_bw() + theme(legend.background = element_blank(), 
                     plot.title = element_text(size=14, face="bold"),
                     axis.text.x = element_text(size=12), axis.title.x = element_text(size=14),
                     axis.text.y = element_blank(), axis.title.y = element_blank(),
                     axis.ticks.y = element_blank(), legend.position = c(0.15,0.48),
                     plot.margin=unit(c(0.2,0.2,0.4,0.2),"mm"), legend.text.align = 0)
p4 <- p4 + inset_element(leg, left = unit(0.4, 'npc'), top = unit(0.86, 'npc'), 
                          bottom= unit(0.8, 'npc'), right=unit(0, 'npc'))

p <- p1 / p2 / (p3 + p4) + plot_layout(heights=c(1,2,2))
ggsave("figures/Figure3.png", p, width=9, height=9.7, dpi=600, bg="transparent")
