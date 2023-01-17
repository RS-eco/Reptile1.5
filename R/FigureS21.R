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
year <- 2050
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
sr_all <- bind_rows(sr1, sr2) %>% 
  pivot_longer(names_to="gcm_rcp_year", values_to="sum", -c(x,y,algorithm,Group)) %>%
  group_by(x,y,algorithm,gcm_rcp_year) %>% summarise(sum=sum(sum, na.rm=T)); rm(sr1, sr2); gc()
sr_all$gcm_rcp_year <- gsub("[.]", "-", tolower(sr_all$gcm_rcp_year))
climatedata$gcm_rcp_year <- tolower(climatedata$gcm_rcp_year)

dat <- inner_join(sr_all, climatedata) %>% 
  mutate(bio1 = round(2*bio1, digits= 0)/2) %>%
  mutate(bio12 = round(bio12/100)*100) %>%
  group_by(year, bio1, bio12) %>%
  summarise(sr = mean(sum))

dat %>% group_by(year) %>% 
  summarize(maxSRbio1 = bio1[which(sr == max(sr))], 
            maxSRbio12 = bio12[which(sr == max(sr))])

# A data frame with labels for each facet
p2 <- dat  %>% filter(year != "2050") %>% 
  ggplot() + geom_tile(aes(x=bio1, y=bio12/100, fill=sr)) + coord_fixed(ratio=0.5) + 
  scale_fill_gradientn(name="Species\nRichness", colours=bluered) + 
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80), limits=c(0,92),
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  geom_text(x = -20, y = 87.5, label = "a) 2050 RCP6.0", size=6, hjust=0) + 
  theme_bw() + theme(strip.background = element_blank(), strip.text = element_blank(),
                     axis.text.y = element_text(size=12), axis.text.x = element_blank(), 
                     axis.title = element_blank(), legend.background=element_blank(), 
                     legend.position = c(0.15,0.48), plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt"))

unique(dat$year)
pres <- dat %>% filter(year == "1995") %>% ungroup() %>% dplyr::select(-c(year))
fut <- dat %>% filter(year == 2050) %>% ungroup() %>% dplyr::select(-c(year))
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
max(lost_novel_clim$bio12)

p3 <- ggplot(data=dat_wide) + geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=lost_novel_clim$bio1, y=lost_novel_clim$bio12/100, 
           fill=factor(lost_novel_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80), labels=c(0, 2000, 4000, 6000, 8000), 
                     expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + 
  annotate("text", x = -20, y = 87.5, label = "b) 2050 - 1995", size=6, hjust=0) + 
  coord_fixed(ratio=0.5) + labs(x="Annual mean temperature (°C)", y="Annual precipitation (mm)") + 
  theme_bw() + theme(legend.background = element_blank(), 
                     axis.text.y = element_text(size=12), axis.text.x = element_blank(), 
                     axis.title.y = element_text(size=14), axis.title.x = element_blank(), 
                     legend.position = c(0.15,0.46), plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt"))

p3 <- p3 + inset_element(leg, left = unit(0.4, 'npc'), top = unit(0.86, 'npc'), 
                          bottom= unit(0.8, 'npc'), right=unit(0, 'npc'))

unique(dat$year)
pres <- dat %>% filter(year == "1995") %>% ungroup() %>% dplyr::select(-c(year))
fut <- dat %>% filter(year == 2050) %>% ungroup() %>% dplyr::select(-c(year))
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
  annotate("text", x = -20, y = 87.5, label = "c) 2050 - 1995", size=6, hjust=0) + 
  coord_fixed(ratio=0.5) + labs(x="Annual mean temperature (°C)", y="") + 
  theme_bw() + theme(legend.background = element_blank(), axis.text = element_text(size=12), 
                     axis.title.x = element_text(size=14),
                     axis.title.y = element_blank(), legend.position = c(0.15,0.48),
                     legend.text.align = 0, plot.margin = margin(t = 4, r = 4, b = 4, l = 4, unit = "pt"))
p4 <- p4 + inset_element(leg, left = unit(0.4, 'npc'), top = unit(0.86, 'npc'), 
                          bottom= unit(0.8, 'npc'), right=unit(0, 'npc'))

p <- p2 / p3 / p4 + plot_layout(widths=c(1,1,1))
ggsave("figures/FigureS21.png", p, width=5, height=11, dpi=600, bg="transparent")
