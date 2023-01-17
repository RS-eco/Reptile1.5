#' ---
#' title: "Relationship of species richness with temperature and precipitation across RCPs for year 2050"
#' author: "RS-eco"
#' ---

rm(list=ls()); invisible(gc())

#' ### Load packages ###

# Load packages
library(dtplyr); library(data.table); library(dplyr, warn.conflicts=F)
library(tidyr); library(ggplot2); library(patchwork); library(magrittr)
library(ggridges)

#' ### Load general data ###

# Specify colour scheme
library(scico)
bluered <- rev(scico(n=255, palette="roma"))
redwhiteblue <- rev(scico(n=255, palette="vik"))

#' ### Load temperature and precipitation data ###

# Load rISIMIP package
#if(!"rISIMIP" %in% installed.packages()[,"Package"]) 
#  remotes::install_github("RS-eco/rISIMIP", build_vignettes = TRUE)
library(rISIMIP)

# Specify parameters
rcp <- c("rcp26", "rcp60", "rcp85")
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
  dplyr::select("x", "y", "bio1", "bio12") %>% mutate(year = 1995, rcp="EWEMBI")
climatedata2 <- lapply(1:length(climatefiles2), function(x){
  data <- get(data(list=climatefiles2[[x]])) %>% dplyr::select("x", "y", "bio1", "bio12")
  data$year <- rep(year, each=12)[x]
  data$rcp <- rep(rep(rcp, each=4), 2)[x]
  return(data)
})  

climatedata <- c(list(climatedata), climatedata2); rm(climatedata2); gc()
names(climatedata) <- c("EWEMBI_1995", sub("_landonly", "", sub("bioclim_", "", climatefiles2)))
climatedata <- data.table::rbindlist(climatedata, idcol="gcm_rcp_year") %>% as.data.frame()

#' ### Load species data ###
sr1 <- readRDS(paste0("data/Reptile_", sub, "_GAM_", disp, "_groups.rds"))
sr2 <- readRDS(paste0("data/Reptile_", sub, "_GBM_", disp ,"_groups.rds"))
sr1$algorithm <- "GAM"
sr2$algorithm <- "GBM"
sr_all <- bind_rows(sr1, sr2) %>% 
  pivot_longer(names_to="gcm_rcp_year", values_to="sum", -c(x,y,algorithm,Group)) %>%
  group_by(x,y,algorithm,gcm_rcp_year) %>%
  summarise(sum=sum(sum)); rm(sr1, sr2); gc()
sr_all$gcm_rcp_year <- gsub("[.]", "-", tolower(sr_all$gcm_rcp_year))
climatedata$gcm_rcp_year <- tolower(climatedata$gcm_rcp_year)

dat <- inner_join(sr_all, climatedata) %>% 
  mutate(bio1 = round(2*bio1, digits= 0)/2) %>%
  mutate(bio12 = round(bio12/100)*100) %>%
  group_by(year, rcp, bio1, bio12) %>%
  summarise(sr = mean(sum)) %>%
  mutate(year = as.factor(year)) %>%
  mutate(rcp = factor(rcp, levels=c("EWEMBI", "rcp26", "rcp60", "rcp85"), 
                      exclude=NULL, labels=c("EWEMBI", "RCP2.6", "RCP6.0", "RCP8.5")))
summary(dat)

# A data frame with labels for each facet
f_labels <- data.frame(rcp = dat %>% filter(year == 2050) %>% 
                         ungroup() %>% select(rcp) %>% unique() %>% unlist(), 
                       label = c("a) 2050", "b) 2050", "c) 2050"))

dat_sub <- dat %>% filter(rcp != "EWEMBI") %>% filter(year == 2050)
p1 <- dat_sub %>% ggplot() + geom_tile(aes(x=bio1, y=bio12/100, fill=sr)) + 
  facet_wrap(. ~ rcp) + coord_fixed(ratio=0.5) + 
  scale_fill_gradientn(name="Species\nRichness", colours=bluered) + 
  scale_y_continuous(limits=c(min(dat_sub$bio12)/100-1, max(dat_sub$bio12)/100), breaks=c(0, 20, 40, 60, 80), 
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(limits=c(min(dat_sub$bio1)-2, max(dat_sub$bio1)), expand=c(0,0)) + 
  geom_text(x = -20, y = 92.5, aes(label = label), data = f_labels, size=5, hjust=0) + 
  labs(x="", y="Annual precipitation (mm)") + 
  theme_bw() + theme(strip.background = element_blank(), strip.text.y = element_blank(),
                     strip.text.x = element_text(size=12, face="bold"),
                     axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
                     axis.text.x = element_blank(), axis.title.x = element_blank(),
                     legend.background=element_blank(), legend.position = c(0.045,0.58),
                     panel.spacing.x = unit(1, "lines"))

pres <- dat %>% filter(year == "1995") %>% ungroup() %>% select(-c(year, rcp))
fut <- dat %>% filter(year == 2050) %>% ungroup() %>% select(-c(year))
colnames(pres)[3] <- "pres"
colnames(fut)[4] <- "fut"

dat_wide <- full_join(pres, fut) %>% mutate(change = fut - pres)
lost_clim <- dat_wide %>% filter(is.na(fut)) %>%
  mutate(lost = "Lost climate")
novel_clim <- dat_wide %>% filter(is.na(pres)) %>%
  mutate(lost = "Novel climate")
lost_novel_clim <- bind_rows(lost_clim, novel_clim)
dat_wide <- dat_wide %>% drop_na()

(lim_map <- c(min(dat_wide$change, na.rm=T), max(dat_wide$change, na.rm=T)))
col_val <- scales::rescale(unique(c(seq(min(dat_wide$change, na.rm=T), 0, length=5), 
                                    seq(0, max(dat_wide$change, na.rm=T), length=5))))

p_sub <- lost_novel_clim %>% filter(rcp %in% c(NA, "RCP2.6")) %>%
  ggplot() + geom_tile(aes(x=bio1, y=bio12/100, fill=lost)) + 
  scale_fill_manual(name="", values= c("black","#E7298A")) +
  theme(legend.background = element_blank(),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
p_sub
leg <- ggpubr::as_ggplot(ggpubr::get_legend(p_sub))

ln_clim <- lost_novel_clim %>% filter(rcp %in% c(NA, "RCP2.6"))
p2 <- dat_wide %>% filter(rcp == "RCP2.6") %>% ggplot() + 
  geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=ln_clim$bio1, y=ln_clim$bio12/100, 
           fill=factor(ln_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  scale_y_continuous(limits=c(min(dat_sub$bio12)/100-1, max(dat_sub$bio12)/100), breaks=c(0, 20, 40, 60, 80), 
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(limits=c(min(dat_sub$bio1)-2, max(dat_sub$bio1)), expand=c(0,0)) + 
  annotate("text", x = -20, y = 92.5, label = "d) 2050 - 1995", size=5, hjust=0) + 
  coord_fixed(ratio=0.5) + labs(x="", y="Annual precipitation (mm)") + 
  theme_bw() + theme(legend.background = element_blank(), 
                     plot.title = element_text(size=14, face="bold"),
                     axis.text.y = element_text(size=12), axis.title.y = element_text(size=14),
                     axis.text.x = element_blank(), axis.title.x = element_blank(), 
                     legend.position = c(0.15,0.4))

ln_clim <- lost_novel_clim %>% filter(rcp %in% c(NA, "RCP6.0"))
p3 <- dat_wide %>% filter(rcp == "RCP6.0") %>% ggplot() + 
  geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=ln_clim$bio1, y=ln_clim$bio12/100, 
           fill=factor(ln_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  annotate("text", x = -20, y = 92.5, label = "e) 2050 - 1995", size=5, hjust=0) + 
  scale_y_continuous(limits=c(min(dat_sub$bio12)/100-1, max(dat_sub$bio12)/100), breaks=c(0, 20, 40, 60, 80), 
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(limits=c(min(dat_sub$bio1)-2, max(dat_sub$bio1)), expand=c(0,0)) + 
  coord_fixed(ratio=0.5) + theme_bw() + 
  theme(legend.background = element_blank(), plot.title = element_text(size=14, face="bold"),
                     axis.ticks.y = element_blank(), axis.text = element_blank(), 
                     axis.title = element_blank(), legend.position = "none")

ln_clim <- lost_novel_clim %>% filter(rcp %in% c(NA, "RCP8.5"))
p4 <- dat_wide %>% filter(rcp == "RCP8.5") %>% ggplot() + 
  geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=ln_clim$bio1, y=ln_clim$bio12/100, 
           fill=factor(ln_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="Richness \nchange",  colours=redwhiteblue, 
                       values=col_val, limits=lim_map) + 
  annotate("text", x = -20, y = 92.5, label = "f) 2050 - 1995", size=5, hjust=0) + 
  scale_y_continuous(limits=c(min(dat_sub$bio12)/100-1, max(dat_sub$bio12)/100), breaks=c(0, 20, 40, 60, 80), 
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(limits=c(min(dat_sub$bio1)-2, max(dat_sub$bio1)), expand=c(0,0)) + 
  coord_fixed(ratio=0.5) + theme_bw() + 
  theme(legend.background = element_blank(), plot.title = element_text(size=14, face="bold"),
                     axis.ticks.y = element_blank(), axis.text = element_blank(), 
                     axis.title = element_blank(), legend.position = "none")

p2 <- p2 + inset_element(leg, left = unit(0.4, 'npc'), top = unit(0.845, 'npc'), 
                         bottom= unit(0.8, 'npc'), right=unit(0, 'npc'))
p2 <- p2 + p3 + p4; rm(p3, p4)

pres <- dat %>% filter(year == "1995") %>% ungroup() %>% select(-c(year, rcp))
fut <- dat %>% filter(year == 2050) %>% ungroup() %>% select(-c(year))
colnames(pres)[3] <- "pres"
colnames(fut)[4] <- "fut"
dat_wide <- full_join(pres, fut) %>% mutate(change = (fut - pres)/pres*100)

lost_clim <- dat_wide %>% filter(is.na(fut)) %>%
  mutate(lost = "Lost climate")
novel_clim <- dat_wide %>% filter(is.na(pres)) %>%
  mutate(lost = "Novel climate")
lost_novel_clim <- bind_rows(lost_clim, novel_clim)
dat_wide <- dat_wide %>% drop_na()

# Manually set data limit to -100 - 100
dat_wide$change[dat_wide$change >= 100] <- 100
summary(dat_wide$change)
dat_wide <- dat_wide %>% drop_na()

(lim_map <- c(min(dat_wide$change, na.rm=T), max(dat_wide$change, na.rm=T)))
col_val <- scales::rescale(unique(c(seq(min(dat_wide$change, na.rm=T), 0, length=5), 
                                    seq(0, max(dat_wide$change, na.rm=T), length=5))))

ln_clim <- lost_novel_clim %>% filter(rcp %in% c(NA, "RCP2.6"))
p3 <- dat_wide %>% filter(rcp == "RCP2.6") %>% ggplot() + 
  geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=ln_clim$bio1, y=ln_clim$bio12/100, 
           fill=factor(ln_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="% change", colours=redwhiteblue, 
                       values=col_val, limits=lim_map,
                       breaks=c(-100, -50, 0, 50, 100), 
                       labels=c("-100", "-50", "0", "50", 
                                parse(text=paste("''",">= 100",sep="")))) + 
  scale_y_continuous(limits=c(min(dat_sub$bio12)/100-1, max(dat_sub$bio12)/100), breaks=c(0, 20, 40, 60, 80), 
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(limits=c(min(dat_sub$bio1)-2, max(dat_sub$bio1)), expand=c(0,0)) + 
  annotate("text", x = -20, y = 92.5, label = "g) 2050 - 1995", size=5, hjust=0) + 
  labs(x="Annual mean temperature (°C)", y="Annual precipitation (mm)") + 
  coord_fixed(ratio=0.5) + theme_bw() + 
  theme(legend.background = element_blank(), 
                     plot.title = element_text(size=14, face="bold"),
                     axis.text = element_text(size=12), axis.title = element_text(size=14),
                     legend.position = c(0.15,0.43), legend.text.align = 0)

ln_clim <- lost_novel_clim %>% filter(rcp %in% c(NA, "RCP6.0"))
p4 <- dat_wide %>% filter(rcp == "RCP6.0") %>% ggplot() + 
  geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=ln_clim$bio1, y=ln_clim$bio12/100, 
           fill=factor(ln_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="% change", colours=redwhiteblue, 
                       values=col_val, limits=lim_map,
                       breaks=c(-100, -50, 0, 50, 100), 
                       labels=c("-100", "-50", "0", "50", 
                                parse(text=paste("''",">= 100",sep="")))) + 
  scale_y_continuous(limits=c(min(dat_sub$bio12)/100-1, max(dat_sub$bio12)/100), breaks=c(0, 20, 40, 60, 80), 
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(limits=c(min(dat_sub$bio1)-2, max(dat_sub$bio1)), expand=c(0,0)) + 
  annotate("text", x = -20, y = 92.5, label = "h) 2050 - 1995", size=5, hjust=0) + 
  labs(x="Annual mean temperature (°C)", y="") + 
  coord_fixed(ratio=0.5) + theme_bw() + 
  theme(legend.background = element_blank(), 
                     plot.title = element_text(size=14, face="bold"),
                     axis.text.x = element_text(size=12), axis.title.x = element_text(size=14),
                     axis.text.y = element_blank(), axis.title.y = element_blank(),
                     axis.ticks.y = element_blank(), legend.position = "none")

ln_clim <- lost_novel_clim %>% filter(rcp %in% c(NA, "RCP8.5"))
p5 <- dat_wide %>% filter(rcp == "RCP8.5") %>% ggplot() + 
  geom_tile(aes(x=bio1, y=bio12/100, fill=change)) + 
  annotate(geom='tile', x=ln_clim$bio1, y=ln_clim$bio12/100, 
           fill=factor(ln_clim$lost, levels=c("Lost climate", "Novel climate"), 
                       labels=c("black","#E7298A"))) + 
  scale_fill_gradientn(name="% change", colours=redwhiteblue, 
                       values=col_val, limits=lim_map,
                       breaks=c(-100, -50, 0, 50, 100), 
                       labels=c("-100", "-50", "0", "50", 
                                parse(text=paste("''",">= 100",sep="")))) + 
  scale_y_continuous(limits=c(min(dat_sub$bio12)/100-1, max(dat_sub$bio12)/100), breaks=c(0, 20, 40, 60, 80), 
                     labels=c(0, 2000, 4000, 6000, 8000), expand=c(0,0)) + 
  scale_x_continuous(limits=c(min(dat_sub$bio1)-2, max(dat_sub$bio1)), expand=c(0,0)) + 
  annotate("text", x = -20, y = 92.5, label = "i) 2050 - 1995", size=5, hjust=0) + 
  labs(x="Annual mean temperature (°C)", y="") + 
  coord_fixed(ratio=0.5) + theme_bw() + 
  theme(legend.background = element_blank(), 
                     plot.title = element_text(size=14, face="bold"),
                     axis.text.x = element_text(size=12), axis.title.x = element_text(size=14),
                     axis.text.y = element_blank(), axis.title.y = element_blank(),
                     axis.ticks.y = element_blank(), legend.position = "none")

p3 <- p3 + inset_element(leg, left = unit(0.4, 'npc'), top = unit(0.845, 'npc'), 
                         bottom= unit(0.8, 'npc'), right=unit(0, 'npc'))
p3 <- p3 + p4 + p5; rm(p4, p5)

p1 / p2 / p3 + plot_layout(widths=c(1,1,1))
ggsave("figures/FigureS34.png", width=11, height=10, dpi=300, bg="transparent")
