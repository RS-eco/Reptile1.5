#' ---
#' title: "Uniform relationship of species richness with temperature and precipitation across RCP & Dispersal"
#' author: "RS-eco"
#' ---

rm(list=ls()); invisible(gc())

#' ### Load packages ###

# Load packages
library(dtplyr); library(data.table); library(dplyr, warn.conflicts=F)
library(tidyr); library(ggplot2); library(patchwork); library(magrittr)
library(ggridges); library(tagger)

#' ### Load general data ###

# Specify colour scheme
library(scico)
bluered <- rev(scico(n=255, palette="roma"))

#' ### Load temperature and precipitation data ###

# Load rISIMIP package
if(!"rISIMIP" %in% installed.packages()[,"Package"]) 
  remotes::install_github("RS-eco/rISIMIP", build_vignettes = TRUE)
library(rISIMIP)

# Specify parameters
rcp <- c("rcp26", "rcp60", "rcp85")
year <- c(2050,2080)
disp <- c("disp_quarter", "disp_eigth", "disp_sixteenth")
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

climatedata <- c(list(climatedata), climatedata2)
rm(climatedata2); rm(list=climatefiles2); gc()
names(climatedata) <- c("EWEMBI_1995", sub("_landonly", "", sub("bioclim_", "", climatefiles2)))
climatedata <- data.table::rbindlist(climatedata, idcol="gcm_rcp_year") %>% as.data.frame()

#' ### Load species data ###
sr1 <- lapply(disp, function(x){
  dat <- readRDS(paste0("data/Reptile_", sub, "_GAM_", x, "_groups.rds"))
  dat$disp <- x
  return(dat)
})
sr2 <- lapply(disp, function(x){
  dat <- readRDS(paste0("data/Reptile_", sub, "_GBM_", x, "_groups.rds"))
  dat$disp <- x
  return(dat)
})
sr1 <- bind_rows(sr1)
sr1$algorithm <- "GAM"
sr2 <- bind_rows(sr2)
sr2$algorithm <- "GBM"
sr_all <- bind_rows(sr1, sr2) %>% 
  pivot_longer(names_to="gcm_rcp_year", values_to="sum", -c(x,y,disp,algorithm, Group)) %>%
  group_by(x,y,disp,algorithm,gcm_rcp_year) %>%
  summarise(sum=sum(sum)); rm(sr1, sr2); gc()
sr_all$gcm_rcp_year <- gsub("[.]", "-", tolower(sr_all$gcm_rcp_year))
climatedata$gcm_rcp_year <- tolower(climatedata$gcm_rcp_year)

#' ### Merge climate and species data
dat1 <- full_join(sr_all, climatedata) %>% 
  mutate(value = round(bio1, digits= 0)) %>% 
  group_by(year, rcp, disp, value) %>%
  summarise(sr = mean(sum), sd_sr = sd(sum)) %>% 
  mutate(var = "Annual mean temperature (°C)")
dat2 <- full_join(sr_all, climatedata) %>% 
  mutate(value = round(bio12/100)*100) %>% 
  group_by(year, rcp, disp, value) %>%
  summarise(sr = mean(sum), sd_sr = sd(sum)) %>% 
  mutate(var = "Annual precipitation (mm)")
dat <- bind_rows(dat1, dat2) %>% 
  mutate(rcp = factor(rcp, levels=c("EWEMBI", "rcp26", "rcp60", "rcp85"), 
                      exclude=NULL,labels=c("EWEMBI", "RCP2.6", "RCP6.0", "RCP8.5"))) %>%
  mutate(disp = factor(disp, levels=c("disp_quarter", "disp_eigth", "disp_sixteenth"),
                       labels=c("d/4", "d/8", "d/16"))) %>%
  mutate(year = as.factor(year)) %>% drop_na()
rm(dat1, dat2); gc()
unique(dat$var)

p1 <- dat %>% filter(year %in% c(1995, 2080)) %>% filter(var == "Annual mean temperature (°C)") %>%
  filter(rcp %in% c("EWEMBI", "RCP6.0")) %>% arrange(value) %>%
  ggplot(aes(x=value, y=sr, lty=year)) + geom_line(colour="black") + 
  geom_ribbon(aes(ymin=sr-sd_sr, ymax=sr+sd_sr, fill=year), alpha=0.25) + 
  facet_grid(disp~., scales="free_x", switch="x") + 
  tag_facets(tag_pool = c("a", "c", "e"), position = list(x = 0.035, y = 0.925)) + 
  labs(x="Annual mean temperature (°C)", y="Species richness") + 
  scale_linetype(name="Year") + 
  scale_fill_manual(name="Year", values=c("#1B9E77", "#D95F02", "#7570B3")) + 
  scale_x_continuous(expand=expansion(add=c(0,2))) + 
  scale_y_continuous(expand=expansion(add=c(0,2))) + 
  theme_bw() + theme(strip.background = element_blank(), strip.placement="outside", 
                     strip.text = element_blank(), legend.background = element_blank(), 
                     legend.position = "bottom", panel.spacing = unit(0.75, "lines"),
                     axis.text = element_text(size=12), axis.title = element_text(size=14),
                     tagger.panel.tag.text = element_text(color = "black", face="plain", size = 10),
                     tagger.panel.tag.background = element_blank())
p2 <- dat %>% filter(var == "Annual precipitation (mm)") %>%
  filter(rcp %in% c("EWEMBI", "RCP6.0")) %>%
  ggplot(aes(x=value, y=sr, lty=year)) + geom_line(colour="black") + 
  geom_ribbon(aes(ymin=sr-sd_sr, ymax=sr+sd_sr, fill=year), alpha=0.25) + 
  facet_grid(disp~., scales="free_x", switch="x") + 
  tag_facets(tag_pool = c("b", "d", "f"), position = list(x = 0.035, y = 0.925)) + 
  labs(x="Annual precipitation (mm)", y="") + scale_linetype_discrete(name="Year") + 
  scale_fill_manual(name="Year", values=c("#1B9E77", "#D95F02", "#7570B3")) + 
  scale_x_continuous(expand=expansion(add=c(0,2))) + 
  scale_y_continuous(expand=expansion(add=c(0,2))) + 
  theme_bw() + theme(strip.background = element_blank(), strip.placement="outside",
                     strip.text = element_text(size=14, face="bold"), legend.background = element_blank(),
                     axis.text = element_text(size=12), axis.title.x = element_text(size=14),
                     axis.title.y = element_blank(), legend.position = "none", 
                     panel.spacing = unit(0.75, "lines"),
                     tagger.panel.tag.text = element_text(color = "black", face="plain", size = 10),
                     tagger.panel.tag.background = element_blank())
leg <- ggpubr::as_ggplot(ggpubr::get_legend(p1))
p <- ((p1 & theme(legend.position="none")) + p2) / leg + plot_layout(heights=c(14,1))
ggsave("figures/FigureS23.png", p, width=9, height=7, dpi=300, bg="transparent")

ewembi <- dat %>% filter(rcp == "EWEMBI")
rcp26_1995 <- ewembi %>% mutate(rcp = factor(rcp, levels=c("EWEMBI"), labels=c("RCP2.6")))
rcp60_1995 <- ewembi %>% mutate(rcp = factor(rcp, levels=c("EWEMBI"), labels=c("RCP6.0")))
rcp85_1995 <- ewembi %>% mutate(rcp = factor(rcp, levels=c("EWEMBI"), labels=c("RCP8.5")))

dat_all <- dat %>% filter(rcp != "EWEMBI") %>% 
  bind_rows(list(rcp26_1995, rcp60_1995, rcp85_1995))

p1 <- dat_all %>% filter(var == "Annual mean temperature (°C)") %>%
  filter(disp == "d/8") %>%
  ggplot(aes(x=value, y=sr, lty=year)) + geom_line(colour="black") + 
  geom_ribbon(aes(ymin=sr-sd_sr, ymax=sr+sd_sr, fill=year), alpha=0.25) + 
  facet_grid(rcp~., scales="free_x", switch="x") + 
  tag_facets(tag_pool = c("a", "c", "e"), position = list(x = 0.035, y = 0.925)) + 
  labs(x="Annual mean temperature (°C)", y="Species richness") + scale_linetype_discrete(name="Year") + 
  scale_fill_manual(name="Year", values=c("#1B9E77", "#D95F02", "#7570B3")) + 
  scale_x_continuous(expand=expansion(add=c(0,2))) + 
  scale_y_continuous(expand=expansion(add=c(0,2))) + 
  theme_bw() + theme(strip.background = element_blank(), strip.placement="outside", 
                     strip.text = element_blank(), legend.background = element_blank(), 
                     legend.position = "bottom", panel.spacing = unit(0.75, "lines"),
                     axis.text = element_text(size=12), axis.title = element_text(size=14),
                     tagger.panel.tag.text = element_text(color = "black", face="plain", size = 10),
                     tagger.panel.tag.background = element_blank())
p2 <- dat_all %>% filter(var == "Annual precipitation (mm)") %>% 
  filter(disp == "d/8") %>%
  ggplot(aes(x=value, y=sr, lty=year)) + geom_line(colour="black") + 
  geom_ribbon(aes(ymin=sr-sd_sr, ymax=sr+sd_sr, fill=year), alpha=0.25) + 
  facet_grid(rcp~., scales="free_x", switch="x") + 
  tag_facets(tag_pool = c("b", "d", "f"), position = list(x = 0.035, y = 0.925)) + 
  labs(x="Annual precipitation (mm)", y="") + scale_linetype_discrete(name="Year") + 
  scale_fill_manual(name="Year", values=c("#1B9E77", "#D95F02", "#7570B3")) + 
  scale_x_continuous(expand=expansion(add=c(0,2))) + scale_y_continuous(expand=expansion(add=c(0,2))) + 
  theme_bw() + theme(strip.background = element_blank(), strip.placement="outside",
                     strip.text = element_text(size=14, face="bold"), legend.background = element_blank(),
                     axis.text = element_text(size=12), axis.title.x = element_text(size=14),
                     axis.title.y = element_blank(), legend.position = "none", 
                     panel.spacing = unit(0.75, "lines"),
                     tagger.panel.tag.text = element_text(color = "black", face="plain", size = 10),
                     tagger.panel.tag.background = element_blank())
leg <- ggpubr::as_ggplot(ggpubr::get_legend(p1))
p <- ((p1 & theme(legend.position="none")) + p2) / leg + plot_layout(heights=c(14,1))
ggsave("figures/FigureS33.png", p, width=9, height=7, dpi=300, bg="transparent")
