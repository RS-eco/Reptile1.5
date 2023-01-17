#' ---
#' title: "Species richness for novel and lost climate spaces"
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

#' ### Load temperature and precipitation data ###

# Load rISIMIP package
#if(!"rISIMIP" %in% installed.packages()[,"Package"]) 
#  remotes::install_github("RS-eco/rISIMIP", build_vignettes = TRUE)
library(rISIMIP)

# Specify parameters
rcp <- c("rcp26", "rcp60", "rcp85")
year <- c(2050, 2080)
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

ewembi_1995 <- climatedata %>% filter(gcm_rcp_year == "EWEMBI_1995") %>%
  rename(time_rcp = year) %>% mutate(time_rcp = as.character(time_rcp))
climfut <- climatedata %>% filter(gcm_rcp_year != "EWEMBI_1995") %>% 
  group_by(x,y,rcp,year,gcm_rcp_year) %>% 
  summarise_at(c("bio1", "bio12"), mean)
climfut$time_rcp <- paste0(climfut$year, " ", substr(toupper(climfut$rcp), 1,4), ".", 
                           substr(toupper(climfut$rcp),5,5))
climatedata <- bind_rows(ewembi_1995, climfut) %>%
  select(-c(year,rcp)); rm(ewembi_1995, climfut); rm(list=climatefiles2); gc()

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
  group_by(time_rcp, bio1, bio12) %>%
  summarise(sr = mean(sum))

pres <- dat %>% filter(time_rcp == "1995") %>% ungroup()
fut <- dat %>% filter(time_rcp != "1995") %>% ungroup()
colnames(pres)[4] <- "pres"
colnames(fut)[4] <- "fut"

dat_wide <- full_join(pres, fut) %>% mutate(change = fut - pres)
lost_clim <- dat_wide %>% filter(is.na(fut)) %>%
  mutate(lost = "Lost climate") %>% mutate(sr = pres) %>%
  select(bio1, bio12, time_rcp, lost, sr)
novel_clim <- dat_wide %>% drop_na(time_rcp) %>% filter(is.na(pres)) %>%
  mutate(lost = "Novel climate") %>% mutate(sr = fut) %>%
  select(bio1, bio12, time_rcp, lost, sr)
lost_novel_clim <- bind_rows(lost_clim, novel_clim)
lost_novel_clim$time_rcp[is.na(lost_novel_clim$time_rcp)] <- "1995"

mean_dat <- lost_novel_clim %>% group_by(time_rcp) %>%
  dplyr::summarise(median_sr = median(sr), 
                   mean_sr = round(mean(sr),2),
                   sd = sd(sr),
                   se = sd(sr)/sqrt(length(sr)),
                   n = n())
mean_dat

# Make plot
p <-ggstatsplot::ggbetweenstats(data=lost_novel_clim, x = time_rcp, y = sr, results.subtitle = FALSE, 
                                bf.message = F, centrality.plotting = F) + 
  labs(x="", y="Species richness", caption=NULL) + 
  ggplot2::geom_point(aes(x=time_rcp, y=mean_sr), data = mean_dat, 
                      inherit.aes = FALSE, size=4, col="red4") +
  ggrepel::geom_label_repel(aes(x=time_rcp, y=mean_sr, label=mean_sr),
                            data = mean_dat, min.segment.length = 0, size=3, inherit.aes = FALSE) + 
  scale_x_discrete(name="", labels=paste0(mean_dat$time_rcp, "\n (n = ", mean_dat$n, ")")) + 
  scale_y_continuous(limits=c(-1,NA), expand=expansion(add=c(0,12))) + 
  scale_colour_manual(name="", values=c("black", "#E7298A", "#E7298A", "#E7298A", "#E7298A", 
                                        "#E7298A", "#E7298A")) + 
  theme_bw() + theme(axis.line = element_line(size = 0.5, colour = "black"),
                     panel.background = element_blank(), strip.background = element_blank(),
                     text=element_text(size = 16), strip.text = element_text(face="bold", size=16),
                     axis.text=element_text(colour="black", size = 12), legend.position = "none")
leg <- p + theme(legend.position="bottom") + 
  scale_colour_manual(name="",  values=c("black", "#E7298A", "#E7298A", "#E7298A", "#E7298A", 
                                         "#E7298A", "#E7298A"), 
                      labels=c("Lost climate", "Novel climate"), limits=c("1995", "2050 RCP2.6"))
leg <- leg %>% ggpubr::get_legend() %>% ggpubr::as_ggplot()
p / leg + plot_layout(heights=c(14,1))
ggsave("figures/lost_novel_clim.png", width=9, height=6, dpi=300, bg="transparent")
