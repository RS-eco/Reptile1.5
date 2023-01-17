#' ---
#' title: "Future richness changes among different reptile groups"
#' author: "RS-eco"
#' ---

#' ### Load packages ###

rm(list=ls()); invisible(gc())

# Load packages
library(tidyverse); library(patchwork); library(magrittr)

#' ### Load raw reptile data ###

#remotes::install_github("RS-eco/rasterSp")
library(rasterSp)
data(gard_reptiles)
unique(length(gard_reptiles$Binomial))

table(gard_reptiles$Group)

#########################

#' ### Load modelled prob data

sumProb1 <- read.csv("data/Reptile_prob_GAM_dispersal1.csv.xz")
sumProb1$model_type <- "GAM"
sumProb2 <- read.csv("data/Reptile_prob_GBM_dispersal1.csv.xz")
sumProb2$model_type <- "GBM"
sumProb <- bind_rows(sumProb1, sumProb2); rm(sumProb1, sumProb2); gc()
head(sumProb)

changeProb <- sumProb %>% 
  mutate_at(vars("EWEMBI_1995":"MIROC5_rcp85_2080"), funs(. - EWEMBI_1995)) %>% 
  mutate(EWEMBI_rcp26_1995=EWEMBI_1995, EWEMBI_rcp60_1995=EWEMBI_1995, EWEMBI_rcp85_1995=EWEMBI_1995) %>%
  gather(gcm_rcp_year, value, -c(x, y, model_type)) %>% 
  separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
  separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
head(changeProb)

sumProb %<>% mutate(EWEMBI_rcp26_1995=EWEMBI_1995, EWEMBI_rcp60_1995=EWEMBI_1995, EWEMBI_rcp85_1995=EWEMBI_1995) %>% 
  gather(gcm_rcp_year, value, -c(x, y, model_type)) %>% 
  separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
  separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
head(sumProb)

#########################

#' ### Figure with SR over time

#' Standard deviation

#sumProb %>% group_by(x, y, year, rcp) %>% summarise(mn_value=mean(value)) %>%
#  ungroup() %>% group_by(year, rcp) %>% summarise(mean=mean(mn_value), sd=sd(mn_value)) %>%
#  ggplot() + geom_pointrange(aes(x=year, y=mean, ymin=mean-sd, ymax=mean+sd, colour=rcp))

#' Standard error

#se <- function(x) sqrt(var(x)/length(x))

# Standard error of the mean
sem <- function(x) sd(x)/sqrt(length(x))

#sumProb %>% group_by(x, y, year, rcp) %>% summarise(mn_value=mean(value)) %>%
#  ungroup() %>% group_by(year, rcp) %>% summarise(mean=mean(mn_value), se=se(mn_value)) %>%
#  ggplot() + geom_pointrange(aes(x=year, y=mean, ymin=mean-se, ymax=mean+se, colour=rcp))

# Calculate mean across gcm, algorithm and location
#sumProb %>% group_by(year, rcp) %>% summarise(mean=mean(value), se=se(value)) %>%
#  ggplot() + geom_pointrange(aes(x=year, y=mean, ymin=mean+se, ymax=mean+se, colour=rcp))

#' Confidence interval

###
# Check distribution of data for confidence interval calculation!!!
###

# ci <- function(x) qt(0.975,df=length(x)-1)*sd(x)/sqrt(length(x)) # https://www.cyclismo.org/tutorial/R/confidence.html

#95% confidence intervals of the mean
ci <- function(x) 2*(sd(x)/sqrt(length(x)))

# see https://stackoverflow.com/questions/35953394/calculating-length-of-95-ci-using-dplyr
#ci <- function(x) (mean(x) + qt(1 - (0.05 / 2), length(x) - 1) * sem)

library(forcats)

#' ### Figures with changes in SR over time

# Violin plot
sumProb %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>%
  group_by(x, y, year, rcp) %>% summarise(mn_value=mean(value)) %>%
  ggplot() + geom_violin(aes(x=year, y=mn_value, colour=rcp)) + 
  coord_trans(y = "log2") + scale_y_continuous(breaks=c(1,10,15,20,25,50,100)) + 
  scale_color_manual(name="", values=c("#2c7bb6", "#fdae61", "#d7191c", "black")) + 
  theme_bw() + labs(x="Year", y="Mean summed probability")

# Point-line graph
sumProb %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>%
  group_by(x, y, year, rcp) %>% summarise(mn_value=mean(value)) %>%
  ungroup() %>% group_by(year, rcp) %>% summarise(mean=mean(mn_value), sem=sem(mn_value)) %>%
  ggplot() + geom_pointrange(aes(x=as.numeric(year), y=mean, ymin=mean-sem, 
                                 ymax=mean+sem, colour=rcp)) + 
  geom_path(aes(x=as.numeric(year), y=mean, colour=rcp)) + 
  scale_x_continuous(breaks=c(1995, 2050, 2080)) + 
  scale_color_manual(name="", values=c("#2c7bb6", "#fdae61", "#d7191c", "black")) + 
  theme_bw() + labs(x="Year", y="Mean summed probability")
ggsave("figures/sumProb_change.png", dpi=600, width=8, height=6)

sumProb_1995 <- sumProb %>% filter(gcm == "EWEMBI") %>% select(-gcm) %>%
  mutate(GFDL.ESM2M = 1, HadGEM2.ES = 1, IPSL.CM5A.LR = 1, MIROC5 = 1) %>%
  gather(gcm, num, -c(x,y,model_type, rcp, year, value)) %>% select(-num)
sumProb %>% filter(gcm != "EWEMBI") %>% bind_rows(sumProb_1995) %>% 
  mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>%
  group_by(year, rcp, gcm, model_type) %>% summarise(mean=mean(value), sem=sem(value)) %>%
  ggplot() + geom_pointrange(aes(x=as.numeric(year), y=mean, ymin=mean-sem, 
                                 ymax=mean+sem, colour=rcp, shape=model_type)) + 
  geom_path(aes(x=as.numeric(year), y=mean, linetype=model_type, colour=rcp)) + 
  facet_wrap(.~gcm, scales = "free_y") +
  scale_x_continuous(breaks=c(1995, 2050, 2080)) + 
  scale_color_manual(name="", values=c("#2c7bb6", "#fdae61", "#d7191c", "black")) + 
  theme_bw() + labs(x="Year", y="Mean summed probability") + 
  theme(strip.background = element_blank(), strip.text = element_text(size=12, face="bold"), 
        panel.spacing = unit(1, "lines"))
ggsave("figures/sumProb_change_gcm.png", dpi=600, width=8, height=6)

sumProb %>% filter(gcm != "EWEMBI") %>% bind_rows(sumProb_1995) %>% 
  mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  drop_na() %>% ggplot() + geom_violin(aes(x=year, y=value, colour=rcp), draw_quantiles = 0.5) + 
  facet_wrap(.~gcm, scales = "free_y") + 
  scale_color_manual(name="", values=c("#2c7bb6", "#fdae61", "#d7191c", "black")) + 
  theme_bw() + labs(x="Year", y="Mean summed probability") + 
  theme(strip.background = element_blank(), strip.text = element_text(size=12, face="bold"), 
        panel.spacing = unit(1, "lines"))

## Time-series of change (not really meaningful)

changeProb %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>%
  group_by(x, y, year, rcp) %>% summarise(mn_value=mean(value)) %>%
  ungroup() %>% group_by(year, rcp) %>% summarise(mean=mean(mn_value), sem=sem(mn_value)) %>%
  ggplot() + geom_pointrange(aes(x=as.numeric(year), y=mean, ymin=mean-sem, ymax=mean+sem, colour=rcp)) + 
  geom_path(aes(x=as.numeric(year), y=mean, group=rcp, colour=rcp)) + 
  scale_x_continuous(breaks=c(1995, 2050, 2080)) + 
  scale_color_manual(name="", values=c("#2c7bb6", "#fdae61", "#d7191c", "black")) + 
  theme_bw() + labs(x="Year", y="Mean summed probability")

changeProb_1995 <-changeProb %>% filter(gcm == "EWEMBI") %>% select(-gcm) %>%
  mutate(GFDL.ESM2M = 1, HadGEM2.ES = 1, IPSL.CM5A.LR = 1, MIROC5 = 1) %>%
  gather(gcm, num, -c(x,y,model_type, rcp, year, value)) %>% select(-num)
changeProb %>% filter(gcm != "EWEMBI") %>% bind_rows(changeProb_1995) %>% 
  mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>%
  group_by(year, rcp, gcm, model_type) %>% summarise(mean=mean(value), sem=sem(value)) %>%
  ggplot() + geom_pointrange(aes(x=as.numeric(year), y=mean, ymin=mean-sem, ymax=mean+sem, colour=rcp, shape=model_type)) + 
  geom_path(aes(x=as.numeric(year), y=mean, linetype=model_type, colour=rcp)) + 
  facet_wrap(.~gcm, scales = "free_y") +
  scale_x_continuous(breaks=c(1995, 2050, 2080)) + 
  scale_color_manual(name="", values=c("#2c7bb6", "#fdae61", "#d7191c", "black")) + 
  theme_bw() + labs(x="Year", y="Change in mean summed probability") + 
  theme(strip.background = element_blank(), strip.text = element_text(size=12, face="bold"), 
        panel.spacing = unit(1, "lines"))

#########################

#' ### Split data by groups ###

sumProb1 <- read.csv("data/Reptile_prob_GAM_dispersal1_groups.csv.xz")
sumProb1$model_type <- "GAM"
head(sumProb1)
sumProb2 <- read.csv("data/Reptile_prob_GBM_dispersal1_groups.csv.xz")
sumProb2$model_type <- "GBM"
sumProb_groups <- bind_rows(sumProb1, sumProb2); rm(sumProb1, sumProb2); gc()
head(sumProb_groups)
unique(sumProb_groups$Group)

changeProb_groups <- sumProb_groups %>% 
  mutate_at(vars("EWEMBI_1995":"MIROC5_rcp85_2080"), funs(. - EWEMBI_1995)) %>% 
  mutate(EWEMBI_rcp26_1995=EWEMBI_1995, EWEMBI_rcp60_1995=EWEMBI_1995, EWEMBI_rcp85_1995=EWEMBI_1995) %>%
  gather(gcm_rcp_year, value, -c(x, y, model_type)) %>% 
  separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
  separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
head(changeProb_groups)

sumProb_groups %<>% mutate(EWEMBI_rcp26_1995=EWEMBI_1995, EWEMBI_rcp60_1995=EWEMBI_1995, EWEMBI_rcp85_1995=EWEMBI_1995) %>% 
  gather(gcm_rcp_year, value, -c(x, y, model_type)) %>% 
  separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
  separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
head(sumProb_groups)

#########################

#' ###  Split data by realms ###

# Turn sumProb into raster
r_sumProb <- sumProb %>% mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
  mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>% group_by(x,y,rcp,year) %>% summarise(value=mean(value)) %>% 
  unite("rcp_year", rcp:year) %>% spread(rcp_year, value) %>% raster::rasterFromXYZ()
rm(sumProb, changeProb); gc()

# Load realm data
library(geodat)
data(zoorealms)

# Extract mean value per realm
#remotes::install_github('isciences/exactextractr')
library(exactextractr)
name_sP <- names(r_sumProb) # "EWEMBI_1995" "RCP2.6_1995" "RCP2.6_2050" "RCP2.6_2080" "RCP6.0_1995" "RCP6.0_2050" "RCP6.0_2080" "RCP8.5_1995" "RCP8.5_2050" "RCP8.5_2080"
mn_sumProb_realm <- exact_extract(r_sumProb,zoorealms, c("mean", "stdev", "count"))

mn_sumProb_realm %>% mutate(Realm = zoorealms$Realm) %>% 
  gather(var, value, -Realm) %>% 
  separate(var, c("stat", "rcp_year"), sep="[.]", fill="right", extra="merge") %>% 
  separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop") %>% 
  spread(stat, value) %>% arrange(desc(rcp)) %>% 
  mutate(se=stdev/sqrt(count)) %>% 
  ggplot() + geom_pointrange(aes(x=as.numeric(year), y=mean, ymin=mean-se, ymax=mean+se, colour=rcp)) + 
  geom_path(aes(x=as.numeric(year), y=mean, group=rcp, colour=rcp)) + facet_wrap(.~Realm, scales="free_y") + 
  scale_x_continuous(breaks=c(1995, 2050, 2080)) + 
  scale_color_manual(name="", values=rev(c("#2c7bb6", "#fdae61", "#d7191c", "black"))) + theme_bw() + 
  theme(legend.position=c(0.875,0.165), strip.background = element_blank(),
        strip.text=element_text(size=12, face="bold")) + labs(x="Year", y="Mean summed probability")
ggsave("figures/sumProb_change_realm.png", dpi=600, width=8, height=6)

r_dat <- raster::stack(r_sumProb, rasterDT::fasterizeDT(zoorealms, r_sumProb[[1]]))
r_dat <- as.data.frame(raster::rasterToPoints(r_dat)) %>% tidyr::drop_na() %>%
  rename(realm = layer) %>% mutate(realm = factor(realm, labels=zoorealms$Realm))
r_dat %>% gather(rcp_year, value, -c(x, y, realm)) %>% 
  separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")  %>% 
  filter(year != 1995 | rcp == "EWEMBI") %>%
  ggplot() + geom_violin(aes(x=year, y=value, colour=rcp), draw_quantiles=0.5) + 
  facet_wrap(realm~., scales="free_y") + 
  scale_color_manual(name="", values=rev(c("#2c7bb6", "#fdae61", "#d7191c", "black"))) + theme_bw() + 
  theme(legend.position=c(0.875,0.165), strip.background = element_blank(),
        strip.text=element_text(size=12, face="bold")) + labs(x="Year", y="Mean summed probability")
ggsave("figures/sumProb_change_realm_viol.png", dpi=600, width=8, height=6)

#########################

# Split data by dispersal scenario!!!

# rm(list=ls())
gc()

#' ### Load modelled prob data

sumProb <- lapply(c("presence", "dispersal1", "dispersal2", "dispersal3", "dispersal4", "fulldisp"), function(disp){
  sumProb1 <- read.csv(paste0("data/Reptile_prob_GAM_", disp, ".csv.xz"))
  sumProb1$model_type <- "GAM"
  sumProb2 <- read.csv(paste0("data/Reptile_prob_GBM_", disp, ".csv.xz"))
  sumProb2$model_type <- "GBM"
  sumProb <- bind_rows(sumProb1, sumProb2); rm(sumProb1, sumProb2); gc()
  sumProb$disp <- disp
  sumProb %<>% mutate(EWEMBI_rcp26_1995=EWEMBI_1995, EWEMBI_rcp60_1995=EWEMBI_1995, EWEMBI_rcp85_1995=EWEMBI_1995) %>% 
    gather(gcm_rcp_year, value, -c(x, y, model_type, disp)) %>% 
    separate(gcm_rcp_year, c("gcm", "rcp_year"), sep="_", fill="right", extra="merge") %>%
    separate(rcp_year, c("rcp", "year"), sep="_", fill="left", extra="drop")
  sumProb_1995 <- sumProb %>% filter(gcm == "EWEMBI") %>% select(-gcm) %>%
    mutate(GFDL.ESM2M = 1, HadGEM2.ES = 1, IPSL.CM5A.LR = 1, MIROC5 = 1) %>%
    gather(gcm, num, -c(x,y,model_type, disp, rcp, year, value)) %>% select(-num)
  sumProb %>% filter(gcm != "EWEMBI") %>% bind_rows(sumProb_1995) %>% 
    mutate(rcp = factor(rcp, labels=c("RCP2.6", "RCP6.0", "RCP8.5"))) %>% 
    mutate(rcp = fct_explicit_na(rcp, "EWEMBI")) %>%
    group_by(year, rcp, gcm, disp, model_type) %>% summarise(mean=mean(value), sem=sem(value))
}); gc()
sumProb <- bind_rows(sumProb)

p <- sumProb %>%
  ggplot() + geom_pointrange(aes(x=as.numeric(year), y=mean, ymin=mean-sem, 
                                 ymax=mean+sem, colour=rcp, shape=disp)) + 
  geom_path(aes(x=as.numeric(year), y=mean, linetype=disp, colour=rcp)) + 
  facet_grid(model_type~gcm, scales = "free_y") +
  scale_x_continuous(breaks=c(1995, 2050, 2080)) + 
  scale_color_manual(name="", values=c("#2c7bb6", "#fdae61", "#d7191c", "black")) + 
  theme_bw() + labs(x="Year", y="Mean summed probability") + 
  theme(strip.background = element_blank(), strip.text = element_text(size=12, face="bold"), 
        panel.spacing = unit(1, "lines"))
ggsave("figures/sumProb_change_gcm_disp.png", dpi=600, width=8, height=6)
