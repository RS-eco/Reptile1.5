########################################

# Calculate change with histogram in probability for disp and year
rm(list=ls()); gc()
library(dplyr); library(ggplot2); library(patchwork); library(magrittr)

lapply(c("2050", "2080"), function(year){
  time_rcp <- expand.grid(time=year, rcp= c("rcp26", "rcp60", "rcp85")) %>% 
    tidyr::unite("time_rcp", c(rcp, time))
  time_rcp <- as.vector(time_rcp$time_rcp)
  
  delta_mean <- lapply(c("presence", "dispersal1"), function(disp){
    
    taxa <- "Reptile"
    sumProb1 <- readRDS(paste0("data/", taxa, "_prob_GAM_", disp, ".rds"))
    sumProb2 <- readRDS(paste0("data/", taxa, "_prob_GBM_", disp ,".rds"))
    sumProb <- left_join(sumProb1, sumProb2, by=c("x","y"))
    
    # Summarise by taxa
    cv_taxa <- lapply(time_rcp, function(x){
      sumProb_sub <- sumProb %>% dplyr::select(c(x,y), matches(x))
      sumProb_sub$CV <- apply(sumProb_sub[,-c(1,2)], 1, raster::cv, na.rm=TRUE)
      sumProb_sub %<>% dplyr::select(c(x,y,CV))
      sumProb_sub$time_rcp <- x
      return(sumProb_sub)
    })
    cv_taxa %<>% bind_rows()
    cv_taxa$time_rcp <- factor(cv_taxa$time_rcp, labels=c(paste0(year, " RCP2.6"), 
                                                          paste0(year, " RCP6.0"),
                                                          paste0(year, " RCP8.5")))
    cv_taxa$x <- round(cv_taxa$x/2.5)*2.5
    cv_taxa$y <- round(cv_taxa$y/2.5)*2.5
    cv_taxa %<>% group_by(x,y,time_rcp) %>% summarise(CV=max(CV)) %>% filter(CV <= 10)
    #cv_taxa %<>% group_by(x,y,time_rcp) %>% summarise(CV=max(CV)) %>% filter(CV <= 25)
    
    deltaChange <- sumProb %>% mutate_at(vars("GFDL.ESM2M_rcp26_2050.x":"MIROC5_rcp85_2080.y"), 
                                         funs(. - EWEMBI_1995.x)) %>% 
      dplyr::select(-c(EWEMBI_1995.x, EWEMBI_1995.y))
    
    # Calculate sign of change
    sign_change <- lapply(time_rcp, function(x){
      deltaProb_sub <- deltaChange %>% dplyr::select(c(x,y), matches(x)) %>% 
        tidyr::gather(var, value, -c(x,y)) %>% filter(value > 0) %>% group_by(x,y) %>% 
        summarise (n = n()) %>% mutate(freq = (n/8)*100)
      deltaProb_sub$time_rcp <- x
      return(deltaProb_sub)
    })
    sign_change %<>% bind_rows
    sign_change$time_rcp <- factor(sign_change$time_rcp, labels=c(paste0(year, " RCP2.6"), 
                                                                  paste0(year, " RCP6.0"),
                                                                  paste0(year, " RCP8.5")))
    
    # Select 5 out of 8 and reduce resolution
    #sign_sub <- sign_change %>% ungroup() %>%
    #  mutate(x=round(x/2.5)*2.5, y=round(y/2.5)*2.5) %>% group_by(x,y,time_rcp) %>% 
    #  summarise(max=max(freq), min=min(freq)) %>% filter(min >= 62.5 | max <= 37.5) 
    
    # Select 6 out of 8 and reduce resolution
    sign_sub <- sign_change %>% ungroup() %>%
      mutate(x=round(x/2.5)*2.5, y=round(y/2.5)*2.5) %>% group_by(x,y,time_rcp) %>% 
      summarise(max=max(freq), min=min(freq)) %>% filter(min >= 75 | max <= 25) 
    
    # Calculate delta mean
    delta_mean <- lapply(time_rcp, function(x){
      deltaProb_sub <- deltaChange %>% dplyr::select(c(x,y), matches(x))
      deltaProb_sub$mean <- deltaProb_sub %>% dplyr::select(-c("x","y")) %>% 
        apply(1, mean, na.rm=TRUE)
      deltaProb_sub %<>% dplyr::select(c(x,y,mean))
      deltaProb_sub$time_rcp <- x
      return(deltaProb_sub)
    })
    delta_mean %<>% bind_rows
    delta_mean$time_rcp <- factor(delta_mean$time_rcp, labels=c(paste0(year, " RCP2.6"), 
                                                                paste0(year, " RCP6.0"),
                                                                paste0(year, " RCP8.5")))
    
    delta_all <- delta_mean %>% group_by(x,y,time_rcp) %>% 
      summarise(total=sum(mean))
    delta_x <- delta_mean %>% group_by(x, time_rcp) %>% 
      summarise(mn=mean(mean, na.rm=T))
    delta_y <- delta_mean %>% group_by(y, time_rcp) %>% 
      summarise(mn=mean(mean, na.rm=T))
    data(outline, package="ggmap2")
    library(sf)
    outline <- sf::st_as_sf(outline)
    col_val <- scales::rescale(unique(c(seq(min(delta_all$total), 0, length=3), 
                                        seq(0, max(delta_all$total), length=3))))
    lim_map <- c(min(delta_all$total), max(delta_all$total))
    lim_histx <- unlist(c(delta_x %>% ungroup() %>% summarise(min(mn)), 
                          delta_x %>% ungroup() %>% summarise(max(mn))))
    lim_histy <- unlist(c(delta_y %>% ungroup() %>% summarise(min(mn)), 
                          delta_y %>% ungroup() %>% summarise(max(mn))))
    p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank() + theme_classic() + 
      theme(legend.position="none", axis.title = element_blank(),
            axis.line = element_blank(), axis.ticks = element_blank(), 
            axis.text = element_blank())
    p2 <- lapply(c("RCP2.6", "RCP6.0", "RCP8.5"), function(rcp){
      ggplot() + geom_histogram(data=delta_x[delta_x$time_rcp == paste(year, rcp),], 
                                aes(x=x, y=mn), width=1, stat="identity", position="stack", colour=NA) + 
        scale_y_continuous(position = "right", expand=c(0,0), limits=lim_histx,
                           breaks=c(-30, -10, 0), labels=c(-30, -10, 0)) + 
        scale_x_continuous(expand=c(0,0)) + scale_fill_grey() + theme_classic() + 
        theme(legend.position="none", axis.title = element_blank(),
              axis.line.x = element_blank(), axis.ticks.x = element_blank(), 
              axis.text.x = element_blank(), axis.text.y = element_text(size=16))
    })
    
    p3 <- lapply(c("RCP2.6", "RCP6.0", "RCP8.5"), function(rcp){
      ggplot() + geom_histogram(data=delta_y[delta_y$time_rcp == paste(year, rcp),], 
                                aes(x=y, y=mn), width=1, stat="identity", position="stack", colour=NA) + 
        scale_y_reverse(limits=rev(lim_histy), breaks=c(0,-25, -50), labels=c(0, "", -50)) + 
        scale_x_continuous(expand=c(0,0)) + scale_fill_grey(name="Taxa", labels=c("Reptile")) + 
        coord_flip(expand=FALSE) + theme_classic() + 
        theme(legend.position="bottom", legend.title=element_text(size=16, face="bold"),
              legend.text=element_text(size=16),
              plot.background = element_rect(fill = "transparent"), 
              axis.title = element_blank(), axis.line.y = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(),
              axis.text.x = element_text(size=16)) + 
        guides(fill = guide_legend(direction = "vertical"))
    })
    
    p4 <- lapply(c("RCP2.6", "RCP6.0", "RCP8.5"), function(rcp){
      data <- delta_all[delta_all$time_rcp == paste(year, rcp),]
      # Make map
      ggplot() + geom_tile(data=data, aes(x=x, y=y, fill=total)) + 
        #geom_point(data=sign_sub, aes(x=x,y=y), colour="grey50", fill="transparent", size=0.03) + 
        geom_point(data=cv_taxa, aes(x=x,y=y), colour="grey50", fill="transparent", size=0.03) + 
        geom_sf(data=outline, fill="transparent", colour="black") + 
        scale_fill_gradientn(name="", 
                             colours=rev(colorRampPalette(
                               c("#00007F", "blue", "#007FFF", "cyan", 
                                 "white", "yellow", "#FF7F00", "red", "#7F0000"))(255)),
                             na.value= "grey50", values=col_val, limits=lim_map) + 
        coord_sf(expand=F, xlim=c(min(data$x), max(data$x)), ylim=c(min(data$y),max(data$y)), 
                 ndiscr=0) + theme_classic() + 
        theme(axis.title = element_blank(),axis.line = element_blank(),
              axis.ticks = element_blank(), axis.text = element_blank(),
              plot.background = element_rect(fill = "transparent"), 
              legend.background = element_rect(fill = "transparent"), 
              legend.key.width=unit(2, "cm"), legend.position="bottom",
              legend.text = element_text(size=16), 
              legend.box.background = element_rect(fill = "transparent", colour=NA))
    })
    
    library(cowplot)
    legend <- ggdraw(get_legend(p4[[1]]))
    
    pall <- {{{p + ggtitle("a)") + theme(plot.title=element_text(size=20, vjust=-10)) + p2[[1]] + 
        p3[[1]] + p4[[1]] + plot_layout(ncol=2, widths=c(1,8), heights=c(1,4)) & 
        theme(legend.position="none")} - {p + ggtitle("b)") + 
            theme(plot.title=element_text(size=20, vjust=-10)) + p2[[2]] + 
            p3[[2]] + p4[[2]] + plot_layout(ncol=2, widths=c(1,8), heights=c(1,4)) & 
            theme(legend.position="none")} + {p + ggtitle("c)") + 
                theme(plot.title=element_text(size=20, vjust=-10)) + p2[[3]] + 
                p3[[3]] + p4[[3]] + plot_layout(ncol=2, widths=c(1,8), heights=c(1,4)) & 
                theme(legend.position="none")} + 
        plot_layout(ncol=1)} + {legend + plot_layout(ncol=1)} +
        plot_layout(ncol=1, heights=c(3,3,3,1))}
    
    ggsave(paste0("figures/delta_sr_", year, "_", disp, "_10CV.png"), pall,
           width=7.5, height=12, unit="in", dpi=600, bg="transparent")
    #ggsave(paste0("figures/delta_sr_", year, "_", disp, "_75agreement.png"), pall,
    #       width=7.5, height=9, unit="in", dpi=600, bg="transparent")
    
  })
})

########################################