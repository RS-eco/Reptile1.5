#-#-# Stacked barchart 400 species data #-#-#

rm(list=ls()); invisible(gc())

library(ggplot2); library(dplyr); library(tidyr)

#-#-# Summarize frequenzies and plot as stacked bar chart #-#-#
AUCall <- readRDS("data/Variables_FinalRank_Reptile.rds")

head(AUCall)
nrow(AUCall)

#-#-# Select the ten best variable combinations to display in graph #-#-#
AUCallTop <- AUCall ## Add data here
AUCallTop$rank[AUCallTop$rank >= 4] <- 0

head(AUCallTop)

AUCallTop <- AUCallTop[,c("Species","Models","rank")]
head(AUCallTop)

AUCTopTable<-data.frame(table(AUCallTop$Models, AUCallTop$rank))
colnames(AUCTopTable) <- c("ClimateVariable","Rank","Frequency")
head(AUCTopTable)

# Drop ClimateVariable with more than 4 variables
AUCTopTable$num_var <- sapply(AUCTopTable$ClimateVariable, function(x){length(grep("bio", unlist(strsplit(as.character(x), split="_"))))})
AUCTopTable <- AUCTopTable %>% filter(num_var <= 4)

AUCTopTable$Rank <- as.numeric(as.character(AUCTopTable$Rank))
AUCTopTable <- subset(AUCTopTable,Rank >= 1)
head(AUCTopTable)
#View(AUCTopTable)

AUCTopTable$Frequency <- as.numeric(as.character(AUCTopTable$Frequency))
AUCTopTable <- aggregate(Frequency ~ ClimateVariable, AUCTopTable, sum)
AUCTopTable <- AUCTopTable[order(-AUCTopTable$Frequency),]
head(AUCTopTable)

AUCTopVariables <- AUCTopTable[1:10,]

TopVariableList <- as.vector(AUCTopVariables$ClimateVariable)

#-#-# Subset the entire results data frame choosing only the best variables #-#-#
AUCall <- AUCall

AUCall$rank[AUCall$rank >= 10] <- "Other"
head(AUCall)
nrow(AUCall)

AUCSub <- AUCall[,c("Species","Models","rank")]
head(AUCSub)

AUCFreqTable<-data.frame(table(AUCSub$Models, AUCSub$rank))
colnames(AUCFreqTable) <- c("ClimateVariable","Rank","Frequency")
head(AUCFreqTable)

AUCallFinal <- AUCFreqTable[AUCFreqTable$ClimateVariable %in% TopVariableList, ]
AUCallFinal %>% arrange(Rank, desc(Frequency)) %>% head()
nrow(AUCallFinal)

AUCallFinal <- subset(AUCallFinal, Frequency > 0)

#-#-# Set colour scheme #-#-#
PaletteBlue2 <-c('blue4','dodgerblue4','deepskyblue','gray20',
                 'gray28','gray39','gray49','gray53','gray63','gray73')

#-#-# Extract all label names #-#-#
#labellist <- as.vector(AUCFreqTable$ClimateVariable)
#labellist <- unique(labellist)

testMax <- (nrow(AUCall))/23

as.character(unique(AUCallFinal$ClimateVariable))
AUCallFinal$ClimateVariable <- factor(AUCallFinal$ClimateVariable,
                                      levels=c("bio4_bio5_bio12", "bio4_bio12_bio15", "bio6_bio12_bio15",
                                               "bio4_bio5_bio12_bio15", "bio4_bio5_bio15_bio18",
                                               "bio4_bio5_bio15_bio19", "bio4_bio5_bio18_bio19",
                                               "bio4_bio15_bio18_bio19", "bio5_bio6_bio12_bio15", 
                                               "bio6_bio15_bio18_bio19"))

#-#-# Plot the variables #-#-#
ggplot(AUCallFinal, aes(x = ClimateVariable, y = Frequency)) +
  geom_bar(aes(fill = Rank), stat="identity") +
  scale_fill_manual(values=PaletteBlue2)+
  scale_y_continuous(limits=c(0,NA), expand=expansion(add=c(0,4))) +
  guides(fill = guide_legend(ncol = 1)) + theme_bw() + coord_flip() + 
  labs(x="") + theme(axis.text=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12, face="bold"), 
                     axis.text.y=element_text(face = c(rep('plain',3),'bold', rep('plain', 6))))
ggsave(file = "figures/FigureS1.png", width = 9, height = 8, dpi = 300, bg="transparent") 
