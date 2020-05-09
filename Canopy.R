



#--------------------------------------------
# Canopy cover
# 08 May 2020
#PEGF
#--------------------------------------------
#

library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

canopycover<- read.csv("canopy.csv")
canopycover


# QPA CHLA ----------------------------------------------------------------

canopy <- canopycover %>% select(TimeCanopy, QPACanopyLog, QPBCanopyLog)
canopy <- na.omit(canopy)

QPACanopy.mod  <- lm(QPACanopyLog~ TimeCanopy, data=canopy)
summary(QPACanopy.mod)

canopy$QPACanopyresid<- QPACanopy.mod$resid
canopy

1/apply(canopy, 2, sd)

cc1 <- ggplot(canopy, aes(TimeCanopy,
                       y=QPACanopyLog))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('') + ylab("Canopy openness (%)") + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
#  ylim(-2,1.5) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
cc1



# QPB CHL-A ---------------------------------------------------------------

QPBCanopy.mod  <- lm(QPBCanopyLog~ TimeCanopy, data=canopy)
summary(QPBCanopy.mod)

canopy$QPBCanopyresid<- QPBCanopy.mod$resid
canopy

1/apply(canopy, 2, sd)

cc2 <- ggplot(canopy, aes(TimeCanopy,
                          y=QPBCanopyLog))+
  geom_point(size = 3) + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('') + ylab("Canopy openness (%)") + # axis x
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  #  ylim(-2,1.5) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
cc2

canopylong <- cc1 / cc2
canopylong + ggsave("Canopy.jpeg", width=6, height=10,dpi=600)



# Long term ---------------------------------------------------------------





