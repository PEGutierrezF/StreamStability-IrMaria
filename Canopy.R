



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

canopyreg <- cc1 / cc2
canopyreg + ggsave("Canopy.jpeg", width=6, height=10,dpi=600)



# Long term ---------------------------------------------------------------

canopycover
# QPA
cc3 <- ggplot(canopycover,aes(TimeCanopy,
                       y=QPACanopy))+
  
  annotate(geom = "rect",xmin=1,xmax=18,ymin=7.72,ymax=18.83,alpha = 0.4,fill = "grey") +
  
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPACanopy-QPAsdCanopy, ymax=QPACanopy+QPAsdCanopy), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 13.28, xend = 6, yend = 13.28))+
  geom_segment((aes(x = 7, y = 13.28, xend = 18, yend = 13.28)), color="red", linetype="dashed", size=1) +
  
  xlab('')+ ylab("Canopy openness (%)") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(5,100) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

cc3

# QPB

cc4 <- ggplot(canopycover,aes(TimeCanopy,
                              y=QPBCanopy))+
  
  annotate(geom = "rect",xmin=1,xmax=18,ymin=8.79,ymax=14.60,alpha = 0.4,fill = "grey") +
  
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin=QPBCanopy-QPBsdCanopy, ymax=QPBCanopy+QPBsdCanopy), width=.2,
                position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 11.69, xend = 6, yend = 11.69))+
  geom_segment((aes(x = 7, y = 11.69, xend = 18, yend = 11.69)), color="red", linetype="dashed", size=1) + 
  
  xlab('')+ ylab("Canopy openness (%)") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(5,100) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

cc4

canopyLong <- cc3 / cc4
canopyLong + ggsave("canopyOpenness.jpeg", width=6, height=10,dpi=600)



