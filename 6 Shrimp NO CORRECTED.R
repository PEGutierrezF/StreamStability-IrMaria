



#--------------------------------------------
# Shrimp no corrected
# 21 May 2020
#PEGF
#--------------------------------------------
#

library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

ShrimpNC<- read.csv("6 ShrimpNOCorrected.csv")
ShrimpNC


# Lm QPA Leaf Litter ------------------------------------------------------

ShrimpNC <- ShrimpNC %>% select(TimeCHLA, QPAShrimpLog, QPBShrimpLog)
ShrimpNC <- na.omit(ShrimpNC)
ShrimpNC

QPAShrimpNC.mod  <- lm(QPAShrimpLog ~ TimeCHLA, na.action=na.omit, data=ShrimpNC)
summary(QPAShrimpNC.mod)

ShrimpNC$QPAresid<- QPAShrimpNC.mod$resid
ShrimpNC

1/apply(ShrimpNC, 2, sd)



# Shrimp QPB --------------------------------------------------------------


QPBShrimpNC.mod  <- lm(QPBShrimpLog ~ TimeCHLA, na.action=na.omit, data=ShrimpNC)
summary(QPBShrimpNC.mod)

ShrimpNC$QPBresid<- QPBShrimpNC.mod$resid
ShrimpNC

1/apply(ShrimpNC, 2, sd)





# Graph Shrimp vs Shrimp no corrected -------------------------------------



Sregression<- read.csv("6 Shrimp regressions.csv")
Sregression

RegressionS <- ggplot(Sregression, aes(Time, 
                                     value, 
                                     shape= variable , 
                                     col = stream)) + 
  geom_point(size = 5) +
  geom_smooth(method = 'lm', se = F,  aes(color=stream)) + #, alpha = .15, aes(fill = stream)) +
  
  
  xlab('Sampling period') + ylab("Resilience") + 
  theme(axis.title.x = element_text(size = 16, angle = 0)) +# axis x
  theme(axis.title.y = element_text(size = 16, angle = 90)) +
  
  theme(legend.title = element_text(color = "black", size = 14),  # Legend
        legend.text = element_text(color = "black", size = 12)) +
  
  geom_hline(yintercept = 0, color="darkred") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  labs(title="", subtitle="LN(Vdis/Vcont)",fill="", caption="") +
  
  ylim(-3.5,3.5) +
  
  theme(strip.text.x  = element_text(size = 14, colour = "black", angle = 0)) +
  theme(strip.background = element_rect(colour = "black", fill = "grey94")) +
  
  facet_grid(~ variable)
# facet_wrap(. ~ variable)
RegressionS
RegressionS + ggsave("RegressionSrimp.jpeg", width=10, height=6,dpi=600)


# Long term Shrimp --------------------------------------------------------

ShrimpNC
# QPA
S3 <- ggplot(ShrimpNC,aes(TimeCHLA,
                              y=QPAShrimp))+
  
  annotate(geom = "rect",xmin=1,xmax=36,ymin=14.32,ymax=22.73,alpha = 0.4,fill = "grey") +
  
  geom_point() + 
  geom_line() +
#  geom_errorbar(aes(ymin=QPACanopy-QPAsdCanopy, ymax=QPACanopy+QPAsdCanopy), width=.2,
 #               position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 18.53, xend = 6, yend = 18.53))+
  geom_segment((aes(x = 7, y = 18.53, xend = 36, yend = 18.53)), color="red", linetype="dashed", size=1) +
  
  xlab('')+ ylab("Canopy cover (%)") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(5,45) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

S3


S4 <- ggplot(ShrimpNC,aes(TimeCHLA,
                          y=QPBShrimp))+
  
  annotate(geom = "rect",xmin=1,xmax=36,ymin=8.03,ymax=11.72,alpha = 0.4,fill = "grey") +
  
  geom_point() + 
  geom_line() +
  #  geom_errorbar(aes(ymin=QPACanopy-QPAsdCanopy, ymax=QPACanopy+QPAsdCanopy), width=.2,
  #               position=position_dodge(0.05)) + 
  geom_segment(aes(x = 1, y = 9.88, xend = 6, yend = 9.88))+
  geom_segment((aes(x = 7, y = 9.88, xend = 36, yend = 9.88)), color="red", linetype="dashed", size=1) +
  
  xlab('')+ ylab("Canopy cover (%)") +
  theme(axis.title.y = element_text(size = 18, angle = 90)) +
  
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  ylim(5,45) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

S4


ShrimpNCorrected <- S3 / S4
ShrimpNCorrected + ggsave("CShrimpNCorrected.jpeg", width=6, height=10,dpi=600)
