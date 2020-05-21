



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
