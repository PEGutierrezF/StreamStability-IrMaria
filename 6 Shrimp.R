



#--------------------------------------------
# Shrimp abundacen
# 12 May 2020
#PEGF
#--------------------------------------------
#



library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

Shrimp<- read.csv("6_Shrimp.csv")
Shrimp


# Lm QPA Leaf Litter ------------------------------------------------------

Shrimp <- Shrimp %>% select(TimeCHLA, QPAShrimpLog, QPBShrimpLog)
Shrimp <- na.omit(Shrimp)
Shrimp

QPAShrimp.mod  <- lm(QPAShrimpLog ~ TimeCHLA, na.action=na.omit, data=Shrimp)
summary(QPAShrimp.mod)

Shrimp$QPAresid<- QPAShrimp.mod$resid
Shrimp

1/apply(Shrimp, 2, sd)



# Shrimp QPB --------------------------------------------------------------


QPBShrimp.mod  <- lm(QPBShrimpLog ~ TimeCHLA, na.action=na.omit, data=Shrimp)
summary(QPBShrimp.mod)

Shrimp$QPBresid<- QPBShrimp.mod$resid
Shrimp

1/apply(Shrimp, 2, sd)

