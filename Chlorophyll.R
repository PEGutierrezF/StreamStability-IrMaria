



#--------------------------------------------
# Chlorophyll
# 07 May 2020
#PEGF
#--------------------------------------------
#

library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

variables<- read.csv("variables.csv")
variables


# QPA CHLA ----------------------------------------------------------------


chla <- variables %>% select(TimeChla,QPAChla,QPBChla)
chla <- na.omit(chla)

QPAChla.mod  <- lm(QPAChla~ TimeChla   , data=chla)
summary(QPAChla.mod)

chla$QPAChlaresid<- QPAChla.mod$resid
chla

1/apply(chla, 2, sd)

c1 <- ggplot(chla,aes(TimeChla,
                      y=QPAChla))+
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)
c1



# QPB CHL-A ---------------------------------------------------------------

QPBChla.mod  <- lm(QPBChla~ TimeChla, data=chla)
summary(QPBChla.mod)

chla$QPBChlaresid<- QPBChla.mod$resid
chla

1/apply(chla, 2, sd)

c2 <- ggplot(chla,aes(TimeChla,
                      y=QPBChla))+
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)
c2




# Canopy QPA ---------------------------------------------------------------


Canopy <- variables %>% select(TimeCanopy,QPACanopy,QPBCanopy)
Canopy <- na.omit(Canopy)

QPACanopy.mod  <- lm(QPACanopy~ TimeCanopy, data=Canopy)
summary(QPACanopy.mod)

coefficients(QPACanopy.mod)
resid(QPACanopy.mod)

Canopy$QPACanopyresid <- NA
Canopy$QPACanopyresid<- QPACanopy.mod$resid
Canopy

1/apply(Canopy, 2, sd)

c1 <- ggplot(Canopy,aes(TimeCanopy ,
                        y=QPACanopy))+
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)
c1


# Canopy QPB --------------------------------------------------------------


