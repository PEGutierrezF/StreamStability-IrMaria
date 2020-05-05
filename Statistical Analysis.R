



#--------------------------------------------
# Linear model variables post huricanes
# 02 May 2020
#PEGF
#--------------------------------------------
#

library(modelr)
library(ggplot2)
library(dplyr)

variables<- read.csv("variables.csv")
variables


# Lm QPA Leaf Litter ------------------------------------------------------

LeafLitter <- variables %>% select(TimeLeaf,QPALeaflitter,QPBLeaflitter )
LeafLitter <- na.omit(LeafLitter)
LeafLitter

QPALeaf.mod  <- lm(log(QPALeaflitter) ~ TimeLeaf, data=LeafLitter)
summary(QPALeaf.mod)

LeafLitter$QPAresid<- QPALeaf.mod$resid
LeafLitter

1/apply(LeafLitter, 2, sd)

p1 <- ggplot(LeafLitter,aes(TimeLeaf ,
                y=QPALeaflitter))+
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)
p1

  # Lm QPB Leaf Litter ------------------------------------------------------

QPBLeaf.mod <- lm(QPBLeaflitter ~ TimeLeaf, data=LeafLitter)
summary(QPBLeaf.mod)

LeafLitter %>% spread_residuals(QPBLeaf.mod)

1/apply(LeafLitter, 2, sd)

p2 <- ggplot(LeafLitter,aes(TimeLeaf ,
                           y=QPBLeaflitter))+
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)
p2




# QPA CHLA ----------------------------------------------------------------


chla <- variables %>% select(TimeChla,QPAChla,QPBChla)
chla <- na.omit(chla)

QPAChla.mod  <- lm(QPAChla~ TimeChla   , data=chla)
summary(QPAChla.mod)

coefficients(QPAChla.mod)
resid(QPAChla.mod)

chla$QPAChlaresid <- NA
chla$QPAChlaresid<- QPAChla.mod$resid
chla

1/apply(chla, 2, sd)



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


