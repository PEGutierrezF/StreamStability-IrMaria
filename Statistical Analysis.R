



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

QPALeaf.mod  <- lm(QPALeaflitter ~ TimeLeaf, data=variables)
summary(QPALeaf.mod)


variables$resid <- NA
variables$QPAresid<- QPALeaf.mod$resid
variables

1/apply(variables, 2, sd)

p1 <- ggplot(variables,aes(TimeLeaf ,
                y=QPALeaflitter))+
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)
p1

  # Lm QPB Leaf Litter ------------------------------------------------------

QPBLeaf.mod <- lm(QPBLeaflitter ~ TimeLeaf, data=variables)
summary(QPBLeaf.mod)

coefficients(QPBLeaf.mod)
resid(QPBLeaf.mod)
variables %>% spread_residuals(QPALeaf.mod,QPBLeaf.mod)


p2 <- ggplot(variables,aes(TimeLeaf ,
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


p3 <- ggplot(variables,aes(TimeChla ,
                           y=QPAChla))+
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)
p3



