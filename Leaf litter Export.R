



#--------------------------------------------
# Leaf Export
# 10 May 2020
#PEGF
#--------------------------------------------
#



library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

LExport<- read.csv("LeafExport.csv")
LExport


# Lm QPA Leaf Litter ------------------------------------------------------

LExport <- LExport %>% select(TimeLeaf,QPALeaflitter ,QPBLeaflitter )
LExport <- na.omit(LExport)
LExport

QPALeaf.mod  <- lm(QPALeaflitter ~ TimeLeaf, na.action=na.omit, data=LExport)
summary(QPALeaf.mod)

LExport$QPAresid<- QPALeaf.mod$resid
LExport

1/apply(na.omit(LExport), 2, sd)


# LM QPB Leaf Export ------------------------------------------------------

LExport 

QPBLExport.mod  <- lm(QPBLeaflitter ~ TimeLeaf, data=LExport)
summary(QPBLExport.mod)

LExport$QPBresid<- QPBLExport.mod$resid
LExport

1/apply(na.omit(LExport), 2, sd)
