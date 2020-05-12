



#--------------------------------------------
# Leaf Export Inside
# 10 May 2020
#PEGF
#--------------------------------------------
#



library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

LExportIn<- read.csv("LeafExportInside.csv")
LExportIn


# Lm QPA Leaf Litter ------------------------------------------------------

LExportIn <- LExportIn %>% select(TimeLeaf,QPALeafInside  ,QPBLeafInside )
LExportIn <- na.omit(LExportIn)
LExportIn

QPAExpIn.mod  <- lm(QPALeafInside ~ TimeLeaf, na.action=na.omit, data=LExportIn)
summary(QPAExpIn.mod)

LExportIn$QPAresid<- QPAExpIn.mod$resid
LExportIn

1/apply(na.omit(LExportIn), 2, sd)


# LM QPB Leaf Export ------------------------------------------------------

LExportIn 

QPBExpIn.mod  <- lm(QPBLeafInside ~ TimeLeaf, na.action=na.omit, data=LExportIn)
summary(QPBExpIn.mod)

LExportIn$QPBresid<- QPBExpIn.mod$resid
LExportIn

1/apply(na.omit(LExportIn), 2, sd)
