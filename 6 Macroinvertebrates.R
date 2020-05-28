



#--------------------------------------------
# Macroinvertebrates
# 27 May 2020
#PEGF
#--------------------------------------------
#


library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)

Macroinvertebrates<- read.csv("6 Macroinvertebrates.csv")
Macroinvertebrates


# Lm QPA Leaf Litter ------------------------------------------------------

Macroinvertebrates <- Macroinvertebrates %>% select(TimeMacro, QPAMacroinvLog, QPBMacroinvLog)
Macroinvertebrates <- na.omit(Macroinvertebrates)
Macroinvertebrates

QPAMacroinv.mod  <- lm(QPAMacroinvLog ~ TimeMacro, data=Macroinvertebrates)
summary(QPAMacroinv.mod)

Macroinvertebrates$QPAresid<- QPAMacroinv.mod$resid
Macroinvertebrates

1/apply(Macroinvertebrates, 2, sd)
