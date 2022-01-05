



#--------------------------------------------
# Macroinvertebrates
# 27 May 2020
#PEGF
#--------------------------------------------
#




rm(list=ls())




macroinvertebrates <- read.csv("data/all_data.csv")
head(macroinvertebrates)

macroiv <- macroinvertebrates %>% dplyr::select(date_shrimp, QPA_shrimp, QPB_shrimp)
macroiv$date_shrimp <- as.POSIXct(macroiv$date_shrimp,"%Y-%m-%d",tz = "UTC")
macroiv <- na.omit(macroiv)


################################################################
# Linear model Macroinvertebrate abundance Prieta A ---------------------
################################################################

# Lm QPA Macroinvertebrates ------------------------------------------------------

Macroinvertebrates <- Macroinvertebrates %>% select(TimeMacro, QPAMacroinvLog, QPBMacroinvLog)
Macroinvertebrates <- na.omit(Macroinvertebrates)
Macroinvertebrates

QPAMacroinv.mod  <- lm(QPAMacroinvLog ~ TimeMacro, data=Macroinvertebrates)
summary(QPAMacroinv.mod)

Macroinvertebrates$QPAresid<- QPAMacroinv.mod$resid
Macroinvertebrates

1/apply(Macroinvertebrates, 2, sd)




# LM QPB Macroinvertebrates  ----------------------------------------------


QPBMacroinv.mod  <- lm(QPBMacroinvLog ~ TimeMacro, data=Macroinvertebrates)
summary(QPBMacroinv.mod)

Macroinvertebrates$QPBresid<- QPBMacroinv.mod$resid
Macroinvertebrates

1/apply(Macroinvertebrates, 2, sd)



