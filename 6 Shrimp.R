



# ---------------------------------------------
# Ecosystem stability - Shrimps
# 14 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#



rm(list=ls())




Shrimp<- read.csv("data/Shrimps.csv")
Shrimp

Shrimp$dateLN<-as.POSIXct(Shrimp$dateLN,"%Y-%m-%d",tz = "UTC")

# Lm QPA Leaf Litter ------------------------------------------------------

QPA_Shrimp <- Shrimp %>% dplyr::select(dateLN, QPAShrimpLog, QPBShrimpLog)
QPA_Shrimp <- na.omit(QPA_Shrimp)
QPA_Shrimp

QPAShrimp.mod  <- lm(QPAShrimpLog ~ dateLN, data=QPA_Shrimp)
summary(QPAShrimp.mod)

QPA_Shrimp$QPAresid<- QPAShrimp.mod$resid
QPA_Shrimp

1/apply(QPA_Shrimp, 2, sd)



# Shrimp QPB --------------------------------------------------------------


QPBShrimp.mod  <- lm(QPBShrimpLog ~ dateLN, na.action=na.omit, data=QPA_Shrimp)
summary(QPBShrimp.mod)

QPA_Shrimp$QPBresid<- QPBShrimp.mod$resid
QPA_Shrimp

1/apply(QPA_Shrimp, 2, sd)

