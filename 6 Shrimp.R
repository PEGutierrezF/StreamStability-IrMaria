



#--------------------------------------------
# Shrimp abundacen
# 12 May 2020
#PEGF
#--------------------------------------------
#

Shrimp<- read.csv("data/Shrimps.csv")
Shrimp


# Lm QPA Leaf Litter ------------------------------------------------------

Shrimp <- Shrimp %>% select(events, QPAShrimpLog, QPBShrimpLog)
Shrimp <- na.omit(Shrimp)
Shrimp

QPAShrimp.mod  <- lm(QPAShrimpLog ~ events, na.action=na.omit, data=Shrimp)
summary(QPAShrimp.mod)

Shrimp$QPAresid<- QPAShrimp.mod$resid
Shrimp

1/apply(Shrimp, 2, sd)



# Shrimp QPB --------------------------------------------------------------


QPBShrimp.mod  <- lm(QPBShrimpLog ~ events, na.action=na.omit, data=Shrimp)
summary(QPBShrimp.mod)

Shrimp$QPBresid<- QPBShrimp.mod$resid
Shrimp

1/apply(Shrimp, 2, sd)

