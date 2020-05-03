



#--------------------------------------------
# Linear model variables post huricanes
# 02 May 2020
#PEGF
#--------------------------------------------
#
library(ggplot2)


setwd("D:/LTER/Manuscript 2017/Ecosystem stability/Statistical Analysis")
variables <- read.csv("variables.csv")
variables

# Lm QPA Leaf Litter ------------------------------------------------------


QPALeaf.mod <- lm(variables,QPALeaflitter ~ TimeLeaf, data=variables)
summary(QPALeaf.mod)

coefficients(QPALeaf.mod)
resid(QPALeaf.mod)


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


