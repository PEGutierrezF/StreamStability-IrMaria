




#--------------------------------------------
# Idealizaed trajectories
# 29 May 2020
#PEGF
#--------------------------------------------
#

install.packages("ggpubr")
libraries <- c("modelr", "ggplot2", "plyr","dplyr", 'patchwork',
               "reshape2", "ggpubr")
lapply(libraries, require, character.only = TRUE)



library(modelr)
library(ggplot2)
library(dplyr)
library(patchwork)


ideal<- read.csv("00 ideal.csv")
ideal


Ideal <- ggplot(ideal, aes(Time, 
                              Value ,
                              color=Variable)) + 
   geom_smooth(se = F, size=5) 

Ideal
  