




#--------------------------------------------
# Idealizaed trajectories
# 29 May 2020
#PEGF
#--------------------------------------------
#

install.packages("reshape2")
libraries <- c("modelr", "ggplot2", "plyr","dplyr", 'patchwork',
               "reshape2")
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
  