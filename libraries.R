




#--------------------------------------------
# Idealizaed trajectories
# 29 May 2020
#PEGF
#--------------------------------------------
#

install.packages("fitdistrplus")
libraries <- c("modelr", "ggplot2", "plyr","dplyr", 'patchwork',
               "reshape2", "ggpubr", "tidyverse", "grid",
               "fitdistrplus", "mgcv")
lapply(libraries, require, character.only = TRUE)


ggplot(ccA, aes(sam_event, value)) + geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))

ccA$date<-as.POSIXct(ccA$date,"%Y-%m-%d",tz = "UTC")
descdist(ccA$value, discrete=FALSE, boot=500)
