




#--------------------------------------------
# Idealizaed trajectories
# 29 May 2020
#PEGF
#--------------------------------------------
#

install.packages("fitdistrplus")
libraries <- c("modelr", "ggplot2", "plyr","dplyr", 'patchwork',
               "reshape2", "ggpubr", "tidyverse", "grid",
               "fitdistrplus")
lapply(libraries, require, character.only = TRUE)

