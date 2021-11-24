



# ---------------------------------------------
# Upload libraries
# 19 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



libraries <- c("modelr", "ggplot2", "plyr","dplyr", 'patchwork',
               "reshape2", "ggpubr", "tidyverse", "grid",
               "fitdistrplus", "mgcv","ggtext", 'brms')
lapply(libraries, require, character.only = TRUE)



