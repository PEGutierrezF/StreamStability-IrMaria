



# ---------------------------------------------
# Upload libraries
# 19 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



remove.packages(c("StanHeaders", "rstan"))
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))



libraries <- c("modelr", "ggplot2", "plyr","dplyr", 'patchwork',
               "reshape2", "ggpubr", "tidyverse", "grid","lubridate",
               "fitdistrplus", "mgcv","ggtext", 'brms','magrittr',
               'readxl')
lapply(libraries, require, character.only = TRUE)






