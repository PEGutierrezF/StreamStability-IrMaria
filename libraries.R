



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
install.packages("mvnormalTest", dependencies=TRUE)


# install.packages('optimx')
libraries <- c("modelr", "ggplot2", "plyr","dplyr", 'patchwork',
               "reshape2", "ggpubr", "tidyverse", "grid","lubridate",
               "fitdistrplus", "mgcv","ggtext", 'brms','magrittr',
               'readxl', 'optimx', 'minpack.lm','openxlsx',
               'lavaan', 'mvnormalTest', 'semPlot', 'lmtest',
               'car')
lapply(libraries, require, character.only = TRUE)


CXX14FLAGS=-O3 -march=native -mtune=native -fPIC -pthread -D_REENTRANT
CXX14 = /usr/bin/g++ -m64 -mtune=native -std=c++1y
CXX14FLAGS+=-I/path/to/TBB/include
LDFLAGS+=-L/path/to/TBB/lib -ltbb



