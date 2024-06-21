



# --------------------------------------------------------
# Ecosystem stability - Stream Physicochemistry 
# Date: Fri Jun 21 2024 18:11:23
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------


rm(list=ls())



env_QPA <- read.xlsx("data/data_stability_metrics.xlsx", sheet='env_QPA', detectDates = TRUE)
head(env_QPA)


# Temperature Prieta A ----------------------------------------------------
temp_QPA <- env_QPA %>%
  dplyr::select(date, QPA_temp)
temp_QPA$date <- as.POSIXct(temp_QPA$date,"%Y-%m-%d",tz = "UTC")

QPA.temp.mod  <- lm(QPA_temp ~ date, data=temp_QPA)
summary(QPA.temp.mod)

# Temporal stability
residuals <- residuals(QPA.temp.mod)
1/sd(residuals)

