



# --------------------------------------------------------
# Ecosystem stability - Stream Physicochemistry 
# Date: Fri Jun 21 2024 18:11:23
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------


rm(list=ls())



env_QPx <- read.xlsx("data/data_stability_metrics.xlsx", sheet='var_environ', detectDates = TRUE)
head(env_QPx)


# Temperature Prieta A ----------------------------------------------------
temp_QPA <- env_QPx %>%
  dplyr::select(date, temp_QPA)
temp_QPA$date <- as.POSIXct(temp_QPA$date,"%Y-%m-%d",tz = "UTC")

QPA.temp.mod  <- lm(temp_QPA ~ date, data=temp_QPA)
summary(QPA.temp.mod)

# Temporal stability
residuals <- residuals(QPA.temp.mod)
1/sd(residuals)

# Temperature Prieta B ----------------------------------------------------
temp_QPB <- env_QPx %>%
  dplyr::select(date, temp_QPB)
temp_QPB$date <- as.POSIXct(temp_QPB$date,"%Y-%m-%d",tz = "UTC")

QPB.temp.mod  <- lm(temp_QPB ~ date, data=temp_QPB)
summary(QPB.temp.mod)

# Temporal stability
res.QPB.temp.mod <- residuals(QPB.temp.mod)
1/sd(res.QPB.temp.mod)


# Conductivity Prieta A ----------------------------------------------------
cond_QPA <- env_QPx %>%
  dplyr::select(date, cond_QPA)
cond_QPA$date <- as.POSIXct(cond_QPA$date,"%Y-%m-%d",tz = "UTC")

QPA.cond.mod  <- lm(cond_QPA ~ date, data=cond_QPA)
summary(QPA.cond.mod)

# Temporal stability
res.QPA.cond.mod <- residuals(QPA.cond.mod)
1/sd(res.QPA.cond.mod)


# Conductivity Prieta A ----------------------------------------------------
cond_QPB <- env_QPx %>%
  dplyr::select(date, cond_QPB)
cond_QPB$date <- as.POSIXct(cond_QPB$date,"%Y-%m-%d",tz = "UTC")
head(cond_QPB)

QPB.cond.mod  <- lm(cond_QPB ~ date, data=cond_QPB)
summary(QPB.cond.mod)

# Temporal stability
res.QPB.cond.mod <- residuals(QPB.cond.mod)
1/sd(res.QPB.cond.mod)
