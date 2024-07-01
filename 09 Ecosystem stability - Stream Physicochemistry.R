



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


# Conductivity Prieta B ----------------------------------------------------
cond_QPB <- env_QPx %>%
  dplyr::select(date, cond_QPB)
cond_QPB$date <- as.POSIXct(cond_QPB$date,"%Y-%m-%d",tz = "UTC")
head(cond_QPB)

QPB.cond.mod  <- lm(cond_QPB ~ date, data=cond_QPB)
summary(QPB.cond.mod)

# Temporal stability
res.QPB.cond.mod <- residuals(QPB.cond.mod)
1/sd(res.QPB.cond.mod)


# Potassium Prieta A ----------------------------------------------------
k_QPA <- env_QPx %>%
  dplyr::select(date, potassium_QPA)
k_QPA$date <- as.POSIXct(k_QPA$date,"%Y-%m-%d",tz = "UTC")

QPA.k.mod  <- lm(potassium_QPA ~ date, data=k_QPA)
summary(QPA.k.mod)

# Temporal stability
res.QPA.k.mod <- residuals(QPA.k.mod)
1/sd(res.QPA.k.mod)


# Potassium Prieta B ----------------------------------------------------
k_QPB <- env_QPx %>%
  dplyr::select(date, potassium_QPB)
k_QPB$date <- as.POSIXct(k_QPB$date,"%Y-%m-%d",tz = "UTC")
head(k_QPB)

QPB.k.mod  <- lm(potassium_QPB ~ date, data=k_QPB)
summary(QPB.k.mod)

# Temporal stability
res.QPB.k.mod <- residuals(QPB.k.mod)
1/sd(res.QPB.k.mod)


# Nitrate Prieta A ----------------------------------------------------
NO3_QPA <- env_QPx %>%
  dplyr::select(date, nitrate_QPA)
NO3_QPA$date <- as.POSIXct(NO3_QPA$date,"%Y-%m-%d",tz = "UTC")

QPA.NO3.mod  <- lm(nitrate_QPA ~ date, data=NO3_QPA)
summary(QPA.NO3.mod)

# Temporal stability
res.QPA.NO3.mod <- residuals(QPA.NO3.mod)
1/sd(res.QPA.NO3.mod)


# Nitrate Prieta B ----------------------------------------------------
NO3_QPB <- env_QPx %>%
  dplyr::select(date, nitrate_QPB)
NO3_QPB$date <- as.POSIXct(NO3_QPB$date,"%Y-%m-%d",tz = "UTC")
head(NO3_QPB)

QPB.NO3.mod  <- lm(nitrate_QPB ~ date, data=NO3_QPB)
summary(QPB.NO3.mod)

# Temporal stability
res.QPB.NO3.mod <- residuals(QPB.NO3.mod)
1/sd(res.QPB.NO3.mod)


#  Dissolved Organic Carbon Prieta A ----------------------------------------------------
doc_QPA <- env_QPx %>%
  dplyr::select(date, doc_QPA)
doc_QPA$date <- as.POSIXct(doc_QPA$date,"%Y-%m-%d",tz = "UTC")

QPA.doc.mod  <- lm(doc_QPA ~ date, data=doc_QPA)
summary(QPA.doc.mod)

# Temporal stability
res.QPA.doc.mod <- residuals(QPA.doc.mod)
1/sd(res.QPA.doc.mod)


# Dissolved Organic Carbon Prieta B ----------------------------------------------------
doc_QPB <- env_QPx %>%
  dplyr::select(date, doc_QPB)
doc_QPB$date <- as.POSIXct(doc_QPB$date,"%Y-%m-%d",tz = "UTC")
head(doc_QPB)

QPB.doc.mod  <- lm(doc_QPB ~ date, data=doc_QPB)
summary(QPB.doc.mod)

# Temporal stability
res.QPB.doc.mod <- residuals(QPB.doc.mod)
1/sd(res.QPB.doc.mod)
