
install.packages('blavaan')
library(blavaan)
library(coda)

data_pm <- read.xlsx("data/data_cfa.xlsx", sheet='full',
                     detectDates = TRUE)
head(data_pm)
summary(data_pm)

# Extract Nitrate values from monthly_avg
nitrate_values <- monthly_avg$Nitrate
# Add Nitrate column to data_pm
data_pm$Nitrate <- nitrate_values
print(data_pm)


data_pm <- read.xlsx("data/data_cfa.xlsx", sheet='post_Hurricane',
                     detectDates = TRUE)
# Extract Nitrate values from monthly_avg
# Potassium_values <- phys_QPA$Potassium
Temp_values <- phys_QPA$Temp
# Add Nitrate column to data_pm
# data_pm$Potassium <- Potassium_values
data_pm$Temp <- Temp_values


data_pm <- data_pm %>% 
  dplyr::select(-date)

head(data_pm)

# Interpolate NAs
data_pm_interpolated <- apply(data_pm, 2, na.approx)

# Standardize variables
data_pm_standardized <- as.data.frame(scale(data_pm_interpolated))

model <- '
  # Regressions
  leaflitter ~ canopy
  epilithon~ canopy 
  decapod ~ epilithon + leaflitter + Temp
  macroinvertebrates ~ epilithon + leaflitter + Temp
'
# Fit the model
mod <- bcfa(model, data = data_pm_standardized,
            n.chains = 4, burnin = 800, sample = 1000,
            seed = 14, mcmcfile = T)

summary(mod)
semPaths(semPlotModel_lavaanModel(model))


# extract information
blavInspect(fit, "rhat")

mcmc.list <- blavInspect(fit, what = "mcmc")
gelman.diag(mcmc.list)

plot(mod)
plot(mod,plot.type = "acf")
plot(data_pm$epilithon,data_pm$canopy)

# Autocorrelation ---------------------------------------------------------
# Extract MCMC samples
mcmc_samples <- mcmc(mod)
# Extract the MCMC chain for each parameter
chain <- mcmc.list(mcmc_samples)

# Plot the autocorrelation function (ACF) for each parameter's chain
par(mfrow=c(3, 4))  # Adjust the layout if needed
for (i in 1:ncol(as.matrix(chain))) {
  acf(mcmc_samples[, i], main=paste("ACF for Parameter", i))
}
