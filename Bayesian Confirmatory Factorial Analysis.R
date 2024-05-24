



# --------------------------------------------------------
# Bayesian Confirmatory Factorial Analysis, Prieta A
# Date: Fri May 24 2024 11:54:15
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------




data_pm <- read.xlsx("data/data_cfa.xlsx", sheet='post_Hurricane',
                     detectDates = TRUE)

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
  epilithon ~ canopy + decapod 
  decapod ~ epilithon + leaflitter 
  macroinvertebrates ~ epilithon + leaflitter + decapod 
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
plot(data_pm$canopy,data_pm$epilithon)

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
