



# --------------------------------------------------------
# Bayesian Confirmatory Factorial Analysis, Prieta A
# Date: Fri May 24 2024 11:54:15
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------




data_pm <- read_excel("data/data_bcfa.xlsx", sheet='QPA_post_Hurricane')

data_pm <- data_pm %>% 
  dplyr::select(-date)

head(data_pm)
summary(data_pm)
data_pm$canopy <- as.numeric(data_pm$canopy)

# Interpolate NAs
data_pm_interpolated <- apply(data_pm, 2, na.approx)
head(data_pm_interpolated)
summary(data_pm_interpolated)

# Standardize variables
data_pm_standardized <- as.data.frame(scale(data_pm_interpolated))

model <- '
  leaflitter ~ prior("normal(0,1)")*canopy
  epilithon ~ prior("normal(0,1)")*decapod + prior("normal(0,1)")*canopy
  decapod ~ prior("normal(0,1)")*epilithon + prior("normal(0,1)")*leaflitter
  macroinvertebrates ~ prior("normal(0,1)")*epilithon + prior("normal(0,1)")*leaflitter + prior("normal(0,1)")*decapod
'
mod <- bsem(
  model,
  data = data_pm_standardized,
  control = list(adapt_delta = 0.9999, max_treedepth = 12),
  n.chains = 4,
  burnin = 98000,
  sample = 100000,
  seed = 14,
  mcmcfile = TRUE
)
summary(mod)


# extract information
# https://www.rensvandeschoot.com/tutorials/wambs-blavaan-tutorial/
blavInspect(mod, "rhat")
blavInspect(mod, "mcobj")

mcmc.list <- blavInspect(mod, what = "mcmc")
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


# extra relationships -----------------------------------------------------

plot(epilithon~canopy, data=data_pm)
plot(decapod~epilithon, data=data_pm)




# No used due to warnings -------------------------------------------------
No used due to warnings:
Warning messages:
1: There were 1685 divergent transitions after warmup. See
https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
to find out why this is a problem and how to eliminate them. 
2: Examine the pairs() plot to diagnose sampling problems


model <- '
  # Regressions
  leaflitter ~ canopy
  epilithon ~ decapod + canopy  
  decapod ~ epilithon + leaflitter 
  macroinvertebrates ~ epilithon + leaflitter + decapod 
'

priors <- list(
  # Normal priors for regression coefficients
  dp = "normal(0, 10)" # This sets a normal(0, 1) prior on all regression parameters
)

# Fit the model
mod <- bcfa(model, data = data_pm_standardized,
            n.chains = 4, burnin = 8000, sample = 10000,
            seed = 14, control = list(adapt_delta = 0.99995, 
                                      dp = priors$dp, max_treedepth=10), mcmcfile = T)


summary(mod)
semPaths(semPlotModel_lavaanModel(model))

