



# --------------------------------------------------------
# Bayesian Confirmatory Factorial Analysis, Prieta B
# Date: Fri May 24 2024 11:54:15
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------




bayes_cfa_QPB <- read.xlsx("data/data_bcfa.xlsx", sheet='QPB_pre_Hurricane')
,
                     detectDates = TRUE)

bayes_cfa_QPB <- bayes_cfa_QPB %>% 
  dplyr::select(-date)

head(bayes_cfa_QPB)

# Interpolate NAs
bayes_cfa_QPB_interp <- apply(bayes_cfa_QPB, 2, na.approx)

# Standardize variables
bayes_cfa_QPB_interp_stand <- as.data.frame(scale(bayes_cfa_QPB_interp))

model_QPB <- '
  # Regressions
  leaflitter ~ canopy
  epilithon ~ canopy + decapod  
  decapod ~ epilithon + leaflitter 
  macroinvertebrates ~ epilithon + leaflitter + decapod 
'

priors <- list(
  # Normal priors for regression coefficients
  dp = "normal(0, 1)" # This sets a normal(0, 10) prior on all regression parameters
)

# Fit the model
mod_QPB <- bcfa(model_QPB, data = bayes_cfa_QPB_interp_stand,
            n.chains = 4, burnin = 8000, sample = 10000,
            seed = 14, control = list(adapt_delta = 0.9999, 
            dp = priors$dp, max_treedepth=12), mcmcfile = T)

summary(mod_QPB)
semPaths(semPlotModel_lavaanModel(model))

# extract information
blavInspect(mod, "rhat")
blavInspect(mod, "mcobj")

mcmc.list <- blavInspect(mod, what = "mcmc")
gelman.diag(mcmc.list)

plot(mod)
plot(mod,plot.type = "acf")
plot(bayes_cfa_QPB$canopy,bayes_cfa_QPB$epilithon)

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
