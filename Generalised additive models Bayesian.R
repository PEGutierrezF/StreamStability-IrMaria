



# ---------------------------------------------
# Bayesian GAM
# 25 Oct 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

# https://fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/
# http://svmiller.com/blog/2021/02/thinking-about-your-priors-bayesian-analysis/
# https://tem11010.github.io/regression_brms/

rm(list=ls())

# Hay que ver el Smooth Terms: ->   sds(sdate_1) -> 
# sds(stimes_1) is the variance parameter, which has the effect of controlling 
# the wiggliness of the smooth - the larger this value the more wiggly the smooth.



# Note that we use the bf() argument to specify this nonlinear model. 

Trajectories <- read.csv("data/Trajectories.csv")
head(Trajectories)



###########################################################################
# Canopy cover Prieta A --------------------------------------------------------
# Normal distribution of value
###########################################################################

cc_A <- Trajectories %>%
  filter(stream =="QPA", variable =="canopy_cover") 

cc_A$date <- as.integer(as.Date(cc_A$date, format = "%Y-%m-%d"))


# Model 1 "cr" --------------------------------------------------------------

priors.cc_A.cr = get_prior(value ~ s(date, bs="cr",k = -1),
                data = cc_A, family = gaussian())
priors.cc_A.cr

cc.qp_A.Bayes.cr <- brms::brm(bf(value ~ s(date, bs="cr",k = -1)),
          data = cc_A, family = gaussian(), cores = 1, seed = 14,
          warmup = 8000, iter = 10000, thin = 1, refresh = 0,
          control = list(adapt_delta = 0.99),
          prior = priors.cc_A.cr)

summary(cc.qp_A.Bayes.cr)
plot(cc.qp_A.Bayes.cr)
plot(conditional_effects(cc.qp_A.Bayes.cr), points = TRUE)
msms <- conditional_smooths(cc.qp_A.Bayes.cr)
plot(msms)

pp_check(cc.qp_A.Bayes.cr, ndraws = 100)

mcmc_plot(cc.qp_A.Bayes.cr, 
          type = "areas",
          prob = 0.95)

# Model 2 "ps" -----------------------------------------------------------------

priors.cc_A.ps = get_prior(value ~ s(date, bs="ps", k=5),
                        data = cc_A, family = gaussian())

cc.qp_A.Bayes.ps <- brms::brm(bf(value ~ s(date, bs="ps", k=5)),
                               data = cc_A, family = gaussian(), cores = 1, seed = 14,
                               warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                               control = list(adapt_delta = 0.99),
                               prior = priors.cc_A.ps)

summary(cc.qp_A.Bayes.ps)
plot(cc.qp_A.Bayes.ps)
plot(conditional_effects(cc.qp_A.Bayes.ps), points = TRUE)

pp_check(cc.qp_A.Bayes.ps, ndraws = 100)

mcmc_plot(cc.qp_A.Bayes.ps, 
          type = "areas",
          prob = 0.95)

# Model 3 "ts" -----------------------------------------------------------------

priors.cc_A.ts = get_prior(value ~ s(date, bs="ts", k=5),
                           data = cc_A, family = gaussian())

cc.qp_A.Bayes.ts <- brms::brm(bf(value ~ s(date, bs="ts", k=5)),
                              data = cc_A, family = gaussian(), cores = 1, seed = 14,
                              warmup = 8000, iter = 10000, thin = 1, refresh = 0,
                              control = list(adapt_delta = 0.99),
                              prior = priors.cc_A.ts)

summary(cc.qp_A.Bayes.ts)
plot(cc.qp_A.Bayes.ts)
plot(conditional_effects(cc.qp_A.Bayes.ts), points = TRUE)

pp_check(cc.qp_A.Bayes.ts, ndraws = 100)

mcmc_plot(cc.qp_A.Bayes.ts, 
          type = "areas",
          prob = 0.95)


# https://tem11010.github.io/regression_brms/
# The pp_check allows for graphical posterior predictive checking. 
# We can generate figures to compare the observed data to simulated 
# data from the posterior predictive distribution. This is a great 
# graphical way to evaluate your model.
# Here, 'nsamples' refers to the number of draws from the posterior 
#distribution to use to calculate yrep values.
pp_check(cc.qp_A.Bayes_mod, ndraws = 100)


mcmc_plot(cc.qp_A.Bayes_mod, 
         type = "areas",
         prob = 0.95)



# Evaluate models ---------------------------------------------------------

# https://tem11010.github.io/regression_brms/
# We can also get an R-squared estimate for our model, 
# thanks to a newly-developed method from Andrew Gelman, 
#Ben Goodrich, Jonah Gabry and Imad Ali, with an explanation here.
# http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2.pdf
bayes_R2(cc.qp_A.Bayes.cr)
bayes_R2(cc.qp_A.Bayes.ps)
bayes_R2(cc.qp_A.Bayes.ts)
# r2(cc.qp_A.Bayes_mod) Existe esta otra, pero usare la de Gelman
# Bayes R2 quantifies the expected fit or variance explained by a model

loo(cc.qp_A.Bayes.cr, cc.qp_A.Bayes.ps, cc.qp_A.Bayes.ts)

waic.bs <- waic(cc.qp_A.Bayes.cr)
waic.ps <- waic(cc.qp_A.Bayes.ps)
waic.ts <- waic(cc.qp_A.Bayes.ts)

loo_compare(waic.bs, waic.ps, waic.ts)  






###########################################################################
# Canopy cover Prieta B --------------------------------------------------------
# Normal distribution of value
###########################################################################

cc_B <- Trajectories %>%
  filter(stream =="QPB", variable =="canopy_cover")

descdist(cc_B$value, discrete=FALSE, boot=500)

cc_B$date <- as.integer(as.Date(cc_B$date, format = "%Y-%m-%d"))

# Bayesian model
#examine the prior options and the brms default

cc.qp_B.Bayes_mod <- brm(bf(value ~ s(date)),
                         data = cc_B, family = gaussian(), cores = 1, seed = 17,
                         iter = 4000, warmup = 2000, thin = 10, refresh = 0,
                         control = list(adapt_delta = 0.99))

summary(cc.qp_B.Bayes_mod)

msms_CC_QPB <- marginal_smooths(cc.qp_B.Bayes_mod)
plot(msms_CC_QPB)
pp_check(cc.qp_B.Bayes_mod)


