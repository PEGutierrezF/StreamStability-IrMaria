



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
# Canopy cover QPA --------------------------------------------------------
# Normal distribution of value
###########################################################################

cc_A <- Trajectories %>%
  filter(stream =="QPA", variable =="canopy_cover") 

cc_A$date <- as.integer(as.Date(cc_A$date, format = "%Y-%m-%d"))

# Bayesian model
priors.cc_A = get_prior(value ~ s(date),
                data = cc_A, family = gaussian())
priors.cc_A

cc.qp_A.Bayes_mod <- brm(bf(value ~ s(date)),
          data = cc_A, family = gaussian(), cores = 1, seed = 14,
          warmup = 8000, iter = 10000, thin = 1, refresh = 0,
          control = list(adapt_delta = 0.99),
          prior = priors.cc_A)

summary(cc.qp_A.Bayes_mod)
plot(cc.qp_A.Bayes_mod)
plot(conditional_effects(cc.qp_A.Bayes_mod), points = TRUE)

# https://tem11010.github.io/regression_brms/
# We can also get an R-squared estimate for our model, 
# thanks to a newly-developed method from Andrew Gelman, 
#Ben Goodrich, Jonah Gabry and Imad Ali, with an explanation here.
# http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2.pdf
bayes_R2(cc.qp_A.Bayes_mod)
# r2(cc.qp_A.Bayes_mod) Existe esta otra, pero usare la de Gelman


msms <- conditional_smooths(cc.qp_A.Bayes_mod)
plot(msms)



# https://tem11010.github.io/regression_brms/
# The pp_check allows for graphical posterior predictive checking. 
# We can generate figures to compare the observed data to simulated 
# data from the posterior predictive distribution. This is a great 
# graphical way to evaluate your model.
# Here, 'nsamples' refers to the number of draws from the posterior 
#distribution to use to calculate yrep values.
pp_check(cc.qp_A.Bayes_mod, nsamples = 100)


mcmc_plot(cc.qp_A.Bayes_mod, 
         type = "areas",
         prob = 0.95)


# How does this model compare with the one fitted 
# using gam()? We can use the gam.vcomp() function 
# to compute the variance component representation 
# of the smooth estimated via gam().
gam.vcomp(cc.qp_A.mod1, rescale = FALSE)




# Canopy cover QPB --------------------------------------------------------

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


