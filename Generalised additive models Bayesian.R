



# ---------------------------------------------
# Bayesian GAM
# 25 Oct 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

# https://fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/

rm(list=ls())

# k = knots. 12 month per year or 24 sampling event per year. 
# ?choose.k
# bs= basis spline
# best model: high R-sq, low AIC, low REML

Trajectories<- read.csv("data/Trajectories.csv")
head(Trajectories)


# Canopy cover QPA --------------------------------------------------------
# Normal distribution of value

cc_A <- Trajectories %>%
  filter(stream =="QPA", variable =="canopy_cover") 

cc_A$date <- as.integer(as.Date(cc_A$date, format = "%Y-%m-%d"))

# Bayesian model
cc.qp_A.Bayes_mod <- brm(bf(value ~ s(date)),
          data = cc_A, family = gaussian(), cores = 1, seed = 17,
          iter = 4000, warmup = 2000, thin = 10, refresh = 0,
          control = list(adapt_delta = 0.99))

summary(cc.qp_A.Bayes_mod)

msms <- marginal_smooths(cc.qp_A.Bayes_mod)
plot(msms)
pp_check(cc.qp_A.Bayes_mod)
