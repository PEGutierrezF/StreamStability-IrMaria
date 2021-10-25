



# ---------------------------------------------
# Bayesian GAM
# 25 Oct 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

# https://fromthebottomoftheheap.net/2018/04/21/fitting-gams-with-brms/

cc.qp_A.mod <- gam(value ~s(date, bs="cr", k=5), data=cc_A, method = "REML") 

m2 <- brm(bf(value ~ s(date)),
          data = cc_A, family = gaussian(), cores = 1, seed = 17,
          iter = 4000, warmup = 1000, thin = 10, refresh = 0,
          control = list(adapt_delta = 0.99))

