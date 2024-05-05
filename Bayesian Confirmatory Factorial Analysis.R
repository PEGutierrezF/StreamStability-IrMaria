

library(blavaan)

data_pm <- read.xlsx("data/data_cfa.xlsx", detectDates = TRUE)
head(data_pm)
summary(data_pm)

model <- '
  # Regressions
  leaflitter ~ canopy
  epilithon ~ decapod
  decapod ~ epilithon + leaflitter + canopy
  macroinvertebrates ~ decapod + epilithon + canopy
'
# Fit the model
fit1 <- bcfa(model, data = data_pm)

summary(fit1)

