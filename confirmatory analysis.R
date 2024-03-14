












# cleans global environment
rm(list = ls())

data_pm <- read.xlsx("data/data_cfa.xlsx", detectDates = TRUE)
head(data_pm)
summary(data_pm)

df <- mardia(data_pm)
df$uv.shapiro



# Define the model with interaction and bidirectional influence between decapod and epilithon
model <- '
  # Regressions
  
  epilithon ~ decapod
  decapod ~ epilithon + leaflitter + canopy
  macroinvertebrates ~ decapod + epilithon + canopy + decapod*epilithon
  
  leaflitter ~ canopy
  '

# Fit the model
fit <- cfa(model, data = data_pm, estimator = "MLR")

# Summarize the results
summary(fit, fit.measures = TRUE, standardized = TRUE)

semPaths(fit2, 'std', layout = 'circle')



# Define the model with interaction and bidirectional influence between decapod and epilithon
mod2 <- '
  # Regressions
  
  epilithon ~ decapod
  decapod ~ epilithon + leaflitter
  macroinvertebrates ~ decapod + epilithon + canopy + decapod*epilithon
  
  leaflitter ~ canopy
  '

# Fit the model
fit2 <- cfa(mod2, data = data_pm, estimator = "MLR")


https://campus.datacamp.com/courses/structural-equation-modeling-with-lavaan-in-r/multi-factor-models?ex=12
fitmeasures(fit, c('aic', 'ecvi'))
fitmeasures(fit2, c('aic', 'ecvi'))

