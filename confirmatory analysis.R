










install.packages('semPlot')
library(semPlot)

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
  leaflitter ~ canopy
  
  epilithon ~ decapod
  decapod ~ epilithon + leaflitter + canopy
  macroinvertebrates ~ decapod + epilithon + canopy + decapod*epilithon
  '

# Fit the model
fit <- cfa(model, data = data_pm, estimator = "MLR")

# Summarize the results
summary(fit, fit.measures = TRUE, standardized = TRUE)

semPaths(fit, 'std', layout = 'circle')



