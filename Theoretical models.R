



# cleans global environment
rm(list = ls())
library(minpack.lm)

# data
canopy_QPA <- c(0.336436902, -0.349667996, 0.08348054, 0.194286951, -0.521518253, 
          0.599072942, 2.035709058, 1.727578155, 1.716528319, 1.669203747, 
          1.581599124, 1.645628869, 1.436226353, 1.434460089, 1.479398525, 
          1.391721113, 1.434460089, 1.323650326, 1.294256092, 1.206443161, 
          1.17562524, 1.110170268, 1.026044255, 0.991775799, 1.070264724, 
          1.026930388, 1.036626365, 0.966693202, 0.895547766, 0.759415592, 
          0.631161257, 0.6258842, 0.577094035, 0.681203769, 0.163260678, 
          0.224379493, 0.556040626, 0.491596663, 0.305941265, 0.419465091, 
          0.52140513, 0.25351904, 0.016155257, 0.238118669, 0.137395756, 
          -0.05633391, 0.006844991, 0.152945865, 0.011934061, -0.194014687, 
          -0.05181515, -0.161291931, -0.023368787, -0.01812402, -0.091360533, 
          -0.278572075, -0.028641207, -0.043732484, -0.026001523, -0.203414234, 
          -0.05181515, -0.099839151, -0.318969951, -0.332011037, 0.152945865)

event <- seq(1, length(canopy_QPA))
data <- data.frame(date, canopy_QPA)

# Define the Nelson-Siegel function
nelson_siegel <- function(x, beta0, beta1, beta2, tau) {
  y <- beta0 + (beta1 + beta2) * (1 - exp(-x / tau)) / (x / tau) - beta2 * exp(-x / tau)
  return(y)
}

# Initial parameter values
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 1)

# Fit the model using nlsLM
mod.QPA <- nlsLM(canopy_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
             data = data, 
             start = start_params)

summary(mod.QPA)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.QPA)
observed_values <- data$canopy_QPA
mean_observed <- mean(observed_values)
ss_total <- sum((observed_values - mean_observed)^2)
ss_residual <- sum((observed_values - fitted_values)^2)
rsquare <- 1 - ss_residual / ss_total

# Print R-squared value
print(paste("R-squared:", round(rsquare, 4)))
pvalue <- summary(mod.QPA)$coefficients[4, 4]  # P-value for the 'tau' parameter
print(paste("p-value:", round(pvalue, 20)))



# Calculate the predicted values from the model
predicted_values <- predict(mod.QPA, newdata = data.frame(event = event))

# Create a ggplot
ggplot(data, aes(x = event, y = canopy_QPA)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "red") +
  labs(title = "Canopy QPA and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal()

