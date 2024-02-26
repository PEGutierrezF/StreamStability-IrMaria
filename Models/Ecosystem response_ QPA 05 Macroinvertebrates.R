













# cleans global environment
rm(list = ls())



# Create a data frame with Decapoda_QPA data (2017-01-01 to 2022-09-01)
macros_QPA <- c(0.072390496, 0.621576988, -0.971860583, -1.202972304, -0.423647427, -0.615185639, 
                0.740076613, 0.089011378, -1.896119485, -1.163751591, -1.559647248, -0.530027831, 
                -1.531476371, -1.819158443, -0.003007521, -1.054552299, -0.490022496, -1.286353913,
                -0.138261567, -0.003007521, 0.398433437, -0.255182905, -0.352821375, 0.038296285, 
                0.631207881, -0.414514944, 0.049790664, -0.078042707, -0.084557388, -0.294713744, 
                -0.335871816, -1.037457865, -0.83832919, -0.745547457, -0.335871816, -0.987860924, 
                -0.637658495, -0.509825123, -1.531476371, -0.987860924, 0.457158723, -0.015128882, 
                0.110751364, -1.377325691, -0.758286483, -0.509825123, -1.037457866, 0.378066134, 
                -0.131388688, -0.784261969, -0.758286483, -0.649087191, 0.784902044, -1.504077397, 
                0.264902044, 0.188309599, 0.624863222, 0.534266326, 0.505405556, -0.824535868, 
                0.116113307, -1.202972304, -0.302810954, 0.547965171, 0.348836496, 0.605316467, 
                -1.979501093, -0.033590944, -0.14518201)


event <- seq(1, length(macros_QPA))
data <- data.frame(event, macros_QPA)





###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(macros_QPA ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared_mod.1 <- summary(mod.1)$r.squared
p_value_mod.1 <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squar:", r_squared_mod.1, "\n")
cat("P-value:", p_value_mod.1, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Canopy QPA and Trend Line",
       x = "Event",
       y = "Macroinvertebrates QPA") +
  theme_minimal()

mod.1.plot



###########################################################################
# Humped yield curve (mod.2) ----------------------------------------------
# Define the Nelson-Siegel function
nelson_siegel <- function(x, beta0, beta1, beta2, tau) {
  y <- beta0 + (beta1 + beta2) * (1 - exp(-x / tau)) / (x / tau) - beta2 * exp(-x / tau)
  return(y)
}

# Initial parameter values
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 1)
# Fit the model using nlsLM
mod.2 <- nlsLM(macros_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$macros_QPA
mean_observed <- mean(observed_values)
ss_total <- sum((observed_values - mean_observed)^2)
ss_residual <- sum((observed_values - fitted_values)^2)
r_square_mod.2 <- 1 - ss_residual / ss_total
pvalue_mod.2 <- summary(mod.2)$coefficients[4, 4]  # P-value for the 'tau' parameter

# Print R-squared value and p-values
cat("R-squared:", r_square_mod.2, "\n")
cat("P-value:", pvalue_mod.2, "\n")


# Calculate the predicted values from the model
predicted_values <- predict(mod.2, newdata = data.frame(event = event))

# Create a ggplot
mod.2.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Macroinvertebrates QPA and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Macoinvertebrates QPA") +
  theme_minimal()

mod.2.plot



###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(macros_QPA ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(macros_QPA), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, macros_QPA = predictions), 
            aes(x = event, y = macros_QPA), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Macros QPA") +
  theme_minimal()

mod.3.plot



###########################################################################
# Logistic curve (mod.4) --------------------------------------------------
# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

# Try different starting parameter values
start_params <- list(A = -1, B = 1, C = 0.1, D = median(data$event))

# Fit the model using nls.lm algorithm
mod.4 <- nlsLM(macros_QPA ~ logistic_function(event, A, B, C, D),
               data = data,
               start = start_params)

# Check the summary of the model
summary(mod.4)


# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$macros_QPA - mean(data$macros_QPA))^2)
r_squared <- 1 - (ss_residuals / ss_total)
# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared), "\n")

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(macros_QPA), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot <- ggplot(data, aes(x = event, y = macros_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, canopy_QPA = predictions), 
            aes(x = event, y = canopy_QPA), color = "red") +
  labs(title = "Logistic Curve Fit",
       x = "Event",
       y = "Macros QPA") +
  theme_minimal()

mod.4.plot

