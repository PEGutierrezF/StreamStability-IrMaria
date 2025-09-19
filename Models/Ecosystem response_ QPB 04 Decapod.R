



# ---------------------------------------------
# Long-term ecosystem response: Decapoda abundance Quebrada Prieta B
# 23 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  




# cleans global environment
rm(list = ls())



shrimp_QPB<- c(0.390235065, 
               0.631231632, -0.043353451, 0.436639122, 0.341128433, 0.673279354, 
               1.122436756, 0.575412531, 0.992127709, 1.208225252, 1.017854627, 
               0.16681027, 0.087772451, 0.256537298, 0.409506162, 0.213170875, 
               0.566073384, 0.586442602, 0.537408582, 0.752616862, 0.080735464, 
               0.56625312, 0.274781187, 0.17606686, 0.416734634, 0.26722363, 0.107660536, 
               0.000172464, -0.22189892, -0.125731072, -0.059017824, 0.009271358, 
               0.950052304, 0.362453294, -0.004009558, 0.119783165, -0.286086287, 
               -0.573716033, -0.727644859, -0.44757473, -0.539695207, -0.755275737, 
               -0.746319302, -0.565880826, -0.430242989, -0.711509319, -1.03760404, 
               -0.351008952, -0.331514746, -0.556119565, -1.272283076, 0.268179237, 
               0.003355149, -0.86495049, -0.131737915, 0.436245583, 0.425157916, 
               1.01180779, -0.34408051, -0.815809062)



event <- seq(1, length(shrimp_QPB))
data <- data.frame(event, shrimp_QPB)



###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(shrimp_QPB ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared_mod.1 <- summary(mod.1)$r.squared
p_value_mod.1 <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared_mod.1, "\n")
cat("P-value:", p_value_mod.1, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Decapoda QPB and Trend Line",
       x = "Event",
       y = "Decapoda QPB") +
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
mod.2 <- nlsLM(shrimp_QPB ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$shrimp_QPB
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
mod.2.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Decapoda QPB and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Decapoda QPB") +
  theme_minimal()

mod.2.plot

###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(shrimp_QPB ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(shrimp_QPB), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, shrimp_QPB = predictions), 
            aes(x = event, y = shrimp_QPB), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Decapoda QPB") +
  theme_minimal()

mod.3.plot


###########################################################################
# Logistic curve (mod.4) --------------------------------------------------
# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

##############################################################################
# Fit the logistic curve model using nlsLM with adjusted control parameters
mod.4 <- nlsLM(
  shrimp_QPB ~ logistic_function(event, A, B, C, D),
  data = data,
  start = list(A = min(shrimp_QPB), B = max(shrimp_QPB), C = 1, D = median(data$event)),
  control = nls.lm.control(maxiter = 1000)  # Increase maximum iterations
)

summary(mod.4)


# Calculate the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals(mod.4)^2))
rmse

# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$shrimp_QPB - mean(data$shrimp_QPB))^2)
r_squared_mod.4 <- 1 - (ss_residuals / ss_total)

# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared_mod.4), "\n")




residuals <- resid(mod.4)
y <- data$shrimp_QPB
rsquared <- 1 - sum(residuals^2) / sum((y - mean(y))^2)
rsquared


# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(shrimp_QPB), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, shrimp_QPB = predictions), aes(x = event, y = shrimp_QPB), color = "red") +
  labs(title = "Logistic Curve Fit",
       x = "Event",
       y = "Decapoda QPB") +
  theme_minimal()

mod.4.plot

###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(shrimp_QPB ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$shrimp_QPB - mean(data$shrimp_QPB))^2)
ss_residual <- sum(residuals(mod.5)^2)
r_squared_mod.5 <- 1 - (ss_residual / ss_total)

# Calculate p-values using t-distribution
coefs <- coef(mod.5)
std_errors <- sqrt(diag(vcov(mod.5)))
t_values <- coefs / std_errors
p_values <- 2 * (1 - pt(abs(t_values), df = length(data$event) - length(coefs)))

# Display results
cat("R-squared:", r_squared_mod.5, "\n")
cat("Parameter estimates:\n")
print(coefs)
cat("Standard errors:\n")
print(std_errors)
cat("t-values:\n")
print(t_values)
cat("p-values:","\n")
print(p_values)


# Create a data frame with predicted values
pred_data <- data.frame(event = data$event, 
                        canopy_QPB_pred = predict(mod.5, newdata = data))

# Create a ggplot
mod.5.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = canopy_QPB_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Decapoda QPB") +
  theme_minimal()

mod.5.plot



###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nlsLM(
  shrimp_QPB ~ exponential(event, A, B, C),
  data = data,
  start = list(A = 1, B = 0.1, C = 0)
)

# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$canopy_QPB - mean(data$shrimp_QPB))^2)
# Calculate residual sum of squares
residual_ss <- sum(fit_summary$residuals^2)
# Calculate R-squared value
rsquared <- 1 - residual_ss / total_ss
# Extract p-values
p_values <- fit_summary$coefficients[, "Pr(>|t|)"]
# Print the results
cat("R-squared value:", rsquared, "\n")
cat("P-values for parameters:\n")
print(p_values)


# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(shrimp_QPB), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "Decapoda QPB") +
  ggtitle("Exponential Curve Fitting") +
  theme_minimal()

mod.6.plot


###########################################################################
# Gompertz asymmetric sigmoid model curve (mod.7) -------------------------
# Define the Gompertz function
gompertz_asymmetric <- function(x, A, b, c, d) {
  A * exp(-b * exp(-c * x)) + d
}

# Initial parameter values
initial_params <- list(A = max(shrimp_QPB) - min(shrimp_QPB), 
                       b = 1, 
                       c = 0.1, 
                       d = min(shrimp_QPB))

# Fit the nonlinear model using nlsLM with adjusted initial parameters
mod.7 <- nlsLM(
  shrimp_QPB ~ gompertz_asymmetric(event, A, b, c, d),
  data = data,
  start = initial_params,
  control = nls.lm.control(maxiter = 200, ftol = 1e-6)
)


# Calculate the residual sum of squares (rss)
rss <- sum(residuals(mod.7)^2)
# Calculate the total sum of squares (tss)
tss <- sum((data$shrimp_QPA - mean(data$shrimp_QPB))^2)
# Calculate R-squared value
rsquared_mod.7 <- 1 - (rss / tss)
# Print the R-squared value
cat("R-squared value:", rsquared_mod.7, "\n")
# Print the summary of the model
summary(mod.7)

curve_data <- data.frame(event = seq(1, length(shrimp_QPB), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "blue") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Canopy QPB") +
  theme_minimal()

mod.7.plot


###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using a different optimization algorithm with an increased iteration limit
mod.8 <- nls(shrimp_QPB ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(shrimp_QPB)),
             algorithm = "port",
             control = list(maxiter = 100))  # Increase the maximum number of iterations


# Print summary of the model
summary(mod.8)

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$canopy_QPB - mean(data$shrimp_QPB))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "canopy_QPA") +
  theme_minimal()

mod.8.plot

(mod.1.plot + mod.2.plot + mod.3.plot + mod.4.plot) /
  (mod.5.plot + mod.6.plot + mod.7.plot + mod.8.plot)



###########################################################################
# Function to compute AICc
AICc <- function(fit, return.K = FALSE) {
  n <- length(residuals(fit))
  k <- length(coef(fit)) + 1  # Including the intercept
  aic <- -2 * logLik(fit) + 2 * k
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
  if (return.K) return(k) else return(aicc)
}

n <- length(data$decapoda_QPB)  # Number of observations

aic_mod.1 <- AICc(mod.1)
aic_mod.2 <- AICc(mod.2)
aic_mod.3 <- AICc(mod.3)
aic_mod.4 <- AICc(mod.4)
aic_mod.5 <- AICc(mod.5)
aic_mod.6 <- AICc(mod.6)
aic_mod.7 <- AICc(mod.7)
aic_mod.8 <- AICc(mod.8)

# Store AICc values in a vector
aic_values <- c(aic_mod.1, aic_mod.2, aic_mod.3, aic_mod.4, aic_mod.5, aic_mod.6, aic_mod.7, aic_mod.8)

# Sort AICc values in ascending order
sorted_indices <- order(aic_values)

# Print sorted AICc values and corresponding model numbers
for (i in sorted_indices) {
  cat("AICc Mod.", i, ":", aic_values[i], "\n")
}


# AIC weight  -------------------------------------------------------------
# Compute ??AICc
delta_aic <- aic_values - min(aic_values)

# Compute Akaike weights
akaike_weights <- exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic))

# Combine into a table
model_table <- data.frame(
  Model = paste0("Mod.", 1:8),
  AICc = aic_values,
  Delta_AICc = delta_aic,
  Akaike_Weight = akaike_weights
)

# Sort table by AICc
model_table <- model_table[order(model_table$AICc), ]
print(model_table)

