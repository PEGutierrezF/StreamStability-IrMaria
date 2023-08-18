



# ---------------------------------------------
# Long-term ecosystem response
# 17 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



shrimp_QPB <- c(4.696673669, 7.102844975, 2.92230371, 4.775812621, 3.786680872, 
                5.861103582, 4.841844408, 4.095107269, 8.184963933, 7.032519247, 
                8.948997943, 4.558330701, 7.366546849, 6.695518125, 9.333305943, 
                14.62520908, 8.463158621, 12.83836099, 15.93527468, 13.17293781, 
                5.624433334, 5.197004403, 6.152430647, 7.169357883, 5.89132428, 
                8.384487872, 8.557024587, 8.147560149, 10.10395362, 5.160561525, 
                8.385994999, 6.265705047, 5.676738116, 7.221369143, 6.218530111, 
                5.30139752, 4.76111744, 3.812979913, 4.197876748, 4.487483645, 
                4.804636027, 12.30938817, 6.83983239, 4.741247918, 5.366055518)

event <- seq(1, length(shrimp_QPB))
data <- data.frame(event, shrimp_QPB)


###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(shrimp_QPB ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared <- summary(mod.1)$r.squared
p_value <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared, "\n")
cat("P-value:", p_value, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Canopy QPA and Trend Line",
       x = "Event",
       y = "Canopy QPA") +
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
rsquare <- 1 - ss_residual / ss_total

# Print R-squared value and p-values
cat("R-squared:", rsquare, "\n")
pvalue <- summary(mod.2)$coefficients[4, 4]  # P-value for the 'tau' parameter
cat("P-value:", pvalue, "\n")


# Calculate the predicted values from the model
predicted_values <- predict(mod.2, newdata = data.frame(event = event))

# Create a ggplot
mod.2.plot <- ggplot(data, aes(x = event, y = shrimp_QPB)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "red") +
  labs(title = "Canopy QPA and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Canopy QPA") +
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
       y = "Canopy QPA") +
  theme_minimal()

mod.3.plot




###########################################################################
# Goodness-of-fit diagnostics based on the log-likelihood -----------------
# Calculate log-likelihood for mod.1 (linear model)
log_likelihood_mod.1 <- sum(dnorm(data$shrimp_QPB, mean = fitted(mod.1), sd = sqrt(sum((data$shrimp_QPB - fitted(mod.1))^2) / (length(data$shrimp_QPB) - 2)), log = TRUE))
log_likelihood_mod.2 <- sum(dnorm(data$shrimp_QPB, mean = fitted(mod.2), sd = sqrt(sum((data$shrimp_QPB - fitted(mod.2))^2) / (length(data$shrimp_QPB) - 2)), log = TRUE))
log_likelihood_mod.3 <- sum(dnorm(data$shrimp_QPB, mean = fitted(mod.3), sd = sqrt(sum((data$shrimp_QPB - fitted(mod.3))^2) / (length(data$shrimp_QPB) - 2)), log = TRUE))
log_likelihood_mod.4 <- sum(dnorm(data$shrimp_QPB, mean = fitted(mod.4), sd = sqrt(sum((data$shrimp_QPB - fitted(mod.4))^2) / (length(data$shrimp_QPB) - 2)), log = TRUE))
log_likelihood_mod.5 <- sum(dnorm(data$shrimp_QPB, mean = fitted(mod.5), sd = sqrt(sum((data$shrimp_QPB - fitted(mod.5))^2) / (length(data$shrimp_QPB) - 2)), log = TRUE))
log_likelihood_mod.6 <- sum(dnorm(data$shrimp_QPB, mean = fitted(mod.6), sd = sqrt(sum((data$shrimp_QPB - fitted(mod.6))^2) / (length(data$shrimp_QPB) - 2)), log = TRUE))
log_likelihood_mod.7 <- sum(dnorm(data$shrimp_QPB, mean = fitted(mod.7), sd = sqrt(sum((data$shrimp_QPB - fitted(mod.7))^2) / (length(data$shrimp_QPB) - 2)), log = TRUE))
log_likelihood_mod.8 <- sum(dnorm(data$shrimp_QPB, mean = fitted(mod.8), sd = sqrt(sum((data$shrimp_QPB - fitted(mod.8))^2) / (length(data$shrimp_QPB) - 2)), log = TRUE))


# Calculate AIC and BIC for mod.1
aic_mod.1 <- -2 * log_likelihood_mod.1 + 2 * length(coef(mod.1))
bic_mod.1 <- -2 * log_likelihood_mod.1 + log(length(data$shrimp_QPB)) * length(coef(mod.1))

# Calculate AIC and BIC for mod.2
aic_mod.2 <- -2 * log_likelihood_mod.2 + 2 * length(coef(mod.2))
bic_mod.2 <- -2 * log_likelihood_mod.2 + log(length(data$shrimp_QPB)) * length(coef(mod.2))

# Calculate AIC and BIC for mod.1
aic_mod.3 <- -2 * log_likelihood_mod.3 + 2 * length(coef(mod.3))
bic_mod.3 <- -2 * log_likelihood_mod.3 + log(length(data$shrimp_QPB)) * length(coef(mod.3))

# Calculate AIC and BIC for mod.2
aic_mod.4 <- -2 * log_likelihood_mod.4 + 2 * length(coef(mod.4))
bic_mod.4 <- -2 * log_likelihood_mod.4 + log(length(data$shrimp_QPB)) * length(coef(mod.4))

# Calculate AIC and BIC for mod.1
aic_mod.5 <- -2 * log_likelihood_mod.5 + 2 * length(coef(mod.5))
bic_mod.5 <- -2 * log_likelihood_mod.5 + log(length(data$shrimp_QPB)) * length(coef(mod.5))

# Calculate AIC and BIC for mod.2
aic_mod.6 <- -2 * log_likelihood_mod.6 + 2 * length(coef(mod.6))
bic_mod.6 <- -2 * log_likelihood_mod.6 + log(length(data$shrimp_QPB)) * length(coef(mod.6))

# Calculate AIC and BIC for mod.1
aic_mod.7 <- -2 * log_likelihood_mod.7 + 2 * length(coef(mod.7))
bic_mod.7 <- -2 * log_likelihood_mod.7 + log(length(data$shrimp_QPB)) * length(coef(mod.7))

# Calculate AIC and BIC for mod.2
aic_mod.8 <- -2 * log_likelihood_mod.8 + 2 * length(coef(mod.8))
bic_mod.8 <- -2 * log_likelihood_mod.8 + log(length(data$shrimp_QPB)) * length(coef(mod.8))


# Compare log-likelihoods, AIC, and BIC
cat("Log-Likelihood Mod.1:", log_likelihood_mod.1, "\n")
cat("Log-Likelihood Mod.2:", log_likelihood_mod.2, "\n")
cat("Log-Likelihood Mod.3:", log_likelihood_mod.3, "\n")
cat("Log-Likelihood Mod.4:", log_likelihood_mod.4, "\n")
cat("Log-Likelihood Mod.5:", log_likelihood_mod.5, "\n")
cat("Log-Likelihood Mod.6:", log_likelihood_mod.6, "\n")
cat("Log-Likelihood Mod.7:", log_likelihood_mod.7, "\n")
cat("Log-Likelihood Mod.8:", log_likelihood_mod.8, "\n")

cat("AIC Mod.1:", aic_mod.1, "\n")
cat("AIC Mod.2:", aic_mod.2, "\n")
cat("AIC Mod.3:", aic_mod.3, "\n")
cat("AIC Mod.4:", aic_mod.4, "\n")
cat("AIC Mod.5:", aic_mod.5, "\n")
cat("AIC Mod.6:", aic_mod.6, "\n")
cat("AIC Mod.7:", aic_mod.7, "\n")
cat("AIC Mod.8:", aic_mod.8, "\n")

cat("BIC Mod.1:", bic_mod.1, "\n")
cat("BIC Mod.2:", bic_mod.2, "\n")
cat("BIC Mod.3:", bic_mod.3, "\n")
cat("BIC Mod.4:", bic_mod.4, "\n")
cat("BIC Mod.5:", bic_mod.5, "\n")
cat("BIC Mod.6:", bic_mod.6, "\n")
cat("BIC Mod.7:", bic_mod.7, "\n")
cat("BIC Mod.8:", bic_mod.8, "\n")

