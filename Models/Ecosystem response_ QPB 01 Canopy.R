



# ---------------------------------------------
# Long-term ecosystem response: Canopy openness Quebrada Prieta B
# 17 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Create a data frame with your canopy_QPB data (2017-09-10 to 2022-09-01)
canopy_QPB <- c(1.913568932, 1.551896958, 1.536349726, 1.490548934,
                1.39148244, 1.294424291, 1.13646499, 1.016906589, 1.246987452,
                1.185334648, 1.109390578, 1.240990432, 1.181352166, 1.195615482,
                1.141460837, 0.782647871, 1.030918963, 0.925352283, 1.106814364,
                1.309983304, 1.233443227, 1.201890013, 1.067352865, 1.085962175,
                0.96675143, 0.881087944, 0.923288298, 1.041071334, 0.774289613,
                0.459874479, 0.852670731, 0.967740061, 0.80962898, 0.85930059,
                0.704782735, 0.823422302, 0.573085869, 0.148375163, 0.631465433,
                0.654706397, 0.614728641, 0.627307423, 0.748787321, 0.398941928,
                0.379558061, 0.34147549, 0.421376001, 0.43325061, 0.404163872,
                0.339625351, 0.449973408, 0.421376001, 0.27459288, 0.305723799,
                0.256646237, 0.161738391, 0.270632479, 0.155079099, 0.388415515
)

event <- seq(1, length(canopy_QPB))
data <- data.frame(event, canopy_QPB)



###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(canopy_QPB ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared <- summary(mod.1)$r.squared
p_value <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared, "\n")
cat("P-value:", p_value, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Canopy QPB and Trend Line",
       x = "Event",
       y = "Canopy QPB") +
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
mod.2 <- nlsLM(canopy_QPB ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$canopy_QPB
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
mod.2.plot <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Canopy QPB and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Canopy QPB") +
  theme_minimal()

mod.2.plot

###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(canopy_QPB ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(canopy_QPB), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, canopy_QPB = predictions), 
            aes(x = event, y = canopy_QPB), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Canopy QPB") +
  theme_minimal()

mod.3.plot


###########################################################################
# Logistic curve (mod.4) --------------------------------------------------
# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

# Fit a logistic curve model
mod.4 <- nls(canopy_QPB ~ logistic_function(event, A, B, C, D),
             data = data,
             start = list(A = min(canopy_QPB), B = max(canopy_QPB), C = 1, D = median(event)))

# Get model summary
summary(mod.4)


# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$canopy_QPB - mean(data$canopy_QPB))^2)
r_squared <- 1 - (ss_residuals / ss_total)
# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared), "\n")

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(canopy_QPB), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, canopy_QPB = predictions), aes(x = event, y = canopy_QPB), color = "red") +
  labs(title = "Logistic Curve Fit",
       x = "Event",
       y = "Canopy QPB") +
  theme_minimal()

mod.4.plot

###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(canopy_QPB ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$canopy_QPB - mean(data$canopy_QPB))^2)
ss_residual <- sum(residuals(mod.5)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Calculate p-values using t-distribution
coefs <- coef(mod.5)
std_errors <- sqrt(diag(vcov(mod.5)))
t_values <- coefs / std_errors
p_values <- 2 * (1 - pt(abs(t_values), df = length(data$event) - length(coefs)))

# Display results
cat("R-squared:", r_squared, "\n")
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
mod.5.plot <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = canopy_QPB_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Canopy QPB") +
  theme_minimal()

mod.5.plot


###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nls(canopy_QPB ~ exponential(event, A, B, C), 
             data = data,
             start = list(A = 1, B = 0.1, C = 0))

# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$canopy_QPB - mean(data$canopy_QPB))^2)
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
new_data <- data.frame(event = seq(1, length(canopy_QPB), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "canopy_QPB") +
  ggtitle("Exponential Curve Fitting") +
  theme_minimal()

mod.6.plot

###########################################################################
# Gompertz asymmetric sigmoid model curve (mod.7) -------------------------
# Gompertz function
gompertz_asymmetric <- function(x, A, b, c, d) {
  y = A * exp(-b * exp(-c * x)) + d
  return(y)
}


mod.7 <- nlsLM(canopy_QPB ~ gompertz_asymmetric(event, A, b, c, d),
               data = data,
               start = list(A = 1, b = 1, c = 1, d = 0))

rss <- sum(residuals(mod.7)^2)
tss <- sum((data$canopy_QPA - mean(data$canopy_QPB))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(canopy_QPB), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "red") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Canopy QPA") +
  theme_minimal()

mod.7.plot


###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using nonlinear least squares with adjusted initial values and different algorithm
mod.8 <- nls(canopy_QPB ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(canopy_QPB)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$canopy_QPB - mean(data$canopy_QPB))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = canopy_QPB)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "canopy_QPB") +
  theme_minimal()

mod.8.plot

(mod.1.plot + mod.2.plot + mod.3.plot + mod.4.plot) /
  (mod.5.plot + mod.6.plot + mod.7.plot + mod.8.plot)



###########################################################################
# Goodness-of-fit diagnostics based on the log-likelihood -----------------
# Calculate log-likelihood for all models
log_likelihood_mod.1 <- sum(dnorm(data$canopy_QPB, mean = fitted(mod.1), sd = sqrt(sum((data$canopy_QPB - fitted(mod.1))^2) / (length(data$canopy_QPB) - 2)), log = TRUE))
log_likelihood_mod.2 <- sum(dnorm(data$canopy_QPB, mean = fitted(mod.2), sd = sqrt(sum((data$canopy_QPB - fitted(mod.2))^2) / (length(data$canopy_QPB) - 2)), log = TRUE))
log_likelihood_mod.3 <- sum(dnorm(data$canopy_QPB, mean = fitted(mod.3), sd = sqrt(sum((data$canopy_QPB - fitted(mod.3))^2) / (length(data$canopy_QPB) - 2)), log = TRUE))
log_likelihood_mod.4 <- sum(dnorm(data$canopy_QPB, mean = fitted(mod.4), sd = sqrt(sum((data$canopy_QPB - fitted(mod.4))^2) / (length(data$canopy_QPB) - 2)), log = TRUE))
log_likelihood_mod.5 <- sum(dnorm(data$canopy_QPB, mean = fitted(mod.5), sd = sqrt(sum((data$canopy_QPB - fitted(mod.5))^2) / (length(data$canopy_QPB) - 2)), log = TRUE))
log_likelihood_mod.6 <- sum(dnorm(data$canopy_QPB, mean = fitted(mod.6), sd = sqrt(sum((data$canopy_QPB - fitted(mod.6))^2) / (length(data$canopy_QPB) - 2)), log = TRUE))
log_likelihood_mod.7 <- sum(dnorm(data$canopy_QPB, mean = fitted(mod.7), sd = sqrt(sum((data$canopy_QPB - fitted(mod.7))^2) / (length(data$canopy_QPB) - 2)), log = TRUE))
log_likelihood_mod.8 <- sum(dnorm(data$canopy_QPB, mean = fitted(mod.8), sd = sqrt(sum((data$canopy_QPB - fitted(mod.8))^2) / (length(data$canopy_QPB) - 2)), log = TRUE))


# Calculate AIC and BIC for mod.1
aic_mod.1 <- -2 * log_likelihood_mod.1 + 2 * length(coef(mod.1))
bic_mod.1 <- -2 * log_likelihood_mod.1 + log(length(data$canopy_QPB)) * length(coef(mod.1))

# Calculate AIC and BIC for mod.2
aic_mod.2 <- -2 * log_likelihood_mod.2 + 2 * length(coef(mod.2))
bic_mod.2 <- -2 * log_likelihood_mod.2 + log(length(data$canopy_QPB)) * length(coef(mod.2))

# Calculate AIC and BIC for mod.3
aic_mod.3 <- -2 * log_likelihood_mod.3 + 2 * length(coef(mod.3))
bic_mod.3 <- -2 * log_likelihood_mod.3 + log(length(data$canopy_QPB)) * length(coef(mod.3))

# Calculate AIC and BIC for mod.4
aic_mod.4 <- -2 * log_likelihood_mod.4 + 2 * length(coef(mod.4))
bic_mod.4 <- -2 * log_likelihood_mod.4 + log(length(data$canopy_QPB)) * length(coef(mod.4))

# Calculate AIC and BIC for mod.5
aic_mod.5 <- -2 * log_likelihood_mod.5 + 2 * length(coef(mod.5))
bic_mod.5 <- -2 * log_likelihood_mod.5 + log(length(data$canopy_QPB)) * length(coef(mod.5))

# Calculate AIC and BIC for mod.6
aic_mod.6 <- -2 * log_likelihood_mod.6 + 2 * length(coef(mod.6))
bic_mod.6 <- -2 * log_likelihood_mod.6 + log(length(data$canopy_QPB)) * length(coef(mod.6))

# Calculate AIC and BIC for mod.7
aic_mod.7 <- -2 * log_likelihood_mod.7 + 2 * length(coef(mod.7))
bic_mod.7 <- -2 * log_likelihood_mod.7 + log(length(data$canopy_QPB)) * length(coef(mod.7))

# Calculate AIC and BIC for mod.8
aic_mod.8 <- -2 * log_likelihood_mod.8 + 2 * length(coef(mod.8))
bic_mod.8 <- -2 * log_likelihood_mod.8 + log(length(data$canopy_QPB)) * length(coef(mod.8))


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

# Store AIC values in a vector
aic_values <- c(aic_mod.1, aic_mod.2, aic_mod.3, aic_mod.4, aic_mod.5, aic_mod.6, aic_mod.7, aic_mod.8)
# Sort AIC values in ascending order
sorted_indices <- order(aic_values)
# Print sorted AIC values and corresponding model numbers
for (i in sorted_indices) {
  cat("AIC Mod.", i, ":", aic_values[i], "\n")
}


cat("BIC Mod.1:", bic_mod.1, "\n")
cat("BIC Mod.2:", bic_mod.2, "\n")
cat("BIC Mod.3:", bic_mod.3, "\n")
cat("BIC Mod.4:", bic_mod.4, "\n")
cat("BIC Mod.5:", bic_mod.5, "\n")
cat("BIC Mod.6:", bic_mod.6, "\n")
cat("BIC Mod.7:", bic_mod.7, "\n")
cat("BIC Mod.8:", bic_mod.8, "\n")



