



# ---------------------------------------------
# Long-term ecosystem response: Leaf litter fall Quebrada Prieta B
# 21 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Create a data frame with your leaf litter data (2017-10-30 to 2022-09-14)
leaflitter_QPB <- c(-0.881529659, -0.946126372, 
                    -1.439828476, -1.023895647, -1.155725351, -1.592679052, -0.788258216, 
                    -1.643160676, -1.073264013, -1.256619939, -0.840765857, -0.502305706, 
                    -0.598869913, -0.70151056, -1.162473227, -0.817998155, -0.947264438, 
                    -0.849296553, -0.70151056, -1.49645676, -0.894219391, -0.936551829, 
                    -0.977840436, -1.102238876, -1.236164349, -1.344213624, -1.110259713, 
                    -1.592403782, -0.818693844, -1.517424033, -0.136211136, 0.801695445, 
                    -0.899936415, -1.181668619, -0.829219278, -0.510525117, -0.276555603, 
                    -0.270391739, -0.763202415, -0.514927158, -0.207406064, -0.514130386, 
                    -0.787170279, -0.998968675, -0.728808123, -0.590584485, -1.133567269, 
                    -1.020126191, -1.035483352, -1.252052964, -1.701579112, -1.237738968, 
                    -0.133874299, -0.235070008, -1.495950815, -0.966139987, -1.988234189, 
                    -1.168609357, -0.495524754, -0.401234574, 0.007524237, 0.332921197, 
                    -0.007038695, -0.198511569, -0.576370464, -0.527011486, -0.493142973, 
                    1.263124688, -0.167082689, -1.013624257, -0.867645692, -0.867645692, 
                    -0.736701466, -0.880997605, -1.098401749, -1.459996491, -1.170577593, 
                    -1.380474715, -1.19369576, -0.988797152, -1.517067507, -0.395008864, 
                    -0.545772984, -0.7124547, 0.229161465, 0.434497298, 0.34879403, 0.491349127, 
                    0.042851553, -0.568133448, -0.298349883, -0.404337914, -0.330591928, 
                    -0.757347211, -0.6529513, 0.082377342, -0.445387166, -0.55238588, 
                    -0.830664876, -0.169119952, -1.225903184, -0.005588603, -1.445335184, 
                    -1.307996861, 3.002915516, -1.324100558, -1.006813101, -0.470105039, 
                    0.018336743, 0.440986107, -0.19981113, -0.183622175, -0.208732115, 
                    -0.410167047, -0.732243959, -0.694876234, -0.122126395, -0.374055086)

event <- seq(1, length(leaflitter_QPB))
data <- data.frame(event, leaflitter_QPB)



###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(leaflitter_QPB ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared <- summary(mod.1)$r.squared
p_value <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared, "\n")
cat("P-value:", p_value, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = leaflitter_QPB)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Leaf litterfall QPA and Trend Line",
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
mod.2 <- nlsLM(leaflitter_QPB ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$leaflitter_QPB
mean_observed <- mean(observed_values)
ss_total <- sum((observed_values - mean_observed)^2)
ss_residual <- sum((observed_values - fitted_values)^2)
r_square_mod.2 <- 1 - ss_residual / ss_total

# Print R-squared value and p-values
cat("R-squared:", r_square_mod.2, "\n")
pvalue <- summary(mod.2)$coefficients[4, 4]  # P-value for the 'tau' parameter
cat("P-value:", pvalue, "\n")


# Calculate the predicted values from the model
predicted_values <- predict(mod.2, newdata = data.frame(event = event))

# Create a ggplot
mod.2.plot <- ggplot(data, aes(x = event, y = leaflitter_QPB)) +
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
mod.3 <- lm(leaflitter_QPB ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("p_value.mod3:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(leaflitter_QPB), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = leaflitter_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, leaflitter_QPB = predictions), 
            aes(x = event, y = leaflitter_QPB), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Canopy QPA") +
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
mod.4 <- nlsLM(leaflitter_QPB ~ logistic_function(event, A, B, C, D),
               data = data,
               start = start_params)

# Get model summary
summary(mod.4)


# Calculate residuals
residuals <- residuals(mod.4)
# Calculate R-squared value
ss_residuals <- sum(residuals^2)
ss_total <- sum((data$canopy_QPB - mean(data$leaflitter_QPB))^2)
r_squared <- 1 - (ss_residuals / ss_total)
# Print R-squared value
cat("R-squared:", sprintf("%.4f", r_squared), "\n")

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(leaflitter_QPB), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Create a ggplot for visualization
mod.4.plot <- ggplot(data, aes(x = event, y = leaflitter_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, leaflitter_QPB = predictions), 
            aes(x = event, y = leaflitter_QPB), color = "red") +
  labs(title = "Logistic Curve Fit",
       x = "Event",
       y = "leaf litterfall QPB") +
  theme_minimal()

mod.4.plot



###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(leaflitter_QPB ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$leaflitter_QPB - mean(data$leaflitter_QPB))^2)
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
                        leaflitter_QPB_pred = predict(mod.5, newdata = data))

# Create a ggplot
mod.5.plot <- ggplot(data, aes(x = event, y = leaflitter_QPB)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = leaflitter_QPB_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Leaf litterfall QPB") +
  theme_minimal()

mod.5.plot




###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nls(leaflitter_QPB ~ exponential(event, A, B, C), 
             data = data,
             start = list(A = 1, B = 0.1, C = 0),
             )

# Get summary of the fitted model
fit_summary <- summary(mod.6)

# Calculate total sum of squares
total_ss <- sum((data$leaflitter_QPB - mean(data$leaflitter_QPB))^2)
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
new_data <- data.frame(event = seq(1, length(leaflitter_QPB), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = leaflitter_QPB)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "Leaf litterfall QPB") +
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

# Adjusted starting values
start_A <- 1
start_b <- 1
start_c <- 0.1  # Adjusted based on expectation of the growth rate
start_d <- 0

# Fit the Gompertz asymmetric model with adjusted starting values
mod.7 <- nlsLM(leaflitter_QPB ~ gompertz_asymmetric(event, A, b, c, d),
               data = data,
               start = list(A = start_A, b = start_b, c = start_c, d = start_d))


rss <- sum(residuals(mod.7)^2)
tss <- sum((data$leaflitter_QPB - mean(data$leaflitter_QPB))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(leaflitter_QPB), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = leaflitter_QPB)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "red") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Leaf litterfall QPB") +
  theme_minimal()

mod.7.plot


###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using nonlinear least squares with adjusted initial values and different algorithm
mod.8 <- nls(leaflitter_QPB ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(leaflitter_QPB)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$leaflitter_QPB - mean(data$leaflitter_QPB))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = leaflitter_QPB)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "Leaf litterfall QPB") +
  theme_minimal()

mod.8.plot




###########################################################################
# Goodness-of-fit diagnostics based on the log-likelihood -----------------
# Calculate log-likelihood for all models
log_likelihood_mod.1 <- sum(dnorm(data$leaflitter_QPB, mean = fitted(mod.1), sd = sqrt(sum((data$leaflitter_QPB - fitted(mod.1))^2) / (length(data$leaflitter_QPB) - 2)), log = TRUE))
log_likelihood_mod.2 <- sum(dnorm(data$leaflitter_QPB, mean = fitted(mod.2), sd = sqrt(sum((data$leaflitter_QPB - fitted(mod.2))^2) / (length(data$leaflitter_QPB) - 2)), log = TRUE))
log_likelihood_mod.3 <- sum(dnorm(data$leaflitter_QPB, mean = fitted(mod.3), sd = sqrt(sum((data$leaflitter_QPB - fitted(mod.3))^2) / (length(data$leaflitter_QPB) - 2)), log = TRUE))
log_likelihood_mod.4 <- sum(dnorm(data$leaflitter_QPB, mean = fitted(mod.4), sd = sqrt(sum((data$leaflitter_QPB - fitted(mod.4))^2) / (length(data$leaflitter_QPB) - 2)), log = TRUE))
log_likelihood_mod.5 <- sum(dnorm(data$leaflitter_QPB, mean = fitted(mod.5), sd = sqrt(sum((data$leaflitter_QPB - fitted(mod.5))^2) / (length(data$leaflitter_QPB) - 2)), log = TRUE))
log_likelihood_mod.6 <- sum(dnorm(data$leaflitter_QPB, mean = fitted(mod.6), sd = sqrt(sum((data$leaflitter_QPB - fitted(mod.6))^2) / (length(data$leaflitter_QPB) - 2)), log = TRUE))
log_likelihood_mod.7 <- sum(dnorm(data$leaflitter_QPB, mean = fitted(mod.7), sd = sqrt(sum((data$leaflitter_QPB - fitted(mod.7))^2) / (length(data$leaflitter_QPB) - 2)), log = TRUE))
log_likelihood_mod.8 <- sum(dnorm(data$leaflitter_QPB, mean = fitted(mod.8), sd = sqrt(sum((data$leaflitter_QPB - fitted(mod.8))^2) / (length(data$leaflitter_QPB) - 2)), log = TRUE))


# Calculate AIC and BIC for mod.1
aic_mod.1 <- -2 * log_likelihood_mod.1 + 2 * length(coef(mod.1))
bic_mod.1 <- -2 * log_likelihood_mod.1 + log(length(data$leaflitter_QPB)) * length(coef(mod.1))

# Calculate AIC and BIC for mod.2
aic_mod.2 <- -2 * log_likelihood_mod.2 + 2 * length(coef(mod.2))
bic_mod.2 <- -2 * log_likelihood_mod.2 + log(length(data$leaflitter_QPB)) * length(coef(mod.2))

# Calculate AIC and BIC for mod.3
aic_mod.3 <- -2 * log_likelihood_mod.3 + 2 * length(coef(mod.3))
bic_mod.3 <- -2 * log_likelihood_mod.3 + log(length(data$leaflitter_QPB)) * length(coef(mod.3))

# Calculate AIC and BIC for mod.4
aic_mod.4 <- -2 * log_likelihood_mod.4 + 2 * length(coef(mod.4))
bic_mod.4 <- -2 * log_likelihood_mod.4 + log(length(data$leaflitter_QPB)) * length(coef(mod.4))

# Calculate AIC and BIC for mod.5
aic_mod.5 <- -2 * log_likelihood_mod.5 + 2 * length(coef(mod.5))
bic_mod.5 <- -2 * log_likelihood_mod.5 + log(length(data$leaflitter_QPB)) * length(coef(mod.5))

# Calculate AIC and BIC for mod.6
aic_mod.6 <- -2 * log_likelihood_mod.6 + 2 * length(coef(mod.6))
bic_mod.6 <- -2 * log_likelihood_mod.6 + log(length(data$leaflitter_QPB)) * length(coef(mod.6))

# Calculate AIC and BIC for mod.7
aic_mod.7 <- -2 * log_likelihood_mod.7 + 2 * length(coef(mod.7))
bic_mod.7 <- -2 * log_likelihood_mod.7 + log(length(data$leaflitter_QPB)) * length(coef(mod.7))

# Calculate AIC and BIC for mod.8
aic_mod.8 <- -2 * log_likelihood_mod.8 + 2 * length(coef(mod.8))
bic_mod.8 <- -2 * log_likelihood_mod.8 + log(length(data$leaflitter_QPB)) * length(coef(mod.8))


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

