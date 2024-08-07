



# ---------------------------------------------
# Long-term ecosystem response: Canopy openness Quebrada Prieta A
# 17 Aug 2023
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Create a data frame with epilithon_QPA data (2017-10-03 (H. Maria, time 0) to 2022-09-01)
epilithon_QPA <- c(-0.326974628, 
                   -0.052401445, -0.424571057, -0.270306082, -0.314461617, -0.77198127, 
                   -0.525529712, 0.353432899, 0.012869536, 0.010726568, 0.172358217, 
                   -0.339336835, 0.057037776, -0.309326024, 0.229468207, 0.277150141, 
                   0.242224405, 0.006403858, -0.036876889, -0.097585005, -0.049060763, 
                   -0.655417682, -0.33919034, -0.579209274, -0.067344119, -0.275802251, 
                   -0.106530181, 0.218548616, 0.315346245, -0.104746737, -0.113798581, 
                   -1.216284093, -0.030794936, 0.128182961, 0.296216546, 0.109570674, 
                   0.135445071, 0.163041949, 0.135653819, -0.172758478, -0.042531071, 
                   0.025660758, 0.543062826, 0.494435909, -0.055966918, -0.086641353, 
                   0.009111772, 0.168652927, 0.589891, 0.359543263, 0.249273612, 
                   0.190985156, 0.776090885, 0.219440474, 0.449951267, 0.503635106, 
                   0.160896007, 0.241941622, 0.53811738, 0.148764519)


event <- seq(1, length(epilithon_QPA))
data <- data.frame(event, epilithon_QPA)



###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(epilithon_QPA ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared <- summary(mod.1)$r.squared
p_value <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared, "\n")
cat("P-value:", p_value, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
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
mod.2 <- nlsLM(epilithon_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)
# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$epilithon_QPA
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
mod.2.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Canopy QPA and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Canopy QPA") +
  theme_minimal()

mod.2.plot

###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(epilithon_QPA ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value_F_statistic.mod3 <- summary(mod.3)$fstatistic["Pr(>F)"]
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("p-value:", p_value_F_statistic.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(epilithon_QPA), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot.algae <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, epilithon_QPA = predictions), 
            aes(x = event, y = epilithon_QPA), color = "blue") +
  labs(title = expression(Quadratic~Model~(italic(y) == beta[0] + beta[1] * italic(x) + beta[2] * italic(x)^2)),
       x = "Sampling event",
       y = expression(Epilithon~(mg~chl-~italic(a) %.% m^{-2}))
       ) +
  geom_rangeframe() + theme_tufte() +
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray50",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray50",size = 0.5,linetype = 3))

mod.3.plot.algae


###########################################################################
# Logistic curve (mod.4) --------------------------------------------------
# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}


# Try different starting parameter values
start_params <- list(A = min(data$epilithon_QPA), B = max(data$epilithon_QPA), C = 0.1, D = median(data$event))

# Fit the model using nlsLM algorithm
mod.4 <- nlsLM(epilithon_QPA ~ logistic_function(event, A, B, C, D),
             data = data,
             start = start_params)

mod.4.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(aes(x = event, y = predicted), color = "blue") +
  labs(title = "Custom Logistic Regression",
       x = "Event",
       y = "Epilithon QPA") +
  theme_minimal()

mod.4.plot

###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(epilithon_QPA ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$event - mean(data$epilithon_QPA))^2)
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
                        eplitihon_QPA_pred = predict(mod.5, newdata = data))

# Create a ggplot
mod.5.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = eplitihon_QPA_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Epilithon QPA") +
  theme_minimal()

mod.5.plot



###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nls(epilithon_QPA ~ exponential(event, A, B, C), 
             data = data,
             start = list(A = 1, B = 0.1, C = 0))

# Get summary of the exponential curve fit
mod.6_summary <- summary(mod.6)
# Extract residual standard error
residual_standard_error <- mod.6_summary$sigma
# Calculate residual sum of squares
RSS <- sum(residuals(mod.6)^2)
# Calculate total sum of squares
TSS <- sum((data$epilithon_QPA - mean(data$epilithon_QPA))^2)
# Calculate R-squared
R_squared <- 1 - (RSS / TSS)
R_squared


# Extract the residual sum of squares
RSS <- sum(residuals(mod.6)^2)
# Calculate the degrees of freedom for residuals
df_residual <- nrow(data) - length(coef(mod.6))
# Obtain the total sum of squares
TSS <- sum((data$epilithon_QPA - mean(data$epilithon_QPA))^2)
# Calculate the F-statistic
F_statistic <- ((TSS - RSS) / 2) / (RSS / df_residual)
# Calculate the p-value associated with the F-statistic
p_value <- pf(F_statistic, 2, df_residual, lower.tail = FALSE)
p_value


# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(epilithon_QPA), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)


# Plot the data and fitted curve using ggplot2
mod.6.plot.algae <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(title = expression(paste("Exponential Curve: ", italic(y) == A + B %.% e^{C * italic(x)} + D)),
    x = "Sampling event",
       y = expression(Epilithon~(mg~chl-~italic(a) %.% m^{-2}))
  ) +
  geom_rangeframe() + theme_tufte() +
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray50",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray50",size = 0.5,linetype = 3))

mod.6.plot.algae


###########################################################################
# Gompertz asymmetric sigmoid model curve (mod.7) -------------------------
# Gompertz function
gompertz_asymmetric <- function(x, A, b, c, d) {
  y = A * exp(-b * exp(-c * x)) + d
  return(y)
}


mod.7 <- nlsLM(epilithon_QPA ~ gompertz_asymmetric(event, A, b, c, d),
               data = data,
               start = list(A = 1, b = 1, c = 1, d = 0))

rss <- sum(residuals(mod.7)^2)
tss <- sum((data$epilithon_QPA - mean(data$epilithon_QPA))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(epilithon_QPA), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "red") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Epilithon QPA") +
  theme_minimal()

mod.7.plot


###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using nonlinear least squares with adjusted initial values and different algorithm
mod.8 <- nls(epilithon_QPA ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(epilithon_QPA)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$canopy_QPA - mean(data$epilithon_QPA))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = epilithon_QPA)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Fitted Goniometric Curve",
       x = "Event",
       y = "Epilithon QPA") +
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

n <- length(data$epilithon_QPA)  # Number of observations

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
