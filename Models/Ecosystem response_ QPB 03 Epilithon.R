



# ---------------------------------------------
# Long-term ecosystem response: Chlorophyll-a Quebrada Prieta B
# 17 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Create a data frame with your canopy_QPA data (2017-01-01 to 2022-09-01)
chlorophyll_QPB <- c(-0.739818027, 
                     -0.631285253, -1.005281876, -0.586985814, -0.704420806, -1.267086154, 
                     -0.51728118, -0.499008016, -0.389993121, -0.380058376, -0.19471036, 
                     -0.818682043, -0.243142403, -0.234457311, -0.351418593, -0.321838209, 
                     -0.25677644, -0.835178915, -0.517806213, -0.604142102, -0.415562593, 
                     -1.278258443, -0.887589297, -1.272011892, -0.215530212, -0.198301758, 
                     -0.462592338, 0.036101327, 0.191078061, -0.683301671, -0.459379957, 
                     -1.553418692, -0.560293394, -0.198035461, 0.086767025, -0.516006206, 
                     -0.328713949, 0.433791523, 0.021171656, -0.272604249, 0.083860925, 
                     -0.10855559, 0.285803829, -0.374389114, 0.269429207, -0.467751563, 
                     -0.021651399, -0.173116019, 0.265943762, 0.061304029, 0.747564142, 
                     0.041108448, 0.684064969, 0.517588636, 0.369352134, -0.077452898, 
                     0.087514826, 0.291396042, 0.53368889, 0.187990095)


event <- seq(1, length(chlorophyll_QPB))
data <- data.frame(event, chlorophyll_QPB)



###########################################################################
# Linear model (mod.1) ----------------------------------------------------
# Create a linear model
mod.1 <- lm(chlorophyll_QPB ~ event, data = data)
# Print the summary of the linear model
summary(mod.1)

# Get R-squared value and p-value
r_squared <- summary(mod.1)$r.squared
p_value <- summary(mod.1)$coefficients[2, 4]
# Print R-squared value and p-values
cat("R-squared:", r_squared, "\n")
cat("P-value:", p_value, "\n")

# Create a ggplot
mod.1.plot <- ggplot(data, aes(x = event, y = chlorophyll_QPB)) +
  geom_point() +         # Scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Trend line without confidence interval
  labs(title = "Epilithon algae and Trend Line",
       x = "Event",
       y = "Epilithon algae QPB") +
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
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 10)

# Fit the model using nlsLM with increased maximum iterations and refined starting values
mod.2 <- nlsLM(chlorophyll_QPB ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params,
               control = nls.lm.control(maxiter = 1000, ftol = 1e-6, ptol = 1e-6))

# Check the summary of the model
summary(mod.2)


# Extract R-squared and p-value
# Calculate the R-squared value manually
fitted_values <- fitted(mod.2)
observed_values <- data$chlorophyll_QPB
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
mod.2.plot <- ggplot(data, aes(x = event, y = chlorophyll_QPB)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_values), color = "blue") +
  labs(title = "Epilithon algae and Fitted Nelson-Siegel Curve",
       x = "Event",
       y = "Epilithon algae QPB") +
  theme_minimal()

mod.2.plot

###########################################################################
# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(chlorophyll_QPB ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients["I(event^2)","Pr(>|t|)"] # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("p-value:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(chlorophyll_QPB), length.out = 100))

# Predict using the model
predictions <- predict(mod.3, newdata = new_data)

# Create a ggplot for visualization
mod.3.plot <- ggplot(data, aes(x = event, y = chlorophyll_QPB)) +
  geom_point() +
  geom_line(data = data.frame(event = new_data$event, chlorophyll_QPB = predictions), 
            aes(x = event, y = chlorophyll_QPB), color = "blue") +
  labs(title = "Inverted Parabolic Curve Fit",
       x = "Event",
       y = "Epilithon algae QPB") +
  theme_minimal()

mod.3.plot


###########################################################################
# Logistic curve (mod.4) --------------------------------------------------
# Define the logistic function
# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

# Starting parameter values
start_params <- list(A = min(chlorophyll_QPB), 
                     B = max(chlorophyll_QPB), 
                     C = 0.1, 
                     D = median(event))


# Fit the logistic model
mod.4 <- try(nlsLM(chlorophyll_QPB ~ logistic_function(event, A, B, C, D),
                   data = data,
                   start = start_params,
                   control = nls.lm.control(maxiter = 1000, ftol = 1e-6, ptol = 1e-6)),
             silent = TRUE)

# Generate predictions
data$predicted <- predict(mod.4, newdata = data)

# Create the plot with ggplot2
mod.4.plot <- ggplot(data, aes(x = event)) +
  geom_point(aes(y = chlorophyll_QPB), color = "black") +  # Original data points
  geom_line(aes(y = predicted), color = "blue") +  # Fitted logistic curve
  labs(title = "Custom Logistic Regression",
       x = "Event",
       y = "Chlorophyll QPB") +
  theme_minimal()

mod.4.plot

###########################################################################
# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5 <- nls(chlorophyll_QPB ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))

# Get summary of the model
summary(mod.5)

# Calculate R-squared manually
ss_total <- sum((data$chlorophyll_QPB - mean(data$chlorophyll_QPB))^2)
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
mod.5.plot <- ggplot(data, aes(x = event, y = chlorophyll_QPB)) +
  geom_point() +
  geom_line(data = pred_data, aes(x = event, y = canopy_QPB_pred), color = "blue") +
  labs(title = "Logarithmic Curve Fitting",
       x = "Event",
       y = "Epilithon algae QPB") +
  theme_minimal()

mod.5.plot



###########################################################################
# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}

# Fit the exponential curve
mod.6 <- nls(chlorophyll_QPB ~ exponential(event, A, B, C), 
             data = data,
             start = list(A = 1, B = 0.1, C = 0))

summary(mod.6)

# Get summary of the exponential curve fit
mod.6_summary <- summary(mod.6)
# Extract residual standard error
residual_standard_error <- mod.6_summary$sigma
# Calculate residual sum of squares
RSS <- sum(residuals(mod.6)^2)
# Calculate total sum of squares
TSS <- sum((data$chlorophyll_QPB - mean(data$chlorophyll_QPB))^2)
# Calculate R-squared
R_squared <- 1 - (RSS / TSS)
R_squared


# Extract the residual sum of squares
RSS <- sum(residuals(mod.6)^2)
# Calculate the degrees of freedom for residuals
df_residual <- nrow(data) - length(coef(mod.6))
# Obtain the total sum of squares
TSS <- sum((data$chlorophyll_QPB - mean(data$chlorophyll_QPB))^2)
# Calculate the F-statistic
F_statistic <- ((TSS - RSS) / 2) / (RSS / df_residual)
# Calculate the p-value associated with the F-statistic
p_value <- pf(F_statistic, 2, df_residual, lower.tail = FALSE)
p_value



# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(chlorophyll_QPB), length.out = 100))
new_data$predicted <- predict(mod.6, newdata = new_data)

# Plot the data and fitted curve using ggplot2
mod.6.plot <- ggplot(data, aes(x = event, y = chlorophyll_QPB)) +
  geom_point() +
  geom_line(data = new_data, aes(x = event, y = predicted), color = "blue") +
  labs(x = "Event", y = "Epilithon algae QPB") +
  ggtitle("Exponential Curve Fitting") +
  theme_minimal()

mod.6.plot


###########################################################################
# Gompertz asymmetric sigmoid model curve (mod.7) -------------------------
# Define the Gompertz function
gompertz_asymmetric <- function(x, A, b, c, d) {
  y = A * exp(-b * exp(-c * x)) + d
  return(y)
}

# Initial parameter values
start_params <- list(A = max(chlorophyll_QPB), b = 1, c = 0.1, d = min(chlorophyll_QPB))

# Fit the Gompertz asymmetric model with modified control parameters
mod.7 <- try(nlsLM(chlorophyll_QPB ~ gompertz_asymmetric(event, A, b, c, d),
                   data = data,
                   start = start_params,
                   control = nls.lm.control(maxiter = 1000, ftol = 1e-6, ptol = 1e-6)),
             silent = TRUE)


rss <- sum(residuals(mod.7)^2)
tss <- sum((data$canopy_QPA - mean(data$chlorophyll_QPB))^2)
rsquared_mod.7 <- 1 - (rss / tss)
pvalue_mod.7 <- summary(mod.7)$coefficients[,"Pr(>|t|)"]["A"]

cat("R-squared value:", rsquared_mod.7, "\n")
cat("p-value value:", pvalue_mod.7, "\n")


curve_data <- data.frame(event = seq(1, length(chlorophyll_QPB), length.out = 100))
curve_data$predicted <- predict(mod.7, newdata = curve_data)

mod.7.plot <- ggplot(data, aes(x = event, y = chlorophyll_QPB)) +
  geom_point() +
  geom_line(data = curve_data, aes(x = event, y = predicted), color = "red") +
  labs(title = "Gompertz Asymmetric Sigmoid Model Fit",
       x = "Event", y = "Epilithon algae QPB") +
  theme_minimal()

mod.7.plot


###########################################################################
# Goniometric curve (mod.8) -----------------------------------------------
# Define the goniometric function
goniometric <- function(x, a, b, c, d) {
  a * sin(b * x + c) + d
}

# Fit the model using nonlinear least squares with adjusted initial values and different algorithm
mod.8 <- nls(chlorophyll_QPB ~ goniometric(event, a, b, c, d), 
             data = data,
             start = list(a = 1, b = 1, c = 0, d = mean(chlorophyll_QPB)),
             algorithm = "port")

# Extract coefficients
coefficients <- coef(mod.8)
# Print coefficients
print(coefficients)
# Calculate R-squared value
residuals <- residuals(mod.8)
SSR <- sum(residuals^2)
SST <- sum((data$canopy_QPA - mean(data$chlorophyll_QPB))^2)
R_squared <- 1 - SSR / SST
cat("R-squared value:", R_squared, "\n")

# Calculate p-value
summary_fit <- summary(mod.8)
p_value.mod8 <- summary_fit$coefficients[4, "Pr(>|t|)"]
cat("p-value:", p_value.mod8, "\n")


# Generate predicted values from the fitted model
data$predicted <- predict(mod.8)

# Create a ggplot with the original data and fitted curve
mod.8.plot <- ggplot(data, aes(x = event, y = chlorophyll_QPB)) +
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

