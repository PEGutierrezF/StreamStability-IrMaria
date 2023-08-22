



# ---------------------------------------------
# Figure 3: Long-term ecosystem responses
# 19 Aug 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

###########################################################################
# Canopy openness Prieta A ------------------------------------------------
# Create a data frame with your canopy_QPA data (2017-01-01 to 2022-09-01)
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
data_canopy_QPA <- data.frame(event, canopy_QPA)

# Calculate the predicted values from the model
# Define the Nelson-Siegel function
nelson_siegel <- function(x, beta0, beta1, beta2, tau) {
  y <- beta0 + (beta1 + beta2) * (1 - exp(-x / tau)) / (x / tau) - beta2 * exp(-x / tau)
  return(y)
}
# Initial parameter values
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 1)
# Fit the model using nlsLM
mod.2.QPA <- nlsLM(canopy_QPA ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data_canopy_QPA, 
               start = start_params)

predicted_values_QPA <- predict(mod.2.QPA, newdata = data.frame(event = event))

# Create a ggplot
mod.2.plot.QPA <- ggplot(data_canopy_QPA, aes(x = event, y = canopy_QPA)) +
  geom_point(shape = 21, fill = "#bdd7e7", color = "#2171b5", size = 3) +
  geom_line(aes(y = predicted_values_QPA), color= "gray20", linewidth=1.7) +
  
  xlab('') + ylab("Canopy openness") + 
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-2,2.5) + 
  theme(legend.position="none")+
  
  theme(panel.grid.major = element_line(colour = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

mod.2.plot.QPA


###########################################################################
# Canopy openness Prieta B ------------------------------------------------
#Create a data frame with your canopy_QPB data (2017-01-01 to 2022-09-01)
canopy_QPB <- c(0.066649426, 0.123402242, 0.218712422, -0.071410803, -0.471304866,
                0.27459288, 1.913568932, 1.551896958, 1.536349726, 1.490548934,
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
data_canopy_QPB <- data.frame(event, canopy_QPB)

# Calculate the predicted values from the model
# Define the Nelson-Siegel function
nelson_siegel <- function(x, beta0, beta1, beta2, tau) {
  y <- beta0 + (beta1 + beta2) * (1 - exp(-x / tau)) / (x / tau) - beta2 * exp(-x / tau)
  return(y)
}
# Initial parameter values
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 1)
# Fit the model using nlsLM
mod.2.QPB <- nlsLM(canopy_QPB ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data_canopy_QPB, 
               start = start_params)
predicted_values_QPB <- predict(mod.2.QPB, newdata = data.frame(event = event))

# Create a ggplot
mod.2.plot.QPB <- ggplot(data_canopy_QPB, aes(x = event, y = canopy_QPB)) +
  geom_point(shape = 21, fill = "#bdd7e7", color = "#2171b5", size = 3) +
  geom_line(aes(y = predicted_values_QPB), color= "gray20", linewidth=1.7) +

  xlab('') + ylab("Canopy openness") + 
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-2,2.5) + 
  theme(legend.position="none")+
  
  theme(panel.grid.major = element_line(colour = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

mod.2.plot.QPB


###########################################################################
# Periphyton Prieta A ----------------------------------------------------
# Create a data frame with your canopy_QPA data (2017-01-01 to 2022-09-01)
chlorophyll_QPA <- c(0.374941114, 0.421008704, 0.059098445, -0.356900918, -0.05326958,
                     -0.133541362, -0.319898689, -0.340327935, -0.49053223, -0.326974628,
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
                     0.190985156, 0.776090885, 0.219440474, 0.449951267)

event <- seq(1, length(chlorophyll_QPA))
data <- data.frame(event, chlorophyll_QPA)


# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(chlorophyll_QPA ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(chlorophyll_QPA), length.out = 100))

# Predict using the model
predictions_peri_QPA <- predict(mod.3, newdata = new_data)


# Create a ggplot for visualization
mod.2.periphyton.QPA <- ggplot(data, aes(x = event, y = chlorophyll_QPA)) +
  geom_point(shape = 21, fill = "#bdd7e7", color = "#2171b5", size = 3) +
  geom_line(data = data.frame(event = new_data$event, periphyton_QPA = predictions_peri_QPA), 
            aes(x = event, y = periphyton_QPA), color = "gray20", linewidth=1.7) +
  
  xlab('') + ylab("Epilithic algae") + 
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-2,2.5) + 
  theme(legend.position="none")+
  
  theme(panel.grid.major = element_line(colour = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

mod.2.periphyton.QPA

  
###########################################################################
# Chlorophyll Prieta B ----------------------------------------------------
chlorophyll_QPB <- c(0.50664315, 0.367207886, -0.014872716, 0.066137971, 0.030785219, -0.420636925,
                     -0.532707828, -0.565502931, -0.302866704, -0.739818027, -0.631285253, -1.005281876,
                     -0.586985814, -0.704420806, -1.267086154, -0.51728118, -0.499008016, -0.389993121,
                     -0.380058376, -0.19471036, -0.818682043, -0.243142403, -0.234457311, -0.351418593,
                     -0.321838209, -0.25677644, -0.835178915, -0.517806213, -0.604142102, -0.415562593,
                     -1.278258443, -0.887589297, -1.272011892, -0.215530212, -0.198301758, -0.462592338,
                     0.036101327, 0.191078061, -0.683301671, -0.459379957, -1.553418692, -0.560293394,
                     -0.198035461, 0.086767025, -0.516006206, -0.328713949, 0.433791523, 0.021171656,
                     -0.272604249, 0.083860925, -0.10855559, 0.285803829, -0.374389114, 0.269429207,
                     -0.467751563, -0.021651399, -0.173116019, 0.265943762, 0.061304029, 0.747564142, 
                     0.041108448, 0.684064969, 0.517588636, 0.369352134)

event <- seq(1, length(chlorophyll_QPB))
data <- data.frame(event, chlorophyll_QPB)



# Inverted Parabola Curve (mod. 3) ----------------------------------------
# Fit a quadratic regression model
mod.3 <- lm(chlorophyll_QPB ~ event + I(event^2), data=data)
# Get model summary
summary(mod.3)

# Extract R-squared value and p-value
r_squared.mod3 <- summary(mod.3)$r.squared
p_value.mod3 <- summary(mod.3)$coefficients[4]  # P-value for the quadratic term
# Display results
cat("R-squared:", r_squared.mod3, "\n")
cat("R-squared:", p_value.mod3, "\n")

# Create a new data frame for prediction
new_data <- data.frame(event = seq(1, length(chlorophyll_QPB), length.out = 100))

# Predict using the model
predictions_peri_QPB <- predict(mod.3, newdata = new_data)


# Create a ggplot for visualization
mod.2.periphyton.QPB <- ggplot(data, aes(x = event, y = chlorophyll_QPB)) +
  geom_point(shape = 21, fill = "#bdd7e7", color = "#2171b5", size = 3) +
  geom_line(data = data.frame(event = new_data$event, periphyton_QPB = predictions_peri_QPB), 
            aes(x = event, y = periphyton_QPB), color = "gray20", linewidth=1.7) +
  
  xlab('') + ylab("") + 
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis 7
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  ylim(-2,2.5) + 
  theme(legend.position="none")+
  
  theme(panel.grid.major = element_line(colour = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

mod.2.periphyton.QPB



Figure3 <- (mod.2.plot.QPA | mod.2.plot.QPB) /
            (mod.2.periphyton.QPA | mod.2.periphyton.QPB)


Figure3 <- Figure3 + plot_annotation(tag_levels = 'A')

# Save the ggplot object as an image file
ggsave("Figure3.jpg", plot = Figure3, path = "figures", width = 9, height = 6, dpi = 300)


