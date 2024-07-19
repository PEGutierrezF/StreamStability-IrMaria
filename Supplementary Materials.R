

library(patchwork)
plot <- (mod.4.plot.canopy + mod.5.plot.leaf)/
  (mod.6.plot.algae + mod.4.plot.shrimp) /
  (mod.5.plot.macros + plot_spacer())


#Ecology format
ggsave(file="Figure 1.jpeg", plot, width = 24, height = 30, units = "cm", dpi = 600)




# Canopy ------------------------------------------------------------------
# Create a data frame with canopy_QPA data (2017-10-04 (H. Maria, time 0) to 2022-09-01)
canopy_QPA <- c(2.035709058, 1.727578155, 1.716528319, 1.669203747, 
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
data <- data.frame(event, canopy_QPA)
head(data)



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


# Linear model for canopy_QPB
linear_model <- lm(canopy_QPB ~ event, data = data)

# Function for logistic curve
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}

# Fit a logistic curve model
mod.4 <- nls(canopy_QPA ~ logistic_function(event, A, B, C, D),
             data = data,
             start = list(A = min(canopy_QPA), B = max(canopy_QPA), C = 1, D = median(event)))

# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(canopy_QPA), length.out = 100))
predictions <- predict(mod.4, newdata = new_data)

# Linear model for canopy_QPB
linear_model <- lm(canopy_QPB ~ event, data = data)


library(ggthemes)

p <- ggplot(data, aes(x = event)) +
  geom_point(aes(y = canopy_QPA), shape = 16, color = "#ce1256", size = 2) +
  geom_line(data = data.frame(event = new_data$event, canopy_QPA = predictions), 
            aes(y = canopy_QPA), color = "#ce1256") +
  
  geom_point(aes(y = canopy_QPB), shape = 16, colour = "#0570b0", size = 2) +
  geom_line(aes(y = linear_model$fitted.values), color = "#0570b0") +
  
  labs(x = "Sampling event",
       y = "Canopy openness (%)") +
  
  ggthemes::geom_rangeframe(y=canopy_QPA) + 
  ggthemes::theme_tufte() +
  
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add vertical line at x = 0
  
  annotate("text", label = "Logistic curve",
           x = 15,y=0.25,
           color    = "#ce1256",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") +
  
  annotate("text", label = "Linear model",
           x = 42,y=1.2,
           color    = "#0570b0",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") 


p



# Leaflitter fall ---------------------------------------------------------
# Create a data frame with your leaf litter data (2017-10-30 (H. Maria, time 0) to 2022-09-14)
leaflitter_QPA <- c(
  -2.565069159, -3.022216147, -2.425299443, -2.356670205, -2.167231853, -2.917151971, 
  -0.300271157, -2.134096292, -2.839665992, -2.044478086, -1.733085534, -1.624609129, 
  -1.211743623, -0.799411473, -2.110757766, -1.871576837, -1.200485106, -1.612131249, 
  -1.764806785, -1.27485795, -1.532273493, -1.261461902, -1.698447453, -1.928908543, 
  -1.338069713, -1.632515941, -1.562239222, -2.545394251, -1.082064703, -2.733135875, 
  -1.939825609, -1.43306269, -1.767640986, -0.965536339, -0.391608814, -0.613991123, 
  -0.508247125, -1.000617123, -1.175868835, -0.824672858, -0.521538434, -1.270373221, 
  -1.17098708, -1.193741075, -1.037061656, -0.599383721, -0.521492258, -1.92107908, 
  -1.586354733, -1.464956636, -2.192426795, -0.971256818, -0.302518176, -0.937543182, 
  -1.670225859, -1.126310067, -1.313000351, -1.194800423, -1.341998189, -0.56209657, 
  -0.5161628, -0.328599799, -0.585934106, -0.953081472, -0.927344875, 0.523726624, 
  -0.865303492, 0.023643143, -0.104554204, -1.377159185, -0.871797957, -1.799831283, 
  -1.171549437, -1.460269437, -1.786864311, -2.056059164, -1.782840537, -1.15055833, 
  -1.091059192, -0.37511391, -0.444649707, -1.236006878, -0.63063159, -1.001865167, 
  -0.249679552, -0.161372181, -0.028637124, -0.563554364, -0.8868843, -0.95364459, 
  -0.482890718, -0.149959004, -0.111404071, -0.771421424, -1.177151156, -1.257598545, 
  -1.289014428, -1.415630091, -0.319359473, -0.115105525, -1.381377565, -1.145349438, 
  -0.862815924, 0.173495884, -1.138115783, -0.599902465, -1.417418296, -0.270925982, 
  -0.135785162, -0.239376617, -0.408619136, -0.525518629, -0.727676895, -0.965580008, 
  -0.479949827, -0.91494735, -0.322411522, -0.056491592)

event <- seq(1, length(leaflitter_QPA))
data <- data.frame(event, leaflitter_QPA)

# Fit a logarithmic curve model
mod.5_Leaf_QPA <- nls(leaflitter_QPA ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))
# Get summary of the model
summary(mod.5_Leaf_QPA)
# Create a data frame with predicted values
pred_data <- data.frame(event = data$event, 
                        leaflitter_QPA_pred = predict(mod.5_Leaf_QPA, newdata = data))




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

# Linear model for canopy_QPB
mod.1.QPB <- lm(leaflitter_QPB ~ event, data = data)


# Create a ggplot
p1 <- ggplot(data, aes(x = event)) +
  geom_point(aes(y = leaflitter_QPA), shape = 16, colour = "#ce1256", size = 2) +
  geom_line(data = pred_data, aes(x = event, y = leaflitter_QPA_pred), color = "#ce1256") +
  
  geom_point(aes(y = leaflitter_QPB), shape = 16, colour = "#0570b0", size = 2) +
  geom_line(aes(y = mod.1.QPB$fitted.values), color='#0570b0') +  
  
  
  labs(x = "Sampling event",
       y = expression(Leaflitter~fall~(g %.% m^{-2} %.% d^{-1}))) +
  
  geom_rangeframe(y=leaflitter_QPA) + 
  geom_rangeframe(y=leaflitter_QPB) + 
  theme_tufte() +
  
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add vertical line at x = 0
  
  annotate("text", label = "Logarithmic curve",
           x = 85,y=-2.2,
           color    = "#ce1256",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") +
  
  annotate("text", label = "Linear model",
           x = 40,y=1,
           color    = "#0570b0",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") 
p1



# Epilithon ---------------------------------------------------------------


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


# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}
# Fit the exponential curve
mod.6_QPA <- nls(epilithon_QPA ~ exponential(event, A, B, C), 
                 data = data,
                 start = list(A = 1, B = 0.1, C = 0))
summary(mod.6_QPA)

# Create a new data frame for prediction for Prieta A
new_data_QPA <- data.frame(event = seq(1, length(epilithon_QPA), length.out = 100))
new_data_QPA$predicted <- predict(mod.6_QPA, newdata = new_data_QPA)



# Exponential curve (mod.6) -----------------------------------------------
# Define the exponential function
exponential <- function(x, A, B, C) {
  A * exp(B * x) + C
}
# Fit the exponential curve
mod.6_QPB <- nls(chlorophyll_QPB ~ exponential(event, A, B, C), 
             data = data,
             start = list(A = 1, B = 0.1, C = 0))
# Create a new data frame for prediction for Prieta B
new_data_QPB <- data.frame(event = seq(1, length(chlorophyll_QPB), length.out = 100))
new_data_QPB$predicted <- predict(mod.6_QPB, newdata = new_data_QPB)

# Plot the data and fitted curve using ggplot2
p2 <- ggplot(data, aes(x = event)) +
  
  geom_point(aes(y = epilithon_QPA), shape = 16, colour = "#ce1256", size = 2) +
  geom_line(data = new_data_QPA, aes(x = event, y = predicted), color = "#ce1256") +
  
  geom_point(aes(y = chlorophyll_QPB), shape = 16, colour = "#0570b0", size = 2) +
  geom_line(data = new_data_QPB, aes(x = event, y = predicted), color = "#0570b0") +
  
  labs(x = "Sampling event",
       y = expression(Epilithon~(mg~chl-~italic(a) %.% m^{-2}))
  ) +
  
  geom_rangeframe(y=chlorophyll_QPB) + 
  theme_tufte() +
  
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add vertical line at x = 0
  
  annotate("text", label = "Exponential Curve",
           x = 18,y=0.5,
           color    = "#ce1256",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") +
  
  annotate("text", label = "Exponential Curve",
           x = 40,y=-1,
           color    = "#0570b0",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") 

p2



# Decapoda Abundance  -----------------------------------------------------
# Create a data frame with Decapoda_QPA data (2017-10-04 (H. Maria, time 0) to 2022-09-01)
decapoda_QPA <- c(-0.23393578, -0.239203865, -0.086031458, 
                  0.194384455, 0.449264798, 0.357700178, 0.923433147, -0.010068608, 0.383830498, 
                  0.645842869, 0.709639939, -0.010001997, 0.180285861, 0.399394425, 0.269734048, 
                  0.098597297, 0.121989466, 0.408274831, 0.371495165, 0.817862356, 0.500372304, 
                  0.240039901, 0.083373911, 0.354399016, 0.320034799, 0.140367341, -0.254432235, 
                  0.003846263, -0.34321992, -0.081858956, 0.105006467, 0.239319973, 0.408686216, 
                  0.12504962, 0.322919454, 0.718199634, -0.19458057, -0.628874296, -0.699498734, 
                  -0.333033181, -0.643381145, -0.497259411, -0.638556847, -0.771123635, -0.209821714, 
                  -0.777532836, -0.725945658, -0.771792055, -0.762260335, -0.393835734, -0.890604404, 
                  -0.267314505, -0.828057819, -0.435227036, -0.41283978, -0.536717338, -0.534703537, 
                  -0.537231614, -0.883109826, -0.995781911)


event <- seq(1, length(decapoda_QPA))
data <- data.frame(event, decapoda_QPA)




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


logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}
# Try different starting parameter values
start_params <- list(A = -1, B = 1, C = 0.1, D = median(data$event))
# Fit the model using nls.lm algorithm
mod.4_QPA <- nlsLM(decapoda_QPA ~ logistic_function(event, A, B, C, D),
                   data = data,
                   start = start_params)


# Generate predictions using the model
new_decapoda_QPA <- data.frame(event = seq(1, length(decapoda_QPA), length.out = 100))
predicted <- predict(mod.4_QPA, newdata = new_decapoda_QPA)



# Define the logistic function
logistic_function <- function(x, A, B, C, D) {
  A + (B - A) / (1 + exp(-C * (x - D)))
}
mod.4_QPB <- nlsLM(
  shrimp_QPB ~ logistic_function(event, A, B, C, D),
  data = data,
  start = list(A = min(shrimp_QPB), B = max(shrimp_QPB), C = 1, D = median(data$event)),
  control = nls.lm.control(maxiter = 1000)  # Increase maximum iterations
)
# Generate predictions using the model
new_data <- data.frame(event = seq(1, length(shrimp_QPB), length.out = 100))
predictions <- predict(mod.4_QPB, newdata = new_data)



# Create a ggplot for visualization
p3 <- ggplot(data, aes(x = event)) +
  
  geom_point(aes(y = decapoda_QPA), shape = 16, colour = "#ce1256", size = 2) +
  geom_line(data = data.frame(event = new_decapoda_QPA$event, decapoda_QPA = predicted), 
            aes(x = event, y = decapoda_QPA), color = "#ce1256") +
  
  geom_point(aes(y = shrimp_QPB), shape = 16, colour = "#0570b0", size = 2) +
  geom_line(data = data.frame(event = new_data$event, shrimp_QPB = predictions), 
            aes(x = event, y = shrimp_QPB), color = "#0570b0") + 
  
  
  labs(x = "Sampling event",
       y = expression(Decapod~abundance~(ind %.% m^{-2} ))
  ) +
  
  geom_rangeframe(y=shrimp_QPB) + theme_tufte() +
  
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add vertical line at x = 0
  
  annotate("text", label = "Logistic curve",
           x = 20,y=-0.75,
           color    = "#ce1256",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") +
  
  annotate("text", label = "Logistic curve",
           x = 40,y=0.5,
           color    = "#0570b0",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") 


p3




# Macroinvertebrates ------------------------------------------------------
# Create a data frame with Decapoda_QPA data (2017-10-05 (after H. Maria, time 0) to 2022-09-01)
macros_QPA <- c(-1.163751591, -1.559647248, -0.530027831, 
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





# Create a data frame with Decapoda_QPA data (2017-10-05 (after H. Maria, time 0) to 2022-09-01)
macros_QPB <- c(0.416040634, 
                0.759091842, 1.909426685, -1.686774259, -0.173728192, 1.748084406, 
                -0.356049749, -1.608812717, -1.877451187, -1.123304901, 0.010575526, 
                -0.222518356, -0.392417393, -1.138809088, -0.722981193, 0.235697317, 
                0.26298946, -0.915665536, -0.241446366, -0.430157721, -0.722981193, 
                -0.061250208, -2.078816347, -0.993627078, -0.765092678, -0.662963183, 
                -0.061250208, -0.966958831, -0.831784052, -1.154557445, -0.235097138, 
                0.30442665, 0.501400483, 0.086802892, -1.138809088, -1.078184466, 
                0.441832752, -0.294713744, 0.292176462, -0.003007521, -0.033590944, 
                -0.046091107, 0.132028763, 0.309954708, -0.797507196, -0.604135803, 
                -0.561118418, 0.072390496, 0.624797603, 0.708050586, -0.758286483, 
                0.540996501, 0.260283098, -0.262965045, 0.922875611, 0.787638024, 
                0.193272388, -1.020650747, -0.262965045, 0.348836496)



event <- seq(1, length(macros_QPB))
data <- data.frame(event, macros_QPB)


# Logarithmic curve (mod.5) -----------------------------------------------
# Fit a logarithmic curve model
mod.5_QPA <- nls(macros_QPA ~ a * log(event) + b, data = data, start = list(a = 1, b = 1))


# Humped yield curve (mod.2) ----------------------------------------------
# Define the Nelson-Siegel function
nelson_siegel <- function(x, beta0, beta1, beta2, tau) {
  y <- beta0 + (beta1 + beta2) * (1 - exp(-x / tau)) / (x / tau) - beta2 * exp(-x / tau)
  return(y)
}

# Initial parameter values
start_params <- c(beta0 = 0.5, beta1 = -0.5, beta2 = 0.5, tau = 1)
# Fit the model using nlsLM
mod.2 <- nlsLM(macros_QPB ~ nelson_siegel(event, beta0, beta1, beta2, tau), 
               data = data, 
               start = start_params)

summary(mod.2)




# Create a data frame with predicted values
pred_data <- data.frame(event = data$event, 
                        macros_QPA_pred = predict(mod.5_QPA, newdata = data))

# Calculate the predicted values from the model
predicted_values_QPB <- predict(mod.2, newdata = data.frame(event = event))


library(ggthemes)
# Create a ggplot
p4<- ggplot(data, aes(x = event)) +
  geom_point(aes(y = macros_QPA), shape = 16, colour = "#ce1256", size = 2) +
  geom_line(data = pred_data, aes(x = event, y = macros_QPA_pred), color = "#ce1256") +
  
  geom_point(aes(y = macros_QPB), shape = 16, colour = "#0570b0", size = 2) +
  geom_line(aes(y = predicted_values_QPB), color = "#0570b0") +
  
  labs(x = "Sampling event",
       y = expression(Macroinvertebrate~density~(ind %.% m^{-2} )) 
  ) +
  
  geom_rangeframe(y=macros_QPB) + theme_tufte() +
  
  theme(axis.text.y = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"), 
        axis.title.x = element_text(size = 14, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  
  theme(panel.grid.major = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  theme(panel.grid.minor = element_line(color = "gray70",size = 0.5,linetype = 3)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add vertical line at x = 0
  
  annotate("text", label = "Logarithmic curve",
           x = 40,y=-1.5,
           color    = "#ce1256",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") +
  
  annotate("text", label = "Humped yield",
           x = 20,y= 1,
           color    = "#0570b0",
           size     = 6, 
           family   = "serif", 
           fontface = "italic") 

p4



q <- (p + p1) / (p2 + p3) /
  (p4 + plot_spacer())
q


# Plot 2 ------------------------------------------------------------------
## Function to extract legend
library(gridExtra)
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 


legend <- g_legend(p + theme(legend.position = c(0.25, 0.6)) +
                     theme(legend.key.size = unit(1, "cm"), #separate
                           legend.key.width = unit(3, 'cm'))+ #line width
                     guides(color = guide_legend(override.aes = list(size=5)))+
                   theme(legend.title=element_text(size=24)) + # legend title size
                     theme(legend.text = element_text(color = "black", 
                                                      size = 22)))

Fig <-grid.arrange(p + theme(legend.position='hidden'), p1 + theme(legend.position='hidden'), 
                   p2 + theme(legend.position='hidden'), p3 + theme(legend.position='hidden'),
                   p4 + theme(legend.position='hidden'), legend,
                   nrow=3)



#Ecology format
ggsave(file="Appendix 1a.jpeg", Fig, width = 24, path = 'figures',
       height = 30, units = "cm", dpi = 600)


