# To use the `hmer` package, follow these steps:
#   
#   1. **Installation**: Install the development version of `hmer` from GitHub using the following R commands:
#   ```r
# # install.packages("devtools")
# devtools::install_github("andy-iskauskas/hmer")
# ```
# 
# 2. **Load the Package**: Load `hmer` into your R session:
#   ```r
# library(hmer)
# ```
# 
# 3. **Train Emulators**: Use the `emulator_from_data` function to train a set of emulators to your data:
#   ```r
# ems <- emulator_from_data(input_data = SIRSample$training,
#                           output_names = names(SIREmulators$targets),
#                           ranges = list(aSI = c(0.1, 0.8), aIR = c(0, 0.5), aSR = c(0, 0.05)))
# ```
# 
# 4. **Perform Diagnostics**: Run diagnostic checks on the emulators using the `validation_diagnostics` function:
#   ```r
# validation <- validation_diagnostics(ems, SIREmulators$targets, SIRSample$validation, plt = FALSE)
# ```
# 
# 5. **Propose New Points**: Generate new points from the emulators with the `generate_new_design` function:
#   ```r
# new_points <- generate_new_design(ems, 50, SIREmulators$targets)
# ```
# 
# 6. **Learn More**: To learn more about emulation and history matching, explore the vignettes included with the package:
#   ```r
# browseVignettes("hmer")
# vignette("low-dimensional-examples", package = 'hmer')
# ```
# The vignettes provide examples and detailed explanations on how to use `hmer` in various scenarios.


if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("mvtnorm", quietly = TRUE)) install.packages("mvtnorm")
library(mvtnorm)
library(ggplot2)
library(dplyr)
library(hmer)

# Define the Rosenbrock function
rosenbrock <- function(x, y, a = 2, b = 0.2) {
  # (a - x)^2 + b * (y - x^2)^2
  c(x/a, y*a+a*b*(x^2+a^2))
}

# y = [(parameters[0][0] / a),
#      (parameters[0][1] * a + a * b * (parameters[0][0]**2 + a**2))]
# 
# return [[multivariate_normal.logpdf(y, [0, 4], [[1.0*scale, 0.5*scale], [0.5*scale, 1.0*scale]])]]

f_log_pdf <- function(v){
  cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  val = log(dmvnorm(v, mean = c(0,4), sigma = cov_matrix))
  val
}

# Define function to generate samplesd
get_samples <- function(n=10) {
  x_samples <- runif(n, min = -2, max = 2) # Adjust the range as needed
  y_samples <- runif(n, min = 2, max = 6) # Adjust the range as needed
  # Calculate Rosenbrock values
  rosen_values <- mapply(rosenbrock, x_samples, y_samples)
  log_pdf = f_log_pdf(t(rosen_values))
  # cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  # log_pdf <- log(dmvnorm(t(rosen_values), mean = c(0,4), sigma = cov_matrix))
  df <- data.frame(x = x_samples, y = y_samples, z = log_pdf)  
  df
}


training <- get_samples(n=1000)
# testing <- get_samples(n=30)
# Parameters we want to emulate
output_names <- c('z')

# **Train Emulators**: Use the `emulator_from_data` function to train a set of emulators to your data:
ems <- emulator_from_data(input_data = training,
                          output_names = output_names,
                          ranges = list(x = c(-5, 5), y = c(-5, 5)))

# # **Perform Diagnostics**: Run diagnostic checks on the emulators using the `validation_diagnostics` function:
# validation <- validation_diagnostics(ems, training, testing, plt = FALSE)

#  **Propose New Points**: Generate new points from the emulators with the `generate_new_design` function:
new_points <- generate_new_design(ems, 100, training)
new_points$z <- f_log_pdf(t(mapply(rosenbrock, new_points$x, new_points$y)))

ggplot() + 
  geom_point(data = training, aes(x = x, y = y), color = 'blue') + # First data set in blue
  geom_point(data = new_points, aes(x = x, y = y), color = 'red') + # Second data set in red
  theme_minimal() +
  labs(title = "Iteration 1", x = "X Axis", y = "Y Axis")

#################################################
training <- new_points
ems <- emulator_from_data(input_data = training,
                          output_names = output_names,
                          ranges = list(x = c(-5, 5), y = c(-5, 5)))
new_points <- generate_new_design(ems, 100, training)
new_points$z <- f_log_pdf(t(mapply(rosenbrock, new_points$x, new_points$y)))

ggplot() + 
  geom_point(data = training, aes(x = x, y = y), color = 'blue') + # First data set in blue
  geom_point(data = new_points, aes(x = x, y = y), color = 'red') + # Second data set in red
  theme_minimal() +
  labs(title = "Iteration 2", x = "X Axis", y = "Y Axis")


#################################################
training <- new_points
ems <- emulator_from_data(input_data = training,
                          output_names = output_names,
                          ranges = list(x = c(-5, 5), y = c(-5, 5)))
new_points <- generate_new_design(ems, 100, training)
new_points$z <- f_log_pdf(t(mapply(rosenbrock, new_points$x, new_points$y)))

ggplot() + 
  geom_point(data = training, aes(x = x, y = y), color = 'blue') + # First data set in blue
  geom_point(data = new_points, aes(x = x, y = y), color = 'red') + # Second data set in red
  theme_minimal() +
  labs(title = "Iteration 3", x = "X Axis", y = "Y Axis")

