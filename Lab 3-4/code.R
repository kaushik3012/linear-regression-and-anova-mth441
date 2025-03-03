
###########################
# Lab 3 Q3  
###########################

library(olsrr)
# Input the data
x <- c(1.0, 1.0, 2.0, 3.3, 3.3, 4.0, 4.0, 4.0, 5.6, 5.6, 6.0,
       6.0, 6.5, 6.9)
y <- c(10.84, 9.30, 16.35, 22.88, 24.35, 24.56, 25.86, 29.16,
       27.20, 25.61, 25.45, 26.56, 21.03, 21.46)
# Fit the linear model
model <- lm(y ~ x)
# Perform the lack-of-fit test
lack_of_fit_test <- ols_pure_error_anova(model)
# Print the results
print(lack_of_fit_test)


###########################
# Lab 4 Q2  
###########################

# Load necessary library for reading Excel files
library(readxl)

# Load the data
data <- read_excel("TimeDeliveryData.xlsx")

# Convert the data to a data frame
df <- as.data.frame(data)

# Fit the linear model
model <- lm(Y ~ X1 + X2, data = df)

# Partial Regression for X1
# Regress Y on X2 and get residuals
residuals_Y_X2 <- residuals(lm(Y ~ X2, data = df))

# Regress X1 on X2 and get residuals
residuals_X1_X2 <- residuals(lm(X1 ~ X2, data = df))

# Plot residuals
plot(residuals_X1_X2, residuals_Y_X2,
     xlab = "Residuals of X1 | X2",
     ylab = "Residuals of Y | X2",
     main = "Partial Regression Plot for X1")
abline(lm(residuals_Y_X2 ~ residuals_X1_X2), col = "blue")

# Partial Regression for X2
# Regress Y on X1 and get residuals
residuals_Y_X1 <- residuals(lm(Y ~ X1, data = df))

# Regress X2 on X1 and get residuals
residuals_X2_X1 <- residuals(lm(X2 ~ X1, data = df))

# Plot residuals
plot(residuals_X2_X1, residuals_Y_X1,
     xlab = "Residuals of X2 | X1",
     ylab = "Residuals of Y | X1",
     main = "Partial Regression Plot for X2")
abline(lm(residuals_Y_X1 ~ residuals_X2_X1), col = "blue")

# Partial Residual Plot for X1
partial_residual_X1 <- residuals_Y_X2 + coef(model)["X1"] * df$X1
plot(df$X1, partial_residual_X1,
     xlab = "X1",
     ylab = "Partial Residual",
     main = "Partial Residual Plot for X1")
abline(lm(partial_residual_X1 ~ df$X1), col = "blue")

# Partial Residual Plot for X2
partial_residual_X2 <- residuals_Y_X1 + coef(model)["X2"] * df$X2
plot(df$X2, partial_residual_X2,
     xlab = "X2",
     ylab = "Partial Residual",
     main = "Partial Residual Plot for X2")
abline(lm(partial_residual_X2 ~ df$X2), col = "blue")



###############################
# Lab 4 Q3 For Electricity Data
###############################

# Load necessary libraries
library(MASS)       # For Box-Cox transformation
library(car)        # For Box-Tidwell transformation
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(readxl)

# Load the data
data <- read_excel("Electricity_Data.xlsx") 
# Use “Wind_Mill_Data.xlsx” for Windmill Data

X <- data$X
Y <- data$Y

# Step 1: Initial Linear Model (before transformations)
model_initial <- lm(Y ~ X, data = data)
residuals_initial <- resid(model_initial)

# Plot initial residuals
ggplot(data, aes(X, residuals_initial)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot (Before Transformation)", x = "X", y = "Residuals")

# Step 2: Box-Cox Transformation
boxcox_result <- boxcox(model_initial, plotit = FALSE)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]

# Apply Box-Cox transformation
Y_boxcox <- (Y^lambda - 1) / lambda
data$Y_boxcox <- Y_boxcox

# Model after Box-Cox transformation
model_boxcox <- lm(Y_boxcox ~ X, data = data)
residuals_boxcox <- resid(model_boxcox)

# Plot residuals after Box-Cox transformation
ggplot(data, aes(X, residuals_boxcox)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot (After Box-Cox Transformation)", x = "X", y = "Residuals")

# Box-Tidwell transformation
box_tidwell <- boxTidwell(Y ~ X, data = data)
transformed_X <- data$X ^ box_tidwell$result[1]

# Fit a linear model with the transformed X
model_boxtidwell <- lm(Y ~ transformed_X, data = data)

# Plot residuals of the Box-Tidwell transformed model
ggplot(data, aes(x = transformed_X, y = residuals(model_boxtidwell))) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Residuals of Box-Tidwell Transformed Model",
       x = "Transformed X",
       y = "Residuals")

# Step 4: Compare Models
summary(model_initial)
summary(model_boxcox)
summary(model_boxtidwell)



###############################
# Lab 4 Q3 For WindMill Data
###############################

# Load necessary libraries
library(MASS)       # For Box-Cox transformation
library(car)        # For Box-Tidwell transformation
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(readxl)

# Load the data
data <- read_excel("Wind_Mill_Data.xlsx") 

X <- data$X
Y <- data$Y

# Step 1: Initial Linear Model (before transformations)
model_initial <- lm(Y ~ X, data = data)
residuals_initial <- resid(model_initial)

# Plot initial residuals
ggplot(data, aes(X, residuals_initial)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot (Before Transformation)", x = "X", y = "Residuals")

# Step 2: Box-Cox Transformation
boxcox_result <- boxcox(model_initial, plotit = FALSE)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]

# Apply Box-Cox transformation
Y_boxcox <- (Y^lambda - 1) / lambda
data$Y_boxcox <- Y_boxcox

# Model after Box-Cox transformation
model_boxcox <- lm(Y_boxcox ~ X, data = data)
residuals_boxcox <- resid(model_boxcox)

# Plot residuals after Box-Cox transformation
ggplot(data, aes(X, residuals_boxcox)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot (After Box-Cox Transformation)", x = "X", y = "Residuals")

# Box-Tidwell transformation
box_tidwell <- boxTidwell(Y ~ X, data = data)
transformed_X <- data$X ^ box_tidwell$result[1]

# Fit a linear model with the transformed X
model_boxtidwell <- lm(Y ~ transformed_X, data = data)

# Plot residuals of the Box-Tidwell transformed model
ggplot(data, aes(x = transformed_X, y = residuals(model_boxtidwell))) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Residuals of Box-Tidwell Transformed Model",
       x = "Transformed X",
       y = "Residuals")

# Step 4: Compare Models
summary(model_initial)
summary(model_boxcox)
summary(model_boxtidwell)




