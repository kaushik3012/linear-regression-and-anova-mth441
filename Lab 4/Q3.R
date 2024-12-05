# Load necessary library for reading Excel files
library(readxl)

# Load necessary library for Box-Cox transformation
library(MASS)

# Load the data
data <- read_excel("Electricity_Data.xlsx")

# Convert the data to a data frame
df <- as.data.frame(data)

# Fit the linear model
model <- lm(Y ~ X, data = df)

# Plot Residuals vs Fitted values
plot(fitted(model), residuals(model), 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted (Before Transformation)")
abline(h = 0, col = "red")

# QQ plot of residuals
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Apply Box-Cox transformation to Y
boxcox_model <- boxcox(lm(Y ~ X, data=df), plotit = TRUE)

# Find the lambda value
lambda <- boxcox_model$x[which.max(boxcox_model$y)]
cat("Optimal lambda for Box-Cox transformation:", lambda, "\n")

# Transform Y using the optimal lambda
if (lambda == 0) {
  df$Y_transformed <- log(df$Y)
} else {
  df$Y_transformed <- (df$Y^lambda - 1) / lambda
}

# Fit the linear model with transformed Y
model_bc <- lm(Y_transformed ~ X, data = df)

# Plot Residuals vs Fitted values after Box-Cox transformation
plot(fitted(model_bc), residuals(model_bc), 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted (After Box-Cox Transformation)")
abline(h = 0, col = "red")

# QQ plot of residuals after Box-Cox transformation
qqnorm(residuals(model_bc))
qqline(residuals(model_bc), col = "red")

# Create log-transformed X for Box-Tidwell
df$log_X <- log(df$X)

# Fit the linear model with log-transformed X
model_bt <- lm(Y ~ X + I(X * log_X), data = df)

# Summary of the model
summary(model_bt)

# Plot Residuals vs Fitted values after Box-Tidwell transformation
plot(fitted(model_bt), residuals(model_bt), 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted (After Box-Tidwell Transformation)")
abline(h = 0, col = "red")

# QQ plot of residuals after Box-Tidwell transformation
qqnorm(residuals(model_bt))
qqline(residuals(model_bt), col = "red")
