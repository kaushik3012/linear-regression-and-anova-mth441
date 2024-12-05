# Load necessary library for reading Excel files
library(readxl)

# Load the data
data <- read_excel("TimeDeliveryData.xlsx")

# Convert the data to a data frame
df <- as.data.frame(data)

# Fit the linear model
model <- lm(Y ~ X1 + X2, data = df)

# Calculate fitted values
fitted_values <- fitted(model)

# Calculate R-Student
r_student <- rstudent(model)

# Plot Fitted Values vs. R-Student
plot(fitted_values, r_student, 
     xlab = "Fitted Values", 
     ylab = "R-Student", 
     main = "Fitted Values vs. R-Student")
abline(h = 0, col = "red")

# Calculate residuals
residuals <- residuals(model)

# QQ Plot
qqnorm(residuals)
qqline(residuals, col = "red")

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



