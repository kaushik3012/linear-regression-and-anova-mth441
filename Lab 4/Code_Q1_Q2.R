library(readxl)

# Load the Data
data <- read_excel("TimeDeliveryData.xlsx")

X1 <- data$X1
X2 <- data$X2
Y <- data$Y

# Combine X1 and X2 into a matrix of predictors
X <- cbind(1, X1, X2)  # Add a column of 1s for the intercept term

# Fit the linear model using ordinary least squares
# Compute the coefficients (beta) using the normal equation: beta = (X'X)^-1 * X'Y
XtX <- t(X) %*% X
XtX_inv <- solve(XtX)
XtY <- t(X) %*% Y
beta <- XtX_inv %*% XtY

# Calculate the fitted values
fitted_values <- X %*% beta

# Calculate residuals
residuals <- Y - fitted_values

# Print results
cat("Coefficients: ", beta,'\n')
cat("Fitted values: ", fitted_values,'\n')
cat("Residuals: ", residuals,'\n')

# Calculate the standard deviation of the residuals
sigma_squared <- sum(residuals^2) / (length(Y) - ncol(X))
std_dev_residuals <- sqrt(sigma_squared)

# Calculate standardized residuals
standardized_residuals <- residuals / std_dev_residuals
cat("Standardized Residuals: ", standardized_residuals)

# Calculate Studentized Residuals
H <- X %*% XtX_inv %*% t(X)

studentized_residuals <- standardized_residuals/sqrt(1-diag(H))
cat("Studentized Residuals: ", studentized_residuals)

# Calculate PRESS Residuals
press_residuals <- residuals/(1-diag(H))
cat("PRESS Residuals: ", press_residuals)

# Calculate R-Student Residuals
e_sq_term <- residuals^2 / (1 - diag(H))
S_squared <- (sum(residuals^2) - e_sq_term)/(length(Y) - ncol(X) -1)

R_student_residuals <- residuals/sqrt(S_squared*(1-diag(H)))
cat("R-Student: ", R_student_residuals)

# Plot Fitted Values vs. R-Student
plot(fitted_values, R_student_residuals, 
     xlab = "Fitted Values", 
     ylab = "R-Student", 
     main = "Fitted Values vs. R-Student")
abline(h = 0, col = "red")

# QQ Plot
qqnorm(residuals)
qqline(residuals, col = "red")