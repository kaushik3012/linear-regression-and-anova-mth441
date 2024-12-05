# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Step 1: Load the data
data <- read_excel("Weighted_Least_Squares_Data.xlsx")
x_og <- data$X
y_og <- data$Y

# Step 2: Plot the residuals from an initial linear model
initial_model <- lm(Y ~ X, data = data)
initial_residuals <- residuals(initial_model)
summary(initial_model)

# Plot the residuals
ggplot(data, aes(x = X, y = initial_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Initial Residual Plot", x = "X", y = "Residuals")

# Step 3: Divide data into groups based on regressor values
# Here, we'll assume splitting the data into quartiles for demonstration. 
data <- data %>%
  mutate(group = ntile(X, 4))

# Step 4: Compute the sample mean and variance of each group
group_stats <- data %>%
  group_by(group) %>%
  summarize(mean_X = mean(X), mean_Y = mean(Y), var_Y = var(Y), var_X=var(X))

# Step 5: Scatter plot of the group means and variances
ggplot(group_stats, aes(x = mean_Y, y = var_Y)) +
  geom_point() +
  labs(title = "Scatter Plot of Group Means", x = "Mean of Y", y = "Variance of Y")

# Step 6: Fit an appropriate regression model (including a quadratic term)
quad_model <- lm(Y ~ poly(X, 2), data = data)
summary(quad_model)

# Step 7: Compute the fitted values
fitted_values <- fitted(quad_model)

# Step 8: Use these fitted values as reciprocal of the weights
weights <- 1 / abs(fitted_values)

# Step 9: Fit a weighted least squares model
wls_model <- lm(Y ~ X, data = data, weights = weights)
summary(wls_model)

# Step 10: Observe the residual plot of the WLS model
wls_residuals <- residuals(wls_model)

ggplot(data, aes(x = X, y = wls_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot after WLS", x = "X", y = "Residuals")


plot(x_og, y_og, xlab="X", ylab="Y", main="Initial Model")
abline(initial_model, col="red")
plot(x_og, y_og, xlab="X", ylab="Y", main="WLS Model")
abline(wls_model, col="red")
