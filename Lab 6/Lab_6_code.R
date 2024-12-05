############################################
# Question 1                              #
###########################################
library(car)
library(readxl)
TimeDeliveryData <- read_excel("TimeDeliveryData.xlsx") %>% as.data.frame()

model <- lm(Y ~ X1 + X2, data = TimeDeliveryData)

leverage_points <- hatvalues(model)
cooks_d <- cooks.distance(model)
dfbetas_values <- dfbetas(model)
dffits_values <- dffits(model)
covratio_values <- covratio(model)

# Print the Results
leverage_points
cooks_d
dfbetas_values
dffits_values
covratio_values

influence_measures <- influence.measures(model)
summary(influence_measures)


############################################
# Question 2                              #
###########################################
library(car)
library(dplyr)
library(readxl)
Acetylene_Data <- read_excel("Acetylene_Data.xlsx") %>% as.data.frame()

# Normalize the dataset
acetylene_norm <- lapply(Acetylene_Data, function(x) ((x - mean(x)) / sqrt((length(x) - 1) * var(x)))) %>% as.data.frame()
acetylene_norm

# Create interaction and second-order terms
acetylene_final <- cbind(acetylene_norm,
                        X1X2 = acetylene_norm$X1 * acetylene_norm$X2,
                        X1X3 = acetylene_norm$X1 * acetylene_norm$X3,
                        X2X3 = acetylene_norm$X2 * acetylene_norm$X3,
                        X1_sq = acetylene_norm$X1^2,
                        X2_sq = acetylene_norm$X2^2,
                        X3_sq = acetylene_norm$X3^2)
acetylene_final

# Calculate correlation matrix
corr <- cor(acetylene_final)

# Print correlation matrix
cat("Correlation Matrix: ")
corr

# Calculate VIF
model <- lm(Y ~ ., data = acetylene_final)
vif_values <- vif(model)

# Print VIF values
cat("VIF Values: ", vif_values)