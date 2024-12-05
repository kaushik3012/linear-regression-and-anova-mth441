# Create the dataset
aircraft_data <- data.frame(
  y = c(0,1,0,0,0,0,1,0,0,2,1,1,1,1,2,3,1,1,1,2,0,1,1,2,5,1,1,5,5,7),
  x1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
  x2 = c(4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,7,7,7,10,10,10,12,12,12,8,8,8,14,14,14),
  x3 = c(91.5,84.0,76.5,69.0,61.5,80.0,72.5,65.0,57.5,50.0,103.0,95.5,88.0,80.5,73.0,
         116.1,100.6,85.0,69.4,53.9,112.3,96.7,81.1,65.6,50.0,120.0,104.4,88.9,73.7,57.8)
)

# Fit Poisson regression model
poisson_model <- glm(y ~ x1 + x2 + x3, family = poisson(link = "log"), data = aircraft_data)

# Display model summary
summary(poisson_model)

# Calculate pseudo R-squared (1 - residual deviance/null deviance)
pseudo_r2 <- 1 - poisson_model$deviance/poisson_model$null.deviance

# Perform analysis of deviance
anova_result <- anova(poisson_model, test = "Chisq")

# Create diagnostic plots
par(mfrow = c(2,2))
plot(poisson_model)

# Print additional model statistics
cat("\nModel Statistics:\n")
cat("Null Deviance:", poisson_model$null.deviance, 
    "on", poisson_model$df.null, "degrees of freedom\n")
cat("Residual Deviance:", poisson_model$deviance, 
    "on", poisson_model$df.residual, "degrees of freedom\n")
cat("AIC:", poisson_model$aic, "\n")
cat("Pseudo R-squared:", pseudo_r2, "\n")

# Display ANOVA results
cat("\nAnalysis of Deviance Table:\n")
print(anova_result)

# Calculate and print odds ratios and confidence intervals
coef_exp <- exp(coef(poisson_model))
conf_int <- exp(confint(poisson_model))
coef_table <- cbind(Estimate = coef_exp, 
                    CI_lower = conf_int[,1], 
                    CI_upper = conf_int[,2])

cat("\nExponentiated Coefficients (Rate Ratios) with 95% CI:\n")
print(coef_table)

# Perform Walt's test for analysis of deviance
library(lmtest)
walt_result <- waldtest(poisson_model)
walt_result
