# Load necessary package
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Create the data frame with the given data
data <- data.frame(
  Fertilizer = rep(c("F1", "F2", "F3"), each = 3),
  Crop = rep(c("Corn", "Rice", "Wheat"), times = 3),
  MonthsHealthy = c(6, 4, 6, 5, 4.2, 5, 6, 5, 5.5)
)

# Check the structure of the data
print(data)

# Perform the two-way ANOVA
anova_result <- aov(MonthsHealthy ~ Fertilizer * Crop, data = data)

# Display the ANOVA table
summary(anova_result)