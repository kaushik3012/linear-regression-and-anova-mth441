# Load necessary packages
library(car)

# Step 1: Prepare the data
y = c(1, 4, 8, 9, 3, 8, 9)
x1 = c(-1, 1, -1, 1, 0, 0, 0)
x2 = c(-1, -1, 1, 1, 0, 1, 2)

A = matrix(c(1,0,0,1,0,-1,0,0),nrow=2)

# Step 2: Fit the regression model
model <- lm(y ~ x1 + x2 + I(x1^2))

# Step 3: Test the hypotheses
# Test H0: beta0 = 0 and beta1 - beta2 = 0
linearHypothesis(model, hypothesis.matrix = A)


library(Matrix)
y = c(1, 4, 8, 9, 3, 8, 9)
x1 = c(-1, 1, -1, 1, 0, 0, 0)
x2 = c(-1, -1, 1, 1, 0, 1, 2)

A = matrix(c(1,0,0,1,0,-1,0,0),nrow=2)
q = rankMatrix(A)[1]
C = rep(0,2)

model = summary(lm(y~x1+x2+I(x1^2)))

n = length(y)
p = 4

X = cbind(1,x1,x2,x1^2)

beta = model$coefficients[,1]
beta
rss_rssh = t(A%*%beta)%*%solve(A%*%solve(t(X)%*%(X))%*%t(A))%*%A%*%beta
rss_rssh

RSS = t(y-X%*%beta)%*%(y-X%*%beta)

f.stat= ((rss_rssh)/q)/(RSS/(n-p))
f.stat


#Q2
# Data
y <- c(1, 4, 8, 9, 3, 8, 9)
x1 <- c(-1, 1, -1, 1, 0, 0, 0)
x2 <- c(-1, -1, 1, 1, 0, 1, 2)

# (a) Restricted LSE under H0
Beta0_hat_restricted <- mean(y)
cat("Restricted LSE for β0 under H0:", Beta0_hat_restricted, "\n")

# (b) Restricted RSS
RSS_restricted <- sum((y - Beta0_hat_restricted)^2)
cat("Restricted RSS:", RSS_restricted, "\n")

# Unrestricted model
X <- cbind(1, x1, x2, x1^2)
Beta_hat_unrestricted <- solve(t(X) %*% X) %*% t(X) %*% y

# Unrestricted RSS
RSS_unrestricted <- sum((y - X %*% Beta_hat_unrestricted)^2)
cat("Unrestricted RSS:", RSS_unrestricted, "\n")

# (c) Compute the F-statistic
n <- length(y)
p <- ncol(X)
q <- 3  # Number of restrictions

F_statistic <- ((RSS_restricted - RSS_unrestricted) / q) / (RSS_unrestricted / (n - p))
cat("F-statistic for testing H0:", F_statistic, "\n")

# (d) Verify using lm function
unrestricted_model <- lm(y ~ x1 + x2 + I(x1^2))
summary(unrestricted_model)

# To test the null hypothesis using the anova function
restricted_model <- lm(y ~ 1)
anova(restricted_model, unrestricted_model)

