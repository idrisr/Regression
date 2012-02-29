# ***************************************************************************
#  Chapter 3 Exercises: Model Performance and Evaluation
#  Book: Regression Basics, Leo Kahane, 1st edition
# ************************************************ **************************

# 3.1 ******************************************** **************************
wage_orig <- read.table('data/WAGE.txt', header=TRUE, sep='\t')
Y <- wage_orig$WAGE
X <- wage_orig$EDUC
b1 <- sum((X - mean(X)) * (Y - mean(Y))) / sum((X - mean(X))^2)
b0 <- mean(Y) - b1*mean(X)
m <- (lm(Y~X))

Y_bar <- mean(Y)
Y_hat <- b1*X + b0
TSS <- sum((Y_bar - Y)^2)  # Total sum of squares
RSS <- sum((Y_hat - Y)^2)  # Residual sum of squares
ESS <- sum((Y_bar - Y_hat)^2)  # Explained sum of squares
(r_sq <- ESS / TSS)
summary(m)$r.squared

# R squared is .261. It means that the model explains 26% of the variation in Y

# 3.2 ***********************************************************************
# H₀: b₀=0, H₁: b₀≠0, α = 0.05
# H₀: b₁=0, H₁: b₁≠0, α = 0.01
alpha <- 0.05
ms <- summary(m)
ms$coefficients['(Intercept)', 'Pr(>|t|)'] <= alpha

alpha <- 0.01
ms$coefficients['X', 'Pr(>|t|)'] <= alpha

# 3.3 ***********************************************************************
# H₀: b₀=50, H₁: b₀≠50, α = 0.05 
# H₀: b₁=50, H₁: b₁≠50, α = 0.05 
alpha <- 0.5

t_val <- function(b, se, h) {
    return ((b-h)/se)
}

t0 <- t_val(ms$coefficients['(Intercept)', 'Estimate'], 
            ms$coefficients['(Intercept)', 'Std. Error'], 50)

t1 <- t_val(ms$coefficients['X', 'Estimate'], 
            ms$coefficients['X', 'Std. Error'], 50)

df <- length(Y) - 2  # number of data points minus number of parameters calced
pt(t0, df)*2 < alpha
pt(t1, df)*2 < alpha

t <- t_val(51.52, 1.0988, 50)
pt(t, 19)

# d is for the density function
# p is for the cumulative density functnio


# Reject both H₀
