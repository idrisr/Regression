# The goodness of fit: R²
# R² simply is a measure that tells us what % of the behavior of Y is explained
# by X
# R² aka the coefficient of determination

# TODO: start using TeX
# TSS = Σ(Yi - Ybar)²      # Total sum of squares, Ybar: average of Yi
# RSS = Σ(Yi - Yhat_i)²    # residual sum of squares
# ESS = Σ(Yhat_i - Ybar)²  # Expected sum of squares. Yhat_i: predicted Y val

# TSS = ESS + RSS
# R² = ESS/TSS

# calc R²
# Election Data *************************************************************
elec.o <- read.table('data/Elect.txt', header=TRUE, sep='\t')
X <- elec.o$Growth
Y <- elec.o$Votes
b <- sum((mean(X) - X) * (mean(Y) - Y)) / sum((mean(X) - X)^2)  #slope 
m <- lm(Y~X)
a <- mean(Y) - mean(X)*b  # intercept
Y_hat <- b*X + a  # predictions
Y_bar <- mean(Y)
TSS <- sum((Y - Y_bar)^2) # diff between mean(Y) and Y
RSS <- sum((Y - Y_hat)^2) # diff between Y and prediction, errors
ESS <- sum((Y_hat - Y_bar)^2) # diff between prediction and mean, the amount
# explained by the model

R_sq = ESS/TSS
(ESS/TSS - summary(m)$r.squared) # zero


# Baseball Data *************************************************************
orig.bb <- read.table('data/bball.txt', header=TRUE, sep='\t')
X <- orig.bb$YEARS
Y <- orig.bb$SALARY

b <- sum((mean(X) - X) * (mean(Y) - Y)) / sum((mean(X) - X)^2)
a <- mean(Y) - b*mean(X)
m <- lm(Y~X)
Y_hat <- b*X + a
Y_bar <- mean(Y)

TSS <- sum((Y_bar - Y)^2)
ESS <- sum((Y_bar - Y_hat)^2)
RSS <- sum((Y - Y_hat)^2)

(r_sq <- ESS / TSS)
(summary(m)$r.squared)

# ****************************************************************************
#               Sample Results and Population Parameters
# ****************************************************************************

# In addition to considering the performance of the overall model we can also
# judge the performance of the estimated parameters a and b.

# We are typically working with a sample of data, not the full population.
# Thus we collect a sample and use it to calculate OLS values for a and b; these
# values then define a sample regression line

# Our hope is that a and b are good reps of α and β.
# suppose we replaced our sample and created a new one
# The new sample could then be used to calculate a new a and b
# The sample values for a and b are random variables

# The measures used to judge the reliability of a and b as estimated of the
# population parameters is the standard error, the smaller the better.

# Standard errors (se) are in the same units as the dependent variables, and we 
# care about the se size relative to the coefficient. 

# Given the values of the standard errors of our a and b, we can use them to
# test various ideas or hypotheses. The most common test is the zero hypothesis
# test as the null hypothesis.

# H₀: β =  0 null hypothesis
# H₁: β <> 0 alt  hypothesis

# The t value for a parameter is its coefficent divided by its standard error
# If we reject a hypothesis when it is in fact true, we are committing a Type I
# error. If we fail to reject a hypothesis when it is in fact false, we are
# committing a Type II error.o
# Therefore, the p value represents the chance that we are committing a Type I
# error
