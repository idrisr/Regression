# **************** Baseball regression ************************************
orig <- read.table('data/bball.txt', header = TRUE)
data <- orig[, c('YEARS', 'SALARY')]
names(data) <- c('years', 'salary')
# b = Σ(Xi - Xbar)×(Yi - Ybar) / Σ(Xi - Xbar)^2
X <- data$years
Y <- data$salary

# slope of regression
# TODO: how is this related to covariance?
# b = Σ(Xi - Xbar)×(Yi - Ybar) / Σ(Xi - Xbar)^2
# a = Ybar - b×Xbar
# careful: put the sums in the right place
b <- sum((X - mean(X)) * (Y - mean(Y))) / sum((X-mean(X))^2)
a <- mean(Y) - b*mean(X)
(lm(salary~years, data=data))


# ******************* Election regression *********************************
orig_pres <- read.table('data/Elect.txt', header=TRUE)
X <- orig_pres$Growth
Y <- orig_pres$Votes
b <- sum((X-mean(X)) * (Y-mean(Y))) / sum((X-mean(X))^2)
a <- mean(Y) - b*mean(X)
m <-(lm(Votes ~ Growth, data=orig_pres))
summary(m)
require(ggplot2)
g <- ggplot(orig_pres, aes(Growth, Votes)) + geom_point() +
        geom_smooth(method=lm, se=FALSE)
g

# ******************* Abortion Regression *********************************
orig_abor <- read.table('data/abort.txt', header=TRUE, sep='\t')
X <- orig_abor$RELIGION
Y <- orig_abor$ABORTION

# This model sucks. F-stat p-value=0.3864, Adj_R²=-0.004
# Didnt knwo you could even get a negative Adj_R²
m <- lm(ABORTION~RELIGION, data=orig_abor)
b <- sum((mean(X) - X) * (mean(Y) - Y)) / sum((mean(X) - X)^2)
a <- mean(Y) - b*mean(X)
g <- ggplot(orig_abor, aes(RELIGION, ABORTION, label=STATE)) + 
        geom_point() + geom_text(aes(size=INCOME)) + geom_smooth(method=lm)
summary(m)

# ****************************************************************************
# *********** Classical Linear Regression Model Assumptions ******************
# ****************************************************************************
# 1. The average of the population errors(residuals) is 0.
# 2. The spread of errors above and below the regression line (ie the variance)
#    is uniform for all values of X. Graphically this means that the actual
#    observations for Yi for given values of Xi, fall within a uniform band around
#    the population regression function (PRF).  The tech term is homoscedastic,
#    meaning equal variance. 
# 3. The errors associated with one observation is not associated with the
#    errors from any other observations. in other words, we assume to
#    autocorrelation among the error terms. If there is a relationship between
#    the errors, that means we can make the model better by incorporating that
#    relationship
# 4. There is no measurement error in Xi and Yi
# 5. The regression model is theoretically sound for our data. This can be
#    violated by not including all relevant independent variables. Also it can
#    be violated if we assume a linear relationship, when perhaps a curve is
#    appropriate.
# 6. The error term is normally distributed.


