# ***************************************************************************
#  Chapter 4 Multiple Regression Analysis
#  Book: Regression Basics, Leo Kahane, 1st edition
# ***************************************************************************

# 4.1 ***********************************************************************
abor_orig <- read.table('data/abort.txt', header=TRUE, sep='\t')
m <- lm(ABORTION ~ EDUC + RELIGION + PRICE + PICKET + INCOME, data=abor_orig)
m1 <- lm(ABORTION ~ RELIGION + PRICE + PICKET + INCOME, data=abor_orig)

# Model with EDUC slightly better
summary(m)$adj.r.squared > summary(m1)$adj.r.squared

# EDUC not a significant covariate
summary(m)$coefficients['EDUC', 'Pr(>|t|)'] < 0.05

# 4.2 ***********************************************************************
# Is the the coefficient for Growth statistically different from 1.0 at α = .1?
alpha <- 0.1
h <- 1
orig_elec <- read.table('data/Elect.txt', header=TRUE, sep='\t')
m <- lm(Votes ~ Growth + Inflation, data=orig_elec)
ms <- summary(m)
se <- ms$coefficients['Growth', 'Std. Error']
b <- ms$coefficients['Growth', 'Estimate']
df <- dim(orig_elec)[1] - dim(ms$coefficients)[1]
t <- (b-h) / se

# qt assumes one-sided test, so therefore divide alpha by 2. 
# to reject H₀: |t| >= t_alpha, ie t must be more extreme than t_alpha
t_alpha <- qt(1-alpha/2, df)

# not statistically different than 1 ∴ fail to reject H₀
print('Reject H₀?')
abs(t) >= t_alpha

# 4.3 ***********************************************************************
# Predict the salary of a MLB player who has 10 years of experience, slugging
# average of 380 and a fielding percentage of 97.5
orig_bb <- read.table('data/bball.txt', header=TRUE, sep='\t')
m <- lm(SALARY ~ YEARS + SLUGGING + FIELDING, data=orig_bb)
ms <- summary(m)
sal_predict <- predict(m, data.frame('YEARS'=10, 'SLUGGING'=380, 
                                     'FIELDING'=97.5))
sal_predict2 <- m$coefficients['YEARS']*10 + m$coefficients['SLUGGING']*380 +
                m$coefficients['FIELDING']*97.5 + m$coefficients['(Intercept)']
print('Predicted Salary:')
print(sal_predict * 10e6)

# 4.4 ***********************************************************************
# Back to the  model about wages from problem 3.1
# WAGE ~ EDUC + EXPERIENCE
orig_wage <- read.table('data/wage.csv', header=TRUE, sep='\t')
m <- lm(WAGE ~ EDUC + EXPER, data=orig_wage)
ms <- summary(m)
# EDUC more important, and stat sig
# EXPER not stat sig

# 4.5 ***********************************************************************
# SUV Price example continuation from problem 2.3
orig_suv <- read.table('data/SUV.txt', header=TRUE, sep='\t')
m <- lm(MSRP ~ HORSE + LITERS, data=orig_suv)
ms <- summary(m)
# horsepower makes a difference with 95% confidence
# engine size does not make a difference with 95% confidence
m1 <- lm(MSRP ~ HORSE, data=orig_suv)
m1s <- summary(m1)
ms$adj.r.squared > m1s$adj.r.squared
ms$adj.r.squared - m1s$adj.r.squared
# adding engine size improves adj R² by .015
