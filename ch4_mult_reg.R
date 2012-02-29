# ****************************************************************************
# **************** Ch 4: Multiple Regression Analysis*************************
# ****************************************************************************

# Baseball salaries revisited
# Yi = α + β₁X₁i + β₂X₂i + ui

orig_bb <- read.table('data/bball.txt', header=TRUE, sep='\t')
m <- lm(SALARY ~ SLUGGING + FIELDING + YEARS, data=orig_bb)
(summary(m))
(anova(m))

# It is only appropriate to compare different R² from different models if the
# dependent variable is the same, and the number of independent variables is the
# same.

# If the number of independent variables is not the same, you must first adjust
# R² before comparison across models the same, you must first adjust R² before
# comparison across models. This must be done because R² can be increased simply
# by adding more independent variables, even if adding those additional
# independent variables is not justified.

# It is possible in a multivariate regression model that none of the independent
# variables have a high t statistic individually, yet the model as a whole might
# have value. For this we have the F-statistic, which says
# H₀: β₁ = β₂ = β₃ ... = 0
# H₀ is saying the model as a whole has no ability to explain the behavior of Y

# Presedential Elections Revisited
orig_elec <- read.table('data/Elect.txt', header=TRUE, sep='\t')
m <- lm(Votes ~ Growth + Inflation, data=orig_elec)

# Abortion Rates Revisited
orig_abor <- read.table('data/abort.txt', header=TRUE, sep='\t')
m <- lm(ABORTION ~ RELIGION + PRICE + INCOME + PICKET, data = orig_abor)
