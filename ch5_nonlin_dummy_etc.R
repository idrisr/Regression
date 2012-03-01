# ****************************************************************************
# ********* Ch 5: Nonlinear, Dummy, Interaction, and Time Variables **********
# ****************************************************************************

# *************** Nonlinear Independent Variables ****************************
orig_bb <- read.table('data/bball.txt', header=TRUE, sep='\t')
# Using years as a quadratic term to predict salary
m <- lm(SALARY ~ YEARS + SLUGGING + FIELDING + I(YEARS^2), data=orig_bb)
# The quadratic term is negative, meaning that its rate of increase slows as it
# increases, and now fielding is not sig. Damn Good Adj. R²
# We are not restricted the square terms, and we are not restricted to positive
# exponent terms

# ******************** Dummy Independent Variables ****************************
# In some cases, one or more of our independent variables may not be a
# continuous measure. For example, we can consider our presidential election
# model. One factor that we have not considered is whather the incumbent-party
# candidate is, in fact, the incumbent himself. As example would be Clinton who
# was the incumbent praty candidate in 1996 when he ran for reelection.
