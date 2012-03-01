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
# model. One factor that we have not considered is whether the incumbent-party
# candidate is, in fact, the incumbent himself. As example would be Clinton who
# was the incumbent party candidate in 1996 when he ran for reelection.

# We can create a dummy variable indicating whether the incumbent party
# candidate is the candidate himself.
orig_elec <- read.table('data/Elect.txt', header=TRUE, sep='\t')
m <- lm(Votes ~ Growth + Incumbent, data=orig_elec)
m1 <- lm(Votes ~ Growth + Incumbent + Inflation, data=orig_elec)


# trick from the data mining with R book
m2 <- lm(Votes ~ ., data=orig_elec)
mf <- step(m2)

# Suppose you want to consider whether the baseball player's race has an effect
# on salary. You would need to encode, say, three values: White, Black,
# Hispanic. So you'd need actually 2 dummy variables
# 1st dummy takes on value 1 if player is black, 0 otherwise
# 2nd dummy takes on value 1 if player is hispanic, 0 otherwise
# if both of those are 0, then player is white
# if you have m categories for which you want to control for, you need m-1 dummy
# variables

# The data set already created the dummy variables. In R, just need to make
# those columns in the dataframe factors and they'll be auto made into dummy
# variables
m <- lm(SALARY ~ NL + YEARS + I(YEARS^2) + SLUGGING + FIELDING + BLACK + 
        HISPANIC, data=orig_bb)
m1 <- lm(SALARY ~ YEARS + SLUGGING + FIELDING + I(YEARS^2), data=orig_bb)
mf <- step(m)

# ******************** Interaction Variables ****************************
# our previous election model which included the dummy variable for the
# incumbent implicitly assumed that the only difference between incumbents and
# non-incumbents is the intercept term. 

# Another possible difference is that there may be differences in the slope
# between coefficients for incumbents and non-incumbents.  the reasoning may be
# that votes consider the actual incumbent as more responsible for the growth in
# the country than simply a candidate who is not he current president, but only
# belongs to the same party as the president.

m0 <- lm(Votes ~ Growth:Incumbent + Growth, data=orig_elec)
m1 <- lm(Votes ~ Inflation + Growth:Incumbent + Growth, data=orig_elec)
# An increase in one point of growth will increase votes for a non-incumbent by
# .39, and for an incumbent by (.39 + .34 = .73)

# You can also put the terms in the interaction by themselves also
m2 <- lm(Votes ~ Inflation + Growth:Incumbent + Growth + Incumbent, 
         data=orig_elec)

# Interactions effect can also be used to consider subcategories in models with
# several dummy variables

# Does being black and in the NL make a difference vs all other players?
m <- lm(SALARY ~ YEARS + SLUGGING + FIELDING + BLACK:NL + I(YEARS^2), 
        data=orig_bb)

# Does being good at fielding and hitting interact?
m <- lm(SALARY ~ YEARS + SLUGGING + FIELDING + SLUGGING:FIELDING + I(YEARS^2), 
        data=orig_bb)
# now the coefs for slugging and fielding went negative.  to understand what
# happens when slugging goes up by one, you take the coef of slugging and the
# coef of slugging:fielding -- ie you need to know the coef for fielding.
# for each increase in slugging, salary goes up by -1.50458 +
# .015443*(98.145)=.01078. A player with an average fielding rate will increase
# his salary by .01078 for every increase in slugging.
# The better your fielding, the more each increase in slugging is worth
# The adj R² for this model is pretty good, .8589

# 98.145 is the mean value for fielding. This is just used to develop intuition,
# for actual prediction use the actual player's fielding %
mean(orig_bb$FIELDING)

# ******************** Time as an Independent Variable ************************
# When dealing with a time series data set, you may want to consider the effect
# time itself has on our dependent variable. That is we want to see if our
# dependent variable follows a trend up or down. 

# As an example we can consider a variation on our baseball example. Instead of 
