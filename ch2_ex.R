# 2.1 **********************************************************************
y <- c(10.5, 9.75, 10, 12.25, 8)
x <- c(13, 12, 12, 14, 10)

# reg slope
b <- sum((x-mean(x)) * (y-mean(y))) / sum((x-mean(x))^2)

# reg intercept
a <- mean(y) - b*mean(x)
(m<-lm(y~x))

# 2.2 **********************************************************************
# predict salary of mark mcgwire
orig_bb <- read.table('data/bball.txt', header=TRUE, sep='\t')
x <- orig_bb$YEARS
y <- orig_bb$SALARY
b <- sum((x-mean(x)) * (y-mean(y))) / sum((x-mean(x))^2)
a <- mean(y) - b*mean(x)
red_roid <- orig_bb[grep('McGwire', orig_bb$PLAYER), ]
# estimated salary from reg
y_est <- b * red_roid$YEARS + a
e <- red_roid$SALARY - y_est

# 2.3 **********************************************************************
# SUV regression
orig_suv <- read.table('data/SUV.txt', header=TRUE, sep='\t')
x <- orig_suv$HORSE
y <- orig_suv$MSRP

# reg slope
b <- sum((x-mean(x)) * (y-mean(y))) / sum((x-mean(x))^2)
a <- mean(y) - b*mean(x)
(lm(y~x))
require(ggplot2)
ggplot(orig_suv, aes(HORSE, MSRP)) + geom_point() + geom_smooth(method=lm)
