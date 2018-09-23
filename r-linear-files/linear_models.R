


#////////////////////////
# 1 Loading packages ----

# To install needed packages:
#   install.packages("tidyverse")
#   install.packages("multcomp")

library(tidyverse)
library(multcomp)

# Much of what we will be using is built in to R without loading any
# packages.
#
# From the [Tidyverse](https://www.tidyverse.org/) we will be using:
#
# * readr::read_csv as a convenient way to specify a data frame.
# * dplyr for data frame manipulations.
# * ggplot2 for graphing.
#
# See [here](https://monashbioinformaticsplatform.github.io/r-more/topic
# s/tidyverse.html) for our introduction to the Tidyverse.
#
# We will also be using:
#
# * multcomp::glht to calculate contrast p-values and confidence
# intervals.



#////////////////////////////
# 2 Vectors and matrices ----

# 2.1 Vector operations ----

a <- c(3,4)
b <- c(5,6)

length(a)

# R performs operations elementwise:

a + b
a * b

# We will be using the dot product a lot. This is:

sum(a*b)

# The *geometric* length of a vector is (by Pythagorus):

sqrt(sum(a*a))
sqrt(sum(a^2))

# 2.2 Matrix operations ----
#
# We can create a matrix with matrix, rbind (row bind), or cbind (column
# bind).

matrix(c(1,2,3,4), nrow=2, ncol=2)
rbind(c(1,3), c(2,4))
cbind(c(1,2), c(3,4))

X <- rbind(
    c(1,0),
    c(1,0),
    c(1,1),
    c(1,1))
X
class(X)

# The matrix transpose is obtained with t.

t(X)

# Matrix multiplication is performed with %*%. The dot product of each
# row of the left hand side matrix and each column of the right hand
# side matrix is calculated. %*% treats a vector as a single column
# matrix. Actually all we need today is to multiply a matrix by a
# vector, in which case we get the dot product of each row of the matrix
# with the vector.

X %*% a
as.vector(X %*% a)



#/////////////////////////////
# 3 Fitting linear models ----

# 3.1 Single numerical predictor ----
#
# The age (year) and height (cm) of 10 people has been measured. We want
# a model that can predict height based on age.

people <- read_csv(
    "age, height
      10,    131
      14,    147
      16,    161
       9,    136
      16,    170
      15,    160
      15,    153
      21,    187
       9,    145
      21,    195")

ggplot(people, aes(x=age, y=height)) + geom_point()

fit <- lm(height ~ age, data=people)

fit

# Coefficients are extracted with coef:

coef(fit)

# The residual standard deviation is extracted with sigma:

sigma(fit)

# Behind the scenes a matrix of predictors has been produced from the
# mysterious notation ~ age. We can examine it explicitly:

model.matrix(fit)

# model.matrix can be used without first calling lm.

model.matrix(~ age, data=people)

# 10 observations minus 2 columns in the model matrix leaves 8 residual
# degrees of freedom:

df.residual(fit)

# 3.1.1 Prediction ----
#
# predict predicts. By default it produces predictions on the original
# dataset.

predict(fit)
predict(fit, interval="confidence")

# We can also calculate predictions manually.

X <- model.matrix(fit)
beta <- coef(fit)
as.vector( X %*% beta )

# predict can be used with new data.

new_people <- data_frame(age=5:25)
predict(fit, new_people)

new_predictions <- cbind(
    new_people,
    predict(fit, new_people, interval="confidence"))

ggplot() +
    geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), data=new_predictions, fill="grey") +
    geom_line(aes(x=age, y=fit), data=new_predictions, color="blue") +
    geom_point(aes(x=age, y=height), data=people) +
    labs(y="height (cm)", x="age (year)",
         subtitle="Ribbon shows 95% confidence interval of the model")

# If you have ever used geom_smooth, it should now be a little less
# mysterious.

ggplot(people, aes(x=age, y=height)) + geom_smooth(method="lm") + geom_point()

# 3.1.2 Residuals ----
#
# The residuals are the differences between predicted and actual values.

residuals(fit)

plot(predict(fit), residuals(fit))

# Residuals should be close to normally distributed.

qqnorm(residuals(fit))
qqline(residuals(fit))

# Ideally the points would lie on the line. Are they far enough away to
# worry about? We can simulate some data to judge against.
# Try this several times:

sim <- rnorm(10, mean=0, sd=sigma(fit))
qqnorm(sim)
qqline(sim)

# plot(fit) produces a series of more sophisticated diagnostic plots.

plot(fit)

# 3.2 Single factor predictor, two levels ----

# 3.2.1 Challenge - the meanings of coefficients ----
#
# Examine the model matrix. What are the meanings of the two
# coefficients that have been fitted?
#
# Suppose instead we fit:

fit <- lm( ~ 0 + ...

# What do the coefficients in this new model represent? Does it fit the
# data better or worse than the original model?
#
# 3.3 Single factor predictor, more levels ----

# 3.4 Gene expression example ----

# 3.4.1 Curve fitting ----

# 3.4.2 Transformation ----
#
# * unpaired t-test
# * paired t-test
# * line
# * curve
#
# * challenges
#     * batched test



#//////////////////////////
# 4 Testing hypotheses ----

# 4.1 Single numerical predictor ----

fit1 <- lm(height ~ age, data=people)
fit0 <- lm(height ~ 1, data=people)

anova(fit0, fit1)

# We reject the null hypothesis that height is a constant.
#
# The p-value exactly matches that of a Pearson's correlation test:

cor.test(people$age, people$height)

# **Warning:** This is not the only way to use the anova function, but I
# think this is the safest way. Once we start using multiple predictors,
# the meaning of the output from anova with a single model is likely to
# be not quite what you want. Read the documentation carefully. The aov
# function also has traps for the unwary. Use lm, and anova with two
# nested models as in this document and the meaning should be as you
# would expect.
#
# summary also outputs p-values. Too many p-values. It doesn't respect
# the hierarchy of terms in the model. The p-value for dropping the
# intercept is nonsense with the age term still present:

summary(fit1)



#//////////////////////////////////
# 5 A very simple linear model ----

simple <- data_frame( y=c(10,11) )

X_simple <- model.matrix(~ 1, data=simple)
X_simple
fit_simple <- lm(y ~ 1, data=simple)
fit_simple

predict(fit_simple, simple, interval="confidence")
resid(fit_simple)
sigma(fit_simple)

X0_simple <- model.matrix(~ 0, data=simple)
X0_simple
dim(X0_simple)
fit0_simple <- lm(y ~ 0, data=simple)

anova(fit0_simple, fit_simple)
t.test(simple$y)

ht_simple <- glht(fit_simple, "`(Intercept)` = 0")
summary(ht_simple)

# Challenge: Try some values other than 0
summary(glht(fit_simple, "`(Intercept)` = 5"))

glht(fit_simple, rbind(c(1)), rhs=c(5)) %>% summary()

confint(ht_simple)




#////////////////////////////
# 6 Confidence intervals ----



#/////////////
# 7 limma ----
#
# A difference between glht and limma's contrasts.fit is that limma uses
# columns as contrasts, rather than rows.
