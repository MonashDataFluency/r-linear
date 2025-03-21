


#/////////////////////
# 1 Load packages ----

library(MASS)       # ginv -- coefficient estimation
library(splines)    # ns, bs -- spline curves
library(multcomp)   # glht -- linear hypotheses
library(emmeans)    # estimated marginal means
library(tidyverse)  # working with data frames, plotting

# Much of what we will be using is built into R without loading any
# packages.



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
t(a) %*% b

# The *geometric* length of a vector is (by Pythagoras, aka Euclidean
# distance):

sqrt(sum(a*a))

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
# side matrix is calculated. %*% treats a vector as either a single
# column or single row matrix as will make sense for matrix
# multiplication. Actually all we need today is to multiply a matrix by
# a vector, in which case we get the dot product of each row of the
# matrix with the vector.

X %*% a
as.vector(X %*% a)

# 2.3 Challenge - use a dot product to calculate ----
#
# The following dot product is an elaborate way to retrieve x[2]:

x <- c(10,20,30,40)
weights <- c(0,1,0,0)       # <-- modify this line
sum(weights*x)

# Modify weights in the above to calculate different quantities:
#
# A. x[3]-x[2]
#
# B. The mean of all four values.
#


#//////////////////////////////////
# 3 Single numerical predictor ----
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

# n=10 observations minus p=2 columns in the model matrix leaves 8
# residual degrees of freedom:

df.residual(fit)

# 3.1 Prediction ----
#
# predict predicts. By default it produces predictions on the original
# dataset.

predict(fit)
predict(fit, interval="confidence")

# (Note: This is a 95% confidence interval for *uncertainty in the
# model*. Individuals *additionally* vary with standard deviation
# sigma.)
#
# We can also calculate predictions manually.

# Prediction for a 15-year old
x <- c(1, 15)
beta <- coef(fit)
sum(x * beta)

# Prediction for all original data
X <- model.matrix(fit)
as.vector( X %*% beta )

# predict can be used with new data.

new_people <- tibble(age=5:25)
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

# 3.2 Residuals ----
#
# The residuals are the differences between the actual and predicted
# values.

residuals(fit)

# There should be no remaining relationship between predictions and the
# residuals (or between any individual predictors and the residual).

plot(predict(fit), residuals(fit))

# A Q-Q (quantile-quantile) plot sorts the residuals and compares them
# to what would be expected from a normal distribution.

qqnorm(residuals(fit))
qqline(residuals(fit))

# Ideally points would lie close to the line, but deviations are not a
# disaster. Our coefficient estimates will tend toward normally
# distributed errors even if the data does not, due to the Central Limit
# Theorem. Wild outliers should be investigated, as they may have a
# large effect on the model. We will see further examples of things to
# look for in a Q-Q plot in section 6.
#
# plot(fit) produces a series of more sophisticated diagnostic plots.

plot(fit)



#///////////////////////////////////////////
# 4 Single factor predictor, two levels ----
#
# Consider a simple experiment where some outcome is measured for an
# untreated and a treated group. This can be viewed as a one-way
# ANalysis Of VAriance (ANOVA) experiment. (This is one of two senses in
# which the term ANOVA will be used today.)

outcomes <- read_csv(
       "group, outcome
    untreated,  4.98
    untreated,  5.17
    untreated,  5.66
    untreated,  4.87
      treated,  8.07
      treated, 11.02
      treated,  9.91")

outcomes$group <- factor(outcomes$group, c("untreated", "treated"))

outfit <- lm(outcome ~ group, data=outcomes)
outfit

df.residual(outfit)
sigma(outfit)

model.matrix(outfit)

# 4.1 How coefficients are estimated ----
#
# Coefficients can be estimated from responses by multiplying by the
# "Moore-Penrose generalized inverse" of X. It can be useful to examine
# this matrix to work out exactly what a fit is doing. Each row shows
# how the corresponding coefficient is estimated.

X <- model.matrix(outfit)
y <- outcomes$outcome
round(ginv(X), 3)
ginv(X) %*% y

# Here we can see the first coefficient is the average of the
# "untreated" samples, and the second is the average of the "treated"
# samples minus that average of the "untreated" samples.
#
# ( y contains noise, assumed to be identically normally distributed for
# each observation. Transformation of this noise by ginv(X) tells us the
# distribution of errors in the coefficients (see vcov()). This can be
# further propagated to give the distribution of errors in predictions,
# and in other linear combinations of coefficients. )

# 4.2 Class exercise - the meanings of coefficients ----
#
# We now consider the formula outcome ~ 0 + group.
#
# Examine the model matrix that will be used:

model.matrix(~ 0 + group, data=outcomes)

# 1. What column has been removed because 0 + was used?
#
# 2. R has responded to this by being a bit clever when representing the
# factor. What column has been added?
#
# 3. The mean of the untreated group is 5.2, the mean of the treated
# group is 9.7, and the difference between them is 4.5. Without using
# lm, what values should the coefficients have to best fit the data?
#
# Now perform the actual linear model fit:

outfit2 <- lm(outcome ~ 0 + group, data=outcomes)

# 4. Looking at the residuals and sigma, does the new model fit the data
# better or worse than the original?
#
# 4.3 Testing a hypothesis ----
#
# Besides data with categorical predictors, the term ANOVA is used to
# refer to the use of the F test. Significance is judged based on
# comparing the Residual Sums of Squares of two models. We fit a model
# representing a null hypothesis. This model formula must nest within
# our original model formula: any prediction it can make must also be
# possible to be made by the original model formula. We compare the
# models using the anova function.

outfit0 <- lm(outcome ~ 1, data=outcomes)

anova(outfit0, outfit)

# **Warning:** This is not the only way to use the anova( ) function,
# but I think it is the safest way. Once we start using multiple
# predictors, the meaning of the output from anova with a single model
# is likely to be not quite what you want, read the documentation
# carefully. drop1(fit, test="F") should usually be used instead.
#
# summary( ) also outputs p-values. Too many p-values, summary( )
# doesn't respect the hierarchy of terms in the model. The p-value for
# dropping the intercept is nonsense. The p-values are based on a t
# statistic, where F=t^2.

summary(outfit)

# confint( ) tells us not only that the difference between groups is
# non-zero but places a confidence interval on the difference. If the
# p-value were 0.05, the confidence interval would just touch zero.
# Whenever we reject a hypothesis that a single coefficient is zero, we
# may also conclude that we know its sign.

confint(outfit)

# These results exactly match those of a t-test.

t.test(outcomes$outcome[5:7], outcomes$outcome[1:4], var.equal=TRUE)

# 4.4 Challenge - does height change with age? ----
#
# Return to the people dataset.
#
# 1. Can we reject the hypothesis that height is unrelated to age?
#
# 2. Compare the result to the outcome of a correlation test using
# cor.test( ).
#
# 3. What is the 95% confidence interval on the slope, in cm per year?
#


#/////////////////////////////////////
# 5 Multiple factors, many levels ----
#
# Particle sizes of PVC plastic produced by a machine are measured. The
# machine is operated by three different people, and eight different
# batches of resin are used. Two measurements are made for each
# combination of these two experimental factors.
#
# (This example is adapted from a data-set in the faraway package.)

pvc <- read_csv("r-linear-files/pvc.csv")
pvc$operator <- factor(pvc$operator)
pvc$resin <- factor(pvc$resin)

ggplot(pvc, aes(x=resin, y=psize)) + geom_point() + facet_grid(~operator)

# 5.1 Main effects ----

pvcfit1 <- lm(psize ~ operator + resin, data=pvc)

summary(pvcfit1)
confint(pvcfit1)

# This model assumes the influence of the two factors is additive, the
# model only contains the "main effects". The meanings of the
# coefficients are:
#
# * "(Intercept)" is the particle size for Alice and R1
# * "operatorBob" is the step in particle size going from Alice to Bob
# * "operatorCarl" is the step in particle size going from Alice to Carl
# * "resinR2" is the step in particle size going from R1 to R2
# * "resinR3" is the step in particle size going from R1 to R3
# * (etc)
#
# We can use anova( ) to test if there is evidence either of these main
# effects is important. For example, to test if there is evidence that
# the operator is important to the outcome we can test pvcfit1 against a
# model in which operator is dropped:

pvcfit0 <- lm(psize ~ resin, data=pvc)
anova(pvcfit0, pvcfit1)

# The drop1( ) function with test="F" can be used as a shorthand to test
# dropping each term that can be dropped from the model formula.

drop1(pvcfit1, test="F")

# 5.2 Interactions ----
#
# We can ask if there is any interaction between the two factors. For
# example Carl might produce particularly small particles with R5. An
# additive model doesn't allow for this.

pvcfit2 <- lm(psize ~ operator + resin + operator:resin, data=pvc)
# or
pvcfit2 <- lm(psize ~ operator*resin, data=pvc)

pvcfit2

# This model allows for interactions between the two factors. There are
# enough predictors in the model matrix that each combination of levels
# can take on a distinct value. So we now have
#
# * "(Intercept)" is particle size for Alice and R1
# * "operatorBob" is the step in particle size from Alice to Bob, for R1
# * "operatorCarl" is the step in particle size from Alice to Carl, for
# R1
# * "resinR2" is the step in particle size from R1 to R2, for Alice
# * (etc)
# * "operatorBob:resinR2" is the particle size for Bob and R2, relative
# to (Intercept)+operatorBob+resinR2.
# * (etc)

anova(pvcfit1, pvcfit2)

# We do not reject the main effects model. Our data does not provide
# evidence that the interaction model is needed. Until fresh data
# demands that we need an interaction model, we will proceed with the
# main effects model.
#
# Note: Ideally we would have checked for evidence of an interaction
# effect *before* examining the main effects model in the previous
# section.

# 5.3 Contrasts and confidence intervals ----
#
# anova( ) lets us test if a particular factor or interaction is needed
# at all, and summary( ) allows us to see if any levels of a factor
# differ from the first level. However we may wish to perform different
# comparisons of the levels of a factor -- this is called a "contrast".
# We might also want to test some more complicated combination of
# coefficients such as a difference between two hypothetical
# individuals. In general this is called a "linear hypothesis" or a
# "general linear hypothesis".
#
# Say we want to compare Bob and Carl's particle sizes. We will use the
# pvcfit1 model.

coef(pvcfit1)
K <- rbind(Carl_vs_Bob = c(0, -1,1, 0,0,0,0,0,0,0))

K %*% coef(pvcfit1)

# This is the estimated difference in particle size between Carl and
# Bob, but can we trust it? The glht function from multcomp can tell us.
# GLHT stands for General Linear Hypothesis Test.

library(multcomp)

result <- glht(pvcfit1, K)
result
summary(result)
confint(result)

# glht can test multiple hypotheses at once. By default it applies a
# multiple testing correction when doing so. This is a generalization of
# Tukey's Honestly Significant Differences.

K <- rbind(
    Bob_vs_Alice  = c(0,  1,0, 0,0,0,0,0,0,0),
    Carl_vs_Alice = c(0,  0,1, 0,0,0,0,0,0,0),
    Carl_vs_Bob   = c(0, -1,1, 0,0,0,0,0,0,0))
result <- glht(pvcfit1, K)
summary(result)
confint(result)
plot(result)

# We can also turn off the multiple testing correction.

summary(result, test=adjusted("none"))

# A reasonable compromise between these extremes is Benjamini and
# Hochberg's False Discovery Rate (FDR) correction.

summary(result, test=adjusted("fdr"))

# Finally, we can ask if *any* of the linear combinations is non-zero,
# i.e. whether the model with all three constraints applied can be
# rejected. This is equivalent to the anova( ) tests we have done
# earlier. (Note that while we have three constraints, the degrees of
# freedom reduction is 2, as any 2 of the constraints are sufficient.
# This makes me uneasy as it is reliant on numerical accuracy, better to
# just use any two of the constraints.)

summary(result, test=Ftest())

pvcfit0 <- lm(psize ~ resin, data=pvc)
anova(pvcfit0, pvcfit1)

# This demonstrates that the two methods of testing hypotheses--with the
# ANOVA test and with linear hypotheses--are equivalent.

# 5.4 Heteroscedasticity ----

ggplot(pvc, aes(x=resin, y=residuals(pvcfit1))) +
    geom_point() + geom_hline(yintercept=0) + facet_grid(~operator)

# Our assumption that the residual noise is uniformly normally
# distributed may not hold. Carl's data seems to have greater standard
# deviation than Alice or Bob's. When comparing Alice and Bob's results,
# including Carl's data in the model may alter the outcome.

# 5.5 Challenge - examine contrasts ----
#
# Using the pvcfit1 model, construct linear hypotheses to see if the
# effect of:
#
# 1. R8 is different to R4
# 2. R2 is different to R1
#
# 5.6 Easier ways to specify contrasts ----
#
# So far we have been manually constructing linear hypotheses. The
# multcomp package automates this for some common situations. To compare
# all pairs of levels within a factor, use the mcp function, giving the
# factor to test as the name of an argument and specifying to test
# "Tukey" contrasts:

result <- glht(pvcfit1, mcp(operator="Tukey"))

# To compare the first level of a factor to all other levels, specify to
# test "Dunnett" contrasts:

result <- glht(pvcfit1, mcp(operator="Dunnett"))

# The linear hypotheses actually used can be inspected with
# result$linfct.
#
# The emmeans package also automates many common comparisons.

library(emmeans)

# Mean of the predictions for a set of typical individuals.
# Not the mean of your input data!
operator_means <- emmeans(pvcfit1, ~ operator)
operator_means

# Differences in the mean of predictions between groups.
operator_pairwise <- emmeans(pvcfit1, pairwise ~ operator)
operator_pairwise
confint(operator_pairwise$contrasts)

operator_vs_control <- emmeans(pvcfit1, trt.vs.ctrl ~ operator)
operator_vs_control
confint(operator_vs_control$contrasts)

# emmeans does not calculate means from your data directly. Instead it
# calculates the mean of the predictions for a set of typical
# individuals (the "reference grid"). If individuals in your groups are
# affected by known variables, and you include these variables as
# predictors in your model, emmeans will naturally adjust for these to
# provide fair comparisons between groups.
#
# By default emmeans will use a reference grid with one individual for
# each combination of the levels of the factors in your model. For any
# numeric predictor, the average of the input data is used. It is up to
# you to decide if this is sensible! This approach provides reasonable
# means even in models with interaction terms.
#
# emmeans is a super cool package, and has great documentation. In
# practice, this is usually our preferred option.



#///////////////////////////////
# 6 Gene expression example ----
#
# Tooth growth in mouse embryos is studied using RNA-Seq. The RNA
# expression levels of several genes are examined in the cells that form
# the upper and lower first molars, in eight individual mouse embryos
# that have been dissected after different times of embryo development.
# The measurements are in terms of "Reads Per Million", essentially the
# fraction of RNA in each sample belonging to each gene, times 1
# million.
#
# (This data was extracted from ARCHS4
# (https://amp.pharm.mssm.edu/archs4/). In the Gene Expression Omnibus
# it is entry GSE76316. The sample descriptions in GEO seem to be out of
# order, but reading the associated paper and the genes they talk about
# I *think* I have the correct order of samples!)

teeth <- read_csv("r-linear-files/teeth.csv")

teeth$tooth <- factor(teeth$tooth, levels=c("lower","upper"))
teeth$mouse <- factor(teeth$mouse)

# It will be convenient to have a quick way to examine different genes
# and different models with this data.

# A convenience to examine different model fits
more_data <- expand.grid(
    day=seq(14.3,18.2,by=0.01),
    tooth=factor(c("lower","upper"), levels=c("lower","upper")))

look <- function(y, fit=NULL) {
    p <- ggplot(teeth,aes(x=day,group=tooth))
    if (!is.null(fit)) {
        more_ci <- cbind(
            more_data,
            predict(fit, more_data, interval="confidence"))
        p <- p +
            geom_ribbon(aes(ymin=lwr,ymax=upr), data=more_ci, alpha=0.1) +
            geom_line(aes(y=fit,color=tooth), data=more_ci)
    }
    p + geom_point(aes(y=y,color=tooth)) +
        labs(y=deparse(substitute(y)))
}

# Try it out
look(teeth$gene_ace)

# We could treat day as a categorical variable, as in the previous
# section. However let us treat it as numerical, and see where that
# leads.

# 6.1 Transformation ----

# 6.1.1 Ace gene ----

acefit <- lm(gene_ace ~ tooth + day, data=teeth)

look(teeth$gene_ace, acefit)

# Two problems:
#
# 1. The actual data appears to be curved, our straight lines are not a
# good fit.
# 2. The predictions fall below zero, a physical impossibility.
#
# In this case, log transformation of the data will solve both these
# problems.

log2_acefit <- lm( log2(gene_ace) ~ tooth + day, data=teeth)

look(log2(teeth$gene_ace), log2_acefit)

# Various transformations of y are possible. Log transformation is
# commonly used in the context of gene expression. Square root
# transformation can also be appropriate with nicely behaved count data
# (technically, if the errors follow a Poisson distribution). This gene
# expression data is ultimately count based, but is overdispersed
# compared to the Poisson distribution so square root transformation
# isn't appropriate in this case. The Box-Cox transformations provide a
# spectrum of further options.

# 6.1.2 Pou3f3 gene ----
#
# In the case of the Pou3f3 gene, the log transformation is even more
# important. It looks like gene expression changes at different rates in
# the upper and lower molars, that is there is a significant interaction
# between tooth and day.

pou3f3fit0 <- lm(gene_pou3f3 ~ tooth + day, data=teeth)
look(teeth$gene_pou3f3, pou3f3fit0)

pou3f3fit1 <- lm(gene_pou3f3 ~ tooth * day, data=teeth)
look(teeth$gene_pou3f3, pou3f3fit1)

anova(pou3f3fit0, pou3f3fit1)

# Examining the residuals reveals a further problem: larger expression
# values are associated with larger residuals.

look(residuals(pou3f3fit1))

plot(predict(pou3f3fit1), residuals(pou3f3fit1))

# It's a little easier to see if we evenly space the points along the
# x-axis using rank.

plot(rank(predict(pou3f3fit1)), residuals(pou3f3fit1))

# A QQ plot also shows some outliers.

qqnorm(residuals(pou3f3fit1))
qqline(residuals(pou3f3fit1))

# Log transformation both removes the interaction and makes the
# residuals more uniform (except for one outlier).

log2_pou3f3fit0 <- lm(log2(gene_pou3f3) ~ tooth + day, data=teeth)
log2_pou3f3fit1 <- lm(log2(gene_pou3f3) ~ tooth * day, data=teeth)

anova(log2_pou3f3fit0, log2_pou3f3fit1)

look(log2(teeth$gene_pou3f3), log2_pou3f3fit0)

qqnorm(residuals(log2_pou3f3fit0))
qqline(residuals(log2_pou3f3fit0))

# 6.2 Curve fitting ----

# 6.2.1 Smoc1 gene ----

log2_smoc1fit <- lm(log2(gene_smoc1) ~ tooth + day, data=teeth)

look(log2(teeth$gene_smoc1), log2_smoc1fit)

# In this case, log transformation does not remove the curve. If you
# think this is a problem for *linear* models, you are mistaken! With a
# little *feature engineering* we can fit a quadratic curve.
# Calculations can be included in the formula if wrapped in I( ):

curved_fit <- lm(log2(gene_smoc1) ~ tooth + day + I(day^2), data=teeth)
look(log2(teeth$gene_smoc1), curved_fit)

# Another way to do this would be to add the column to the data frame:

teeth$day_squared <- teeth$day^2
curved_fit2 <- lm(log2(gene_smoc1) ~ tooth + day + day_squared, data=teeth)

# Finally, the poly( ) function can be used in a formula to fit
# polynomials of arbitrary degree. poly will encode day slightly
# differently, but produces an equivalent fit.

curved_fit3 <- lm(log2(gene_smoc1) ~ tooth + poly(day,2), data=teeth)

sigma(curved_fit)
sigma(curved_fit2)
sigma(curved_fit3)

# poly( ) can also be used to fit higher order polynomials, but these
# tend to become wobbly and extrapolate poorly. A better option may be
# to use the ns( ) or bs( ) functions in the splines package, which can
# be used to fit piecewise "B-splines". In particular ns( ) (natural
# spline) is appealing because it extrapolates beyond the ends only with
# straight lines. If the data is cyclic (for example cell cycle or
# circadian time series), sine and cosine terms can be used to fit some
# number of harmonics from a Fourier series.

library(splines)
spline_fit <- lm(log2(gene_smoc1) ~ tooth * ns(day,3), data=teeth)

look(log2(teeth$gene_smoc1), spline_fit)

# 6.3 Day is confounded with mouse ----
#
# There may be individual differences between mice. We would like to
# take this into our account in a model. In general it is common to
# include batch effect terms in a model, to correctly model the data
# (and obtain smaller p-values), even if they are not directly of
# interest.

badfit <- lm(log2(gene_ace) ~ tooth + day + mouse, data=teeth)
summary(badfit)

# In this case this is not possible, and R has arbitrarily dropped a
# predictor from the model. As a different mouse produced data for each
# different day, mouse is confounded with day. The model matrix suffers
# from multicollinearity: the day column can be constructed as a linear
# combination of the intercept column and the mouse columns. There is no
# single best choice of coefficients.

summary( lm(day ~ mouse, data=teeth) )

# Another example of confounding would be an experiment in which each
# treatment is done in a separate batch.
#
# Even if predictors are not perfectly multicollinear, correlation
# between predictors can make their coefficient estimates inaccurate.
# One way to check for this is to attempt to predict each of the
# predictors with a linear model that uses the remaining predictors (see
# "Variance Inflation Factor").
#
# A possible solution to this problem would be to use a "mixed model",
# but this is beyond the scope of today's workshop.

# 6.4 Challenge - Wnt2 gene (20 minutes) ----
#
# Look at the expression of gene Wnt2 in column gene_wnt2.
#
# 1. Try some different model formulas.
#
# 2. Justify a particular model by rejecting simpler alternatives using
# anova( ).
#
# Some notes:
#
# * I haven't given you a way to compare different transformations of
# the response variable gene_wnt2. This is a tricky problem! Here I ask
# you to eyeball the data or trust me when I say log transformation is
# appropriate.
#
# * An alternative approach is to rank models by the Akaike Information
# Criterion (AIC) or similar. AIC lets you compare models even if they
# are not nested (but you still can't compare different transformations
# of the response variable).
#
# * Mechanical step-wise comparison of models based on many possible
# variables raises problems similar to multiple testing, and will not
# properly control statistical significance. Use significance tests (or
# AIC) in moderation, taking into account your understanding of the
# domain. If there are nuisance variables that you are not sure if you
# need to adjust for, it is ok to leave them in a model whether or not
# they appear significant.
#


#//////////////////////////////
# 7 Appendix: mixed models ----
#
# In this workshop we have looked at linear models with fixed effects
# only. A linear mixed model contains both fixed and random effects.
# Random effects are appropriate for individuals drawn from a
# population. The model we obtain will include how much variation there
# is in this population.
#
# Mixed models would be a whole further workshop, but I will attempt a
# rough outline here.
#
# In R, mixed models are most commonly fitted using the lmer function in
# lme4. A point of confusion you will encounter is that there are a
# variety of ways to perform significance tests. The authors of lme4
# duck out of this debate entirely and only provide t statistics.
# However packages such as lmerTest, emmeans and pbkrtest can provide
# p-values and CIs from an lmer model.
#
# My recommendation of a safe choice, from my reading, is to fit a model
# using the REML method (the default for lmer) and then test it using
# the Kenward-Roger method. The Satterthwaite method is also decent,
# usually producing nearly identical results with less computation.
# Other methods may produce p-values that are too small with small data
# sets. This includes the multcomp package that we used earlier,
# unfortunately. For more information see:
#
# Luke, S.G. (2017). Evaluating significance in linear mixed-effects
# models in *R. Behav Res* 49, 1494–1502
# https://doi.org/10.3758/s13428-016-0809-y
#
# The differences between methods disappear when your data is large
# enough, but "large enough" might mean not just many rows in your data
# frame but also many levels for your random effects.
#
# I will briefly demonstrate lmer with the PVC particle dataset, now
# treating resin batches as a random effect.

library(lmerTest)     # lmerTest builds on lme4, can produce p-values
library(emmeans)      # emmeans can provide Kenward-Roger CIs

pvc <- read_csv("r-linear-files/pvc.csv")
pvc$operator <- factor(pvc$operator)
pvc$resin <- factor(pvc$resin)

pvcfit_mixed <- lmer(psize ~ operator + (1|resin), data=pvc, REML=TRUE)
pvcfit_mixed

# We use new ( | ) notation to specify random effects. Right of the |
# you give the factor specifying individuals from a population. Left of
# the | you give predictors whose coefficients may vary between
# individuals. In the example above we are saying that the intercept may
# vary between individuals.
#
# The mean about which individuals vary must also be included as a fixed
# effect in the model! Here an intercept term is implicitly included in
# the model as usual. However if your random effect specified a slope or
# some-such it would also need to be given as a fixed effect. Otherwise
# you would be saying the slope varies around zero.
#
# A standard deviation has been estimated for the effect of different
# batches of resin on particle size, as well as the usual residual
# standard deviation. This is an extra source of random variation around
# the prediction from the fixed effect part of the model.
#
# drop1 from lmerTest can be used to test whether fixed effects are
# statistically significant.

drop1(pvcfit_mixed, ddf="Kenward-Roger")

# ranova from lmerTest can be used to test whether random effects are
# statistically significant.

ranova(pvcfit_mixed)

# emmeans can be used on the fixed effects of the model.

result <- emmeans(pvcfit_mixed, pairwise ~ operator, lmer.df="Kenward-Roger")
result$emmeans
confint(result$contrasts)

# For comparison, here are similar results from the fixed effects model:

pvcfit_fixed <- lm(psize ~ operator + resin, data=pvc)

drop1(pvcfit_fixed, test="F")

result_fixed <- emmeans(pvcfit_fixed, pairwise ~ operator)
result_fixed$emmeans
confint(result_fixed$contrasts)

# The simpler fixed effects model can be a useful check that you are
# fitting the mixed effects model you intend. The results will usually
# be similar, and any differences may be instructive. A fixed effect
# model might be chosen either by ignoring random effects or including
# them as fixed effects. In this example we see:
#
# * The pairwise comparisons are identical.
#
# * CIs for the means for each operator are wider for the mixed model.
# This is because they are an estimate of the mean for the whole
# population of resins. For the fixed effects model, we get the CI for
# the mean of exactly the eight resins present in the data.
#
# ---

sessionInfo()
