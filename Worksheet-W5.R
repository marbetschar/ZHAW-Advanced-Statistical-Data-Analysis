install.packages("robustbase")

source("lib.R")
library(robustbase)

#############
# Exercise 1:
#############
#
# a) identify b(·), c(·), and d(·):
#
# General Exponential Family Definition:
# f(yi; mu_i, phi) = exp( (yi * b(mu_i) - c(mu_i)) / phi * wi + d(yi; phi, wi) )
#
# For Y ~ Pois(lambda):
#   P(Y = y) = (1 / y!) * lambda^y * e^(-lambda)  =>  Pois(lambda)  =>  E[Y] = lambda
#   b(·), c(·), d(·) ? 
#
# P(Y = y) = lambda^y * e^(-lambda) / y!
#
# log P(Y = y) = y * log(lambda) - lambda * 1 - log(y!)
#   => linear in y : y * b(·) - c(·) + d(·)
#
# => P(Y = y) = exp( y * log(lambda) - lambda - log(y!) )
#             = exp( y * log(mu) - mu - log(y!) )  for mu = lambda
#             = exp( y * b(mu) - c(mu) + d(y) )
#
# So:
#   b(mu) = log(mu)
#   c(mu) = mu
#   d(y)  = -log(y!)
#
# b) Find mean, variance, variance function V(·) and canonical link for
# Poisson distributions using b(·) and c(·):
# 
# mean := c'(mu) / b'(mu) = (mu^1)' / (log(mu))' = 1 / (1 / mu) = mu
# V(mu) := 1 / b'(mu) = 1 / (log(mu))' = 1 / (1 / mu) = mu
# var(y) := Phi / wi * V*(mu) = 1 * V(mu) = 1 * mu = mu
# canonical link function:
# g(mu) = b(mu) = log(mu)

#############
# Exercise 2:
#############

dar <- read.table("./data/dial-a-ride.dat", header = TRUE)
dar$IND <- factor(dar$IND)
str(dar)

# RDR := number of riders using the system
# VH  := the number of vehicles in operation
# HR  := hours of operation
# F   := the fare
# POP := the population
# AR  := the area of the place where the service was provided
# IND := binary variable if the service is more enhanced (=1) or limited (=0)

# The reason for collecting the data was to construct a travel demand model,
# i.e, a model that expresses number of riders in terms of other variables.
# Such models are used to forecast ridership when new systems are planned.

# a)
# Numerical Summary:
summary(dar)

# Graphical Summary:
pairs(dar)
lib.hist_df(dar)

# b)
# Fit ordinary linear regression model with original data:
dar.lm1 <- lm(RDR ~ VH + HR + F + POP + AR + IND, data = dar)
summary(dar.lm1)
# Inspect the residuals:
lib.lmSim(dar.lm1)
# The residual analysis fails due to multiple reasons:
# b1) Tukey-Anscombe (Residuals vs. Fitted): Mean of residuals is not zero; linearity assumption violated!
# b2) Q-Q: The residuals are not covered by the expected fluctuation for a normal distribution; Gaussian distribution assumption violated!
# b3) Scale-Location: The residuals are non-constant and are not covered by the grey spaghetti; constant variance assumption violated!
# b4) Residual vs. Leverage: Observation 53 has a Cooks' Distance above 0.5 and is worth checking (not too bad)

# c)
# c1) Apply Tukeys First Aid Transformations:
# Concentration and Amounts: log(x)
# Count Data: sqrt(x)
# Counted Fractions / Shares: logit(x)
dar_t <- data.frame(
  RDR = log(dar$RDR),
  VH = log(dar$VH),
  HR = sqrt(dar$HR),
  F = log(dar$F),
  POP = log(dar$POP),
  AR = log(dar$AR),
  IND = dar$IND
)
lib.hist_df(dar_t)
dar_t.lm1 <- lm(RDR ~ VH + HR + F + POP + AR + IND, data = dar_t)
summary(dar_t.lm1)
lib.lmSim(dar_t.lm1)

# c2) Fitting an additive model:
dar_t.gam1 <- gam(RDR ~ lo(VH) + lo(HR) + lo(F) + lo(POP) + lo(AR) + IND, data = dar_t)
lib.plot(dar_t.gam1, se=TRUE)
summary(dar_t.gam1)

# c3) Apply robust fitting methods:
dar_t.rlm <- lmrob(RDR ~ lo(VH) + lo(HR) + lo(F) + lo(POP) + lo(AR) + IND, data = dar_t)
summary(dar_t.rlm)
lib.lmSim(dar_t.rlm)