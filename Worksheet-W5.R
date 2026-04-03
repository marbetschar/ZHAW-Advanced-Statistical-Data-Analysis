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

# a)
# ...
