install.packages("gRbase")
install.packages("gRain")

library(gRbase)
library(gRain)
source("lib.R")

############
# Exercise 1
############

# a)
g.toys <- dag(
  c("Toy", "Gender"),
  c("Color", "Toy"),
  c("Color", "Gender"),
  c("Toy")
)
plot(g.toys)

# b)
load("data/toys.rda")
gn.toys <- grain(g.toys, data = toys)
summary(gn.toys)

# c) Calculate the two conditional probability:
# i.e. P(Toy = car | Gender = girl) and P(Toy = car | Gender = boy).
# Describe in your own words how these probabilities should be interpreted.
querygrain(gn.toys, nodes = c("Toy", "Gender"), type = "conditional")
# the probability a girl chooses a car is 0.39, where else the probability 
# a boy chooses a car is 0.89

# d) Calculate the two joint probabilities
# P(Toy = car, Gender = girl) and P(Toy = car, Gender = boy).
# What is the difference compared to the conditional probabilities in b)?
querygrain(gn.toys, nodes = c("Toy", "Gender"), type = "joint")
# The probability that its a boy AND a car is chosen is 0.49 where else
# the probability that its a girl AND a car is chosen is 0.059.
# The difference is, that in this case we don't know the gender upfront,
# so we are asking "how likely is it a boy and a car?" instead of
# "how likely is a boy to choose a car?"

# e) Describe the meaning of the probability
# P(Toy = car | do(Gender = girl))? Why is it impossible to observe
# data for this?
# 
# What we like to do here is to make all humanity just girls
# and then observe data for this new scenario. That's impossible.

# f) Which paths transport causal effect from gender on toy?
# Justify your answer.
# Causal effects can only be transported along the direction of arrows.
# There is only one directed paths from Gender to Toy: Gender -> Toy

############
# Exercise 2
############

# a) Build a Bayesian network with R using the given conditional probabilities.
# Visualize the resulting causal diagram.
cpt.sun <- lib.cptable(c("Sun"), levels = c("little", "much"), values = c(0.7, 0.3))
cpt.water <- lib.cptable(c("Water", "Sun"), levels = c("little", "much"), values = c(0.05, 0.95, 0.65, 0.35))
cpt.height <- lib.cptable(c("Height", "Sun", "Water"), levels = c("little", "much"), values = c(0.4, 0.6, 0.45, 0.55, 0.55, 0.45, 0.25, 0.75))
cpt.parasites <- lib.cptable(c("Parasites"), levels = c("yes", "no"), values = c(0.18, 0.82))
cpt.crop <- lib.cptable(c("Crop", "Height", "Parasites"), levels = c("low", "high"), values = c(0.67, 0.33, 0.01, 0.99, 0.9, 0.1, 0.42, 0.58))

cpt.apple_tree <- compileCPT(list(cpt.sun, cpt.water, cpt.height, cpt.parasites, cpt.crop))
gn.apple_tree <- grain(cpt.apple_tree, compile = FALSE)
plot(gn.apple_tree)

# b) Calculate the marginal probabilities of a high crop yield, P(Crop = high),
# and a low crop yield, P(Crop = low), using R.
querygrain(gn.apple_tree, nodes = c("Crop"))
# low = 0.592775, high = 0.407225

# c) What is the probability of a high crop yield given that a tree has parasites?
querygrain(gn.apple_tree, nodes = c("Crop", "Parasites"), type="conditional")
# 0.664125

# d) Are Height and Parasites independent given the Crop?
# this is a Collider structure: Parasites -> Crop <- Height
# Therefore, given the Crop, Parasites and Height become _conditionally dependant_.