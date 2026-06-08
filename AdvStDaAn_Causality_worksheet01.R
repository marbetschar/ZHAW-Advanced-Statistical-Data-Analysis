install.packages("gRbase")
library(gRbase)

############
# Exercise 1
############

g <- dag(
  c("A"),
  c("B", "A", "E"), # = root node + all incoming node-edges
  c("C", "A", "B", "D"),
  c("D", "B"),
  c("E")
)
plot(g)

# a)
# Yes, the depicted graphical model is a Directed-Acyclic-Graph (it has no cycles)

# b)
# directed path: A -> B -> C, E -> B -> D, A -> C
# undirected path: (D,B,C,A), (C,B,D) and (E,B,A)

# c)
# Children:
# Ch(B) = {C, D}, Ch(D) = {C}
children("B", g); children("D", g)

# Parents:
# Pa(B) = {A, E}, Pa(D) = {B}
parents("B", g); parents("D", g)

# Ancestors:
# An(B) = {A, E}, An(D) = {B, A, E}
ancestralSet("B", g); ancestralSet("D", g)

# d)
# Is the total cause of B on C transported by path B -> C?
# No, becuase there is also cause of A and E on B

# e) see code above.

############
# Exercise 2
############

# a)
load("data/NewRoute.rda") # -> reisezeit
reisezeit$route <- factor(reisezeit$route)
summary(reisezeit)
par(mfrow=c(1,2))
boxplot(time ~ route, data = reisezeit)
boxplot(time ~ route + weekday, data = reisezeit)
par(mfrow=c(1,1))
# The first boxplot show almost identical mean travel time between the routes.
# But as we also take the day of the week into account in the second boxplot,
# we can clearly see that Route B performs much worse during weekdays (Mo-Fr),
# while the travel time of Route A behaves more consistent across the entire
# week. Lets calculate the difference in numbers:
aggregate(time ~ route, data = reisezeit, mean)
aggregate(time ~ route + weekday, data = reisezeit, mean)

# b) we know that weekday is a confounding variable; so lets see how many
# data points we have:
table(reisezeit$route, reisezeit$weekday)
# most of the data points during the week are from Route A, while on weekends
# most of our data points are from Route B. Either something went wrong during
# data acquisition, or people prefer a different route based on the day of week
# for an unknown reason.
# Therefore, for the causal diagram we assume:
# - day of the week influences the choice of the route
# - day of the week influences the travel time (i.e. more traffic during weekdays)
# - the route influences the travel time
reisezeit_g <- dag(c("Time", "Weekday"),
         c("Time", "Route"),
         c("Route", "Weekday"))
plot(reisezeit_g)

# c) Intervention needed: do(Route = A), then do(Route = B)
#
# But we can't do these interventions in real life and
# we don't have a lot of samples to compare the routes on either
# weekdays (only 37 for B) or weekends (only 52 for A), we need to collect
# more data and while doing so also define whether we want to do this on
# weekends or during work days.

# d) How would you design experimental data to measure the effect of the new Route?
#
# Since Weekday influences both, Route and Time, we need to get rid of it by
# pre-defining it. then we'll need to collect more data on that specific day
# for both routes to be able to ultimately compare their travel time.

############
# Exercise 3
############

# a) Draw a causal diagram of the four variables technique, weight, gender and performance.
weight_lifting_g1 <- dag(c("Weight", "Gender"),
                   c("Performance", "Gender"),
                   c("Technique", "Weight"),
                   c("Performance", "Weight"),
                   c("Performance", "Technique"))
plot(weight_lifting_g1)

# b) Which intervention would need to be carried out to estimate a causal effect
# of weight on performance?
#
# We'd need to intervene on Weight so its the same regardless of Gender:
# do(Weight = weight). This breaks the confounding effect of Gender on Weight.

# c) How does the causal graphical model look like under the intervention b)?
weight_lifting_g2 <- dag(c("Performance", "Gender"),
                         c("Technique", "Weight"),
                         c("Performance", "Weight"),
                         c("Performance", "Technique"))
plot(weight_lifting_g2)

# d) Does the causal effect of weight on performance change when all
# participants receive a specific instruction on the technique?
#
# Yes it does, because there is still an indirect effect of Weight
# on the Performance based on the Technique: If the Technique changes,
# so will the overall Performance.

############
# Exercise 4
############

# a) You recruit 300 persons suffering from cold symptoms. You feed all 300
# persons all lots of oranges, which are full of vitamin C. Eight days later,
# 292 out of 300 people are free of cold symptoms. Did the oranges cure people?
#
# Most people recover from a cold within a week or two - even without eating
# oranges. So it is unlikely that the oranges hat a causal effect. However,
# without having a control group it is impossible to say whether the
# oranges had a causal effect or not.

# b) You recruit 300 persons for a winter race on a test track. You assign
# randomly 150 persons to cars with winter tires and the other 150 persons to
# cars with summer tires. There are 30 fewer breakdowns in the group with
# winter tires. Is driving with winter tires safer?

plot(dag(
  c("Breakdown", "Tire"),
  c("Breakdown", "Season"),
  c("Tire", "Season")))

# Based on this experiment, we cannot tell that driving with winter tires is
# generally safer, because the season influences the choice of tires. That said,
# we would need to repeat the same experiment on the same test track in a
# summer race using summer and winter tires. However, what we can conclude
# from this experiment is that driving with winter tires is safer
# _under winter conditions_.

# c) You are comparing two female teams of floorball players: U21 vs. seniors.
# There were 8 cases of cruciate ligament injuries in the U21 team last year
# and only 1 case in the senior team. Can we conclude from this that the
# senior team is stronger?

plot(dag(
  c("Team", "Age"),
  c("Injury", "Team"),
  c("Injury", "Age")))

# No. We have to take into account that the females playing in the U21 team
# are not adults yet - they are still growing. So the age is likely to also
# have an effect on the likelihood of a cruciate ligament injury - and
# it also influences the team! so Age is a confounder variable.

############
# Exercise 5
############

# Formulate possible research questions on association, intervention and
# imagination for the following pairs (with reference to Pearl’s ladder).

# a) Effect of snow and ice on car accidents
#
# Assocation: What is the expected amount of car accidents for ice and snow?
# Interventions: What if someone only drives on snow and ice free roads, is he less likely to have an accident?
# Counterfactuals: I had an accident last winter. Was it avoidable if there would not have been any snow and ice?

# b) Effect of smoking on lung cancer
# 
# Association: What is the likelihood of lung cancer for smokers?
# Intervention: What if I quit smoking, am I then less likely to get lung cancer?
# Counterfactuals: I don't have lung cancer. Is it because I'm not smoking?
#