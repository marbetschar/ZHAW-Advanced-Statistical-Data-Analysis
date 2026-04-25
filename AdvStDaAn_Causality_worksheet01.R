install.packages("gRbase")
library(gRbase)

############
# Exercise 1
############

g <- dag("A",
         c("C", "D", "B", "A"),
         c("C", "B", "A"),
         c("C", "A"),
         c("B", "E"),
         c("B", "A"),
         c("D", "B"))
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
# But we can't do these interventions in real life and
# we don't have a lot of samples to compare the routes on either
# weekdays (only 37 for B) or weekends (only 52 for A), we need to collect
# more data and while doing so also define whether we want to do this on
# weekends or during work days.

# d) How would you design experimental data to measure the effect of the new Route?
# Since Weekday influences both, Route and Time, we need to get rid of it by
# pre-defining it. then we'll need to collect more data on that specific day
# for both routes to be able to ultimately compare their travel time.