## Example: Sprinkler

# Define conditional probabilities:
library(gRain)
# P(Season = hot) = 0.6 and P(Season = cold) = 0.4
a <- cptable(c("Season"), values = c(0.6,0.4), levels = c("hot","cold"))
# P(Sprinkler = on | Season = hot) = 0.9
# P(Sprinkler = on | Season = cold) = 0.01
b <- cptable(c("Sprinkler","Season"),
             values = c(0.9, 0.1, 0.01, 0.99), 
             levels = c("on","off"))
# P(Rain = yes | Season = hot) = 0.15
# P(Rain = yes | Season = cold) = 0.45
c <- cptable(c("Rain","Season"), values = c(0.15, 0.85, 0.45, 0.55), 
             levels = c("yes","no"))
# P(Wet = yes | Sprinkler = on, Rain = yes) = 1
# P(Wet = yes | Sprinkler = on, Rain = no) = 1
# P(Wet = yes | Sprinkler = off, Rain = yes) = 1
# P(Wet = yes | Sprinkler = off, Rain = no) = 0
d <- cptable(c("Wet","Rain","Sprinkler"), values = c(1,0,1,0,1,0,0,1), 
             levels = c("yes","no"))
# P(Slippery = yes | Wet = yes) = 0.95
e <- cptable(c("Slippery","Wet"), values=c(0.95,0.05), levels=c("yes","no"))
# Build Bayesian Network:
plist.complete <- compileCPT(list(a, b, c, d, e))
pn.complete <- grain(plist.complete, compile = FALSE)

# Graph
set.seed(29)
plot(pn.complete)

# Backdoor Criterion:
# Non-Causal Path: Sprinkler <- Season -> Rain -> Wet
# Blocked by: Rain, Season , Rain & Season

# P(Wet = yes | do(Sprinkler = on)) 
#  = P(Wet = yes | Sprinkler = on, Rain = yes) * P(Rain = yes) +
#.   P(Wet = yes | Sprinkler = on, Rain = no) * P(Rain = no)
#  = P(Wet = Yes | Sprinkler = on, Season = hot) * P(Season = hot) +
#.   P(Wet = Yes | Sprinkler = on, Season = cold) * P(Season = cold) +

pwsr <- querygrain(pn.complete, nodes = c("Wet","Sprinkler","Rain"), type = "conditional")
pwsr["yes","on","yes"] # 1
pwsr["yes","on","no"] # 1
pr <- querygrain(pn.complete, nodes = "Rain", type = "marginal")
pr
1 * 0.27 + 1 * 0.73
# P(Wet | do(Sprinkler = off))
#  = P(Wet = yes | Sprinkler = off, Rain = yes) * P(Rain = yes) +
#.   P(Wet = yes | Sprinkler = off, Rain = no) * P(Rain = no)
pwsr["yes","off","yes"] # 1
pwsr["yes","off","no"] # 0
1 * 0.27 + 0 * 0.73
# ACE = 1 - 0.27 = 0.73