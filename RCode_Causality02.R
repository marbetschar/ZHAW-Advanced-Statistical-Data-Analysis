library(gRain)
a <- cptable(c("Season"), values = c(0.6,0.4), levels = c("hot","cold"))
b <- cptable(c("Sprinkler","Season"),
             values = c(0.9, 0.1, 0.01, 0.99),
             # P(Sprinkler = on | Season = hot), P(Sprinkler = off | Season = hot)
             # P(Sprinkler = on | Season = cold), P(Sprinkler = off | Season = cold)
             levels = c("on","off"))
plist <- compileCPT(list(a, b)) # builds up a graph + probabilities
pnc <- grain(plist, compile = FALSE)
plot(pnc)
# P(Sprinkler = on), P(Sprinkler = off)
querygrain(pnc, nodes = c("Sprinkler"), type = "marginal")
0.9*0.6 + 0.01 * 0.4
# P(Season = hot), P(Season = cold)
querygrain(pnc, nodes = c("Season"), type = "marginal")
# P(Season = hot | Sprinkler = on)
querygrain(pnc, nodes = c("Season", "Sprinkler"), type = "conditional")
# P(Season = hot, Sprinkler = on)
querygrain(pnc, nodes = c("Season", "Sprinkler"), type = "joint")
querygrain(pnc, nodes = c("Season", "Sprinkler"), type = "joint")["hot","on"]
