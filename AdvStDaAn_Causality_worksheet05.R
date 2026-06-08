library(gRain)

############
# Exercise 4
############

# a)
g <- dag(
  c("Temp"),
  c("Adv", "Temp"),
  c("Sales", "Temp", "Adv")
)

# b)
load("data/ice_cream.rda")
lm.adv <- lm(Adv ~ Temp, data = ice_cream)
lm.sales <- lm(Sales ~ Temp + Adv, data = ice_cream)
lm.adv
lm.sales
E(g)$weight <- c(1.74, 22.293, 2.032)
plot(g, edge.label = E(g)$weight)

# c)
ice_cream[ice_cream$Temp == max(ice_cream$Temp), ]
# -> there were 885 ice creams sold on hottest day (= day 19)

day19 <- ice_cream[19,]
Es <- day19$Sales - (22.293 * day19$Temp + 2.032 * day19$Adv)
Ea <- day19$Adv - (1.74 * day19$Temp)
Et <- day19$Temp

# do(Temp = 30):
temp_c1 <- 30
adv_c1 <- 1.74 * temp_c1 + Ea
sales_c1 <- 22.293 * temp_c1 + 2.032 * adv_c1 + Es
sales_c1
# -> if the temperature would have been 30, then 779.1 ice creams would have been sold

# do(Adv = 0):
temp_c2 <- day19$Temp
adv_c2 <- 0
sales_c2 <- 22.293 * temp_c2 + 2.032 * adv_c2 + Es
sales_c2
# -> without advertisement on that day, then 530.51 ice creams would have been sold

# d)
x0 <- data.frame(
  Temp = c(30, day19$Temp),
  Adv  = c(day19$Adv, 0))
predict(lm.sales, newdata = x0)
# The prediction describes the average number of ice creams sold for days with
# the given temperature and the given amount of advertisement. In contrast,
# the counterfactual describes how many ice creams would have been sold on the
# specific day (corresponding to data point 19) if the temperature
# and advertising had been different.
