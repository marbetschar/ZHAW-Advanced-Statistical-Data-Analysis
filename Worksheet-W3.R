
# Exercise 3:
install.packages("gam")
library(gam)

df <- read.table("data/ExpressDS.dat", header = TRUE)
str(df)

df.gam1 <- gam(lo(cost) ~ lo(weight) + lo(distance), data=df)
par(mfrow=c(2,2))
plot(df.gam1, se=TRUE)

df.gam2 <- gam(lo(cost) ~ weight + lo(distance), data=df)
par(mfrow=c(2,2))
plot(df.gam2, se=TRUE)
