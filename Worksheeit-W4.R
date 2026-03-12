# Exercise 1
# a)
df1 <- read.table("data/turbines.dat", header = TRUE)
str(df1)
par(mfrow=c(1,1))
plot(df1$Hours, df1$Fissures / df1$Turbines)
sunflowerplot(x=df1$Hours, y=df1$Fissures / df1$Turbines, number=df1$Turbines, las=1)

# b )
# df1.glm <- glm((Fissures / Turbines) ~ Hours, family = binomial, data=df1) !! WARNING !!
df1.glm <- glm(cbind(Fissures, Turbines - Fissures) ~ Hours, family=binomial, data=df1)
summary(df1.glm)
coef(df1.glm)

# d)
predict(df1.glm, type='response', newdata=data.frame(Hours=3000))
