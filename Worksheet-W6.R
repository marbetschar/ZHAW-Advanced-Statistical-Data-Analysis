#############
# Exercise 6
#############

chal <- read.table("data/O-rings.dat", header=TRUE)
str(chal)

# a)
sunflowerplot(chal$Temp, chal$Fails/chal$m, number=chal$m)
# -> Shows a tendency for more failures at lower temperatures

chal.glm1 <- glm(cbind(Fails, m-Fails) ~ Pres + Temp, family = binomial(), data=chal)
summary(chal.glm1)

chal.glm2 <- glm(cbind(Fails, m-Fails) ~ Temp, family = binomial(), data=chal)
summary(chal.glm2)

# deviance
anova(chal.glm1, chal.glm2, test="Chisq")

confint.default(chal.glm1)
confint(chal.glm1)

predict(chal.glm2, newdata=data.frame(Temp=31), type="link",)