############
# Exercise 1
############
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

# c)
# ...?


# d)
predict(df1.glm, type='response', newdata=data.frame(Hours=3000))

# e)
turbN <- data.frame(Hours=seq(100, 10000, by=100))
y.p <- predict(df1.glm, newdata=turbN, type='response')
sunflowerplot(
  x=df1$Hours,
  y=df1$Fissures / df1$Turbines,
  number=df1$Turbines,
  las=1,
  xlim=c(0, 10000),
  ylim=c(0, 1))
lines(turbN$Hours, y.p, col=3)

# f): probit
df1.probit <- glm(cbind(Fissures, Turbines - Fissures) ~ Hours, family=binomial(link = probit), data=df1)
summary(df1.probit)
coef(df1.probit)
y.p2 <- predict(df1.probit, type='response', newdata=turbN)
lines(turbN$Hours, y.p2, col='blue', lty="dashed")

# f): clog-log
df1.cloglog <- glm(cbind(Fissures, Turbines - Fissures) ~ Hours, family = binomial(link = cloglog), data=df1)
summary(df1.cloglog)
coef(df1.cloglog)
y.p3 <- predict(df1.cloglog, type='response', newdata=turbN)
lines(turbN$Hours, y.p3, col='violet', lty="dashed")

# f): discussion
# The in-sample predictions are pretty similar. However, the out-of-sample predictions aren't:
# The clog-log does the most conservative extrapolation, while the logit and probit ones are
# very similar.

############
# Exercise 2
############
df2 <- read.csv("data/creditdata.csv", header = TRUE)
df2$Creditability <- factor(df2$Creditability)
df2$Account.Balance <- factor(df2$Account.Balance)
df2$Gender <- factor(df2$Gender)
df2$Marital.Status <- factor(df2$Marital.Status)
df2$Occupation <- factor(df2$Occupation)
df2$Savings <- factor(df2$Savings)
str(df2)

# a) 
df2.glm <- glm(Creditability ~ Account.Balance + Duration, family=binomial, data=df2)
summary(df2.glm)
coef(df2.glm)
#
# Account.Balance is a categorical variable with the follow meaning:
# Account.Balance2 := account balance at this bank of 0-200 EUR
# Account.Balance3 := account balance at this bank of >= 200 EUR
# Account.Balance4 := no account at this bank
# => The coefficients state stat the chance of repaying the loan is higher,
# the higher the account balance is with this bank. But it its highest for
# customers who do not have an account with this bank.
#
# Duration describes the credit duration in months. The estimate coefficient
# states that the odds for settling the loan decreases with a constant factor
# of exp(-0.03681652) = 0.963853 for each additional month.

# b)
min(df2$Duration); max(df2$Duration)
durations <- seq(0, 100, by=0.1)

# Account.Balance = 1
df2.predict1 <- data.frame(
  Duration=durations,
  Account.Balance=factor(
    rep(1, length(durations)),
    levels=c(1,2,3,4)))
str(df2.predict)
y2.p1 <- predict(df2.glm, newdata = df2.predict1, type = 'response')

# Account.Balance = 2
df2.predict2 <- df2.predict1
df2.predict2$Account.Balance <- factor(rep(2, length(durations)), levels=c(1,2,3,4))
y2.p2 <- predict(df2.glm, newdata = df2.predict2, type = 'response')

# Account.Balance = 3
df2.predict3 <- df2.predict1
df2.predict3$Account.Balance <- factor(rep(3, length(durations)), levels=c(1,2,3,4))
y2.p3 <- predict(df2.glm, newdata = df2.predict3, type = 'response')

# Account.Balance = 4
df2.predict4 <- df2.predict1
df2.predict4$Account.Balance <- factor(rep(4, length(durations)), levels=c(1,2,3,4))
y2.p4 <- predict(df2.glm, newdata = df2.predict4, type = 'response')

# Plot:
cols <- c("green", "blue", "orange", 'violet')
plot(
  df2$Duration,
  as.numeric(as.character(df2$Creditability)),
  col=cols[df2$Account.Balance],
  xlim=c(0, 100))
lines(durations, y2.p1, col=cols[1])
lines(durations, y2.p2, col=cols[2])
lines(durations, y2.p3, col=cols[3])
lines(durations, y2.p4, col=cols[4])
abline(h=0.5, col="black")
n_levels <- nlevels(df2$Account.Balance)
legend_labels <- paste0("Account.Balance = ", 1:n_levels)
legend("topright", legend = legend_labels, text.col = cols[1:length(levels(df2$Account.Balance))])

# c)
# The loan durations which are expected to be settled are:
# Account.Balance <= 0 EUR: Less than 20 months
# Account Balance 0-200 EUR: Less than 35 months
# Account.Balance >= 200 EUR: Less than 55 months
# No account at this bank: Less than 75 months
