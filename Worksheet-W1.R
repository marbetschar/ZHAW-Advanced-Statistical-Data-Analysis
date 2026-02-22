source("RFn_Plot-lmSim.R")

#############
# Exercise 1
#############

df <- read.table("data/Softdrink.dat", header=TRUE)
str(df)
par(mfrow=c(1,1))
model_naive <- df$Time ~ df$volume
plot(model_naive)

# a)
lm_naive <- lm(df$Time ~ df$volume)
summary(lm_naive)
par(mfrow=c(2,4))
plot(lm_naive)
plot.lmSim(lm_naive, SEED=4711)
plot(df$Time ~ df$volume, data=df); abline(lm_naive, col='red')
# The quality of fit:
# The residuals vs. fitted plot (Tukey) looks very bad (mean 0 of residuals violated)
# Scale-Location plot looks bad as well
# Observation 9 seems to be a leverage point (Cook's distance > 1)

# b)
df_first_aid <- data.frame(
  Time=log(df$Time),
  volume=sqrt(df$volume),
  distance=log(df$distance),
  location=df$location)

model_first_aid <- df_first_aid$Time ~ df_first_aid$volume
lm_first_aid <- lm(model_first_aid)
summary(lm_first_aid)
par(mfrow=c(2,4))
plot(lm_first_aid)
plot.lmSim(lm_first_aid, SEED=4711)
plot(model_first_aid, data=df_first_aid); abline(lm_first_aid, col='red')
# The quality of fit:
# Is very good; all plots now show that the assumptions of the linear regression are satisfied.


# c)
# log(time) = beta0 + beta1 * sqrt(volume) + error // exp
# exp(log(time)) = exp(beta0 + beta1 * sqrt(volume) + error)
# time = exp(beta0) * exp(beta1 * sqrt(volume)) * exp(error)

# d)
model_d <- df_first_aid$Time ~ df_first_aid$volume + df_first_aid$distance
lm_d <- lm(model_d)
summary(lm_d)
par(mfrow=c(2,4))
plot(lm_d)
plot.lmSim(lm_d, SEED=4711)

#############
# Exercise 2
#############

# a)
df <- read.table("data/Windmill.dat", header=TRUE)
str(df)
par(mfrow=c(1,1))
model_naive <- df$DC.output ~ df$velocity
plot(model_naive)

lm_naive <- lm(model_naive)
summary(lm_naive)
par(mfrow=c(2,4))
plot(lm_naive)
plot.lmSim(lm_naive, SEED=4711)
plot(model_naive, data=df); abline(lm_naive, col='red')
# error mean is zero: Residuals vs. Fitted show non-zero mean
# error is normally distributed: QQ-Residuals look Ok
# error has constant variance: Scale-Location plot could be better
# residuals vs. leverage are not too bad - but obs 25 has > 0.5 cook's distance, so not the best

# b)
df_first_aid <- data.frame(
  DC.output=log(df$DC.output),
  velocity=log(df$velocity))

model_first_aid <- df_first_aid$DC.output ~ df_first_aid$velocity
lm_first_aid <- lm(model_first_aid)
summary(lm_first_aid)
par(mfrow=c(2,4))
plot(lm_first_aid)
plot.lmSim(lm_first_aid, SEED=4711)
plot(model_first_aid, data=df_first_aid); abline(lm_first_aid, col='red')
# error mean is zero: Residuals vs. Fitted looks better now
# error is normally distributed: QQ-Residuals is bad now, because we don't cover all areas of data
# error has constant variance: Scale-Location plot could be better
# residuals vs. leverage looks worse as well
# => we can't remedy the model inadequacies using Tukey's first aid transformations

# c)
model_theory_aid <- df$DC.output ~ I(1.0 / df$velocity)
lm_theory_aid <- lm(model_theory_aid)
summary(lm_theory_aid)
par(mfrow=c(2,4))
plot(lm_theory_aid)
plot.lmSim(lm_theory_aid, SEED=4711)
plot(model_theory_aid, data=df); abline(lm_theory_aid, col='red')
# error mean is zero: Residuals vs. Fitted show that the mean is in the expected range
# error is normally distributed: QQ-Residuals look Ok
# error has constant variance: Scale-Location plot could be better, but it looks Okayish
# residuals vs. leverage looks Ok
# => yes, this transformation helps to remedy the model inadequacies

# d)
par(mfrow=c(1,1))
plot(model_theory_aid, data=df); abline(lm_theory_aid, col='red')
# beta0 is the intercept: it describes the DC.output at zero velocity
# beta1 is the slope: it describes the influence of velocity for DC.output which
# seems to have a negative impact.


#############
# Exercise 3
#############
df <- read.table('data/NPScosts.dat', header=TRUE)
str(df)

# a)
df <- data.frame(
  cost=log(df$cost),
  date=df$date,
  t1=df$t1,
  t2=df$t2,
  cap=log(df$cap),
  pr=df$pr,
  ne=df$ne,
  ct=df$ct,
  bw=df$bw,
  cum.n=sqrt(df$cum.n),
  pt=df$pt
)

# b)
model <- df$cost ~ df$date + df$t1 + df$t2 + df$cap + df$pr + df$ne + df$ct + df$bw + df$cum.n + df$pt
model_fit <- lm(model, data=df)
summary(model_fit)
# no, Pr(>|t|) for df$pt is 0.05499, therefore not significant on the 5% level.
# and we must retain the H0 hypothesis claiming that pt can be zero.

# c)
par(mfrow=c(2,4))
plot(model_fit)
plot.lmSim(model_fit, SEED=4711)
plot(model, data=df)