source("RFn_Plot-lmSim.R")

df <- read.table("data/Softdrink.dat", header=TRUE)
str(df)
par(mfrow=c(1,1))
plot(df$Time ~ df$volume)

# a)
model_naive <- lm(df$Time ~ df$volume)
summary(model_naive)
par(mfrow=c(2,4))
plot(model_naive)
plot.lmSim(model_naive, SEED=4711)
plot(df$Time ~ df$volume, data=df); abline(model_naive, col='red')
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

model_first_aid <- lm(df_first_aid$Time ~ df_first_aid$volume)
summary(model_first_aid)
par(mfrow=c(2,4))
plot(model_first_aid)
plot.lmSim(model_first_aid, SEED=4711)
plot(df_first_aid$Time ~ df_first_aid$volume, data=df_first_aid); abline(model_first_aid, col='red')
# The quality of fit:
# Is very good; all plots now show that the assumptions of the linear regression are satisfied.