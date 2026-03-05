install.packages("car")

library(car)

source("RFn_Plot-lmSim.R")

############
# Exercise 1
############

df <- read.table("data/sniffer.dat", header = TRUE)
str(df)
summary(df)
pairs(df)

# a)
# Y ∼ Temp.Tank + Temp.Gas + Vapor.Tank + Vapor.Dispensed
df.model_a <- df$Y ~ df$Temp.Tank + df$Temp.Gas + df$Vapor.Tank + df$Vapor.Dispensed
df.lm_a <- lm(df.model_a, data=df)

# Estimated coefficients:
summary(df.lm_a)
# What do you conclude when considering the marginal P-values and the p-value
# of the “F-statistic” in the summary output?
# -> From the coefficient p-values, only Temp.Gas and Vapor.Dispensed seem to be
# relevant (p-value < 0.05). The F-statistic indicates, that at least one of the
# predictors is non-zero (aka. relevant) because it is < 0.05.

# Determine the variance inflation factors for all of the coeﬃcients:
car::vif(df.lm_a)
# What do you conclude from these results?
# -> The VIF for Temp.Tank, Vapor.Tank and Vapor.Dispensed are all significantly
# bigger than 5, so multicollinearity is an issue and needs to be adressed.
# Even Temp.Gas is pretty close to 5.

# Does the model adequately describe the data? Conduct a residual analysis.
par(mfrow=c(2,4))
plot(df.lm_a)
plot.lmSim(df.lm_a, SEED=4711)
# -> yes, it does. However, there seem to be some leverage points in the data
# for which the cook's distance is higher than 0.5


# b) Start from the model fitted in part (a) and perform a variable selection
# using the AIC stepwise.
df.vs_a <- step(df.lm_a, scope=list(
  lower=~1,
  upper=~df$Temp.Tank + df$Temp.Gas + df$Vapor.Tank + df$Vapor.Dispensed
))
# Report and discuss the result:
# -> we do have the lowest AIC if all of the predictors are removed!?

# c) no, we did not remedy the multicollinearity deficiencies by the variable
# selection. So we are now fixing it by using mean and difference:
# check multicollinearity by inspecting the pairs:
pairs(df)

df$Temp.Diff <- df$Temp.Gas - df$Temp.Tank
df$Temp.Mean <- (df$Temp.Gas + df$Temp.Tank) / 2
df$Vapor.Diff <- df$Vapor.Dispensed - df$Vapor.Tank
df$Vapor.Mean <- (df$Vapor.Dispensed + df$Vapor.Tank) / 2

# check multicollinearity again by inspecting the pairs:
pairs(df)
# -> Vapor.Diff and Vapor.Mean are less correlated than Vapor.Tank
# and Vapor Dispensed, so we use them instead. Temp.Tank and Temp.Gas
# look less correlated by default, so we keep them:
df.model_c1 <- df$Y ~ df$Temp.Tank + df$Temp.Gas + df$Vapor.Mean + df$Vapor.Diff
df.lm_c1 <- lm(df.model_c1, data=df)
summary(df.lm_c1)
car::vif(df.lm_c1)

# Drop: df$Temp.Tank
df.model_c2 <- df$Y ~ df$Temp.Gas + df$Vapor.Mean + df$Vapor.Diff
df.lm_c2 <- lm(df.model_c2, data=df)
summary(df.lm_c2)
car::vif(df.lm_c2)
par(mfrow=c(2,4))
plot(df.lm_c2)
plot.lmSim(df.lm_c2, SEED=4711)
# -> everything looks good now!

# Interpretation
# ...?

############
# Exercise 2
############

# a)
df <- read.table("data/Jet.dat", header = TRUE)

# tukeys first aid transformations - we don't transform temperatures
# because they can be negative.
tdf <- data.frame(
  lY = log(df$Y),
  lx1 = log(df$x1),
  lx2 = log(df$x2),
  lx3 = log(df$x3),
  lx4 = log(df$x4),
  x1 = df$x1,
  x2 = df$x2,
  x5 = df$x5,
  x6 = df$x6
)

tdf.model1 <- lY ~ lx1 + lx2 + lx3 + lx4 + x5 + x6
tdf.lm1 <- lm(tdf.model1, data=tdf)
summary(tdf.lm1)
par(mfrow=c(2,4))
plot(tdf.lm1)
plot.lmSim(tdf.lm1, SEED=4711)

# !! data point 20 is too influential !!
tdf_clean <- tdf[-20, ]
tdf_clean.model1 <- lY ~ lx1 + lx2 + lx3 + lx4 + x5 + x6
tdf_clean.lm1 <- lm(tdf_clean.model1, data=tdf_clean)
summary(tdf_clean.lm1)
par(mfrow=c(2,4))
plot(tdf_clean.lm1)
plot.lmSim(tdf_clean.lm1, SEED=4711)

# do variable selection:
step(tdf_clean.lm1, scope = list(upper=tdf_clean.model1, lower=~1))
# lY ~ lx1 + lx2 + x6

tdf_clean.model2 <- lY ~ lx1 + lx2 + x6
tdf_clean.lm2 <- lm(tdf_clean.model2, data=tdf_clean)
summary(tdf_clean.lm2)
par(mfrow=c(2,4))
plot(tdf_clean.lm2)
plot.lmSim(tdf_clean.lm2, SEED=4711)
# Residual plots look good, now check for multicollinearity:
pairs(tdf_clean)
car::vif(tdf_clean.lm2)
# ups; multicollinearity is problematic!

# see whether lx1+lx2 mean and diff solves the issue:
tdf_clean$lx.mean <- (tdf_clean$lx1 + tdf_clean$lx2) / 2
tdf_clean$lx.diff <- tdf_clean$lx1 - tdf_clean$lx2
tdf_clean.model3 <- lY ~ lx.mean + lx.diff + x6
tdf_clean.lm3 <- lm(tdf_clean.model3, data=tdf_clean)
summary(tdf_clean.lm3)
par(mfrow=c(2,4))
plot(tdf_clean.lm3)
plot.lmSim(tdf_clean.lm3, SEED=4711)
pairs(tdf_clean)
car::vif(tdf_clean.lm3)
# multicolinearity is still an issue; mean and diff doesn't work!

# see whether x1+x2 mean and diff solves the issue:
tdf_clean$x.mean <- (tdf_clean$x1 + tdf_clean$x2) / 2
tdf_clean$x.diff <- tdf_clean$x1 - tdf_clean$x2
tdf_clean.model4 <- lY ~ x.mean + x.diff + x6
tdf_clean.lm4 <- lm(tdf_clean.model4, data=tdf_clean)
summary(tdf_clean.lm4)
par(mfrow=c(2,4))
plot(tdf_clean.lm4)
plot.lmSim(tdf_clean.lm4, SEED=4711)
pairs(tdf_clean)
car::vif(tdf_clean.lm4)

# multicollinearity is still a huge problem and x.diff is not significant;
# lets see what happens if we drop it:
tdf_clean.model5 <- lY ~ x.mean + x6
tdf_clean.lm5 <- lm(tdf_clean.model5, data=tdf_clean)
summary(tdf_clean.lm5)
par(mfrow=c(2,4))
plot(tdf_clean.lm5)
plot.lmSim(tdf_clean.lm5, SEED=4711)
pairs(tdf_clean)
car::vif(tdf_clean.lm5)

# lets also see what happens if we take log of x.mean:
tdf_clean$x.lmean <- log(tdf_clean$x.mean)
tdf_clean.model6 <- lY ~ x.lmean + x6
tdf_clean.lm6 <- lm(tdf_clean.model6, data=tdf_clean)
summary(tdf_clean.lm6)
par(mfrow=c(2,4))
plot(tdf_clean.lm6)
plot.lmSim(tdf_clean.lm6, SEED=4711)
pairs(tdf_clean)
car::vif(tdf_clean.lm6)
# the residual plots now look even better!