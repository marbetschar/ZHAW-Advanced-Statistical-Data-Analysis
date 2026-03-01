install.packages("car")

library(car)

source("RFn_Plot-lmSim.R")

df <- read.table("data/sniffer.dat", header = TRUE)
str(df)
summary(df)

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
df.vs <- step(df.lm_a, scope=list(
  loser=~1,
  upper=~df$Temp.Tank + df$Temp.Gas + df$Vapor.Tank + df$Vapor.Dispensed
))
# Report and discuss the result:
# -> 
