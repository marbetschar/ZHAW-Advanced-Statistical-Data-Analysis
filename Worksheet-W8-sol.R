##
## Worksheet Week 8 - Solution
## ***************************
##

## Exercise 1
## ~~~~~~~~~~

source("../RFn_Plot-glmSim.R") # --> plot.glmSim


## (a)
## See slides of Week 8 and Lecture Notes, Sec 5.2
## Residual Analysis:
library(MASS)
Ships <- ships[!is.na(match(as.character(ships$type), c("A", "B", "C"))),]
Ships <- Ships[Ships$service > 0,   ]
Ships$type <- as.factor(as.character(Ships$type))
Ships$year <- as.factor(as.character(Ships$year))
Ships$period <- as.factor(as.character(Ships$period))
Ships$lService <- log(Ships$service)
names(Ships)[5] <- "Y"
Ships <- droplevels(Ships)

Ships.glm1 <- glm(Y ~ offset(lService) + type + period + year, family=poisson,
                  data=Ships)
drop1(Ships.glm1, test="Chisq")

summary(Ships.glm1)

## Overdispersion?
1-pchisq(17.565, 14) ## = 0.2273165
## Since the P-value of 0.227 is larger than 5%, there is no evidence on
## the 5% level that there is overdispersion

## Residual Analysis
par(mfrow=c(2,4))
plot(Ships.glm1)
plot.glmSim(Ships.glm1, SEED=3018)
## Residual Plot: Since the red smoother is within (depends on the seed) the
##   stochastic fluctuation (gray spaghettis), there is no evidence that the
##   expectation is incorrectly specified. (*)
## Normal Plot: Since the black points are within the stochastic fluctuation
##   (gray points), there is no evidence that there are outliers or some
##   distortions of the distributional assumption. (*)
## Scale-location Plot: Since the red smoother is within the stochastic
##   fluctuation (gray spaghettis), there is no evidence that the
##   variance is incorrectly specified.
## Residuals vs Leverage: Since all points have a Cook's distance smaller
##   than 1 there is nor too influential observation.
##   Accoding to the rule of thumb there are several leverage points which
##   are however not dangerous:
2*7/21 ## =0.6666667

## There is no evidence that the model does not fit the data adequately.
## (*) In the residual plot, an outlier (No. 21) is clearly visible. However,
##     this observation does not appear as an outlier in the half-normal plot.
##     The difference in these conclusions arises from the fact that the
##     half-normal plot is based on standardized residuals, whereas the
##     residual plot displays unstandardized residuals.


## (b)
Ships.glm2 <- glm(Y ~ lService + type + period + year, family=poisson,
                  data=Ships)
par(mfrow=c(2,4))
plot(Ships.glm2)
plot.glmSim(Ships.glm2, SEED=3018)
## Residual Plot: Since the red smoother is within (depends on the seed) the
##   stochastic fluctuation (gray spaghettis), there is no evidence that the
##   expectation is incorrectly specified.
## Normal Plot: Since the black points is within the stochastic fluctuation
##   (gray points), there is no evidence that there are outliers or some
##   distortions of the distributional assumption.
## Scale-location Plot: Since the red smoother is within the stochastic
##   fluctuation (gray spaghettis), there is no evidence that the
##   variance is incorrectly specified.
## Residuals vs Leverage: Observation 9 has a Cook's distance larger than 1 and
##   is therefore a too influential observation.

## There is no evidence that the model does not fit the data adequately. But
## there might be a too influential obervation


summary(Ships.glm2)
confint(Ships.glm2, parm="lService")
##     2.5 %    97.5 %
## 0.7385696 1.2800320

## Because the coefficient for the variable lService is 1 there is not a big
## difference between the fits of (a) and (b).


## --------------------------------------------------------------------------

## Exercise 2
## ~~~~~~~~~~
## See slides of Week 8 and Lecture Notes, Sec 5.3

## --------------------------------------------------------------------------


## Exercise 3   (Continuation of Exercise 2d, Worksheet-W7)
## ~~~~~~~~~~
Mine <- read.table("Data/mine.dat", header=T)

## (a)
## Could the dataset also be analyses using a rate model?
## No, it couln't. Because there are "0" in the variable oTime, it is not an
## adequate approach:

## Let's see
Mine[Mine$oTime==0,]
##    Y INB EXTRP sHeight oTime tEXTRP     lINB tINB t2EXTRP
## 10 4 160    80      38     0     80 5.075174   80      75
## 12 4 145    85      38     0     85 4.976734   85      75
## 14 5  43    80      40     0     80 3.761200   80      75
## 16 5  42    85      51     0     85 3.737670   85      75
## 17 5  45    85      42     0     85 3.806662   85      75
## 40 2 150    80      48     0     80 5.010635   80      75
## 42 5  11    75      42     0     75 2.397895   75      75

## To our surprise, there is an operating time of 0 but still some
## fractures (Y), i.e. Y > 0.
## Could it be that we really do not understand what the response Y counts
## or what operating time means?


## (b)
## If Y is 0 for all observations with oTime equal to 0, then we could think
## of a rate model; that is, we could consider Y/oTime as the response and
## apply a rate model approach.


## --------------------------------------------------------------------------
