##
## Worksheet Week 1 - Solution
## ***************************
##
## Exercise 1
## ~~~~~~~~~~
DPath <- "data/"
SD <- read.table(paste(DPath,"Softdrink.dat", sep=""), header=T)
summary(SD)

plot(Time ~ volume, data=SD)

## Exercise 1(a)
## Fit
SD.lm <- lm(Time ~ volume, data=SD) ## fitting
summary(SD.lm)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    3.321      1.371   2.422   0.0237 *
## volume         2.176      0.124  17.546 8.22e-15 ***
## ---
## Residual standard error: 4.181 on 23 degrees of freedom
## Multiple R-squared:  0.9305,	Adjusted R-squared:  0.9275
## F-statistic: 307.8 on 1 and 23 DF,  p-value: 8.22e-15


## Residual and Sensitivity Analysis
source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(SD.lm)
plot.lmSim(SD.lm, SEED=1798)
## Residual plot: Smoother shows a banana form which is outside the
##   stochastic fluctuation. Hence, there is evidence that the expectation
##   of the error is not  constant.
## Scale-location plot: Smoother shows a increasing trend which is outside
##   the stochastic fluctuation and quite untypical in form. Hence, there
##   is evidence that assumption of a constant error variance is violated.
## Normal Q-Q plot: The points deviate from a straight line at both ends
##   indicating a longer tailed distribution than the Gaussian distribution.
##   The point at the right end is outside the stochastic fluctuation and
##   hence, there is some evidence that the assumption of Gaussian distributed
##   errors is violated.
## Residuals vs leverage: Observations 9 and 22 have a Cook's distance
##   larger than 1 and hence, are too influential. Both of these two
##   observations have a too high leverage.

## To sum up: The model does not fit the data adequately! The most severe
##  problems seems that the variance is not constant. Thus, a
##  log-transformation of the response may help.


##
## Exercise 1 (b)
## Both variables are amounts. Hence, the Tukey's first-aid transformations
## suggest to log- transform both variables:
SD$lTime <- log(SD$Time)     # natural logarithm
SD$lVolume <- log(SD$volume)
SD.lm2 <- lm(lTime ~ lVolume, data=SD)
summary(SD.lm2)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1.50560    0.10897   13.82 1.26e-12 ***
## lVolume      0.74575    0.05317   14.03 9.25e-13 ***
## ---
## Residual standard error: 0.1738 on 23 degrees of freedom
## Multiple R-squared:  0.8953,	Adjusted R-squared:  0.8908
## F-statistic: 196.7 on 1 and 23 DF,  p-value: 9.252e-13

## Residual and Sensitivity Analysis
par(mfrow=c(2,4))
plot(SD.lm2)
plot.lmSim(SD.lm2, SEED=1798)
## Residual plot: Smoother still shows a banana form which is outside the
##   stochastic fluctuation on the r.h.s. Hence, there is evidence that the
##   expectation of the error is not  constant.
## Scale-location plot: Smoother shows a slight banana form which, however,
##   is within the stochastic fluctuation. Hence, there
##   is NO evidence that the assumption of a constant error variance is
##   violated.
## Normal Q-Q plot: The points do not deviate from a straight line and are
##   within the stochastic fluctuation. Hence, there is NO evidence that
##   the assumption of Gaussian distributed errors is violated.
## Residuals vs leverage: All observations have a Cook's distance
##   smaller than 1 and hence, there is NO too influential observation.

## To sum up: The model does still not fit the data really adequately! But
##   the is just a slight evidence that the errors have not a constant
##   expectation.

## An alternative transformation for volume could be the square root
## transformation because the volume is measured in "number of cases"!
SD$sVolume <- sqrt(SD$volume)
SD.lm3 <- lm(lTime ~ sVolume, data=SD)
summary(SD.lm3)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1.55465    0.09425   16.50 3.08e-14 ***
## sVolume      0.50133    0.03184   15.74 8.27e-14 ***
## ---
## Residual standard error: 0.1565 on 23 degrees of freedom
## Multiple R-squared:  0.9151,	Adjusted R-squared:  0.9114
## F-statistic: 247.8 on 1 and 23 DF,  p-value: 8.271e-14


## Residual and Sensitivity Analysis
par(mfrow=c(2,4))
plot(SD.lm3)
plot.lmSim(SD.lm3, SEED=1798)
## Yes! We could remedy the model inadequacies by suitable transformations
## of the variables.

##
## Exercise 1 (c)
##
## Regression model in its original scale:
## Time = alpha * exp(beta_1 * sqrt(Volume_i) * exp(E_i)
##   where alpha = exp(beta_0)
##         exp(E_i) is lognormal distributed with parameters mu=0 and
##                  sigma=sigma

##
## Exercise 1 (d)
## The variables distance is an amount as well. Hence, the Tukey's first-aid
## transformations suggest to log- transform it:
SD$lDist <- log(SD$distance)     # natural logarithm
SD$lVolume <- log(SD$volume)
SD.lm4 <- lm(lTime ~ sVolume + lDist, data=SD)
summary(SD.lm4)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1.14704    0.14300   8.021 5.65e-08 ***
## sVolume      0.41553    0.03649  11.389 1.08e-10 ***
## lDist        0.14401    0.04234   3.401  0.00256 **
## ---
## Residual standard error: 0.1296 on 22 degrees of freedom
## Multiple R-squared:  0.9443,	Adjusted R-squared:  0.9393
## F-statistic: 186.6 on 2 and 22 DF,  p-value: 1.587e-14

## Residual and Sensitivity Analysis
par(mfrow=c(2,4))
plot(SD.lm4)
plot.lmSim(SD.lm4, SEED=1798)
## Residual plot: Smoother is almost horizontal and within the stochastic
##   fluctuation. Hence, there is NO evidence that the expectation of the
##   error is not  constant.
## Scale-location plot: Smoother is almost horizontal and is within the
##   stochastic fluctuation. Hence, there is NO evidence that the assumption
##   of a constant error variance is violated.
## Normal Q-Q plot: The points do not deviate from a straight line and are
##   within the stochastic fluctuation. Hence, there is NO evidence that
##   the assumption of Gaussian distributed errors is violated.
## Residuals vs leverage: All observations have a Cook's distance
##   smaller than 1 though some observations are leverage points. (2*3/25=0.24)
##   Hence, there are NO too influential observations.


## According to the source information, this is an example based on real data -
##                             but the result is almost too good to be real!

## ======================================================================
##
## Exercise 2
## ~~~~~~~~~~
DPath <- "Data/"
windmill <- read.table(paste(DPath,"Windmill.dat", sep=""), header=T)
summary(windmill)
plot(DC.output ~ velocity, data=windmill)


## Exercise 2 (a)
## Fit
WM.lm1 <- lm(DC.output ~ velocity, data=windmill)

abline(WM.lm1)

## Residual and Sensitivity Analysis
par(mfrow=c(2,4))
plot(WM.lm1)
plot.lmSim(WM.lm1, SEED=1798)
## Residual plot: Smoother shows a strong banana form which is outside the
##   stochastic fluctuation. Hence, there is evidence that the expectation
##   of the error is not  constant.
## Scale-location plot: Smoother shows a slightly decreasing trend which,
##   however, is within the stochastic fluctuation. Hence, there
##   is NO evidence that assumption of a constant error variance is violated.
## Normal Q-Q plot: The points deviate from a straight line at both ends.
##   But all points are within the stochastic fluctuation and,
##   hence, there is NO evidence that the assumption of Gaussian distributed
##   errors is violated.
## Residuals vs leverage: All observations have a Cook's distance
##   smaller than 1 and, hence, there are NO too influential observations.

## To sum up: The model does not fit the data adequately, because the
##   expectation of the error is not  constant.


##
## Exercise 2 (b)
windmill$lDC.output <- log(windmill$DC.output)
windmill$lVelocity <- log(windmill$velocity)

## Fit
WM.lm2 <- lm(lDC.output ~ lVelocity, data=windmill)

## Residual and Sensitivity Analysis
par(mfrow=c(2,4))
plot(WM.lm2)
plot.lmSim(WM.lm2, SEED=1798)
## Residual plot: Smoother shows a clear banana form which is outside the
##   stochastic fluctuation (in the middle). Hence, there is evidence that
##   the expectation of the error is not  constant.
## Scale-location plot: Smoother shows a waveform which, is outside the
##   stochastic fluctuation. Hence, there is some evidence that assumption
##   of a constant error variance is violated.
## Normal Q-Q plot: There is one point (on l.h.s) which clearly deviates from
##   a straight line. It's an outlier which is outside the stochastic
##   fluctuation and, hence, there is clear evidence that the assumption of
##   Gaussian distributed errors does not hold for all observations.
## Residuals vs leverage: Observations 25, the outlier, has a Cook's distance
##   larger than 1 and, hence, it is a too influential observations.

## To sum up: This model does fit the data even worse than the regression
##   model from 1 (a)!



##
## Exercise 2 (c)
windmill$x <- 1/windmill$velocity

## Fit
WM.lm3 <- lm(DC.output ~ x, data=windmill)

## Residual and Sensitivity Analysis
par(mfrow=c(2,4))
plot(WM.lm3)
plot.lmSim(WM.lm3, SEED=1798)
## Residual plot: Smoother shows some slight structure which is, however,
##   within the stochastic fluctuation. Hence, there is NO evidence that
##   the expectation of the error is not  constant.
## Scale-location plot: Smoother shows a waveform which is outside the
##   stochastic fluctuation. Hence, there is some evidence that assumption
##   of a constant error variance is violated but it is unclear how to fix this.
## Normal Q-Q plot: On the r.h.s., Some points deviate from
##   a straight line, indicating a shorter tail than the Gaussian distribution.
##   Usually, such a deviation is not a problem for the inference. And it is
##   within the  stochastic fluctuation. Hence, there is NO evidence that the
##   assumption of Gaussian distributed errors does not hold for all
##   observations.
## Residuals vs leverage: All observations have a Cook's distance
##   smaller than 1 and, hence, there is NO too influential observation.

## To sum up: This model does fit the data best among the three models. The
## indicated deviation from the assumption is hard to explain. According to
## the construction of the simulation, it might be that there are displayed
## violations though there is none (like in statistical tests - here about 5%)

##
## Exercise 2 (d)
## Scatterplot DC.output vs x; the view where the model is fitted
par(mfrow=c(1,1))
plot(DC.output ~ x, data=windmill)
abline(WM.lm3, col="blue")

## Scatterplot DC.output vs velocity
plot(DC.output ~ velocity, data=windmill,
     ylim=range(windmill$DC.output,coef(WM.lm3)[1],0))

## coef(WM.lm3)[1] = max. DC.output:
abline(h=coef(WM.lm3)[1], col="blue")

## predicted (fitted) expected DC.output:
range(windmill$x)  ## 0.04381808 0.18242629
wm.new2 <- data.frame(x=seq(0.043, 0.185, length=50))
lines(1/wm.new2$x, predict(WM.lm3, newdata=wm.new2))

## coef(WM.lm3)[2] is related to how much wind is needed at least
## to produce DC.output?
(h.minW <- -coef(WM.lm3)[2]/coef(WM.lm3)[1])  ## = 5.208521 m/s
abline(v=h.minW, col=5, lty=6)
abline(h=0)


### ==============================================
##
## Exercise 3
## ~~~~~~~~~~

NPS <- read.table("data/NPScosts.dat", header=T)
str(NPS)

## Exercise 3 (a)
##
## Name |  Type       | transformation
## -----|-------------|----------------
## cost |  amount     | log
## date |  continuous | -
## t1   |  amount     | -       (*)
## t2   |  amount     | -       (*)
## cap  |  amount     | log
## pr   |  binary     | -
## ne   |  binary     | -
## ct   |  binary     | -
## bw   |  binary     | -
## cum.n|  count      | sqrt
## pt   |  binary     |-

## Because of the compound interest calculation time goes in linearly
## on a log-transformed cost.

NPS$lCost <- log(NPS$cost)
NPS$lCap <- log(NPS$cap)
NPS$sqrtN <- sqrt(NPS$cum.n)


##
## Exercise 3 (b)

NPS.lm0 <- lm(lCost ~ date + t1 + t2 + lCap+ pr + ne + ct + bw + sqrtN + pt,
              data=NPS)
summary(NPS.lm0)
##  Coefficients:
## (Intercept) -13.875045   5.404826  -2.567  0.01796 *
## date          0.219318   0.082426   2.661  0.01463 *
## t1            0.006067   0.021990   0.276  0.78531
## t2            0.005273   0.004564   1.155  0.26092
## lCap          0.692542   0.137131   5.050 5.32e-05 ***
## pr           -0.105307   0.082004  -1.284  0.21307
## ne            0.254326   0.078075   3.257  0.00377 **
## ct            0.122969   0.068386   1.798  0.08654 .
## bw            0.029418   0.104469   0.282  0.78101
## sqrtN        -0.069016   0.040985  -1.684  0.10700
## pt           -0.229133   0.128059  -1.789  0.08800 .
## ---
## Residual standard error: 0.1666 on 21 degrees of freedom
## Multiple R-squared:  0.8684,	Adjusted R-squared:  0.8057
## F-statistic: 13.85 on 10 and 21 DF,  p-value: 3.983e-07

## Because the P-value for pt is larger than 0.05 (it is 0.0880),
## we cannot reject the null-hypothesis that the coefficient
## associated to the variable pt is zero. Hence, there is no
## evidence that the variable pt affects the response.


## Exercise 3 (c)
par(mfrow=c(2,4))
plot(NPS.lm0)
plot.lmSim(NPS.lm0, SEED=2018)
## Just a comment on the scale-location plot:
## The smoother shows an increasing trend which, however, is within the
## stochastic fluctuation. If we had not yet log-transformed the response,
## this trend would still suggest to log-transform the response. Because
## double log-transform the response is a very, very unusual transformation,
## we emphasis the fact that the trend is within the stochastic fluctuation.


#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* ENDE *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

