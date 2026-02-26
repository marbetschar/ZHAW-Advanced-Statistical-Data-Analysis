##
## Worksheet Week 2 - Solution
## ***************************
##
## Exercise 1
## ~~~~~~~~~
sniffer <- read.table("Data/sniffer.dat", header=T)
summary(sniffer)

## For a first impression:
pairs(sniffer, las=1)
## The explanatory variables are highly correlated with each other and with
## the response. Technically, Tukey's first-aid transformations will have a
## very small effect since the two pressure variables have a small spread:
## max/min ~= 3


## 1(a)
## Fit
sn.fit <- lm(Y ~ . , data=sniffer)

## (a) (i)
coef(sn.fit)
## (Intercept)       Temp.Tank        Temp.Gas      Vapor.Tank Vapor.Dispensed
##  1.01501756     -0.02860886      0.21581693     -4.32005167      8.97488928

## Note that there is an additional parameter which must be estimated: sigma

## (a) (ii)
summary(sn.fit)
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)      1.01502    1.86131   0.545  0.59001
## Temp.Tank       -0.02861    0.09060  -0.316  0.75461
## Temp.Gas         0.21582    0.06772   3.187  0.00362 **
## Vapor.Tank      -4.32005    2.85097  -1.515  0.14132
## Vapor.Dispensed  8.97489    2.77263   3.237  0.00319 **
##
## Residual standard error: 2.73 on 27 degrees of freedom
## Multiple R-squared: 0.9261,     Adjusted R-squared: 0.9151
## F-statistic: 84.54 on 4 and 27 DF,  p-value: 7.249e-15

## The P-value of the F-statistic in the model sn.fit indicates that the model
## can not only consist of the intercept (since the null hypothesis that all
## coefficients except this of the intercept are 0 is rejected due to a
## P-value ~ = 0 << 0.05). However, according to the marginal t-tests
## (individual lines under coefficients) not all variables are important for
## the explanation of the response.


## (a) (iii)
## VIF
library(car)
vif(sn.fit)
##      Temp.Tank        Temp.Gas      Vapor.Tank Vapor.Dispensed
##      12.997379        4.720998       71.301491       61.932647

## All variables but Temp.Gas have a VIF greater than 10. So, according to the
## rule of thumb, there are problems with multicollinearities. The Vapor.Tank
## variable is most affected according to the VIF values.


## (a) (iv)
## Residual Analysis
source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(sn.fit)
plot.lmSim(sn.fit, SEED=1803)
## Tukey-Anscombe Plot: Since the red line lies within the stochastic
##   fluctuation, there is no evidence that the linear predictor or the link
##   is incorrectly specified.
## Scale-Location Plot: Since the red line lies within the stochastic
##   fluctuation, there is no evidence that the linear predictor
##   variance / variance funtion is incorrectly specified.
## Normal Plot: Since the black dots lie within the stochastic
##   fluctuation, there is no evidence that the distribution is
##   incorrectly specified.
## Residual vs Leverage: Since all obersvations habe a Cook's distance smaller
##   than 1 (cf red contour lines), there is not too influential observation

## Conclusion: we have no evidence that the model does not fit the data
##             adequately.


## 1(b)
step(sn.fit)
## Stepwise model selection
## Initial Model:
## Y ~ Temp.Tank + Temp.Gas + Vapor.Tank + Vapor.Dispensed
##
## Final Model:
## Y ~ Temp.Gas + Vapor.Tank + Vapor.Dispensed

## Just one variable has been dropped!

step(sn.fit, direction="both",
     scopr=list(lower=~1,
                upper=~Temp.Tank + Temp.Gas + Vapor.Tank + Vapor.Dispensed))


## 1(c)
sn.fit2 <- lm(Y ~ Temp.Gas + Vapor.Tank + Vapor.Dispensed, data=sniffer)

vif(sn.fit2)
##       Temp.Gas      Vapor.Tank Vapor.Dispensed
##       4.255787       42.899447       55.907555

## The problem with multicollinearity persists.


## What could we do better? - Again, look at
pairs(sniffer[,-5])
## The 4 explanatory variables are pairwise correlated; especially strong
## 'Vapor.Tank' and 'Vapor.Dispensed'

##
## Build a new dataset:
sn2 <- data.frame(MeanVapor=0.5*(sniffer$Vapor.Tank + sniffer$Vapor.Dispensed),
                 DeltaVapor=sniffer$Vapor.Tank - sniffer$Vapor.Dispensed)
## Since the two temperature variables are less correlated and, as shown above,
## only one of them may be needed to describe the response, we do not change
## these two variables.
sn2 <- cbind(sn2, sniffer[, c('Temp.Tank', 'Temp.Gas', 'Y')])

## Fit
sn2M.fit <- lm(Y ~ . , data=sn2)
step(sn2M.fit)
## As expected: Y ~ MeanVapor + DeltaVapor + Temp.Gas

sn2M.fit1 <- lm(Y ~  MeanVapor + DeltaVapor + Temp.Gas, data=sn2)
vif(sn2M.fit1)
## MeanVapor DeltaVapor   Temp.Gas
##  4.450470   1.538981   4.255787

## Everything is fine now.

## Note that the two models
##    Y ~ Temp.Gas + Vapor.Tank + Vapor.Dispensed
## and
##    Y ~  MeanVapor + DeltaVapor + Temp.Gas
## yield the same fitted values:
sum(abs(fitted(sn2M.fit1)- fitted(sn.fit2))) ## 1.314504e-13


## Can you interpret the coefficients?
summary(sn2M.fit1)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1.06553    1.82437   0.584  0.56386
## MeanVapor    4.35974    0.71833   6.069 1.52e-06 ***
## DeltaVapor  -7.06810    2.36554  -2.988  0.00579 **
## Temp.Gas     0.20910    0.06325   3.306  0.00260 **
## ---
## Residual standard error: 2.686 on 28 degrees of freedom
## Multiple R-squared:  0.9258,	Adjusted R-squared:  0.9178
## F-statistic: 116.4 on 3 and 28 DF,  p-value: 6.427e-16


##
par(mfrow=c(2,4))
plot(sn2M.fit1)
plot.lmSim(sn2M.fit1)
## Since model sn2M.fit1 yields the same fitted values as model sn.fit2
## we will obtain the same results in the residual analysis.
## Hence, see (a - iv):
##        we have no evidence that the model does not fit the data
##        adequately.


## ======================================================================
##
## Exercise 2
## ~~~~~~~~~~
Jet <- read.table("Data/Jet.dat", header=T)
pairs(Jet)
summary(Jet)

## Step 1: Tukey's first-aid transformations (forgot to ask for that):
## Because the first four explanatory variables (x1 to x4) and the response
## are amounts we log-transform them:
Jet$lx1 <- log(Jet$x1)
Jet$lx2 <- log(Jet$x2)
Jet$lx3 <- log(Jet$x3)
Jet$lx4 <- log(Jet$x4)
Jet$lY <- log(Jet$Y)
## x5 and x6 are not transformed because temperature can be negative.

## But:
## Since the respective value range from the minimum to the maximum never
## exceeds the factor 2, the log-transformation will not bring any visible
## effects (it is well approximated by a straight line in this range).
## However, the transformation of the response will change the error structure
## in the model.


## Step 2: Fit of the full (main effect) model:
##
Jet.lm1 <- lm(lY ~ lx1 + lx2 + lx3 + lx4 + x5 + x6, data=Jet)
summary(Jet.lm1)
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -9.704e+00  8.614e+00  -1.127   0.2681
## lx1          4.090e-01  1.766e-01   2.316   0.0269 *
## lx2         -6.751e-02  2.043e-01  -0.330   0.7431
## lx3          1.364e+00  9.452e-01   1.443   0.1584
## lx4          2.897e-01  1.389e-01   2.086   0.0448 *
## x5           2.494e-04  9.415e-05   2.648   0.0123 *
## x6          -3.925e-03  7.019e-04  -5.592 3.21e-06 ***
## ---
## Residual standard error: 0.007329 on 33 degrees of freedom
## Multiple R-squared:  0.9974,	Adjusted R-squared:  0.997
## F-statistic:  2143 on 6 and 33 DF,  p-value: < 2.2e-16

source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(Jet.lm1)
plot.lmSim(Jet.lm1, SEED=1809)
## Observation 20 is an outlier and it is too influential (Cook's distance is
## lager than 1). The smoother in the Tukey-Anscombe plot shows a banana form
## which is not within the stochastic fluctuation.


Jet.lm1a <- lm(lY ~ lx1 + lx2 + lx3 + lx4 + x5 + x6, data=Jet[-20,])
par(mfrow=c(2,4))
plot(Jet.lm1a)
plot.lmSim(Jet.lm1a, SEED=1809)
## As identified above observation 20 is too influential so we will drop it for
## the variable selcetion (a check later again)


## Step 3:
## Variable selection without observation 20:
## [You will obtain a different selected model when observation 20 is included.
## Hence, observation 20 is also too influential in the variable selection
## process!]
step(Jet.lm1a, scope=list(upper=~ lx1 + lx2 + lx3 + lx4 + x5 + x6, lower=~1))
## ly ~ lx1 + lx2 + x6

step(lm(lY ~ 1, data=Jet[-20,]), direction="both",
     scope=list(upper=~ lx1 + lx2 + lx3 + lx4 + x5 + x6, lower=~1))
## ly ~ lx1 + x6 + lx2 ; that is the same solution as before
## Watch the path of the added and dropped variables.


Jet.lm2 <- lm(lY ~ lx1 + lx2 + x6, data=Jet, subset=-20)
summary(Jet.lm2)
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -6.5248156  1.4320310  -4.556 6.08e-05 ***
## lx1          0.5217840  0.0690280   7.559 7.36e-09 ***
## lx2          1.1337552  0.1989418   5.699 1.93e-06 ***
## x6          -0.0032750  0.0002786 -11.757 1.04e-13 ***
## ---
## Residual standard error: 0.005775 on 35 degrees of freedom
## Multiple R-squared:  0.9982,	Adjusted R-squared:  0.9981
## F-statistic:  6550 on 3 and 35 DF,  p-value: < 2.2e-16

##
## Multicollinearity?
library(car)
vif(Jet.lm2)
##        lx1        lx2         x6
## 109.721932 108.742539   1.991382
## Uhhh....

## (Observation 20 is flagged!)
h.col <- rep("blue", nrow(Jet)); h.col[20] <- "red"
pairs(Jet[, c("lx1", "lx2", "x6")], col=h.col)

## Since both variables (lx1 and lx2) are rotational speeds, one can e.g.
## construct the following two new variables: mean rotation speed and delta
## rotation speed
Jet$lProdRG <- 0.5*(Jet$lx1 + Jet$lx2)
Jet$lQuodRG <- Jet$lx1 - Jet$lx2
pairs(Jet[, c("lProdRG", "lQuodRG", "x6")])

pairs(Jet[, c("lProdRG", "lQuodRG", "lx1", "lx2")])
## This is leading nowhere... (it might be because the variable lx1 is
## dominating the variable lx2 with respect to the range and lx2 has much
## higher values. thus, both variables  lProdRG and lQuadRG are highly
## correlated with lx1.

##
## Alternatives?
## *************

## Use the untransformed x1 and x2

Jet$meanRG <- 0.5*(Jet$x1 + Jet$x2)
Jet$diffRG <- Jet$x1 - Jet$x2
Jet.lm3 <- lm(lY ~ meanRG + diffRG + x6, data=Jet, subset=-20)
summary(Jet.lm3)
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)  6.499e+00  1.088e-01  59.737  < 2e-16 ***
## meanRG       2.539e-04  2.773e-05   9.158 8.04e-11 ***
## diffRG       3.152e-05  2.365e-05   1.333    0.191
## x6          -3.931e-03  3.137e-04 -12.532 1.70e-14 ***
## ---
## Residual standard error: 0.00709 on 35 degrees of freedom
## Multiple R-squared:  0.9973,	Adjusted R-squared:  0.9971

## Residual standard error is slightly larger than in Jet.lm2 (=0.005775)


vif(Jet.lm3)
##     meanRG     diffRG         x6
## 206.281338 206.481767   1.674926


## Because diffRG is not significant, we drop this term:
Jet.lm4 <- lm(lY ~ meanRG  + x6, data=Jet, subset=-20)

vif(Jet.lm4)
##   meanRG       x6
## 1.000405 1.000405
## --> very good

summary(Jet.lm4)
## Residual standard error: 0.007166 on 36 degrees of freedom


par(mfrow=c(2,4))
plot(Jet.lm4)
plot.lmSim(Jet.lm4, SEED=1802)
## No evidence that any of the assumptions is violated


## Doesn^t the model lm(lY ~ meanRG  + x6) contradict the subject matter knowledge?

## lY = beta_0 + beta_1*meanRG + beta_2*x6 + E_i with W_i indep. ~ N(0, sigma^2)
## --> Y = gamma * exp(beta_1*meanRG) * exp(beta_2*x6) + exp(E_i)


## ======================================================================
##
## Exercise 3
## ~~~~~~~~~~
DPath <- "Data/"
windmill <- read.table(paste(DPath,"Windmill.dat", sep=""), header=T)
windmill$x <- 1/windmill$velocity

## Fit
WM.lm3 <- lm(DC.output ~ x, data=windmill)

## Prediction at velocity = 1 or 10 m/s
WM.new <- data.frame(x=1/c(1,10))
predict(WM.lm3, newdata=WM.new)
##          1          2
## -12.536597   1.42714


## with prediction intervals
(WM.lm3p <- predict(WM.lm3, newdata=WM.new, interval="prediction", level=0.95))
##          fit        lwr        upr
## 1 -12.536597 -13.430108 -11.643086
## 2   1.427314   1.228331   1.626298

## Comments:
## The first row is garbage because there can be no negative DC.output values!
## We have extrapolated beyond the data range. In this case, the model is
## clearly no longer valid there.

## Take home message: Be careful when predicting (especially prediction in
## the sense of extrapolation)!


## ======================================================================
##
## Exercise 4
## ~~~~~~~~~~
NPS <- read.table("Data/NPScosts.dat", header=T)
NPS$lCost <- log(NPS$cost)
NPS$lCap <- log(NPS$cap)
NPS$sqrtN <- sqrt(NPS$cum.n)

NPS.lm0 <- lm(lCost ~ date + t1 + t2 + lCap+ pr + ne + ct + bw + sqrtN + pt,
              data=NPS)

## 4(a) Variable selection with AIC
mtc2.vs <- step(NPS.lm0,
                scope=list(lower=~1,
                           upper=~date + t1 + t2 + lCap+ pr + ne + ct + bw
                                  + sqrtN + pt))
## snip ... (the end of the output is shown)
## lCost ~ date + t2 + lCap + pr + ne + ct + sqrtN + pt
##
##         Df Sum of Sq     RSS      AIC
## <none>               0.58592 -110.010
## - pr     1   0.05231 0.63823 -109.273
## - t2     1   0.05233 0.63825 -109.272
## + bw     1   0.00094 0.58498 -108.061
## + t1     1   0.00085 0.58507 -108.057
## - sqrtN  1   0.08294 0.66886 -107.773
## - ct     1   0.08740 0.67332 -107.561
## - pt     1   0.08764 0.67356 -107.549
## - ne     1   0.30004 0.88596  -98.778
## - date   1   0.61189 1.19781  -89.128
## - lCap   1   0.71083 1.29675  -86.588

## Just two variables have been dropped, bw and t1

## Let's start from the smallest model 'lCost ~ 1':
mtc2.vs <- step(lm(lCost ~ 1, data=NPS),
                scope=list(lower=~1,
                           upper=~date + t1 + t2 + lCap+ pr + ne + ct + bw
                                  + sqrtN + pt))
## Start:  AIC=-61.29
## lCost ~ 1
##
##         Df Sum of Sq    RSS     AIC
## + pt     1   2.01272 2.4153 -78.685
## + date   1   1.75252 2.6755 -75.411
## + t1     1   0.91394 3.5141 -66.686
## + lCap   1   0.76606 3.6620 -65.367
## + ne     1   0.65915 3.7689 -64.446
## + ct     1   0.29142 4.1366 -61.467
## <none>               4.4281 -61.289
## + sqrtN  1   0.15052 4.2775 -60.395
## + bw     1   0.08878 4.3393 -59.937
## + pr     1   0.05087 4.3772 -59.658
## + t2     1   0.00581 4.4223 -59.331
##
## middle part of the output has been sniped
##
## lCost ~ pt + lCap + date + ne + ct + sqrtN
##
##         Df Sum of Sq     RSS      AIC
## <none>               0.65683 -110.354
## + bw     1   0.02032 0.63651 -109.360
## + t2     1   0.01859 0.63823 -109.273
## + pr     1   0.01857 0.63825 -109.272
## + t1     1   0.00639 0.65044 -108.667
## - sqrtN  1   0.08994 0.74677 -108.248
## - pt     1   0.11283 0.76965 -107.282
## - ct     1   0.15243 0.80925 -105.676
## - ne     1   0.26944 0.92626 -101.355
## - date   1   0.54419 1.20101  -93.042
## - lCap   1   0.92655 1.58338  -84.198

## Starting from the smallest model AIC, adds all variables except four of
## them: bw, t2, pr and t1. This model is more parsimonious than the
## above one. Because its AIC value of -110.354 is smaller than that of
## the above one (AIC=-110.010), this is the better solution.

## If you follow the path of inclusion and exclusion of terms you see that the
## variable pt is the most important one according to the AIC .

## Please, do not conclude that the second way of using step() is the
## proven way. It just results in the better model in this example.


##
## 4(b)
NPS.lm1 <- lm(lCost ~ pt + lCap + date + ne + ct + sqrtN, data=NPS)
par(mfrow=c(2,4))
plot(NPS.lm1)
plot.lmSim(NPS.lm1, SEED=2018)
## Residual plot: Smoother shows some decreasing trend on the r.s.h. which
##   is, however, within the stochastic fluctuation. Hence, there is NO
##   evidence that the expectation of the error is not  constant.
## Scale-location plot: Smoother shows an increasing trend which, however,
##   is within the stochastic fluctuation. Hence, there is NO evidence that
##   the assumption of a constant error variance is violated.
##   (See also discussion in 3 (b))
## Normal Q-Q plot: On the r.h.s., Some points deviate from
##   a straight line, indicating a longer tail than the Gaussian distribution
##   which is, however, within the  stochastic fluctuation. Hence, there is NO
##   evidence that the assumption of Gaussian distributed errors does not hold
##   for all observations.
## Residuals vs leverage: All observations have a Cook's distance
##   smaller than 1 and, hence, there is NO too influential observation.

## Conclusion: This reduced model fits the data adequately.


##
## 4(c)
confint(NPS.lm1)
##                    2.5 %       97.5 %
## (Intercept) -20.47573093 -6.371552102
## pt           -0.47935760 -0.001481439
## lCap          0.47395060  0.977234875
## date          0.11775366  0.312423592
## ne            0.08586099  0.395323607
## ct            0.02177208  0.278632684
## sqrtN        -0.14819161  0.007933141

## The partial turnkey guarantees affect the cost of a LWR plant significantly
## and has a  cost-cutting effect. (But keep in mind that the model has been
## developed with the same data and, hence, the result may be too optimistic
## with respect to the confidence level.)


##
## 4(d)
library(car)
vif(NPS.lm1)
##       pt     lCap     date       ne       ct    sqrtN
## 2.497443 1.089288 2.716501 1.289015 1.142437 2.248181

## Since all vif values are smaller than 5 there is no multicollinearity issue
## in the resulting model of (a).

## ======================================================================
##
## Exercise 5
## ~~~~~~~~~~
##
## See Lecture Note 2.3.c and 2.3.d
## You find the corresponding R code in the file AdvStDaAn Chap2withR.R.
#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* ENDE *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

