#
## Worksheet Week 7 - Solution
## ***************************
##


## Exercise 1
## ~~~~~~~~~
##
## 1a turb Data
##    ~~~~~~~~~
turb <- read.table("Data/turbines.dat", header=T)
str(turb)
turb.glm <- glm(cbind(Fissures, Turbines-Fissures) ~ Hours,
				family=binomial, data=turb)
summary(turb.glm)
##     Null deviance: 112.670  on 10  degrees of freedom
## Residual deviance:  10.331  on  9  degrees of freedom

## Because the response is binomially distributed with m > 1,
## we can test on overdispersion:
1-pchisq(10.331, 9)  ## 0.3243594
pchisq(10.331, 9, lower.tail=FALSE)  ## 0.3243594
## Since the p-value > 0.05 we have no evidence against the
## null hypothesis that phi=1 --> no overdispersion

source("../RFn_Plot-glmSim.R") # --> plot.glmSim

par(mfrow=c(2,4))
plot(turb.glm)
plot.glmSim(turb.glm, SEED=113)
## Residual Plot: Since the red smoother is within (depends on the seed) the
##   stochastic fluctuation (gray spaghettis), there is no evidence that the
##   expectation is incorrectly specified.
## Normal Plot: Since the black points is within the stochastic fluctuation
##   (gray points), there is no evidence that there are outliers or some
##   distortions of the distributional assumption.
## Scale-location Plot: Since the red smoother is within the stochastic
##   fluctuation (gray spaghettis), there is no evidence that the
##   variance is incorrectly specified.
## Residuals vs Leverage: Since all points have a Cook's distance smaller
##   than 1 there is nor too influential observation.
##   Accoding to Hubers rule of thumb there are there leverage points which
##   are however not dangerous. However, according to the other rule of thumb,
##   there are no leverage points:
2*2/11 ## = 0.3636364

## There is no evidence that the model does not fit the data adequately.



## 1b Premature Birth Data
##    ~~~~~~~~~~~~~~~~~~~~

bw <- read.table("Data/birth-weight.dat", header=T)
str(bw)

bw.glm1 <- glm(cbind(Y, m-Y) ~ weight, family=binomial, data=bw)
summary(bw.glm1)
##     Null deviance: 87.046  on 9  degrees of freedom
## Residual deviance: 12.439  on 8  degrees of freedom

## Because the response is binomially distributed with m > 1,
## we can test on overdispersion:
1-pchisq(12.439, 8)  ## 0.1326652
## Since the p-value > 0.05 we have no evidence against the
## null hypothesis that phi=1 --> no overdispersion

par(mfrow=c(2,4))
plot(bw.glm1)
plot.glmSim(bw.glm1, SEED=194)
## Residual plot: The smoother indicates a banana form of the expectation.
##   The curvature is too strong to be ignored, since the red smoother is
##   outside the stochastic fluctuation.
## (Otherwise there is no evidence that the assumptions are violated because
##  the "structures" are within the stochastic fluctuation.)
## If you choose a different SEED, you might come to different conclusions.

## Hence:
bw$lWeight <- log(bw$weight)
bw.glm2 <- glm(cbind(Y, m-Y) ~ lWeight, family=binomial, data=bw)
summary(bw.glm2)
##     Null deviance: 87.0456  on 9  degrees of freedom
## Residual deviance:  8.7335  on 8  degrees of freedom

## The residual deviance is smaller and thus there will be
## no evidence against the null hypothesis that phi=1
## --> no overdispersion

par(mfrow=c(2,4))
plot(bw.glm2)
plot.glmSim(bw.glm2, SEED=194)
## The Residual Plot still show the same deficiencies. However, since the red
## smoother is within the stochastic fluctuation (gray spaghettis), there is
## no evidence that the expectation is is incorrectly specified.

## But still, let's try
library(gam)
bw.gam2 <- gam(cbind(Y, m-Y) ~ lo(lWeight, span=0.6), family=binomial,
               data=bw)
par(mfrow=c(1,1))
plot(bw.gam2, residuals=TRUE, se=T)
## The gam fit does not indicate a necessary transformation. So there
## might be variables which are important for modelling the survival of
## babies, but are not included.

## with the original varaible 'weight'
bw.gam1 <- gam(cbind(Y, m-Y) ~ lo(weight, span=0.6), family=binomial,
               data=bw)
par(mfrow=c(1,1))
plot(bw.gam1, residuals=TRUE, se=T)


## Let's also look at the extended dataset baby (Exercise 1 of Worksheet Week 6)
baby <- read.table("Data/baby.dat", header=T)
str(baby)

## In the lecture (week 7) we applied the following
## transformations:
baby$tWeight <- ifelse(baby$Weight < 950, baby$Weight, 950)
baby$tApgar1 <- ifelse(baby$Apgar1 < 6.5, baby$Apgar1, 6.5)
baby$tpH <- ifelse(baby$pH < 7.27, 0, baby$pH-7.27)

baby.glm5 <- glm(Survival ~ tWeight + Age + Apgar1
                 + pH + tpH, family=binomial, baby)

par(mfrow=c(2,4))
plot(baby.glm5)
plot.glmSim(baby.glm5, SEED=1291)
## See also slides of Week 7



## 1c Dial-a-ride Data
##    ~~~~~~~~~~~~~~~~
DaR <- read.table("Data/Dial-a-ride.dat", header=T)
DaR1 <- DaR[-c(1, 33, 35, 40, 45, 53),]
str(DaR)

DaR1$sPOP <- sqrt(DaR1$POP)
DaR1$lAR <- log(DaR1$AR)
DaR1$hHR <- ifelse(DaR1$VH <= 12,0,  DaR1$HR - 12)
DaR1$hVH <- ifelse(DaR1$VH <= 7,0,  DaR1$VH - 7)
DaR1$fF <- as.factor(cut(DaR1$F, breaks=c(0,0.1,0.4,0.7,1)))

DaR1.glm1 <-  glm(RDR ~ sPOP + lAR + HR + hHR + VH + hVH + fF + IND,
                 family=poisson, data=DaR1)
summary(DaR1.glm1)
##     Null deviance: 3958.4  on 47  degrees of freedom
## Residual deviance:  387.2  on 37  degrees of freedom

## The residual deviance is 10 times larger than the df, that there is very
## clear evidence that there is overdispersion.

par(mfrow=c(2,4))
plot(DaR1.glm1)
plot.glmSim(DaR1.glm1, SEED=184)
## There are very, very much too influential points because there are at least
## 4 observations with a Cook's ditance clearly larger than 1. Three of them
## are caused by too large leverage.
## Because of the too large overdispersion all "structures" are outsice the
## stochastic fluctuation. Hence there is clear evidence that the model does
## not describe the data adequately.


## You can find a better model on the slides of Week 8, where we finally
## dropped 5 observations.


## 1d Transaction Data
##    ~~~~~~~~~~~~~~~~
transa <- read.table("Data/transactions.dat", header=T)
str(transa)
transa.glmI  <- glm(Time ~ Type1 + Type2, family=Gamma(link=identity),
		data=transa)
par(mfrow=c(2,4))
plot(transa.glmI)
plot.glmSim(transa.glmI, SEED=184)

## Residual plot: The smoother is almost a horizontal straight line. Because it
##   is within the stochastic fluctuation, there there is no  evidence that the
##   expectation is incorrectly specified.
## Location-scale plot: The smoother shows a decreasing trend. Since
##   it is outside the stochastic fluctuation, there is evidence that the
##   variance is incorrectly specified.
## Normal Q-Q plot: The points scatter quite nicely around a straight line and
##   within the stochstic flucuation expect at the right end. Hence, there is
##   a light evidence that there is something wrong with the distributional
##   assumption.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*3/nrow(transa) ## = 0.02298851
##   --> there are at least 5 leverage points but they do not harm

## Conclusion: Model might be not fully adequate. Because the variance is
## inadequately specified, the response may not be gamma distributed!




## 1e Nambeware Polishing Times
##    ~~~~~~~~~~~~~~~~~~~~~~~~~
nw <- read.table("Data/nambeware.txt", header=T)

nw.glm1 <- glm(Time ~ Diam + Type, family=Gamma(link=log), data=nw)
summary(nw.glm1)
par(mfrow=c(2,4))
plot(nw.glm1,
     panel=function(x,y) panel.smooth(x, y, iter=1, span=0.6))
plot.glmSim(nw.glm1, SEED=184,
            smoother=function(x,y) lowess(x, y, iter=1, f=0.6))
## Residual Plot: Since the red smoother is within the stochastic fluctuation
##   (gray spaghettis), there is no evidence that the expectation is
##    incorrectly specified.
## Normal Plot: Since the black points is within the stochastic fluctuation
##   (gray points), there is no evidence that there are outliers.
## Scale-location Plot: Since the red smoother is within the stochastic
##   fluctuation, there is no evidence that the variance is incorrectly
##   specified.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*6/nrow(nw) ## = 0.2033898  -
##   -> there are 2 leverage points but they do not harm

## Conclusion: Model might be adequate.


## ======================================================================
##
## Exercise 2
## ~~~~~~~~~~
Mine <- read.table("Data/mine.dat", header=T)
str(Mine)
summary(Mine)

## (a)
Mine.glm <- glm(Y ~ INB + EXTRP + sHeight + oTime, family=poisson, data=Mine)
summary(Mine.glm)
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept) -3.5930896  1.0256803  -3.503  0.00046 ***
## INB         -0.0014066  0.0008358  -1.683  0.09240 .
## EXTRP        0.0623458  0.0122862   5.074 3.89e-07 ***
## sHeight     -0.0020803  0.0050661  -0.411  0.68134
## oTime       -0.0308135  0.0162648  -1.894  0.05816 .
## ---
## (Dispersion parameter for poisson family taken to be 1)
##     Null deviance: 74.984  on 43  degrees of freedom
## Residual deviance: 37.856  on 39  degrees of freedom
## AIC: 144.13


## The response  Y_i is ~ Pois(lambda_i), independent with E(Y_i)=mu_i
## The explanatory variables are IND, EXTRP, sHeight and oTimes.
## A linear combination of them yield the linear predictor eta_i
## The canonical link, log(), is used: log(mu_i) = eta_i


## (b)
## Since the residual deviance is even smaller than the expectation of
## its corresponding chi-squared distribution with 39df we have no evidence
## that there is overdispersion.


## (c)
## 95% Wald confidence interval:
confint.default(Mine.glm)
##                    2.5 %        97.5 %
## (Intercept) -5.603385932 -1.5827932230
## INB         -0.003044757  0.0002315809
## EXTRP        0.038265248  0.0864262726
## sHeight     -0.012009801  0.0078491173
## oTime       -0.062691956  0.0010649705


## 95% profile confidence interval:
confint(Mine.glm)
## Waiting for profiling to be done...
##                   2.5 %        97.5 %
## (Intercept) -5.69525976 -1.6604283925
## INB         -0.00316198  0.0001348221
## EXTRP        0.03923837  0.0875324865
## sHeight     -0.01287548  0.0070791852
## oTime       -0.06418168 -0.0002029767

## There is a big difference in the coefficient oTime because in the Wald-type
## ci the coefficient is not significant different from 0 on the 5% levels
## (0 is within the ci) and in the profiled ci the coefficient is significantly
## different from 0!


## (d)
par(mfrow=c(2,4))
plot(Mine.glm)
plot.glmSim(Mine.glm, SEED=18417)
## Residual plot: Since the red smoother is partially outside the stochastic
##   fluctuation there is some evidence that the expectation is incorrectly
##   specified.
## Location-scale plot: The smoother is slightly decreasing on the r.h.s. Since
##   the red smoother is within the stochastic fluctuation there is no evidence
##   that the variance is incorrectly specified.
## Normal Q-Q plot: The points scatter more or less around a straight line and
##   are within the stochastic fluctuation. Hence there is no evidence for
##   outliers or some distortions of the distributional assumption.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*5/nrow(Mine) ## = 0.2272727
##   --> there are 5 leverage points but they are not too influential.

## Conclusion: There is weak evidence that the model does not adequately
##             represent the data. Therefore, try to improve the model and
##             achieve a better fit in the expected value.


## (e)
library(gam)
Mine.gam <- gam(Y ~ lo(INB) + lo(EXTRP, span=0.7) + lo(sHeight) + lo(oTime),
                family=poisson, data=Mine, bf.maxit=500)
## A different value for span with EXTRP is needed to avoid warning massages

summary(Mine.gam)
## Anova for Nonparametric Effects
##                       Npar Df Npar Chisq  P(Chi)
## (Intercept)
## lo(INB)                   4.4     6.7327 0.1887
## lo(EXTRP, span = 0.7)     1.4     4.1984 0.0691 .
## lo(sHeight)               4.2     2.2489 0.7150
## lo(oTime)                 3.4     0.3573 0.9683
## Accoding this output no transformations are need since all P-Values are
## larger than 0.05

par(mfrow=c(2,2))
plot(Mine.gam, se=TRUE, residuals=TRUE)

## It is very challenging to improve the model. Try now a step-by-step approch

## According to the summary output, the variable sHeight ist not needed.
## Hence:
Mine.gam1 <- gam(Y ~ lo(INB) + lo(EXTRP, span=0.7) + lo(oTime),
                family=poisson, data=Mine, bf.maxit=500)
summary(Mine.gam1)
par(mfrow=c(2,2))
plot(Mine.gam1, se=TRUE, residuals=TRUE)
## no new insights ....

## It seems that a log-transformation of INB could improve the fit as Tukey's
## first-aid transfromation suggests:
Mine$lINB <- log(Mine$INB)
Mine.gam2 <- gam(Y ~ lo(lINB) + lo(EXTRP, span=0.7) + lo(oTime),
                family=poisson, data=Mine, bf.maxit=500)
summary(Mine.gam2)
par(mfrow=c(2,2))
plot(Mine.gam2, se=TRUE, residuals=TRUE)
## log(INB) seems a useful transformation

## Next step: transforming EXTRP
## According to Tukey's first-aid transformation: logit
##                                                - but it has the wrong form
h <- Mine$EXTRP/100; Mine$tEXTRP <- log(h/(1-h))
Mine.gam3 <- gam(Y ~ lo(lINB) + lo(tEXTRP, span=0.7) + lo(oTime),
                family=poisson, data=Mine, bf.maxit=500)
summary(Mine.gam3)
par(mfrow=c(2,2))
plot(Mine.gam3, se=TRUE, residuals=TRUE)


## In a next attempt, I started to transform EXTRP by a hockey stick
## transformation:
## one can try to construct a smooth up-side down hockey stick:
## f4 <- function(x) {1-log(1+exp(7.5*(0.2-x)))}
## curve(f4, 0.5, 0.99, add=T, col="red")
## Mine$tEXTRP <- f4(Mine$EXTRP/100)
## Mine.gam4 <- gam(Y ~ lo(lINB) + lo(tEXTRP, span=0.7) + lo(oTime),
##                 family=poisson, data=Mine, bf.maxit=500)
## summary(Mine.gam2)
## par(mfrow=c(2,2))
## plot(Mine.gam4, se=TRUE, residuals=TRUE)
## ... but this a bit too fancy to reliable

## let's start with a simple hockey stick at the lower end:
table(Mine$EXTRP)
## 50 60 65 70 75 80 85 88 90
##  2  3  8  4  4  7  7  2  7

Mine$tEXTRP <- ifelse(Mine$EXTRP < 65, 65, Mine$EXTRP)
Mine.gam4 <- gam(Y ~ lo(lINB) + lo(tEXTRP) + lo(oTime),
                family=poisson, data=Mine, bf.maxit=500)
summary(Mine.gam2)
par(mfrow=c(2,2))
plot(Mine.gam4, se=TRUE, residuals=TRUE)

## Try a double hokey stick transformation with EXTRP
Mine$t2EXTRP <- ifelse(Mine$tEXTRP > 75, 75, Mine$tEXTRP)
## Since t2EXTRP has just three different values, gam cannot be applied.
## So fit a glm and run a residual analysis

Mine.glm2 <- glm(Y ~ lINB + t2EXTRP + oTime, family=poisson, data=Mine)
summary(Mine.glm2)
## t2EXTRP is significant, but the other two variables not!!
par(mfrow=c(2,4))
plot(Mine.glm2)
plot.glmSim(Mine.glm2, SEED=187)
## No evidence that any of the assumptions are violated


## variable selection with AIC
step(Mine.glm2)
## Y ~ lINB + t2EXTRP + oTime

## variable selection with BIC
step(Mine.glm2, k=log(nrow(Mine)))
## Y ~ lINB + t2EXTRP

## Fit the model resulted by BIC and run a residual analysis
Mine.glm3 <- glm(Y ~ lINB + t2EXTRP, family=poisson, data=Mine)
summary(Mine.glm3)

par(mfrow=c(2,4))
plot(Mine.glm3)
plot.glmSim(Mine.glm3, SEED=187)
## No evidence that any of the assumptions are violated



## (for Week 8): Could a rate model be applied as well?
## Because there are "0" in the variable oTime, it is not an adequate approach.


##
## ======================================================================
##
## Exercise 3
## ~~~~~~~~~~
source("../RFn_Plot-glmSim.R")
baby <- read.table("Data/baby.dat", header=T)

## (a)
baby.glm1 <- glm(Survival ~ Weight + Age + Apgar1 + Apgar5 + pH,
                 family=binomial, baby)
summary(baby.glm1) ## only two of the five variables have a significant
##                    contribution
par(mfrow=c(2,4), las=1)
plot(baby.glm1)
plot.glmSim(baby.glm1, SEED=1291)
## According to the Tukey-Anscombe plot, there is evidence that the linear
## predictor (or link) is misspecified because the red smoother is outside
## the stochastic fluctuation. The other three plots show no evidence that
## any other assumptions are violated.


## (b)
library("gam")
baby.gam1 <- gam(Survival ~ lo(Weight) + lo(Age) + lo(Apgar1) + lo(Apgar5)
                 + lo(pH), family=binomial, baby)
summary(baby.gam1) ## Weight and pH should be transformed

par(mfrow=c(2,3), las=1)
plot(baby.gam1, residuals=T, se=TRUE)

## To log-transform Weight is not satisfying (cf. the corresponding gam)
baby$lWeight <- log(baby$Weight)
baby.gam2 <- gam(Survival ~ lo(lWeight) + lo(Age) + lo(Apgar1) + lo(Apgar5)
                 + lo(pH), family=binomial, baby)
summary(baby.gam2) ## Weight (still) and pH should be transformed

par(mfrow=c(2,3), las=1)
plot(baby.gam2, residuals=T, se=TRUE)

## Hence, let's try
baby$tWeight <- ifelse(baby$Weight < 1000, 0, baby$Weight-1000)
baby$tpH <- ifelse(baby$pH < 7.27, 0, baby$pH-7.27)

## (c)
## fit the corresponding glm
baby.glm2 <- glm(Survival ~ Weight + tWeight + Age + Apgar1 + Apgar5
                 + pH + tpH, family=binomial, baby)
par(mfrow=c(2,4), las=1)
plot(baby.glm2)
plot.glmSim(baby.glm2, SEED=1291)
## The four plots show no evidence that any of the assumptions is violated.

## (d)
summary(baby.glm2)
## Since the coefficient of tWeight is minus the coefficient of Weight, we
## can replace Weight and tWeight by a hokey stick transformation:
baby$t2Weight <- ifelse(baby$Weight > 1000, 1000, baby$Weight)

## Since the coefficient of tpH is minus twice the coefficient of pH, we
## may replace tpH and pH by
baby$t2pH <- ifelse(baby$pH < 7.27, baby$pH-7.27, -(baby$pH-7.27))

baby.glm3 <- glm(Survival ~ t2Weight + Age + Apgar1 + Apgar5 + t2pH,
                 family=binomial, baby)
par(mfrow=c(2,4), las=1)
plot(baby.glm3)
plot.glmSim(baby.glm3, SEED=1356)
## The four plots show no evidence that any of the assumptions is violated.

## (e)
step(baby.glm3) ##  Survival ~ t2Weight + Age + Apgar1 + pH + t2pH,
step(baby.glm3, k=log(nrow(baby))) ## Survival ~ t2Weight + t2pH

baby.glm4 <- glm(Survival ~ t2Weight + t2pH, family=binomial, baby)
par(mfrow=c(2,4), las=1)
plot(baby.glm4)
plot.glmSim(baby.glm4, SEED=1356)
## The plots "residuals vs Fitted" and the "Scale-Location" show some evidence
## on the right hand side that the linear predictor and the variance may be
## mispecified.

baby.gam5 <- gam(Survival ~ lo(t2Weight) + lo(tpH), family=binomial, baby)
summary(baby.gam5) ## Weight and pH should be transformed

par(mfrow=c(2,3), las=1)
plot(baby.gam5, residuals=T, se=TRUE)

#
##
## ======================================================================
##
## Exercise 4
## ~~~~~~~~~~
eU <- read.table("Data/eUsage.dat", header=T)
str(eU)


## (a)
eU.lm <- lm(Y ~ x,  data=eU)
coef(eU.lm)
##  (Intercept)            x
## -0.831303660  0.003682843

source("RFn/RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(eU.lm)
plot.lmSim(eU.lm, SEED=1798)
## Residual plot: The smoother indicates some problems with the specification
##   of the expectation at the r.h.s., because it is outside the stochastic
##   fluctuation. But it is caused by one observation. This is (too) little
##   evidence that the expectation is incorrectly specified
## Location-scale plot: The smoother is steeply increasing on the l.h.s. and
##   later more moderately. On the r.h.s., it is outside the  stochastic
##   fluctuation. So there is no evidence that the variance is incorrectly
##   specified.
## Normal Q-Q plot: The points scatter within the stochastic fluctuation, hence
##    we have no evidence for outliers or some distortions of the
##    distributional assumption.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*2/nrow(Mine) ## = 0.09090909
##                --> there is one clear leverage points but it does not harm

## Conclusion: The model does not fit the data adequately.
## Possible remedy: Either log-transform the response or use a gamma model.




## (b)
eU.glm <- glm(Y ~ x, family=Gamma(link=identity), data=eU)
coef(eU.glm)
##  (Intercept)            x
## -0.767512719  0.003661987

## The coefficient estimates are very similar to those in part (a)


par(mfrow=c(2,4))
plot(eU.glm)
plot.glmSim(eU.glm, SEED=213)
## Residual plot: The smoother is within the stochastic fluctuation. Hence
##   there is no evidence that the expectation is incorrectly specified.
## Location-scale plot: The smoother is decreasing on the r.h.s.  Since the red
##   smoother is outside the stochastic fluctuation, there is some evidence that
##   the variance is incorrectly specified.
## Normal Q-Q plot: The points scatter nicely around a straight line and are
##   within the stochastic fluctuation. Hence there is no evidence for outliers
##   or some distortions of the distributional assumption.
## Residual against leverage: Since all Cook's distances are smaller than 1
##   there is no observation with a too large influence.
2*2/nrow(Mine) ## = 0.09090909
##                --> there are three clear leverage points but they do not harm


## Alternative link:
eU.glm2 <- glm(Y ~ x, family=Gamma(link=log), data=eU)
par(mfrow=c(2,4))
plot(eU.glm2)
plot.glmSim(eU.glm2, SEED=184)
## No, that's worse.


## (c)
library(gam)
eU.gam <- gam(Y ~ lo(x), family=Gamma(link=identity), data=eU)
par(mfrow=c(1,1))
plot(eU.gam, se=TRUE, residuals = TRUE)
## The estimated curve can be approximated well by a straight line. Thus there
## is no need for transforming the explanatory variable x.
summary(eU.gam)
## Anova for Nonparametric Effects
##             Npar Df Npar F  Pr(F)
## (Intercept)
## lo(x)           3.4 1.0314 0.3932


## (d)
## Wald (from the summary output)
summary(eU.glm)
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.7675127  0.1894508  -4.051 0.000174 ***
## x            0.0036620  0.0003701   9.895 1.85e-13 ***
## (Dispersion parameter for Gamma family taken to be 0.2810353)

h <- summary(eU.glm)$coefficients
c(h[2,1] - qnorm(0.975)*h[2,2], h[2,1] + qnorm(0.975)*h[2,2])
## [1] 0.002936609 0.004387364

## or with t distribution ("since the dispersion parameter is estimated"):
c(h[2,1] - qt(0.975, df=51)*h[2,2], h[2,1] + qt(0.975, df=51)*h[2,2])
## [1] 0.002918985 0.004404988

## the difference are very small, since qt(0.975, df=51) ~=  qnorm(0.975):
qt(0.975, df=51) - qnorm(0.975) ## = 0.04761979


## with deviances (i.e., profiling):
confint(eU.glm)
## Warning message: glm.fit: algorithm did not converge

## The algorithm needs more iterations within profiling; hence
eU.glm <- glm(Y ~ x, family=Gamma(link=identity), data=eU, maxit=100)
confint(eU.glm)
## Waiting for profiling to be done...
##                    2.5 %       97.5 %
## (Intercept) -1.075321127 -0.349845578
## x            0.002984377  0.004404863

## the 95% confidence interval is  [0.0030 0.0044].

## The differences between these three different types of confidence intervals
## is neglecting.


##
## (e)
## If the response is exponentially distributed then the dispersion parameter
## is fixed at 1: So we can test the null hypothesis phi=1 as in the Poisson
## and the binomial case

## From the summary output we have
## Residual deviance: 18.05  on 51  degrees of freedom

## Since the residual deviance is outside of the acceptance region
qchisq(c(0.025,0.975), 51) ## = 33.16179 72.61599
## the null hypothesis must be rejected on the 5% level. Hence the response
## cannot be exponentially distributed, it must be a gamma distribution.


#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* END *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

