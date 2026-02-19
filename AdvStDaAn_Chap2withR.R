##
## AdvStDaAn - Chapter 2 with R
## ****************************
##

## Section 2.2
## ***********

library(datasets)

str(mtcars)
## 'data.frame':	32 obs. of  12 variables:
##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
##  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
##  $ disp: num  160 160 108 258 360 ...
##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
##  $ qsec: num  16.5 17 18.6 19.4 17 ...
##  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
##  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
##  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
##  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
##  $ fAM : Factor w/ 2 levels "automatic","manual": 2 2 2 1 1 1 1 1 1 1 ...


## Section 2.2
## ***********

## a) Initial Anlaysis
##    ****************
summary(mtcars)
pairs(mtcars)
par(mfrow=c(3,4))
plot(mpg ~ ., data=mtcars)

par(mfrow=c(1,1))
boxplot(mpg ~ am, xlab="am", ylab="mpg", data=mtcars)

##
## Data Preparation - Tukey's first aid transformations:
mtcars1 <- data.frame(lMPG=log(mtcars$mpg), wCyl=sqrt(mtcars$cyl),
                      lDisp=log(mtcars$disp), lHP=log(mtcars$hp),
                      drat=mtcars$drat, lWT=log(mtcars$wt),
                      qsec=mtcars$qsec, VS=mtcars$vs, AM=mtcars$am,
                      wGear=sqrt(mtcars$gear), wCarb=sqrt(mtcars$carb))


## --------------------------------------------------------------------------

## b) Multiple linear Regression: Case Study (1st Part)
##    **************************************

## (b-i) with the dataset mtcars1

## Least squares fit:
mtc1.lm1 <- lm(lMPG ~ lDisp + lHP + drat + lWT + qsec  + wCarb + wCyl  + VS
               + AM + wGear, data = mtcars1)
summary(mtc1.lm1)
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)  3.733511   1.325541   2.817   0.0103 *
## lDisp       -0.167383   0.179541  -0.932   0.3618
## lHP         -0.159416   0.134980  -1.181   0.2508
## drat         0.004172   0.074864   0.056   0.9561
## lWT         -0.377992   0.250297  -1.510   0.1459
## qsec         0.014235   0.033289   0.428   0.6733
## wCarb       -0.149562   0.115652  -1.293   0.2100
## wCyl         0.188301   0.217927   0.864   0.3973
## VS          -0.040222   0.088789  -0.453   0.6552
## AM          -0.054940   0.098564  -0.557   0.5831
## wGear        0.449734   0.264221   1.702   0.1035
## ---
## Residual standard error: 0.1134 on 21 degrees of freedom
## Multiple R-squared:  0.9018,	Adjusted R-squared:  0.8551
## F-statistic: 19.29 on 10 and 21 DF,  p-value: 2.097e-08

## Because there are many predictor variables with a non-significant
## coefficient including am, we should select variables which have a
## significant influence on the response.


## Residual and sensitivity analysis
source("RFn_Plot-lmSim.R")
par(mfrow=c(2,4))
plot(mtc1.lm1)
plot.lmSim(mtc1.lm1, SEED=4711)

2*11/nrow(mtcars1)  ## = 0.6875


## 2nd Week:
##
## Stepwise variable selection with AIC
mtc1.vs <- step(mtc1.lm1,
                scope=list(lower=~1,
                           upper=~lDisp + lHP + drat + lWT + qsec  + wCarb
                           + wCyl  + VS + AM + wGear))
## --> lMPG ~ lWT + lHP
coef(mtc1.vs)
## (Intercept)         lHP         lWT
##   4.8346929  -0.2553185  -0.5622822

## To summarize the stepwise variable selection process use
mtc1.vs$anova
##       Step Df     Deviance Resid. Df Resid. Dev       AIC
## 1          NA           NA        19  0.2663283 -127.2404
## 2   - fCyl  2 0.0066715271        21  0.2729998 -130.4486
## 3   - qsec  1 0.0005223985        22  0.2735222 -132.3875
## 4   - drat  1 0.0020805632        23  0.2756027 -134.1450
## 5    - lHP  1 0.0063945830        24  0.2819973 -135.4110
## 6    - fVS  1 0.0118809298        25  0.2938783 -136.0904
## 7    - fAM  1 0.0098653547        26  0.3037436 -137.0338
## 8  - fGear  2 0.0268440434        28  0.3305877 -138.3238
## 9    + lHP -1 0.0206446994        27  0.3099430 -138.3873
## 10 - wCarb  1 0.0054291280        28  0.3153721 -139.8316
## 11 - lDisp  1 0.0066352237        29  0.3220073 -141.1653


## The best regression model:
mtc1.lm2 <- lm(lMPG ~ lHP + lWT, data = mtcars1)

## Is it also adequate to describe the data?

##
## Residual and sensitivity analysis with mtc1.lm2
source("../../../Bs-StMo/MitR/RFn_Plot-lmSim.R")
par(mfrow=c(2,4), las=1)
plot(mtc1.lm2)
plot.lmSim(mtc1.lm2, SEED=4711)

2*3/nrow(mtcars1)  ## = 0.19 --> to identify leverage points


## Some plot to obtain more insights why "am" is not used
par(mfrow=c(1,3), las=1)
boxplot(resid(mtc1.lm2) ~ am, xlab="am", ylab="Residuals", data=mtcars)
boxplot(hp ~ am, data=mtcars, xlab="am", ylab="hp")
boxplot(wt ~ am, data=mtcars, xlab="am", ylab="wt")

aggregate(resid(mtc1.lm2) ~ AM, data=mtcars1, FUN=mean)
##         fAM resid(mtc2.lm2)
## 1 automatic    -0.001772201
## 2    manual     0.002590141


## (b-ii) with the dataset mtcars2
## Treat cyl and gears as categorical variables:
mtcars2 <- data.frame(lMPG=log(mtcars$mpg), fCyl=as.factor(mtcars$cyl),
                      lDisp=log(mtcars$disp), lHP=log(mtcars$hp),
                      drat=mtcars$drat, lWT=log(mtcars$wt),
                      qsec=mtcars$qsec,
                      fVS=factor(mtcars$vs, labels=c("v", "straight")),
                      fAM=factor(mtcars$am, labels = c("automatic", "manual")),
                      fGear=as.factor(mtcars$gear), wCarb=sqrt(mtcars$carb))
str(mtcars2)

levels(mtcars2$fCyl)
## [1] "4" "6" "8"  --> fCyl has three levels

mtc2.lm1 <- lm(lMPG ~ lDisp + lHP + drat + lWT + qsec  + wCarb + fCyl  + fVS
               + fAM + fGear, data = mtcars2)
summary(mtc2.lm1)
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)  4.692596   1.374603   3.414  0.00291 **
## lDisp       -0.147767   0.192292  -0.768  0.45167
## lHP         -0.105234   0.178671  -0.589  0.56281
## drat        -0.006039   0.091073  -0.066  0.94782
## lWT         -0.430819   0.285105  -1.511  0.14722
## qsec         0.013366   0.035253   0.379  0.70878
## wCarb       -0.174821   0.132516  -1.319  0.20276
## fCyl6        0.079618   0.115481   0.689  0.49888
## fCyl8        0.115996   0.204936   0.566  0.57801
## fVSstraight -0.065825   0.106624  -0.617  0.54433
## fAMmanual   -0.075435   0.112065  -0.673  0.50896
## fGear4       0.168072   0.135325   1.242  0.22935
## fGear5       0.231956   0.141129   1.644  0.11671
## ---
## Residual standard error: 0.1184 on 19 degrees of freedom
## Multiple R-squared:  0.9031,	Adjusted R-squared:  0.8419
## F-statistic: 14.76 on 12 and 19 DF,  p-value: 3.649e-07

## Variable selection
mtc2.vs <- step(mtc2.lm1,
                scope=list(lower='lMPG ~1',
                           upper=formula(mtc2.lm1)))
## final model:
## Step:  AIC=-141.17
## lMPG ~ lWT + lHP
## it is the same as in (b-i)

mtc2.vs$anova
##       Step Df     Deviance Resid. Df Resid. Dev       AIC
## 1          NA           NA        19  0.2663283 -127.2404
## 2   - fCyl  2 0.0066715271        21  0.2729998 -130.4486
## 3   - qsec  1 0.0005223985        22  0.2735222 -132.3875
## 4   - drat  1 0.0020805632        23  0.2756027 -134.1450
## 5    - lHP  1 0.0063945830        24  0.2819973 -135.4110
## 6    - fVS  1 0.0118809298        25  0.2938783 -136.0904
## 7    - fAM  1 0.0098653547        26  0.3037436 -137.0338
## 8  - fGear  2 0.0268440434        28  0.3305877 -138.3238
## 9    + lHP -1 0.0206446994        27  0.3099430 -138.3873
## 10 - wCarb  1 0.0054291280        28  0.3153721 -139.8316
## 11 - lDisp  1 0.0066352237        29  0.3220073 -141.1653


## ----------------------------------------------------------------

## c) Section 2.3 Discussing Some Published Results - 2nd Part of the Case Study
##    ***********

mtcars3 <- mtcars  ## later, we will add some additional variables

## (c-i) Hocking's best model
##
## Stepwise variable selection with original variables:
mtc3.lm0 <- lm(mpg ~ ., data=mtcars3)
step(mtc3.lm0, scope=list(lower='mpg ~ 1', upper=formula(mtc3.lm0)))
## final model:
## Step:  AIC=61.31
## mpg ~ wt + qsec + am


## This corresponds to  Hocking's best model:
mtc3.lm1 <- lm(mpg ~ am + wt + qsec, data=mtcars3)
summary(mtc3.lm1)

confint(mtc3.lm1, level=0.95)
##                   2.5 %    97.5 %
## (Intercept) -4.63829946 23.873860
## am           0.04573031  5.825944
## wt          -5.37333423 -2.459673
## qsec         0.63457320  1.817199

## In this model, am has an  important influence on the response!

## Residual analysis of mtc3.lm1
par(mfrow=c(2,4), las=1)
plot(mtc3.lm1, cex.caption=0.9)
plot.lmSim(mtc3.lm1, SEED=4711)
## But the model does not adequately describe the data.


## Multicollinearity?
## variance inflaction factor of the mtc3.lm0 model
library(car)
vif(mtc3.lm0)
##       cyl      disp        hp      drat        wt      qsec        vs
## 15.373833 21.620241  9.832037  3.374620 15.164887  7.527958  4.965873
##       am       gear      carb
## 4.648487   5.357452  7.908747

## there are some multicollinearity issues because there are some VIFs > 10

vif(mtc3.lm1)
##       am       wt     qsec
## 2.541437 2.482952 1.364339
## seems fine (--> no multicollinearity issue anymore)



##
## (c-ii) Henderson and Velleman's best model:
##
## They argued for using gpm (gallon per 100 miles) as response:
mtcars3$gpm <- 1/mtcars3$mpg*100

## because of collinearity issues between wt and hp, they suggested
mtcars3$op <- mtcars3$hp / mtcars3$wt

## Their best model is
mtc3.lm2 <- lm(gpm ~ wt + op, data=mtcars3)
summary(mtc3.lm2)

## Residual and sensitivity analysis:
par(mfrow=c(2,4), las=1)
plot(mtc3.lm2, cex.caption=0.9)
plot.lmSim(mtc3.lm2, SEED=4711)
## for determining leverage points:
2*length(coef(mtc3.lm2))/nrow(mtcars3)

## This model also does not adequately describe the data.

## ----------------------------------------------------------------
##
## d) 2.4 Prediction
##        **********
##
## Repeated for convenience:
mtc1.lm2 <- lm(lMPG ~ lHP + lWT, data = mtcars1)
mtc3.lm1 <- lm(mpg ~ am + wt + qsec, data=mtcars3)
mtc3.lm2 <- lm(gpm ~ wt + op, data=mtcars3)

##
## (d-i) Prediction Peugeot 694 SL; mpg=16.2
mtc2.new <- data.frame(lWT=log(3.410), lHP=log(133))
## mtc2.lm2 <- lm(lMPG ~ lHP + lWT, data = mtcars2)
peu.lp <- predict(mtc1.lm2, newdata=mtc2.new)
exp(peu.lp)  ## = 18.10771
exp(peu.lp +  summary(mtc1.lm2)$sigma^2/2)  ## = 18.20852

peu.lp <- predict(mtc1.lm2, newdata=mtc2.new, interval="prediction", level=0.95)
exp(peu.lp)
##        fit      lwr      upr
## 1 18.10771 14.53761 22.55453
##
## the observed value of 16.2 mpg is within the perdiction interval

## with correction
exp(peu.lp[1] +  summary(mtc1.lm2)$sigma^2/2)
## [1] 18.20852



## Using Henderson and Velleman's best model:
## mtc3.lm2 <- lm(gpm ~ wt + op, data=mtcars3), where gpm = 1/mpg*100

## op = hp / wt
133 / 3.410   # = 39.00293
predict(mtc3.lm2, newdata=data.frame(wt=3.41, op=39.00),
         interval="confidence", level=0.95)
##        fit      lwr      upr
## 1 5.554472 5.292003 5.816942

## Prediction interval in the original scale:
1/predict(mtc3.lm2, newdata=data.frame(wt=3.41, op=39.0),
            interval="prediction", level=0.95)*100
##        fit      lwr      upr
## 1 18.00351 23.94141 14.42568

## compared to the results based on "our best model"
    ##        fit      lwr      upr
    ## 1 18.10771 14.53761 22.55453
## there is not mutch difference. There length of the prediction interval is
## slightly larger



## (d-ii) Leave-one-out validation of the three models
##       ********************************************
##

## My own function to calculate the PRESS residuals:
f.PRESSres <- function(obj){
    resid(obj)/(1-lm.influence(obj)$hat)}
## My own function to calculate the leave-one-out prediction:
f.pLOO <- function(y,obj){
    y - resid(obj)/(1-lm.influence(obj)$hat)}

## PRESS for model mtc2.lm2
mtc1.lm2.looP <-  f.pLOO(y=mtcars1$lMPG, mtc1.lm2)
mtc1.lm2.looPo <- exp(mtc1.lm2.looP + summary(mtc1.lm2)$sigma^2/2)
PRESSres1 <- mtcars$mpg - mtc1.lm2.looPo
sum(PRESSres1^2)  ## = 160.9463

## PRESS for model mtc3.lm1
PRESSres2 <- f.PRESSres(mtc3.lm1)
sum(PRESSres2^2) ## = 231.3035

## PRESS for model mtc3.lm2
mtc3.lm2.looP <-  f.pLOO(y=mtcars3$gpm, mtc3.lm2)
mtc3.lm2.looPo <- 1/mtc3.lm2.looP*100
PRESSres3 <- mtcars$mpg - mtc3.lm2.looPo
sum(PRESSres3^2)  ## =  203.2456

## Boxplots of the PRESS residuals for the three models
h.pErr <- data.frame(pErr=c(PRESSres1, PRESSres2, PRESSres3),
                    Model=paste("M", rep(1:3,rep(nrow(mtcars),3)), sep=""))
boxplot(pErr ~ Model, data=h.pErr, col="lightgreen")

aggregate(pErr ~ Model, data=h.pErr, FUN=mean)
##   Model         pErr
## 1    M1 -0.035220261
## 2    M2 -0.005329216
## 3    M3  0.091915206


## =*=*=*=*=*=*=*=*=*=*=*=* END =*=*=*=*=*=*=*=*=*=*=*=*
