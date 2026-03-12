#
## Worksheet Week 4 - Solution
## ***************************
##
## Exercise 1
## ~~~~~~~~~
turb <- read.table("Data/turbines.dat", header=T)
str(turb)


## (a)
plot(turb$Hours, turb$Fissures/turb$Turbines)
## Does not show all the information given for each observation
## Still not appropriate
## The R function sunflowerplot is in one of the core packages.
sunflowerplot(turb$Hours, turb$Fissures, las=1)
sunflowerplot(turb$Hours, turb$Fissures/turb$Turbines, las=1)

## much better::
sunflowerplot(x=turb$Hours, y=turb$Fissures/turb$Turbines,
              number=turb$Turbines, las=1)
## or
symbols(x=turb$Hours, y=turb$Fissures/turb$Turbines,
		 circles=sqrt(turb$Turbines), inches=0.75/2.54)

##
## (b)
## Let Y_i be the number of wheels with fissures. Then
##  Y_i ~ independent Binomial(pi_i, #Turbines) with
##        log(pi_i/(1-pi_i)) = beta_0 + beta_1 * Hours

turb.glm <- glm(cbind(Fissures, Turbines-Fissures) ~ Hours,
				family=binomial, data=turb)
coef(turb.glm)
## (Intercept)        Hours
##   -3.923597 0.0009992372

## An incorrect way of using glm() !!!
turb.glm2 <- glm((Fissures/Turbines) ~ Hours,
				family=binomial, data=turb)
## Warnmeldung:
## In eval(family$initialize) : non-integer #successes in a binomial glm!
coef(turb.glm2)
##  (Intercept)        Hours
## -3.911595607  0.001011164
## similar to the correct solution but not identical!


##
## (c)
## log(pi_i/(1-pi_i)) =  -3.923597 + 0.0009992372*Hours
## Hence the chance of fissures (odds) on the wheel increased by a factor
## of exp(0.0009992372*100) = 1.105087 (or by 10.5%)  with every additional
## 100 hours of operation time.


##
## (d)
predict(turb.glm, type="response", newdata=data.frame(Hours=3000))
## = 0.2837603
## The estimated probability of a "defective" turbine wheel is 0.284 at
## 3'000 hours of operation time?


##
## (e)
turbN <- data.frame(Hours=seq(100, 10000, by=100))
y.p <- predict(turb.glm, newdata=turbN, type="response")
sunflowerplot(x=turb$Hours, y=turb$Fissures/turb$Turbines, las=1,
              number=turb$Turbines, xlim=c(0, 10000), ylim=c(0,1))
lines(turbN$Hours, y.p, col=3)

abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), lty=7, col=2)
## Estimated operation time at which xx% of the turbine wheels exhibits any
## fissures:
## xx=  10%,   30%,   50%,   70%,    90%
##    1'900, 3'200, 4'000, 4'500, 6'000
## these values are read from the graphic and are very crude readings.

## Note that the values for 70% and 90% are based on an extrapolation. Such an
## extrapolation just possible with a parametric model. However, the
## extrapolation is only reliable when the model is adequate!


## (f)
turb.Probit <- glm(cbind(Fissures, Turbines-Fissures) ~ Hours,
                   family=binomial(link=probit), data=turb)
coef(turb.Probit)
##   (Intercept)         Hours
## -2.2758074623  0.0005783211

turb.cll <- glm(cbind(Fissures, Turbines-Fissures) ~ Hours,
                family=binomial(link=cloglog), data=turb)
coef(turb.cll)
##   (Intercept)         Hours
## -3.6032798443  0.0008104936

## To compare the results, let's overlay the fitted lines:
y.p2 <- predict(turb.Probit, newdata=turbN, type="response")
y.p3 <- predict(turb.cll, newdata=turbN, type="response")
lines(turbN$Hours, y.p2, col="blue", lty=2)
lines(turbN$Hours, y.p3, col="magenta", lty=5)


## Comparing the coefficient estimates, the logit model and the
## complementary log-log model seem to be similar, the probit model has
## clearly different estimates.

## With respect to the fitted curves, the probit and the logit model are
## very similar, but the  complementary log-log model shows a clearly
## different curve. The comparission of the fitted curves is much more
## crutial than the comparission of the estimated coefficients.

## ======================================================================
##
## Exercise 2
## ~~~~~~~~~~
CD <- read.csv("Data/creditdata.csv", header = TRUE, stringsAsFactors=TRUE)
str(CD)

## Make additional factor variables (where the levels are coded by numbers)
CD$Account.Balance <- as.factor(CD$Account.Balance)
CD$Savings <- as.factor(CD$Savings)
summary(CD)


## (a)
CD.glm <- glm(Creditability ~ Duration + Account.Balance, data = CD,
                  family = binomial)
coef(CD.glm)
## (Intercept)      Duration Account.Balance2 Account.Balance3 Account.Balance4
##  0.77170556   -0.03681652       0.48338850       1.16268367       1.96724234

## The coefficient for Duration states that the odds of the loan being settled
## fall by the constant factor exp(-0.0369) = 0.9638 with each additional month
## of the loan term. The chance of the loan being settled therefore falls by
## 3.6\% with each additional month of duration.
## There are 3 coefficients for Account.Balance, as this is a categorical
## variable and was converted into indicator variables for modelling. The
## reference category is ‘1’, i.e. a negative account balance. All three
## coefficients are positive. The chance of settling a loan therefore increases
## with a positive account balance. If you look at the values of the
## Account.Balance2 and Account.Balance3 coefficients, you can see that the
## second is greater. This means that a higher account balance also has a
## higher probability of credit settlement. Account.Balance4 has the highest
## value, i.e. customers without an account with this bank have the highest
## probability of repaying the loan.


##
## (b)
## Generate predictions for all possible combinations:
N <- 200
xx <- seq(min(CD$Duration)-2,max(CD$Duration)+2,length=N)
h.ABlev <- levels(CD$Account.Balance)
PredProb <- matrix(NA, nrow=length(xx), ncol=length(h.ABlev))
for(i in 1:length(h.ABlev)){
  dat1 <- data.frame(Duration=xx, Account.Balance=rep(h.ABlev[i],N))
  PredProb[,i] <- predict(CD.glm, newdata=dat1, type="response")
}

## Graphic:
par(mar = c(4,4,1.5,1))
plot(CD$Duration, CD$Creditability, col=as.numeric(h.ABlev), las=1,
     ylab="Credit Worthiness", xlab="Duration of the Credit (in month)")
for(i in 1:ncol(PredProb))
  lines(xx, PredProb[,i], col=i)
legend(0,0.5, h.ABlev, col = 1:length(h.ABlev), lty = 1, bty = "n")
abline(h=0.5)


##
## (c)
## If the account balance is less than 0 euros, the term should be less than
## 20 months. For an account balance between 0 and 200 euros, the term should
## be less than 35 months. If the account balance is more than 200 euros, the
## term should be less than 52 months. If the customer does not have an
## account with the bank, then less than 75 months.


## ======================================================================
##
## Exercise 3
## ~~~~~~~~~~
bw <- read.table("Data/birth-weight.dat", header=T, stringsAsFactors=TRUE)
str(bw)

## (a)
par(mfrow=c(1,2), oma=c(2,0,0,0), las=1)
symbols(x=bw$weight, y=bw$Y/bw$m, circles=sqrt(bw$m), inches=0.5/2.54,
        xlab="Weight", ylab="Survival Probability")
sunflowerplot(x=bw$weight, y=bw$Y/bw$m, number=bw$m, ylim=c(0,1),
              xlab="Weight", ylab="Survival Probability")
mtext(side=1, text="Premature Birth Data", outer=TRUE)


par(mfrow=c(1,1), oma=c(0,0,0,0), las=1)
## Empirical logits: We should observe a linear relationship:
yel <- log((bw$Y+0.5)/(bw$m-bw$Y+0.5))
sunflowerplot(x=bw$weight, y=yel, number=bw$m, ylab="Empirical Logits")
## This does not look like a linear relationship!

## Let's try a first-aid transformation on weight
sunflowerplot(x=log(bw$weight), y=yel, number=bw$m,
              xlab="log(Weight)", ylab="Empirical Logits")

## that is much better
## Hence
bw$lWeight <- log(bw$weight)


##
## (b) least squares using empirical logits
##     (conceptionally not a sound approach because the empirical logits
##     are not normally distributed)
bw.elm <- lm(yel ~ lWeight, data=bw)

## display data and fit in the logitic scale:
sunflowerplot(x=bw$lWeight, y=yel, number=bw$m,
              xlab="log(Weight)", ylab="Empirical Logits")
abline(bw.elm, col=4, lwd=4, lty=6)
## looks fine!

## display data and fit in the response scale
sunflowerplot(x=bw$lWeight, y=bw$Y/bw$m, number=bw$m,
              xlab="log(Weight)", ylab="Survival Probability")
x <- seq(min(bw$lWeight), max(bw$lWeight), length=50)
mu.logit.p <- predict(bw.elm, newdata=data.frame(lWeight=x))
## back-transformation into the response scale:
lines(x, 1/(1+exp(-mu.logit.p)), col="blue", lwd=2, lty=6)




##
## (c)
## With GLM
bw.glm <- glm(cbind(Y, m-Y) ~ lWeight, family=binomial, data=bw)

## display data and both fits in the logitic scale:
sunflowerplot(x=bw$lWeight, y=yel, number=bw$m,
              xlab="log(Weight)", ylab="Empirical Logits")
abline(bw.elm, col="blue", lwd=4, lty=6)
abline(coef(bw.glm), col="magenta", lwd=4) ## glm fit
## Two almost paralell lines

## display data and both fits in the response scale
sunflowerplot(x=bw$lWeight, y=bw$Y/bw$m, number=bw$m,
              xlab="log(Weight)", ylab="Survival Probability")
x <- seq(min(bw$lWeight), max(bw$lWeight), length=50)
mu.logit.p <- predict(bw.elm, newdata=data.frame(lWeight=x))
lines(x, 1/(1+exp(-mu.logit.p)), col="blue", lwd=4, lty=6)
mu.glm.p <- predict(bw.glm, newdata=data.frame(lWeight=x), type="response")
lines(x, mu.glm.p, col="magenta", lwd=4)
## Difference in the fits is small.
## It might be that the glm fit fits better on the r.h.s.



### display data and both fits in a scatterplot of the response against Weights:
sunflowerplot(x=bw$weight, y=bw$Y/bw$m, nmber=bw$m,
              xlab="Weight", ylab="Survival Probability")
lines(exp(x), 1/(1+exp(-mu.logit.p)), col='blue', lwd=4, lty=6)
lines(exp(x), mu.glm.p, col="magenta", lwd=4) ## from GLM

#
##=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* END *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

