plot.glmSim <- function(obj, which=c(1L:3L), rob=FALSE, SEED=NULL, Nsim=19,
                        smoother=function(x,y) lowess(x, y, f=2/3, iter=0),
                        singular.ok=TRUE)
{
    ## rkst, Version Oct/27/2023 Anpassungen wegen half normal plot
    ## und quasipoisson und quasibinomial Modell
    if(!inherits(obj, "glm")) stop("use only with \"glm\" objects")
    dropInf <- function(x, h) {
        if (any(isInf <- h >= 1)) {
          warning(gettextf("not plotting observations with leverage one:\n  %s",
                           paste(which(isInf), collapse = ", ")), call. = FALSE,
                  domain = NA)
          x[isInf] <- NaN
        }
        x
    }
    show <- rep(FALSE, 3)
    show[which] <- TRUE

    X <- model.matrix(obj)
    x.n <- nrow(X)
    etaH <- predict(obj, type="link")
    ## w <- weights(obj)
    ## if(is.null(w)) w <- rep(1, nrow(X))
    pWeights <- weights(obj, type="prior")
    binaryResp <- (family(obj)$family == "binomial") & any(pWeights==1)

    phi <- summary(obj)$dispersion
    OffSet <- obj$offset
    Family <- family(obj)
    Control <- obj$control
    muH <- predict(obj, type="response")

    y.sim <- switch(obj$family$family,
                    "binomial" = {function(){
                        rbinom(x.n,size=pWeights, prob=muH)/pWeights}},
                    "gaussian" = function() rnorm(x.n,muH,sqrt(phi)),
                    "Gamma" = {function()
                        rgamma(x.n, shape=1/phi, scale= muH*phi)},
                    "inverse.gaussian" = {
                        require(statmod)
                        warning("under construction")
                        function() rinvgauss(x.n, mean=muH,
                                                dispersion=phi)},
                    "poisson" = function() rpois(x.n, lambda=muH),
                    "quasibinomial" = {function(){
                        rbinom(x.n,size=pWeights, prob=muH)/pWeights}},
                    "quasipoisson" = function() rpois(x.n, lambda=muH),
                    stop("not (yet) implemented"))

    ## Tukey-Anscombe Plot
    if(show[1L]){
        res <- residuals.glm(obj, type="pearson")
        ylim <- extendrange(r=range(res, na.rm = TRUE), f = 0.08)
        plot(etaH, res, xlab="Fitted Linear Predictor",
             ylab = "Pearson Residuals", main="", ylim=ylim, type="n")
        abline(h=0, lty=3) ## , col = "gray"
        lines(smoother(etaH, res), lwd=1.5, col="red")

        if(!is.null(SEED)) set.seed(SEED)
        for(i in 1:Nsim){
            FIT <- glm.fit(x=X, y=y.sim(), weights=pWeights, start=coef(obj),
                           offset=OffSet, family=Family, control=Control,
                           intercept=FALSE, singular.ok=singular.ok)
            ## class(FIT) <- c(FIT$class, c("glm", "lm"))
            ## lines(lowess(predict(FIT), resid(FIT), f=2/3, iter=3),col="grey")
            h.r <- residuals.glm(FIT, type="pearson")
            if(obj$family$family %in% c("quasibinomial","quasipoisson"))
                    h.r <- h.r*sqrt(phi)

            lines(smoother(predict.glm(FIT), h.r), col="grey")
        }
         lines(smoother(etaH, res), lwd=1.5, col="red")
   }
    if(any(show[2L:3L])){
        if(getRversion() < "4.3-0"){
            f.stdres <- function(glmFit, pW=pWeights, type="pearson"){
                r <- residuals.glm(glmFit, type="pearson")
                hPhi <- summary.glm(glmFit)$dispersion
                hHii <- lm.influence(glmFit, do.coef=FALSE)$hat
                ## Be careful, there are problems when hii==1
                sr <- r*sqrt(pW/(hPhi*(1 - hHii)))
                sr[hHii >= 1] <- NaN
                sr
            }

        } else{## wegen Gewichtung pW, die hier nicht vorkommt
            f.stdres <- function(glmFit, pW=pWeights, type="pearson") {
                r <- residuals.glm(glmFit, type=type)
                hPhi <- summary.glm(glmFit)$dispersion
                hHii <- lm.influence(glmFit, do.coef=FALSE)$hat
                ## Be careful, there are problems when hii==1
                sr <- r*sqrt(1/(hPhi*(1 - hHii)))
                sr[hHii >= 1] <- NaN
                sr
            }
        }
    }
    ##
    if(show[2L]){
        if(getRversion() < "4.3-0"){ # with normal plot
            SIM <- matrix(NaN, ncol=Nsim, nrow=x.n)
            if(Nsim>0){
                if(!is.null(SEED)) set.seed(SEED)
                for(i in 1: Nsim){
                    FIT <- glm.fit(x=X, y=y.sim(), weights=pWeights,
                                   start=coef(obj), offset=OffSet,
                                   family=Family, control=Control,
                                   intercept=FALSE,
                                   singular.ok=singular.ok)

                    SIM[,i] <- sort(f.stdres(FIT))
                }
            }
            RQQN <- qqnorm(f.stdres(obj), plot.it=FALSE)
            ylim <- range(c(RQQN$y, SIM), finite=TRUE)
            plot(range(RQQN$x, finite=TRUE), ylim, type="n",
                 xlab = "Theoretical Quantiles",
                 ylab = "Std. Pearson Resid.")
            if(Nsim>0){
                points(rep(sort(RQQN$x),Nsim), as.vector(SIM), col="gray")
                }
            points(RQQN$x, RQQN$y, lwd=2)
            ##
        } else{ ## half-normal plot
            qhalfnorm <- function(p) qnorm((p + 1)/2)
            qqhalfnorm <- function(y, xlab = "Theoretical Quantiles",
                                   add=FALSE, col="gray", ...) {
                    if (has.na <- any(ina <- is.na(y))) {
                        yN <- y
                        y <- y[!ina]
                    }
                    if (0 == (n <- length(y)))
                        stop("y is empty or has only NAs")
                    x <- qhalfnorm(ppoints(n))[order(order(y))]
                    if (has.na) {
                        y <- x
                        x <- rep.int(NA_real_, length(ina))
                        x[!ina] <- y
                        y <- yN
                    }
                    if(add) points(x, y, col=col, ...)
                    else plot(x, y, xlab = xlab, lwd=2, ...)
                    invisible(list(x = x, y = y))
            }
            ##
            SIM <- matrix(NaN, ncol=Nsim, nrow=x.n)
            if(Nsim>0){
                if(!is.null(SEED)) set.seed(SEED)
                for(i in 1: Nsim){
                    FIT <- glm.fit(x=X, y=y.sim(), weights=pWeights,
                                   start=coef(obj), offset=OffSet,
                                   family=Family, control=Control,
                                   intercept=FALSE,
                                   singular.ok=singular.ok)
                    SIM[,i] <- abs(f.stdres(FIT, type="deviance"))
                }
            }
            absSDR <- abs(f.stdres(obj, type="deviance"))
            ylim <- c(0,max(c(absSDR, SIM), na.rm=TRUE)*1.075)
            yl <- as.expression(substitute(abs(YL),
                                  list(YL = as.name("Std. Deviance resid."))))
            qqhalfnorm(absSDR, main="", ylab=yl, ylim = ylim)
            if(Nsim>0){
                for(i in 1: Nsim)  qqhalfnorm(SIM[,i], add=TRUE)
            }
            qqhalfnorm(absSDR, add=TRUE, col=par("fg"), lwd=2)
        }
    }
    ## Scale-Location Plot
    if(show[3L]){
        sqrtabsR <- sqrt(abs(f.stdres(obj)))
        ok <- is.finite(sqrtabsR)
        ylim <- c(0, max(sqrtabsR[ok]))
        yl <- as.expression(substitute(sqrt(abs(YL)),
                                 list(YL=as.name("Std. Pearson Resid."))))
        plot(etaH, sqrtabsR, xlab="Fitted Linear Predictor", ylab=yl, main="",
             ylim=ylim, type="n")
        lines(smoother(etaH[ok], sqrtabsR[ok]), lwd=1.5, col="red")
        if(!is.null(SEED)) set.seed(SEED)
        for(i in 1:Nsim){
            FIT <- glm.fit(x=X, y=y.sim(), weights=pWeights,
                           start=coef(obj), offset=OffSet, family=Family,
                           control=Control, intercept=FALSE,
                           singular.ok=singular.ok)
            class(FIT) <- c(FIT$class, c("glm", "lm"))

            h.sasRes  <- sqrt(abs(f.stdres(FIT)))
            ok <- is.finite(h.sasRes)
            lines(smoother(predict(FIT)[ok], h.sasRes[ok]), col="grey")
        }
         lines(smoother(etaH[ok], sqrtabsR[ok]), lwd=1.5, col="red")
    }
    invisible()
}
