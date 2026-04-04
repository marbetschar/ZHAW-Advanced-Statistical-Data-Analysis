library(graphics)
library(gam)
source("RFn_Plot-lmSim.R")

#' Plot histograms for all numeric columns in a data frame
#'
#' This function creates a grid of histograms, one for each numeric column.
#'
#' @param df A data.frame.
#' @return Invisibly returns NULL.
#' @examples
#' hist_df(mtcars)
lib.hist_df <- function(df) {
  num_cols <- sapply(df, is.numeric)
  df_num <- df[ , num_cols]
  
  n <- ncol(df_num)
  par(mfrow = n2mfrow(n))  # set grid layout
  
  for (xname in sort(names(df_num))) {
    hist(
      x = df_num[[xname]],
      main = paste('Histogram of', xname),
      xlab = xname)
  }
  
  par(mfrow=c(1,1))
}

lib.lmSim <- function(obj, which=c(1L:3L), rob=FALSE, SEED=NULL, Nsim=19) {
  par(mfrow=c(2,4))
  plot(obj)
  plot.lmSim(obj, which, rob, SEED, Nsim)
  par(mfrow=c(1,1))
}

lib.plot <- function(model, ...) {
  # Determine how many panels n to expect
  if (inherits(model, "Gam")) {
    # gam::Gam: count non-intercept terms
    tt <- terms(model)
    n  <- length(attr(tt, "term.labels"))
  } else {
    n <- 1
  }
  
  if (n < 1) n <- 1
  
  # Set layout and plot once
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  par(mfrow = grDevices::n2mfrow(n))
  plot(model, ...)
  par(mfrow = c(1, 1))
}