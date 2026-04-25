library(graphics)
library(gam)
library(gRain)
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

lib.cptable <- function(vpar, levels = NULL, values = NULL,
                            normalize = TRUE, smooth = 0) {
  if (is.null(values)) {
    stop("'values' must be provided.")
  }
  
  if (!is.numeric(values)) {
    stop("'values' must be numeric.")
  }
  
  if (any(is.na(values))) {
    stop("'values' contains NA.")
  }
  
  if (any(values < 0 | values > 1)) {
    stop("'values' must be in [0, 1].")
  }
  
  vars <- if (inherits(vpar, "formula")) {
    all.vars(vpar)
  } else if (is.character(vpar)) {
    vpar
  } else {
    stop("'vpar' must be a formula or character vector.")
  }
  
  if (length(vars) == 0) {
    stop("Could not infer variables from 'vpar'.")
  }
  
  if (is.null(levels)) {
    stop("'levels' must be provided.")
  }
  
  if (is.list(levels)) {
    missing_vars <- setdiff(vars, names(levels))
    if (length(missing_vars) > 0) {
      stop(sprintf(
        "'levels' is missing definitions for: %s",
        paste(missing_vars, collapse = ", ")
      ))
    }
    dims <- vapply(vars, function(v) length(levels[[v]]), integer(1))
  } else {
    if (!is.atomic(levels)) {
      stop("'levels' must be a vector or a named list.")
    }
    dims <- rep(length(levels), length(vars))
  }
  
  expected_len <- prod(dims)
  if (length(values) != expected_len) {
    stop(sprintf(
      "'values' has length %d, but expected %d for variables %s.",
      length(values), expected_len, paste(vars, collapse = ", ")
    ))
  }
  
  child_states <- dims[1]
  
  if (length(vars) == 1) {
    s <- sum(values)
    if (abs(s - 1) > 1e-8) {
      stop(sprintf(
        "Root CPT is invalid: probabilities sum to %.6f, not 1.",
        s
      ))
    }
  } else {
    m <- matrix(values, nrow = child_states)
    cs <- colSums(m)
    bad <- which(abs(cs - 1) > 1e-8)
    
    if (length(bad) > 0) {
      stop(sprintf(
        "Conditional CPT is invalid: block(s) %s sum to %s, not 1.",
        paste(bad, collapse = ", "),
        paste(sprintf("%.6f", cs[bad]), collapse = ", ")
      ))
    }
  }
  
  gRain::cptable(
    vpar = vpar,
    levels = levels,
    values = values,
    normalize = normalize,
    smooth = smooth
  )
}