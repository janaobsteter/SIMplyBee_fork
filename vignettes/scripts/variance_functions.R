# Shared helper functions for variance calculations vignette

# cor2cov: convert correlation matrix (or vector) and variances to covariance matrix
cor2cov <- function(corr, var) {
  corr <- as.matrix(corr)
  var  <- as.numeric(var)
  sd <- sqrt(var)
  cov <- corr * (sd %o% sd)
  return(cov)
}

# mapIndToColonyVar: map individual-level variances/covariances to colony-level
mapIndToColonyVar <- function(varA_q, varA_w, corA_qw, 
                              varE_q, varE_w, corE_qw,
                              nW, nF, nDPQ, workersFUN = "sum") {
  
  if (!workersFUN %in% c("sum", "mean")) {
    stop("Invalid value for workersFUN. We currently only support either 'sum' or 'mean'.")
  }

  # Pair counts
  n_SS <- (nW * nW / nF) - nW
  n_FS <- (nW * nW / nDPQ) -  (nW * nW / nF)
  n_HS <- (nW * nW / nDPQ) * (nDPQ - 1)

  if (workersFUN == "mean") {
    B1 = 1 / nW * varA_w
  } else if (workersFUN == "sum") {
    B1 = nW * varA_w
  }

  B2_ss <- n_SS * 0.75 * varA_w
  B2_fs <- n_FS * 0.50 * varA_w
  B2_hs <- n_HS * 0.25 * varA_w

  if (workersFUN == "mean") {
    varA_wbar = B1 + 1/nW^2 * (B2_ss + B2_fs + B2_hs)
  } else if (workersFUN == "sum") {
    varA_wbar = B1 + B2_ss + B2_fs + B2_hs
  }

  covA_qw = corA_qw * sqrt(varA_q) * sqrt(varA_w)

  if (workersFUN == "mean") {
    covA_qwbar <- covA_qw
  } else if (workersFUN == "sum") {
    covA_qwbar <- nW * covA_qw
  }
  corA_qwbar <- covA_qwbar / (sqrt(varA_q) * sqrt(varA_wbar))

  varA_c <- varA_q + varA_wbar + 2*covA_qwbar

  # Environmental part
  if (workersFUN == "mean") {
    varE_wbar = 1 / nW * varE_w
  } else if (workersFUN == "sum") {
    varE_wbar = nW * varE_w
  }

  covE_qw = corE_qw * sqrt(varE_q) * sqrt(varE_w)

  if (workersFUN == "mean") {
    covE_qwbar <- covE_qw
  } else if (workersFUN == "sum") {
    covE_qwbar <- nW * covE_qw
  }
  corE_qwbar <- covE_qwbar / (sqrt(varE_q) * sqrt(varE_wbar))

  varE_c <- varE_q + varE_wbar + 2*covE_qwbar

  return(list(varA_q = varA_q, varA_wbar = varA_wbar, covA_qwbar = covA_qwbar, corA_qwbar = corA_qwbar, varA_c = varA_c,
              varE_q = varE_q, varE_wbar = varE_wbar, covE_qwbar = covE_qwbar, corE_qwbar = corE_qwbar, varE_c = varE_c))
}

# mapColonyToIndVar: map colony-level variance components back to individual-level
mapColonyToIndVar <- function(varA_q,
                              varA_wbar,
                              corA_qwbar,
                              varE_q,
                              varE_wbar,
                              corE_qwbar,
                              nW,
                              nF,
                              nDPQ,
                              workersFUN = "sum") {
  
  if (!workersFUN %in% c("sum", "mean")) {
    stop("Invalid value for workersFUN. We currently only support either 'sum' or 'mean'.")
  }
  
  covA_qwbar <- corA_qwbar * sqrt(varA_q) * sqrt(varA_wbar)

  if (workersFUN == "sum") {
    covA_qw <- covA_qwbar / nW 
  } else if (workersFUN == "mean") {
    covA_qw <- covA_qwbar
  }

  n_SS <- (nW * nW / nF) - nW
  n_FS <- (nW * nW / nDPQ) - (nW * nW / nF)
  n_HS <- (nW * nW / nDPQ) * (nDPQ - 1)

  if (workersFUN == "sum") {
    K <- nW +
         n_SS * 0.75 +
         n_FS * 0.50 +
         n_HS * 0.25
  } else if (workersFUN == "mean") {
    K <- 1 / nW + ((n_SS * 0.75 + 
                    n_FS * 0.50 + 
                    n_HS * 0.25) / nW^2)
  }

  varA_w <- varA_wbar / K

  corA_qw <- covA_qw / (sqrt(varA_q) * sqrt(varA_w))

  # Environmental part
  covE_qwbar <- corE_qwbar * sqrt(varE_q) * sqrt(varE_wbar)

  if (workersFUN == "sum") {
    varE_w <- varE_wbar / nW
    covE_qw <- covE_qwbar / nW
  } else if (workersFUN == "mean") {
    varE_w <- varE_wbar * nW
    covE_qw <- covE_qwbar
  }

  corE_qw <- covE_qw / (sqrt(varE_q) * sqrt(varE_w))

  return(list(varA_q = varA_q, varA_wbar = varA_wbar, varA_w = varA_w, 
              covA_qwbar = covA_qwbar, covA_qw = covA_qw, corA_qw = corA_qw,
              varE_q = varE_q, varE_wbar = varE_wbar, varE_w = varE_w,
              covE_qwbar = covE_qwbar, covE_qw = covE_qw, corE_qw = corE_qw))
}
