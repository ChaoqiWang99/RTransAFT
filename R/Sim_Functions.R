#' Generate Simulated Data with Five Covariates
#'
#' Generates primary and auxiliary datasets with five covariates under different simulation settings.
#'
#' @param nprim Integer. Sample size for primary data. Default is 300.
#' @param naux Integer. Sample size for auxiliary data. Default is 1500.
#' @param setting Integer from 1 to 4. Specifies simulation setting.
#' @param Dist Character. Distribution of the third covariate: "Norm" (normal) or "Unif" (uniform).
#'
#' @return A list with elements:
#'   \item{primData}{Data frame containing primary dataset.}
#'   \item{auxData}{Data frame containing auxiliary dataset.}
#'
#' @export
GenData_FiveCov <- function(nprim = 300,
                            naux = 1500, 
                            setting = 1,
                            Dist = "Norm") {

  X1_p = rnorm(nprim, 0, 1)
  X2_p = rbinom(nprim, size = 1, prob = 0.5)
  
  X1_a = rnorm(naux, 0, 1)
  X2_a = rbinom(naux, size = 1, prob = 0.5)
  
  if (Dist == "Norm") {
    X3_p = rnorm(nprim, 0, 1)
    X3_a = rnorm(naux, 0, 1)
  } else if (Dist == "Unif") {
    X3_p = runif(nprim, 0, 2)
    X3_a = runif(naux, 0, 1)
  }
  
  X4_p = rnorm(nprim, 0, 1)
  X4_a = rnorm(naux, 0, 1)
  
  X5_p = rnorm(nprim, 0, 1)
  X5_a = rnorm(naux, 0, 1)

  b1_p = 2
  b2_p = 1.5
  b3_p = 1
  b4_p = 0.5
  b5_p = -0.5
  
  XB_p = cbind(X1_p, X2_p, X3_p, X4_p, X5_p) %*% c(b1_p, b2_p, b3_p, b4_p, b5_p)
  eps_m_p = eps_m_a = 0
  eps_s_p = 0.5
  eps_p = rnorm(nprim, mean = eps_m_p, sd = eps_s_p)
  
  if(setting %in% c(1,2,3)) {
    b1_a = b1_p
    b2_a = b2_p
    b3_a = b3_p
    b4_a = b4_p
    b5_a = b5_p
  } else if(setting %in% c(4)) {
    b1_a = b1_p
    b2_a = b2_p
    b3_a = b3_p
    b4_a = b4_p-0.2
    b5_a = b5_p+0.2
  }
  
  if(setting %in% c(1,4)) {
    XB_a = cbind(X1_a, X2_a, X3_a, X4_a, X5_a) %*% c(b1_a, b2_a, b3_a, b4_a, b5_a)
    eps_m_a = eps_m_p = 0
    eps_s_a = eps_s_p = 0.5
    eps_a = rnorm(naux, mean = eps_m_a, sd = eps_s_a)
  } else if(setting %in% c(2)) {
    XB_a = cbind(X1_a, X2_a, X3_a, X4_a, X5_a) %*% c(b1_a, b2_a, b3_a, b4_a, b5_a)
    eps_m_a = eps_m_p
    eps_s_a = 0.25
    eps_a = rnorm(naux, mean = eps_m_a, sd = eps_s_a)
  } else if(setting %in% c(3)) {
    XB_a = cbind(X1_a, X2_a, X3_a, X4_a, X5_a) %*% c(b1_a, b2_a, b3_a, b4_a, b5_a)
    eps_m_a = eps_m_p
    eps_s_a = 1
    eps_a = rnorm(naux, mean = eps_m_a, sd = eps_s_a)
  }
  
  T_p = XB_p + eps_p
  C_p = log(runif(nprim, 0, 0.95))
  survTime_p = ifelse(T_p <= C_p, T_p, C_p)
  event_p = ifelse(T_p <= C_p, 1, 0)
  
  T_a = XB_a + eps_a
  C_a = log(runif(naux, 0, 150))
  survTime_a = ifelse(T_a <= C_a, T_a, C_a)
  event_a = ifelse(T_a <= C_a, 1, 0)
  
  primData = data.frame(X1 = X1_p,
                        X2 = X2_p,
                        X3 = X3_p,
                        X4 = X4_p,
                        X5 = X5_p,
                        time = survTime_p,
                        delta = event_p)
  auxData = data.frame(X1 = X1_a,
                       X2 = X2_a,
                       X3 = X3_a,
                       X4 = X4_a,
                       X5 = X5_a,
                       time = survTime_a,
                       delta = event_a)
  
  return(list(primData = primData,
              auxData = auxData))
}

#' Generate Simulated Data with Outliers in X1
#'
#' Same as `GenData_FiveCov` but adds outliers in X1 for the primary dataset.
#'
#' @inheritParams GenData_FiveCov
#'
#' @return A list with primary and auxiliary datasets including outliers.
#' @export
GenData_FiveCov_outlier <- function(nprim = 300,
                            naux = 1500, 
                            setting = 1,
                            Dist = "Norm") {
  
  X1_p = rnorm(nprim, 0, 1)
  idx_outlier <- sample(1:nprim, size = floor(0.05 * nprim))
  X1_p[idx_outlier] <- X1_p[idx_outlier] + 5

  X2_p = rbinom(nprim, size = 1, prob = 0.5)
  
  X1_a = rnorm(naux, 0, 1)
  X2_a = rbinom(naux, size = 1, prob = 0.5)
  
  if (Dist == "Norm") {
    X3_p = rnorm(nprim, 0, 1)
    X3_a = rnorm(naux, 0, 1)
  } else if (Dist == "Unif") {
    X3_p = runif(nprim, 0, 2)
    X3_a = runif(naux, 0, 1)
  }
  
  X4_p = rnorm(nprim, 0, 1)
  X4_a = rnorm(naux, 0, 1)
  
  X5_p = rnorm(nprim, 0, 1)
  X5_a = rnorm(naux, 0, 1)

  b1_p = 2
  b2_p = 1.5
  b3_p = 1
  b4_p = 0.5
  b5_p = -0.5
  
  XB_p = cbind(X1_p, X2_p, X3_p, X4_p, X5_p) %*% c(b1_p, b2_p, b3_p, b4_p, b5_p)
  eps_m_p = eps_m_a = 0
  eps_s_p = 0.5
  eps_p = rnorm(nprim, mean = eps_m_p, sd = eps_s_p)
  
  if(setting %in% c(1,2,3)) {
    b1_a = b1_p
    b2_a = b2_p
    b3_a = b3_p
    b4_a = b4_p
    b5_a = b5_p
  }else if(setting %in% c(4)) {
    b1_a = b1_p
    b2_a = b2_p
    b3_a = b3_p
    b4_a = b4_p-0.2
    b5_a = b5_p+0.2
  }
  
  if(setting %in% c(1,4)) {
    XB_a = cbind(X1_a, X2_a, X3_a, X4_a, X5_a) %*% c(b1_a, b2_a, b3_a, b4_a, b5_a)
    eps_m_a = eps_m_p = 0
    eps_s_a = eps_s_p = 0.5
    eps_a = rnorm(naux, mean = eps_m_a, sd = eps_s_a)
  } else if(setting %in% c(2)) {
    XB_a = cbind(X1_a, X2_a, X3_a, X4_a, X5_a) %*% c(b1_a, b2_a, b3_a, b4_a, b5_a)
    eps_m_a = eps_m_p
    eps_s_a = 0.25
    eps_a = rnorm(naux, mean = eps_m_a, sd = eps_s_a)
  } else if(setting %in% c(3)) {
    XB_a = cbind(X1_a, X2_a, X3_a, X4_a, X5_a) %*% c(b1_a, b2_a, b3_a, b4_a, b5_a)
    eps_m_a = eps_m_p
    eps_s_a = 1
    eps_a = rnorm(naux, mean = eps_m_a, sd = eps_s_a)
  }
  
  T_p = XB_p + eps_p
  C_p = log(runif(nprim, 0, 0.95))
  survTime_p = ifelse(T_p <= C_p, T_p, C_p)
  event_p = ifelse(T_p <= C_p, 1, 0)
  
  T_a = XB_a + eps_a
  C_a = log(runif(naux, 0, 150))
  survTime_a = ifelse(T_a <= C_a, T_a, C_a)
  event_a = ifelse(T_a <= C_a, 1, 0)
  
  primData = data.frame(X1 = X1_p,
                        X2 = X2_p,
                        X3 = X3_p,
                        X4 = X4_p,
                        X5 = X5_p,
                        time = survTime_p,
                        delta = event_p)
  auxData = data.frame(X1 = X1_a,
                       X2 = X2_a,
                       X3 = X3_a,
                       X4 = X4_a,
                       X5 = X5_a,
                       time = survTime_a,
                       delta = event_a)
  
  return(list(primData = primData,
              auxData = auxData))
}

#' Compute Kaplan-Meier Based Weights
#'
#' @param data Data frame with 'time' and 'delta' columns.
#'
#' @return A numeric vector of weights.
#' @export
GetWeights <- function(data) {
  data <- data[order(data$time),]
  n <- nrow(data)
  w <- rep(NA, n)
  
  for (i in 1:n) {
    if (i == 1) {
      w[i] <- data$delta[1] / n
    }
    else{
      prod <- 1
      for (j in 1:(i-1)) {
        prod <- prod * ((n - j) / (n - j + 1)) ^ data$delta[j]
      }
      w[i] <- (data$delta[i] / (n - i + 1)) * prod
    }
  }
  return(w)
}

#' Estimate Regression Coefficients via Weighted Quantile Regression
#'
#' @param data Data frame containing covariates X1–X5 and time, delta.
#'
#' @return Estimated regression coefficients as a numeric vector.
#' @export
GetBeta <- function(data) {
  data <- data[order(data$time),]
  Y <- data$time
  X <- as.matrix(data[c('X1','X2','X3','X4','X5')])
  w <- GetWeights(data)
  
  beta <- rq(formula = time ~ X1 + X2 + X3 + X4 + X5 - 1, tau = 0.5, data = data,
             weights = w)$coefficients
  
  return(as.vector(beta))
}

#' Estimate Adjustment Term Eta
#'
#' @param primdata Primary data frame.
#' @param beta_a Auxiliary model coefficients.
#'
#' @return Estimated adjustment vector eta.
#' @export
GetEta <- function(primdata,beta_a) {
  primdata <- primdata[order(primdata$time),]
  Y <- primdata$time
  X <- as.matrix(data[c('X1','X2','X3','X4','X5')])
  
  w_p <- GetWeights(primdata)
  primdata$Y <- Y - X%*%beta_a
  eta <- rq(formula = Y ~ X1 + X2 + X3 + X4 + X5 - 1, tau = 0.5, data = primdata,
            weights = w_p)$coefficients
  return(as.vector(eta))
}

#' Estimate Eta Using LAD Regression
#'
#' @param primdata Primary data frame.
#' @param beta_a Auxiliary model coefficients.
#'
#' @return Estimated eta vector from LAD regression.
#' @export
GetEtalad <- function(primdata,beta_a) {
  primdata <- primdata[order(primdata$time),]
  Y <- primdata$time
  X <- as.matrix(primdata[c('X1','X2','X3','X4','X5')])

  w_p <- GetWeights(primdata)

  Y <- Y - X%*%beta_a
  data <- data.frame(Y=Y,X)
  
  eta_lad <- rq(formula = Y ~ X1 + X2 + X3 + X4 + X5 - 1, tau = 0.5, data = data,
             weights = w_p)$coefficients
  
  return(as.vector(eta_lad))
}

#' Compute BIC for Model Selection
#'
#' @param data Data frame.
#' @param beta_a Auxiliary coefficient vector.
#' @param eta Adjustment vector.
#' @param lambda Regularization parameter.
#' @param cutoff Threshold for non-zero coefficients. Default is 1e-5.
#'
#' @return BIC value.
#' @export
GetBIC <- function(data,beta_a, eta, lambda, cutoff=1e-5) {
  n <- nrow(data)
  Y <- data$time
  X <- as.matrix(data[c('X1','X2','X3','X4','X5')])
  w <- diag(GetWeights(data))
  
  Y <- Y - X%*%beta_a
  
  ## Compute GCV
  GCV <- sum(w %*% abs(Y - X%*%eta))
  ## Compute BIC
  K = sum(c(abs(eta)>cutoff))
  BIC <- log(GCV)+log(n)*K/n
  return(BIC)
}

#' Refined RTransAFT Estimator via Adaptive LAD
#'
#' @param primdata Primary dataset.
#' @param beta_a Auxiliary model coefficients.
#'
#' @return Estimated adjustment vector eta.
#' @export
RTransAFT <- function(primdata, beta_a) {
  primdata <- primdata[order(primdata$time),]
  X <- as.matrix(primdata[, c('X1','X2','X3','X4','X5')])
  Y <- primdata$time
  n <- length(Y)
  
  w_p <- GetWeights(primdata)
  eta_lad <- GetEtalad(primdata,beta_a)
  
  Y <- Y - X%*%beta_a
  wY <- w_p*Y
  wX <- w_p*X
  ######### hqreg #########
  sol.rq <- hqreg_raw(wX,wY,tau = 0.5, method = "quantile", intercept = F,
                      penalty.factor = 1/abs(eta_lad))
  eta.path <- as.matrix(coef(sol.rq))
  lambda <- sol.rq$lambda
  eta.path.GCV <- sapply(seq_len(ncol(eta.path)), function(j) {
    GetBIC(primdata, beta_a, eta.path[,j], lambda[j])
  })
  min_GCV.idx <- which.min(eta.path.GCV);min_GCV.idx
  eta <- eta.path[,min_GCV.idx];eta
  
  return(eta)
}

#' Bootstrap a Single Replicate for Refined Transfer AFT
#'
#' @param r Integer index for the bootstrap iteration.
#'
#' @return A list of estimated coefficient vectors: luad, seer, both, and transfer.
#' @export
runBtsp_RTransAFT_one <- function(r){
  
  adata_btsp <- auxdata[sample(1:nrow(auxdata), nrow(auxdata), replace = TRUE), ]
  pdata_btsp <- primdata[sample(1:nrow(primdata), nrow(primdata), replace = TRUE), ]
  cdata_btsp <- dat.both[sample(1:nrow(dat.both), nrow(dat.both), replace = TRUE), ]
  
  beta.luad <- GetBeta(pdata_btsp)
  beta.seer <- GetBeta(adata_btsp)
  beta.both <- GetBeta(cdata_btsp)
  
  eta <- RTransAFT(primdata=pdata_btsp, beta_a=beta.seer)
  beta.trans <- beta.seer + eta
  return(list(beta.luad=beta.luad,beta.seer=beta.seer,beta.both=beta.both,beta.trans=beta.trans))
}



