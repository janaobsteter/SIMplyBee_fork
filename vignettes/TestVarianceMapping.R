library(SIMplyBee)
library(tidyr)
library(ggplot2)

# Create a function that maps individual to colony variance
mapIndToColonyVar <- function(varA_q, varA_w, corA_qw, 
                              varE_q, varE_w, corE_qw,
                              nW, nF, nDPQ, workersFUN = "sum") {
  
  # First handle the genetic part
  # Determine how many pairs of each you have
  nW = nW
  n_SS = (nW * nW / nF) - nW
  n_FS = (nW * nW / nDPQ) -  (nW * nW / nF)
  n_HS = (nW * nW / nDPQ) * (nDPQ - 1)

  varA_q <- varA_q
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

  # Next handle the environmental part
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

# Test the function through reps
nRep = 10
variances0 = data.frame()

for (rep in 1:nRep) {
  print(paste0("Mean, rep ", rep))
  founderGenomes <- quickHaplo(nInd = 20, nChr = 16, segSites = 1000)
  SP <- SimParamBee$new(founderGenomes)
  nQtlPerChr <- 100
  mean <- c(0, 0)
  varA <- c(1, 1)
  corA <- matrix(data = c( 1.0, -0.5,
                          -0.5,  1.0), nrow = 2, byrow = TRUE)
  SP$addTraitA(nQtlPerChr = nQtlPerChr, mean = mean, var = varA, corA = corA,
              name = c("queenTrait", "workersTrait"))
  varE <- c(3, 3)
  corE <- matrix(data = c(1.0, 0.3,
                          0.3, 1.0), nrow = 2, byrow = TRUE)
  SP$setVarE(varE = varE, corE = corE)
  basePop <- createVirginQueens(founderGenomes, n = 20)
  head(basePop@gv)
  head(basePop@pheno)
  drones <- createDrones(x = basePop[1:5], nInd = 3)
  colony <- createColony(x = basePop[6])
  colony <- cross(x = colony, drones = drones, checkCross = "warning")
  colony <- addWorkers(x = colony, nInd = 50)
  colony <- buildUp(colony)
  apiary <- createMultiColony(basePop[7:20])
  drones <- createDrones(basePop[1:5], nInd = 100)
  apiary <- cross(x = apiary, drones = drones, crossPlan = "create", checkCross = "warning")
  apiary <- buildUp(apiary)
  colonyGv <- calcColonyGv(apiary)

  # Get the real number of fathers and DPQs
  nW = round(mean(nWorkers(apiary)), 0)
  nF = round(mean(nFathers(apiary)), 0)
  nDPQ <- round(mean(sapply(getFathers(apiary), function(x) length(unique(x@mother)))), 0)


  # When the function to summarise worker effects is SUM
  calcVar <- mapIndToColonyVar(varA_q = varA[1], 
                    varA_w= varA[2],
                    corA_qw = corA[1,2],
                    varE_q = varE[1], 
                    varE_w = varE[2], 
                    corE_qw = corE[1,2],
                    nW = nW, nF = nF,
                    nDPQ = nDPQ,
                    workersFUN = "mean")

  real_gv_q <- calcColonyGv(apiary, FUN = mapCasteToColonyGv, queenTrait = 1, workersTrait = NULL)
  real_gv_wbar <- calcColonyGv(apiary, FUN = mapCasteToColonyGv, queenTrait = NULL, workersTrait = 2, workersFUN = colMeans)
  real_covA_qwbar <- cov(real_gv_q, real_gv_wbar)
  real_corA_qwbar <- cor(real_gv_q, real_gv_wbar)
  real_varA_q <- var(real_gv_q)
  real_varA_wbar <- var(real_gv_wbar)
  real_varA_c <- var(calcColonyGv(apiary, FUN = mapCasteToColonyGv, workersFUN = colMeans))
  real_varP_c <- var(calcColonyPheno(apiary, FUN = mapCasteToColonyPheno, workersFUN = colMeans))

  variances0 <- rbind(variances0, data.frame(rep = rep, 
                                             calc_varA_q = calcVar$varA_q, calc_varA_wbar = calcVar$varA_wbar, calc_varA_c = calcVar$varA_c, calc_varE_c = calcVar$varE_c,
                                             real_varA_q = real_varA_q[1,1], real_varA_wbar = real_varA_wbar[1,1], real_varA_c = real_varA_c[1,1],
                                             real_covA_qwbar = real_covA_qwbar[1,1], real_corA_qwbar = real_corA_qwbar[1,1],
                                             real_varP_c = real_varP_c[1,1],
                                             fun = "mean"))

  print(paste0("Sum, rep ", rep))
  # Run the example from the quantitative genetic vignette
  founderGenomes <- quickHaplo(nInd = 20, nChr = 16, segSites = 1000)
  SP <- SimParamBee$new(founderGenomes)
  SP$nWorkers <- 100
  SP$nFathers <- 15
  nQtlPerChr <- 100
  mean <- c(0, 0)
  varA <- c(1, 1 / SP$nWorkers)
  corA <- matrix(data = c( 1.0, -0.5,
                          -0.5,  1.0), nrow = 2, byrow = TRUE)
  SP$addTraitA(nQtlPerChr = nQtlPerChr, mean = mean, var = varA, corA = corA,
              name = c("queenTrait", "workersTrait"))
  varE <- c(3, 3 / SP$nWorkers)
  corE <- matrix(data = c(1.0, 0.3,
                          0.3, 1.0), nrow = 2, byrow = TRUE)
  SP$setVarE(varE = varE, corE = corE)
  
  basePop <- createVirginQueens(founderGenomes, n = 20)
  head(basePop@gv)
  head(basePop@pheno)
  drones <- createDrones(x = basePop[1:5], nInd = 3)
  colony <- createColony(x = basePop[6])
  colony <- cross(x = colony, drones = drones, checkCross = "warning")
  colony <- addWorkers(x = colony, nInd = 50)
  colony <- buildUp(colony)
  apiary <- createMultiColony(basePop[7:20])
  drones <- createDrones(basePop[1:5], nInd = 100)
  apiary <- cross(x = apiary, drones = drones, crossPlan = "create", checkCross = "warning")
  apiary <- buildUp(apiary)
  colonyGv <- calcColonyGv(apiary)
  colonyPheno <- calcColonyPheno(apiary)  
 
  nW = round(mean(nWorkers(apiary)), 0)
  nF = round(mean(nFathers(apiary)), 0)
  nDPQ <- round(mean(sapply(getFathers(apiary), function(x) length(unique(x@mother)))), 0)


  # When the function to summarise worker effects is SUM
  calcVar <- mapIndToColonyVar(varA_q = varA[1], 
                    varA_w= varA[2],
                    corA_qw = corA[1,2],
                    varE_q = varE[1],
                    varE_w = varE[2],
                    corE_qw = corE[1,2],
                    nW = nW, nF = nF,
                    nDPQ = nDPQ,
                    workersFUN = "sum")
  
  real_gv_q <- calcColonyGv(apiary, FUN = mapCasteToColonyGv, queenTrait = 1, workersTrait = NULL)
  real_gv_wbar <- calcColonyGv(apiary, FUN = mapCasteToColonyGv, queenTrait = NULL, workersTrait = 2, workersFUN = colSums)
  real_covA_qwbar <- cov(real_gv_q, real_gv_wbar)
  real_corA_qwbar <- cor(real_gv_q, real_gv_wbar)
  real_varA_q <- var(real_gv_q)
  real_varA_wbar <- var(real_gv_wbar)
  real_varA_c <- var(calcColonyGv(apiary, FUN = mapCasteToColonyGv, workersFUN = colSums))
  real_varP_c <- var(calcColonyPheno(apiary, FUN = mapCasteToColonyPheno, workersFUN = colMeans))


  variances0 <- rbind(variances0, data.frame(rep = rep, 
                                             calc_varA_q = calcVar$varA_q, calc_varA_wbar = calcVar$varA_wbar, calc_varA_c = calcVar$varA_c, calc_varE_c = calcVar$varE_c,
                                             real_varA_q = real_varA_q[1,1], real_varA_wbar = real_varA_wbar[1,1], real_varA_c = real_varA_c[1,1],
                                             real_covA_qwbar = real_covA_qwbar[1,1], real_corA_qwbar = real_corA_qwbar[1,1],
                                             real_varP_c = real_varP_c[1,1],
                                             fun = "sum"))


}

variances0Long <- pivot_longer(variances0 |> select(-c(real_covA_qwbar, real_corA_qwbar)), 
                              cols = c("calc_varA_q", "calc_varA_wbar", "calc_varA_c", "calc_varE_c",
                                        "real_varA_q", "real_varA_c", "real_varA_wbar", "real_varP_c"), names_to = "VarType", values_to = "Var")
meanRealVar = variances0Long %>% group_by(fun, VarType) %>% summarise(meanVar = mean(Var))  

variances0Long %>%
  mutate(rep = as.factor(rep)) %>%
  mutate(VarType = factor(VarType, levels = c("calc_varA_q", "calc_varA_wbar", "calc_varA_c", "calc_varE_c",
                                              "real_varA_q", "real_varA_wbar", "real_varA_c", "real_varP_c"))) %>%
  ggplot(aes(x = rep, y = Var, color = VarType)) +
  geom_point(size = 3) + 
  geom_hline(data = meanRealVar, aes(yintercept = meanVar, colour = VarType), linewidth = 3) +
  facet_wrap(. ~fun, scales = "free") +
   theme_bw(base_size = 20) + 
   scale_colour_manual(values = c(
 "#D55E00",  # vermillion
 "#084159",  # blue
 "#32c237",
   "#b560b4",
   "#a80805",   # reddish purple
   "#0f77a3",  # bluish green
   "#1a5e1d",
   "#610b60"
   ))

variances0  |> filter(rep == 1)

pivot_longer(variances0 |> select(c(rep, fun, real_covA_qwbar, real_corA_qwbar)), cols = c("real_covA_qwbar", "real_corA_qwbar"), names_to = "CovType", values_to = "Cov") |> 
  mutate(rep = as.factor(rep)) %>%
  ggplot(aes(x = rep, y = Cov, color = CovType)) +
  geom_point(size = 3) +
  facet_wrap(. ~fun+CovType, scales = "free") + 
  theme_bw(base_size = 20)

########################################################################################3
# The other way around
# Create a function that maps colony level variance to individual level variance
########################################################################################
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
  
  # First handle the genetic part
  covA_qwbar <- corA_qwbar * sqrt(varA_q) * sqrt(varA_wbar)

  # scaling factor
  if (workersFUN == "sum") {
    covA_qw <- covA_qwbar / nW 
  } else if (workersFUN == "mean") {
    covA_qw <- covA_qwbar
  }

  # pair counts
  n_SS <- (nW * nW / nF) - nW
  n_FS <- (nW * nW / nDPQ) - (nW * nW / nF)
  n_HS <- (nW * nW / nDPQ) * (nDPQ - 1)

  # worker variance coefficient
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

  # Next handle the environmental part
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

# Test the function through reps
nRep = 50
variances = data.frame()

for (rep in 1:nRep) {
  varA_q <- 5
  varA_wbar <- 10
  corA_qwbar <- -0.5

  varE_q <- 10
  varE_wbar <- 20
  corE_qwbar <- 0.3

  nW <- 100
  nF <- 15
  nDPQ <- 5

  for (fun in c("mean", "sum")) {
    indVarComp <- mapColonyToIndVar(varA_q = varA_q, varA_wbar = varA_wbar, corA_qwbar = corA_qwbar, 
                                    varE_q = varE_q, varE_wbar = varE_wbar, corE_qwbar = corE_qwbar, 
                                    nW = nW, nF = nF, nDPQ = nDPQ, workersFUN = fun)
    calc_var_c <- mapIndToColonyVar(varA_q = indVarComp$varA_q, varA_w = indVarComp$varA_w, corA_qw = indVarComp$corA_qw, 
                                    varE_q = indVarComp$varE_q, varE_w = indVarComp$varE_w, corE_qw = indVarComp$corE_qw,
                                    nW = nW, nF = nF, nDPQ = nDPQ, workersFUN = fun)

    print(paste0("Sum, rep ", rep))
    founderGenomes <- quickHaplo(nInd = 20, nChr = 16, segSites = 1000)
    SP <- SimParamBee$new(founderGenomes)
    SP$nWorkers = nW
    SP$nFathers = nF
    nQtlPerChr <- 100
    mean <- c(0, 0)
    varA <- c(indVarComp$varA_q, indVarComp$varA_w)
    corA <- matrix(data = c( 1.0, indVarComp$corA_qw,
                            indVarComp$corA_qw,  1.0), nrow = 2, byrow = TRUE)
    SP$addTraitA(nQtlPerChr = nQtlPerChr, mean = mean, var = varA, corA = corA,
                name = c("queenTrait", "workersTrait"))
    varE <- c(indVarComp$varE_q, indVarComp$varE_w)
    corE <- matrix(data = c(1.0, indVarComp$corE_qw,
                            indVarComp$corE_qw, 1.0), nrow = 2, byrow = TRUE)
    SP$setVarE(varE = varE, corE = corE)
    basePop <- createVirginQueens(founderGenomes, n = 20)
    
    drones <- createDrones(x = basePop[1:nDPQ], nInd = 3)
    colony <- createColony(x = basePop[nDPQ+1])
    colony <- cross(x = colony, drones = drones, checkCross = "warning")
    colony <- addWorkers(x = colony, nInd = 50)
    colony <- buildUp(colony)
    apiary <- createMultiColony(basePop[7:20])
    drones <- createDrones(basePop[1:nDPQ], nInd = 100)
    apiary <- cross(x = apiary, drones = drones, crossPlan = "create", checkCross = "warning")
    apiary <- buildUp(apiary)

    if (fun == "sum") {
      workersFUN <- colSums
    } else if (fun == "mean") {
      workersFUN <- colMeans
    }
    
    # Get the real number of fathers and DPQs
    real_A_wbar <- calcColonyGv(apiary, mapCasteToColonyGv, queenTrait = NULL, workersTrait = 2, workersFUN = workersFUN)
    real_varA_wbar <- popVar(real_A_wbar)
    real_A_c <- calcColonyGv(apiary, mapCasteToColonyGv, queenTrait = 1, workersTrait = 2, workersFUN = workersFUN)
    real_varA_c <- popVar(real_A_c)
    real_P_wbar <- calcColonyPheno(apiary, mapCasteToColonyPheno, queenTrait = NULL, workersTrait = 2, workersFUN = workersFUN)
    real_E_wbar <- real_P_wbar - real_A_wbar
    real_varE_wbar <- popVar(real_E_wbar)
    real_P_c <- calcColonyPheno(apiary, mapCasteToColonyPheno, queenTrait = 1, workersTrait = 2, workersFUN = workersFUN)
    real_E_c <- real_P_c - real_A_c
    real_varE_c <- popVar(real_E_c)

    variances <- rbind(variances, data.frame(rep = rep, 
                                                  set_var_wbar = varA_wbar, 

                                                  calc_var_wbar = calc_var_c$varA_wbar, calc_var_c = calc_var_c$varA_c, 
                                                  real_var_wbar = real_varA_wbar[1,1], real_var_c = real_varA_c, 

                                                  component = "A",

                                                  fun = fun))
  
      variances <- rbind(variances, data.frame(rep = rep, 
                                                set_var_wbar = varE_wbar,

                                                calc_var_wbar = calc_var_c$varE_wbar, calc_var_c = calc_var_c$varE_c,
                                                real_var_wbar = real_varE_wbar[1,1], real_var_c = real_varE_c,

                                                component = "E",
                                                fun = fun))
  


  }
}



variancesLong <- variances |> 
  #select(rep, fun, real_varA_wbar, set_varA_wbar, real_varA_c, calc_varA_c, calc_varA_wbar) |> 
  pivot_longer(cols = c("set_var_wbar", 
                        "calc_var_wbar", "calc_var_c", 
                        "real_var_wbar",  "real_var_c"), 
               names_to = "VarType", values_to = "Var")

meanRealVar = variancesLong %>% group_by(fun, component, VarType) %>% summarise(meanVar = mean(Var))  

library(viridis)
variancesLong %>% 
  mutate(rep = as.factor(rep)) %>%
  mutate(VarType = factor(VarType, levels = c("set_var_wbar", "calc_var_wbar", "real_var_wbar", "calc_var_c", "real_var_c"))) %>%
  ggplot(aes(x = rep, y = Var, color = VarType)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_hline(data = meanRealVar, aes(yintercept = meanVar, colour = VarType), linewidth = 3) +
  facet_wrap(. ~fun + component, scales = "free") +
  theme_bw(base_size = 20) + 
  scale_colour_manual(values = c(
  # Cool tones
  "#1B4965",  # deep blue
  "#048BA8",  # cyan-blue
  "#16DB93",  # green-teal
  
  # Warm tones
  "#F4A261",  # sand orange
  "#D62828",  # red
  "#9D4EDD"  # purple
))
