## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## ----load packages------------------------------------------------------------
library(package = "SIMplyBee")
library(package = "ggplot2")

## ----founderGenomes-----------------------------------------------------------
# Founder genomes
founderGenomes <- quickHaplo(nInd = 20, nChr = 16, segSites = 1000)

## ----SimParamBee--------------------------------------------------------------
# Global simulation parameters
SP <- SimParamBee$new(founderGenomes)

# Quantitative genetic parameters - for a single trait with queen and worker genetic effects
meanP <- c(20, 0)
varA <- c(1, 1 / SP$nWorkers)
corA <- matrix(data = c( 1.0, -0.5, 
                        -0.5,  1.0), nrow = 2, byrow = TRUE)
SP$addTraitA(nQtlPerChr = 100, mean = meanP, var = varA, corA = corA)

varE <- c(3, 3 / SP$nWorkers)
# TODO: what is a reasonable environmental correlation between queen and worker effects?
corE <- matrix(data = c( 1.0, -0.3, 
                        -0.3,  1.0), nrow = 2, byrow = TRUE)
# TODO: Move this to passing setColonyPheno()!
SP$setVarE(varE = varE)
SP$setCorE(corE = corE)

