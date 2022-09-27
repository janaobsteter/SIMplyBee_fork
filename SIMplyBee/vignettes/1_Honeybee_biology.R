## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#",
  include = TRUE
)

## ----load package-------------------------------------------------------------
library(package = "SIMplyBee")

## ----founder genomes----------------------------------------------------------
founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)

## ----SimParamBee--------------------------------------------------------------
SP <- SimParamBee$new(founderGenomes)

## ----SP, eval = FALSE---------------------------------------------------------
#  show(SP)

## ----base pop virgin queens---------------------------------------------------
baseQueens <- createVirginQueens(founderGenomes)
baseQueens
isVirginQueen(baseQueens)

## ----base pop drones----------------------------------------------------------
baseDrones <- createDrones(x = baseQueens[1], nInd = 15)
baseDrones

## ----colony-------------------------------------------------------------------
colony <- createColony(x = baseQueens[2])
colony

## ----cross colony-------------------------------------------------------------
colony <- cross(colony, drones = baseDrones)
colony

## ----build up colony----------------------------------------------------------
buildUp(colony, nWorkers = 10, nDrones = 7)

## ----SP n values--------------------------------------------------------------
SP$nWorkers

## ----SP n values 2------------------------------------------------------------
SP$nDrones

## ----build up colony with defaults--------------------------------------------
colony <- buildUp(colony)
colony

## ----colony numbers 1---------------------------------------------------------
nQueens(colony)

## ----colony numbers 2---------------------------------------------------------
nFathers(colony)

## ----colony numbers 3---------------------------------------------------------
nWorkers(colony)

## ----colony numbers 4---------------------------------------------------------
nDrones(colony)

## ----colony numbers 5---------------------------------------------------------
nVirginQueens(colony)

## ----colony castes via get 1--------------------------------------------------
(queen <- getQueen(colony))

## ----colony castes via get 2--------------------------------------------------
(fathers <- getFathers(colony))

## ----colony castes via get 3--------------------------------------------------
(workers <- getWorkers(colony))

## ----colony castes via get 4--------------------------------------------------
(drones <- getDrones(colony))

## ----colony castes via get 5--------------------------------------------------
(virginQueens <- getVirginQueens(colony))

## ----colony structure, eval = FALSE-------------------------------------------
#  str(colony)

## ----help, eval = FALSE-------------------------------------------------------
#  help(SIMplyBee)

## ----caste queen--------------------------------------------------------------
getCaste(queen)

## ----caste fathers------------------------------------------------------------
getCaste(fathers)

## ----caste bees---------------------------------------------------------------
bees <- c(queen, fathers[1:2], workers[1:2], drones[1:2])
getCaste(bees)

## ----csd----------------------------------------------------------------------
getCsdAlleles(queen)

## ----inbred colony------------------------------------------------------------
inbredColony <- createColony(x = createVirginQueens(x = colony, nInd = 1))
fathers <- selectInd(drones, nInd = SP$nFathers, use = "rand")
inbredColony <- cross(inbredColony, drones = fathers)
getCsdAlleles(inbredColony)
getCsdAlleles(inbredColony, unique = TRUE)

## ----pHomBrood----------------------------------------------------------------
pHomBrood(inbredColony)

## ----hHomBrood----------------------------------------------------------------
inbredColony <- addWorkers(inbredColony, nInd = 100)
inbredColony
nHomBrood(inbredColony)

## ----hHomBrood II-------------------------------------------------------------
inbredColony <- addWorkers(inbredColony, nInd = 100)
inbredColony
nHomBrood(inbredColony)

## ----queens counters----------------------------------------------------------
getMisc(getQueen(inbredColony))

## ----queens haplo-------------------------------------------------------------
getQueenSegSiteHaplo(inbredColony)[, 1:10]

## ----queens geno--------------------------------------------------------------
getQueenSegSiteGeno(inbredColony)[, 1:10]

## ----fathers haplo------------------------------------------------------------
getFathersSegSiteHaplo(inbredColony)[, 1:10]

## ----fathers geno-------------------------------------------------------------
getFathersSegSiteGeno(inbredColony)[, 1:10]

## ----fathers haplo 2----------------------------------------------------------
getFathersSegSiteHaplo(inbredColony, nInd = 1, dronesHaploid = FALSE)[, 1:10]

## ----fathers geno 2-----------------------------------------------------------
getFathersSegSiteGeno(inbredColony, nInd = 1, dronesHaploid = FALSE)[, 1:10, drop = FALSE]

## ----workers haplo------------------------------------------------------------
getWorkersSegSiteHaplo(inbredColony, nInd = 2)[, 1:10]

## ----workers geno-------------------------------------------------------------
getWorkersSegSiteGeno(inbredColony, nInd = 2)[, 1:10]

## ----drones haplo-------------------------------------------------------------
inbredColony <- addDrones(inbredColony, nInd = 4)
getDronesSegSiteHaplo(inbredColony)[, 1:10]

## ----drones geno--------------------------------------------------------------
getDronesSegSiteGeno(inbredColony)[, 1:10]

## ----help II------------------------------------------------------------------
help(SIMplyBee)

