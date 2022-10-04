############ FUNCTION LIST ###############################
rm(list = ls())
#library(NCmisc) list.functions.in.file() doesen't work properly

get_functions <- function(..., local=NULL) {
  tmp <- new.env(parent=parent.frame())
  source(..., local = tmp)
  funs <- names(tmp)[unlist(eapply(tmp, is.function))]
  for(x in names(tmp)) {
    assign(x, tmp[[x]], envir = parent.frame())
  }
  list(functions=funs)
}

L0 <- get_functions("C:/Users/jernejb/Desktop/git/SplyBeeJernejFork/SIMplyBee/SIMplyBee/R/Functions_L0_auxilary.R")
L1 <- get_functions("C:/Users/jernejb/Desktop/git/SplyBeeJernejFork/SIMplyBee/SIMplyBee/R/Functions_L1_Pop.R")
L2 <- get_functions("C:/Users/jernejb/Desktop/git/SplyBeeJernejFork/SIMplyBee/SIMplyBee/R/Functions_L2_Colony.R")
L3 <- get_functions("C:/Users/jernejb/Desktop/git/SplyBeeJernejFork/SIMplyBee/SIMplyBee/R/Functions_L3_Colonies.R")

# L0 functions ------------------------------------------------------------

# ---- Get QTL haplo info ----

getCasteQtlHaplo
getQtlHaplo
getColonyQtlHaplo
getQueensQtlHaplo
getFathersQtlHaplo
getDronesQtlHaplo
getWorkersQtlHaplo
getVirginQueensQtlHaplo

# ---- Get QTL Geno info ----

getCasteQtlGeno
getQtlGeno
getColonyQtlGeno
getQueensQtlGeno
getFathersQtlGeno
getDronesQtlGeno
getWorkersQtlGeno
getVirginQueensQtGeno

# ---- Get seg site haplo info ----

getCasteSegSiteHaplo
getColonySegSiteHaplo
getSegSiteHaplo
getQueensSegSiteHaplo
getDronesSegSiteHaplo
getFathersSegSiteHaplo
getWorkersSegSiteHaplo
getVirginQueensSegSiteHaplo

# ---- Get seg site geno info ----

getCasteSegSiteGeno
getColonySegSiteGeno
getSegSiteGeno
getQueensSegSiteGeno
getDronesSegSiteGeno
getFathersSegSiteGeno
getWorkersSegSiteGeno
getVirginQueensSegSiteGeno

# ---- Get SNP haplo info ----

getCasteSnpHaplo
getSnpHaplo
getColonySnpHaplo
getQueensSnpHaplo
getFathersSnpHaplo
getDronesSnpHaplo
getWorkersSnpHaplo
getVirginQueensSnpHaplo

# ---- Get SNP geno info ----

getSnpGeno
getCasteSnpGeno
getColonySnpGeno
getQueensSnpGeno
getFathersSnpGeno
getDronesSnpGeno
getWorkersSnpGeno
getVirginQueensSnpGeno

# ---- Get GV ----

getColonyGv
getCasteGv
getVirginQueensGv
getQueensGv
getDronesGv
getFathersGv
getWorkersGv

# ---- Get BV ----

getCasteBv
getColonyBv
getVirginQueensBv
getQueensBv
getDronesBv
getFathersBv
getWorkersBv

# ---- Get DD ----

getCasteDd
getColonyDd
getVirginQueensDd
getQueensDd
getDronesDd
getFathersDd
getWorkersDd

# ---- IBD ----

getCasteIbdHaplo
getColonyIbdHaplo
getIbdHaplo
getQueensIbdHaplo
getFathersIbdHaplo
getVirginQueensIbdHaplo
getWorkersIbdHaplo
getDronesIbdHaplo

# ---- random geno functions ----

isGenoHeterozygous
editGenomeGeno
simulateHoneyBeeGenomes
getPooledGeno

# ---- calc geno related stuff ----

calcBeeGRMIbs
calcBeeGRMIbd
calcBeeAlleleFreq

# ----  csd related info ----

calcQueensPHomBrood
getCsdAlleles
getCsdGeno
isCsdHeterozygous
isCsdActive
editCsdLocus
nCsdAlleles
nHomBrood
pHomBrood

# ----  get info ----

getId
getCaste
getCastePop
getQueensYearOfBirth
getQueensAge
getCasteId
getCasteSex
getLocation

# ----  get ind ----

getDrones
getFathers
getQueen
getVirginQueens
getWorkers

# ---- event info ----

getEvents
hasCollapsed
hasSplit
hasSuperseded
hasSwarmed

# ---- present ----

areFathersPresent
areVirginQueensPresent
areWorkersPresent
areDronesPresent
isQueenPresent

# ---- is ----

isCaste
isColony
isEmpty
isMultiColony
isNULLColonies
isPop
isProductive

# ---- is ind ----

isVirginQueen
isDrone
isFather
isQueen
isWorker

# ---- n ----

nCaste
nColonies
nNULLColonies
nEmptyColonies

# ---- n ind ----

nDrones
nFathers
nQueens
nVirginQueens
nWorkers

# ---- special operations on drone genome ----

reduceDroneGeno
reduceDroneHaplo

# L1 functions ------------------------------------------------------------
# ---- create ----

createWorkers
createDCA
createVirginQueens
createDrones

# ---- pull ----

pullCastePop
pullDrones
pullVirginQueens
pullWorkers
pullDroneGroupsFromDCA
pullQueen
pullInd

# ---- get ----

getWorkers
getFathers
getDrones
getQueen
getVirginQueens
getCastePop

# ---- combine ----

combineBeeGametesHaploDiploid
combineBeeGametes

# ---- misc ----

cross
setQueensYearOfBirth


# L2 functions ------------------------------------------------------------
# ---- replace ----

replaceVirginQueens
replaceDrones
replaceWorkers
reQueen

# ---- remove ----

removeDrones
removeWorkers
removeVirginQueens
removeQueen

# ---- colony ----

downsize
buildUp
createColony
setColonyPheno
setLocation

# ---- colony events ----

supersede
split
resetEvents
combine
swarm
collapse

# ---- add ----

addDrones
addWorkers
addVirginQueens


# L3 functions ------------------------------------------------------------

selectColonies
removeColonies
createMultiColony
pullColonies

