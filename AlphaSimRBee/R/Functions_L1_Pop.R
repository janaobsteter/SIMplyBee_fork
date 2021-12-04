# Level 1 Pop Functions

#' @rdname getCaste
#' @title Access individuals of a caste
#'
#' @description Access individuals of a caste
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' getCaste(colony1, caste = "queen")
#' getCaste(colony1, caste = "fathers")
#' getCaste(colony1, caste = "fathers", nInd = 2)
#' getCaste(colony1, caste = "fathers", nInd = 2)@id
#' getCaste(colony1, caste = "fathers", nInd = 2)@id
#'
#' apiary <- c(colony1, colony2)
#' getCaste(apiary, caste = "queen")
#' getCaste(apiary, caste = "queen")[[1]]@id
#' getCaste(apiary, caste = "queen")[[2]]@id
#'
#' getWorkers(apiary, nInd = 10)
#' getWorkers(apiary, nInd = 10)[[1]]@id
#' getWorkers(apiary, nInd = 10)[[2]]@id
#'
#' @return
#' When \code{x} is Colony then return is Pop, population object with individuals
#' When \code{x} is Colonies then return is a list of Pop, population objects with individuals
#'
#' @export
getCaste <- function(x, caste, nInd = NULL) {
  if ("Colony" %in% class(x)) {
    if (is.null(nInd)) {
      if (caste == "fathers") {
        ret <- x@queen@misc$fathers
      } else {
        ret <- slot(x, caste)
      }
    } else {
      if (caste == "fathers") {
        ret <- selectInd(pop = x@queen@misc$fathers, nInd = nInd, use = "rand")
      } else {
        ret <- selectInd(pop = slot(x, caste), nInd = nInd, use = "rand")
      }
    }
  } else if ("Colonies" %in% class(x)) {
    if (is.null(nInd)) {
      if (caste == "fathers") {
        ret <- lapply(X = x@colonies, FUN = function(z) z@queen@misc$fathers)
      } else {
        ret <- lapply(X = x@colonies, FUN = function(z) slot(z, caste))
      }
    } else {
      if (caste == "fathers") {
        ret <- lapply(X = x@colonies, FUN = function(z) selectInd(pop = z@queen@misc$fathers, nInd = nInd, use = "rand"))
      } else {
        ret <- lapply(X = x@colonies, FUN = function(z) selectInd(pop = slot(z, caste), nInd = nInd, use = "rand"))
      }
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getQueen
#' @title Access the queen
#'
#' @description Access the queen of a colony
#'
#' @param x Colony or Colonies
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' getQueen(colony1)
#' getQueen(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getQueen(apiary)
#'
#' @return
#' When \code{x} is Colony then return is Pop, population object with the queen
#' When \code{x} is Colonies then return is a list of Pop, population objects with the queen
#'
#' @export
getQueen <- function(x) {
  ret <- getCaste(x, caste = "queen")
  return(ret)
}

#' @rdname getFathers
#' @title Access fathers
#'
#' @description Access fathers (drones the queen mated with)
#'
#' @param x Colony or Colonies
#' @param nInd numeric, number of fathers to access, if NULL all fathers
#' are accessed, otherwise a random sample

#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' getFathers(colony1)
#' getFathers(colony1)@id
#' drones@id
#'
#' apiary <- c(colony1, colony2)
#' getFathers(apiary)
#' getFathers(apiary)[[1]]@id
#' getFathers(apiary)[[2]]@id
#'
#' @return
#' When \code{x} is Colony then return is Pop, population object with fathers
#' When \code{x} is Colonies then return is a list of Pop, population objects with fathers
#'
#' @export
getFathers <- function(x, nInd = NULL) {
  ret <- getCaste(x, caste = "fathers", nInd = nInd)
  return(ret)
}

#' @rdname getVirginQueens
#' @title Access virgin queens
#'
#' @description Access virgin queens
#'
#' @param x Colony or Colonies
#' @param nInd numeric, number of virgin queens to access, if NULL all virgin queens
#' are accessed, otherwise a random sample
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addVirginQueens(colony1, nVirginQueens = 1)
#' colony2 <- addVirginQueens(colony1, nVirginQueens = 10)
#' getVirginQueens(colony1)
#' getVirginQueens(colony2)
#' getVirginQueens(colony2, nInd = 2)
#' getVirginQueens(colony2, nInd = 2)@id
#' getVirginQueens(colony2, nInd = 2)@id
#'
#' apiary <- c(colony1, colony2)
#' getVirginQueens(apiary)
#' getVirginQueens(apiary)[[1]]@id
#' getVirginQueens(apiary)[[2]]@id
#'
#' @return
#' When \code{x} is Colony then return is Pop, population object with virgin queens
#' When \code{x} is Colonies then return is a list of Pop, population objects with virgin queens
#'
#' @export
getVirginQueens <- function(x, nInd = NULL) {
  ret <- getCaste(x, caste = "virgin_queens", nInd = nInd)
  return(ret)
}

#' @rdname getWorkers
#' @title Access workers
#'
#' @description Access workers
#'
#' @param x Colony or Colonies
#' @param nInd numeric, number of workers to access, if NULL all workers
#' are accessed, otherwise a random sample
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' getWorkers(colony1)
#' getWorkers(colony1)@id
#' getWorkers(colony1, nInd = 2)@id
#' getWorkers(colony1, nInd = 2)@id
#'
#' apiary <- c(colony1, colony2)
#' getWorkers(apiary)
#' getWorkers(apiary)[[1]]@id
#' getWorkers(apiary)[[2]]@id
#'
#' getWorkers(apiary, nInd = 10)
#' getWorkers(apiary, nInd = 10)[[1]]@id
#' getWorkers(apiary, nInd = 10)[[2]]@id
#'
#' @return
#' When \code{x} is Colony then return is Pop, population object with workers
#' When \code{x} is Colonies then return is a list of Pop, population objects with workers
#'
#' @export
getWorkers <- function(x, nInd = NULL) {
  ret <- getCaste(x, caste = "workers", nInd = nInd)
  return(ret)
}

#' @rdname getDrones
#' @title Access drones
#'
#' @description Access drones
#'
#' @param x Colony or Colonies
#' @param nInd numeric, number of drones to access, if NULL all drones
#' are accessed, otherwise a random sample
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#' getDrones(colony1)
#' getDrones(colony1)@id
#' getDrones(colony1, nInd = 2)@id
#' getDrones(colony1, nInd = 2)@id
#'
#' apiary <- c(colony1, colony2)
#' getDrones(apiary)
#' getDrones(apiary)[[1]]@id
#' getDrones(apiary)[[2]]@id
#'
#' getDrones(apiary, nInd = 10)
#' getDrones(apiary, nInd = 10)[[1]]@id
#' getDrones(apiary, nInd = 10)[[2]]@id
#'
#' @return
#' When \code{x} is Colony then return is Pop, population object with drones
#' When \code{x} is Colonies then return is a list of Pop, population objects with drones
#'
#' @export
getDrones <- function(x, nInd = NULL) {
  ret <- getCaste(x, caste = "drones", nInd = nInd)
  return(ret)
}

#' @rdname crateFounderDrones
#' @title Creates drones from base population
#' @usage \method{createFounderDrones}(pop, nDronesPerQueen)
#' @description Creates population of drones from base population.
#'       \Drones are created as double haploids. Founder drones are used when crating colonies to be used as fathers.
#' @param pop AlphaSimRBee Colony object from the \code{createColony(...)}
#' @param nDronesPerQueen Integer, number of drones to create
#'
#' @examples
#' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' founderDrones <- createFounderDrones(pop[2:110], nDronesPerQueen = 100)
#' colony1 <- createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' @return AlphaSim population object of created drones.
#'
#' @export
createFounderDrones <- function(pop, nDronesPerQueen) {
  if (!("Pop" %in% class(pop))) {
    stop("Argument pop must be a Pop class object!")
  }
  drones <- makeDH(pop, nDH = nDronesPerQueen)
  return(drones)
}

#' @rdname createWorkers
#' @title Creates workers of the colony
#' @usage \method{createWorkers}(colony, nInd)
#' @description Creates the specified number of workers in the colony
#'       \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, number of workers to create
#'
#' @examples
#' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones <- createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 <- createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' #Creat workers
#' colony1@workers <- createWorkers(colony1, nInd = 1000)
#'
#' @return AlphaSim population object of created workers.
#'
#' @export
createWorkers <- function(colony, nInd){
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.null(colony@queen)) {
    stop("Missing queen!")
  }
  if (!isQueenMated(colony)) {
    stop("Unmated queen!")
  }
  workerPop <- randCross2(females = colony@queen,
                         males = colony@queen@misc$fathers,
                         nCrosses = nInd)
  return(workerPop)
}

#' @rdname createDrones
#' @title Creates drones of the colony as double haploids
#' @usage \method{createDrones}(colony, nInd)
#' @description Creates the specified number of drones in the colony
#'       \as double haploids from the current queen \code{colony@queen}.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, the umber of drones to create.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones <- createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 <- createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' #Create drones
#' colony1@drones <- createDrones(colony1, nInd = 200)
#'
#' @return AlphaSim population object of created drones.
#' @export
createDrones <- function(colony, nInd){
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.null(colony@queen)) {
    stop("Missing queen!")
  }
  drones <- makeDH(pop = colony@queen, nDH = nInd)
  return(drones)
}

#' @rdname createVirginQueens
#' @title Creates virgin queen
#' @usage \method{createVirginQueens}(colony, nInd)
#' @description Creates the specified number of virgin queens in the colony
#'        \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, the number of virgin queens to create.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones <- createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 <- createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' #Crate virgin queens
#' colony1@virgin_queens <- createVirginQueens(colony1, nInd = 17)
#'
#' @return AlphaSim population object of created virgin queens.
#' @export
createVirginQueens <- function(colony, nInd){
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  virginQueens <- createWorkers(colony, nInd = nInd)
  return(virginQueens)
}

#' @rdname createDCA
#' @title Create a drone congregation area (DCA)
#'
#' @description Create a drone congregation area (DCA) from colony or colonies.
#'
#' @param x Colony or Colonies
#' @param nInd numeric, number of drones to access, if NULL all drones
#' are accessed, otherwise a random sample
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#' createDCA(colony1)
#' createDCA(colony2)
#' createDCA(colony2, nInd = 10)@id
#' createDCA(colony2, nInd = 10)@id
#'
#' apiary <- c(colony1, colony2)
#' createDCA(apiary)
#' createDCA(apiary, nInd = 10)
#'
#' @return Pop, population object with drones
#'
#' @export
createDCA <- function(x, nInd = NULL) {
  if ("Colony" %in% class(x)) {
    DCA <- getDrones(x, nInd = nInd)
  } else if ("Colonies" %in% class(x)) {
    DCA <- getDrones(x, nInd = nInd)
    DCA <- mergePops(popList = DCA)
  } else {
    stop("Argument x must be a Colony of Colonies class object!")
  }
  return(DCA)
}

#' @rdname pullIndFromPop
#' @title Pull individuals from a population
#'
#' @description Pull individuals from a population and update the population
#'
#' @param pop Pop
#' @param nInd numeric, number of individuals to pull
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#' pullIndFromPop(basePop, nInd = 2)
#' pullIndFromPop(basePop, nInd = 3)
#'
#' @return Pop, population object with drones
#'
#' @export
pullIndFromPop <- function(pop, nInd) {
  if (!"Pop" %in% class(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  selectedInd <- sample(pop@id, size = nInd, replace = FALSE)
  sel <- pop@id %in% selectedInd
  pulled <- pop[sel]
  remainder <- pop[!sel]
  ret <- list(pulled = pulled, remainder = remainder)
  return(ret)
}

#' @rdname pullDroneGroupsFromDCA
#' @title Pulls drone groups from a DCA
#'
#' @description
#' Pulls drone groups from a DCA to use them later in mating. Number of drones
#' per group is sampled from a Poisson distribution with average group size.
#' Pulled drones are removed from the DCA.
#'
#'@param DCA Pop, population of drones
#'@param nGroups Integer, number of drone groups to be created
#'@param avgGroupSize Numeric, average number of drones per group
#'
#'@examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#' apiary <- c(colony1, colony2)
#' DCA <- createDCA(apiary)
#' pullDroneGroupsFromDCA(DCA, nGroup = 4, avgGroupSize = 5)
#'
#'@return Two AlphaSim population objects of the colony and the group of pulled individuals.
#'
#'@export
pullDroneGroupsFromDCA <- function(DCA, nGroup, avgGroupSize) {
  if (!"Pop" %in% class(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  nDrones <- rpois(n = nGroup, lambda = avgGroupSize)
  if (sum(nDrones) > DCA@nInd) {
    stop("Not enough drones in the DCA!")
  }
  ret <- vector(mode = "list", length = nGroup)
  for (group in 1:nGroup) {
    tmp <- pullIndFromPop(pop = DCA, nInd = nDrones[group])
    ret[[group]] <- tmp$pulled
    DCA <- tmp$remainder
  }
  return(ret)
}

#' @rdname pullIndFromCaste
#' @title Randomly pulls a number of individuals from any caste group.
#' @usage \method{pullIndFromCaste}(colony, caste, nInd)
#' @description Pulls and separates a random number of individuals from any caste group.
#' Two list groups are created, the group of pulled individuals and the colony.
#'
#' @seealso \code{\link[??????]{pullIndFromCaste}}
#' @param colony Colony class. AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param caste Character. Replicating the caste class structure present in the hive (queen, drones, workers etc)
#' @param nInd Integer. Number of individuals to be pulled from the caste; if NULL
#' all individuals are pulled
#'
#' @examples inst/examples/examples_pullIndFromCaste.R
#' Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones <- createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 <- createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' #Create drones
#' colony1@drones <- createDrones(colony1, nInd = 300)
#'
#' # Pull individuals from the caste
#' indDrone <- pullIndFromCaste(colony1, caste = 'drones', nInd = 1)
#'
#'@return Two AlphaSim population objects of the colony and the group of pulled individuals.
pullIndFromCaste <- function(colony, caste, nInd = NULL) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.nul(nInd)) {
    nInd <- nInd(slot(colony, caste))
  }
  if (nInd > nInd(slot(colony, caste))) {
    stop(paste0("Not enough individuals in ", caste, " ! " ,
                nInd, " required, but only ", slot(colony, caste)@nInd, " available."))
  }
  pullId <- sample(slot(colony, caste)@id, nInd, replace = F)
  pullMatch <- slot(colony, caste)@id %in% pullId
  stayMatch <- !slot(colony, caste)@id %in% pullId

  indPull <- slot(colony, caste)[pullMatch]
  indStay <- slot(colony, caste)[stayMatch]

  slot(colony, caste) <- indStay
  ret <- list(colony = colony, pulledInd = indPull)
  return(ret)
}

#' @rdname crossVirginQueen
#' @title Crosses a virgin queen to a group drones.
#' @usage \method{crossVirginQueen}(virginQueen, fathers)
#' @description Crosses a virgin queen to a group of drones
#' @param virginQueen AlphaSimR population object
#' @param fathers AlphaSimR population class.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones <- createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 <- createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 <- createColony(queen = pop[2], fathers = founderDrones[18:37])
#'
#' #Create drones that will be in DCA
#' colony1@drones <- createDrones(colony1, nInd = 300)
#' colony2@drones <- createDrones(colony2, nInd = 300)
#'
#' DCA <- createDCA(c(colony1, colony2))
#'
#' #pull drone packages from DCA
#' dronePack <- pullDroneGroupsFromDCA(DCA, n = 7, nAvgFathers = 19)
#'
#'# Cross the virgin queen
#'
#' virginQueen <- colony1@virgin_queens
#' queen <- crossVirginQueen(virginQueen, fathers = dronePack[4])
#'
#' @return AlphaSim population object of a mated queen
#' @export
crossVirginQueen <- function(virginQueen, fathers) {
  if (!"Pop" %in% class(virginQueen)) {
    stop("Argument virginQueen must be a Pop class object!")
  }
  if (!"Pop" %in% class(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  if (isQueenMated(virginQueen)) {
    stop("The queen is mated already!")
  }
  if (is.null(fathers)) {
    stop("Missing fathers!")
  }
  if (virginQueen@nInd > 1) {
    stop("#TODO: Expand the function to mate multiple virgin queens at once!")
  }
  virginQueen@misc$fathers <- fathers
  return(virginQueen)
}
