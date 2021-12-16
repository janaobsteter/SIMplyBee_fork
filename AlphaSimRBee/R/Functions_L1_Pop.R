# Level 1 Pop Functions

#' @rdname getCaste
#' @title Access individuals of a caste
#'
#' @description Access individuals of a caste. These individuals stay in the
#' colony.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed
#' @param use character, all options provided by \code{\link{selectInd}}
#'
#' @seealso \code{\link{getQueen}}, \code{\link{getFathers}},
#'   \code{\link{getVirginQueens}}, \code{\link{getWorkers}}, and
#'   \code{\link{getDrones}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 4)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getCaste(colony1, caste = "queen")
#' getQueen(colony1)
#'
#' drones@id
#' getCaste(colony1, caste = "fathers")
#' getCaste(colony1, caste = "fathers")@id
#' getCaste(colony1, caste = "fathers", nInd = 2)@id
#' getCaste(colony1, caste = "fathers", nInd = 2)@id
#' getFathers(colony1)
#' getFathers(colony1)@id
#' getFathers(colony1, nInd = 2)@id
#' getFathers(colony1, nInd = 2)@id
#'
#' getCaste(colony1, caste = "virgin_queens")
#' getCaste(colony1, caste = "virgin_queens")@id
#' getCaste(colony1, caste = "virgin_queens", nInd = 2)@id
#' getCaste(colony1, caste = "virgin_queens", nInd = 2)@id
#' getVirginQueens(colony1)
#' getVirginQueens(colony1)@id
#' getVirginQueens(colony1, nInd = 2)@id
#' getVirginQueens(colony1, nInd = 2)@id
#'
#' getCaste(colony1, caste = "workers")
#' getCaste(colony1, caste = "workers")@id
#' getCaste(colony1, caste = "workers", nInd = 2)@id
#' getCaste(colony1, caste = "workers", nInd = 2)@id
#' getWorkers(colony1)
#' getWorkers(colony1)@id
#' getWorkers(colony1, nInd = 2)@id
#' getWorkers(colony1, nInd = 2)@id
#'
#' getCaste(colony1, caste = "drones")
#' getCaste(colony1, caste = "drones")@id
#' getCaste(colony1, caste = "drones", nInd = 2)@id
#' getCaste(colony1, caste = "drones", nInd = 2)@id
#' getDrones(colony1)
#' getDrones(colony1)@id
#' getDrones(colony1, nInd = 2)@id
#' getDrones(colony1, nInd = 2)@id
#'
#' getCaste(colony2, caste = "drones")
#' getDrones(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getCaste(apiary, caste = "queen")
#' getQueen(apiary)
#' getCaste(apiary, caste = "queen")[[1]]@id
#' getCaste(apiary, caste = "queen")[[2]]@id
#'
#' getCaste(apiary, caste = "fathers")
#' getFathers(apiary)
#' getFathers(apiary)[[1]]@id
#' getFathers(apiary)[[2]]@id
#' getFathers(apiary, nInd = 2)
#'
#' getCaste(apiary, caste = "virgin_queens")
#' getVirginQueens(apiary)
#' getVirginQueens(apiary)[[1]]@id
#' getVirginQueens(apiary)[[2]]@id
#' getVirginQueens(apiary, nInd = 1)
#' getVirginQueens(apiary, nInd = 2) # TODO: can we do anything here - just give what is available?
#'
#' getCaste(apiary, caste = "workers")
#' getWorkers(apiary)
#' getWorkers(apiary)[[1]]@id
#' getWorkers(apiary)[[2]]@id
#' getWorkers(apiary, nInd = 2)
#'
#' getCaste(apiary, caste = "drones")
#' getDrones(apiary)
#' getDrones(apiary)[[1]]@id
#' getDrones(apiary)[[2]]@id # TODO: can we do anything here - just give what is available?
#' getDrones(apiary, nInd = 2) # TODO: can we do anything here - just give what is available?
#'
#' @return Pop when \code{x} is Colony, but \code{NULL} if caste is not present,
#'   and list of Pop when \code{x} is Colonies, named by colony id when \code{x}
#'   is Colonies
#'
#' @export
getCaste <- function(x, caste, nInd = NULL, use = "rand") {
  if (isColony(x)) {
    if (is.null(nInd)) {
      if (caste == "fathers") {
        ret <- x@queen@misc$fathers
      } else {
        ret <- slot(x, caste)
      }
    } else {
      if (caste == "fathers") {
        ret <- selectInd(pop = x@queen@misc$fathers, nInd = nInd, use = use)
      } else {
        ret <- selectInd(pop = slot(x, caste), nInd = nInd, use = use)
      }
    }
  } else if (isColonies(x)) {
    # Could have called Colony method for every colony of x, but the code below will be faster
    if (is.null(nInd)) {
      if (caste == "fathers") {
        ret <- lapply(X = x@colonies, FUN = function(z) z@queen@misc$fathers)
      } else {
        ret <- lapply(X = x@colonies, FUN = function(z) slot(z, caste))
      }
    } else {
      if (caste == "fathers") {
        ret <- lapply(X = x@colonies, FUN = function(z) selectInd(pop = z@queen@misc$fathers, nInd = nInd, use = use))
      } else {
        ret <- lapply(X = x@colonies, FUN = function(z) selectInd(pop = slot(z, caste), nInd = nInd, use = use))
      }
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCaste Access the queen
#' @export
getQueen <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- getCaste(x, caste = "queen")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCaste Access fathers (drones the queen mated with)
#' @export
getFathers <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- getCaste(x, caste = "fathers", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCaste Access virgin queens
#' @export
getVirginQueens <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- getCaste(x, caste = "virgin_queens", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCaste Access workers
#' @export
getWorkers <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- getCaste(x, caste = "workers", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCaste Access drones
#' @export
getDrones <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- getCaste(x, caste = "drones", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname createFounderDrones
#' @title Creates drones from base population
#'
#' @description Creates population of drones from base population.
#'       \Drones are created as double haploids. Founder drones are used when crating colonies to be used as fathers.
#'
#' @param pop AlphaSimRBee Colony object from the \code{createColony(...)}
#' @param nDronesPerQueen Integer, number of drones to create
#'
#' @examples
#' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderPop)
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

createFounderDrones <- function(pop, nDronesPerQueen = 100) {
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  drones <- makeDH(pop, nDH = nDronesPerQueen)
  return(drones)
}

#' @rdname createWorkers
#' @title Creates workers of the colony
#'
#' @description Creates the specified number of workers in the colony
#'       \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#'
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, number of workers to create
#'
#' @examples
#' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderPop)
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

createWorkers <- function(colony, nInd) {
  if (!isColony(colony)) {
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
#'
#' @description Creates the specified number of drones in the colony
#'       \as double haploids from the current queen \code{colony@queen}.
#'
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, the umber of drones to create.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderPop)
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
#' @return AlphaSim population object of created drones
#'
#' @export
createDrones <- function(colony, nInd) {
  if (!isColony(colony)) {
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
#'
#' @description Creates the specified number of virgin queens in the colony
#'        \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#'
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, the number of virgin queens to create.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderPop)
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
#' @return AlphaSim population object of created virgin queens
#'
#' @export
createVirginQueens <- function(colony, nInd) {
  if (!isColony(colony)) {
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
#' @param nInd numeric, number of drones to access, if \code{NULL} all drones
#' are accessed, otherwise a random sample
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
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
#' @return Pop
#'
#' @export
createDCA <- function(x, nInd = NULL) {
  if (isColony(x)) {
    DCA <- getDrones(x, nInd = nInd)
  } else if (isColonies(x)) {
    DCA <- getDrones(x, nInd = nInd)
    DCA <- mergePops(popList = DCA)
  } else {
    stop("Argument x must be a Colony of Colonies class object!")
  }
  return(DCA)
}

#' @rdname pullInd
#' @title Pull individuals from a population
#'
#' @description Pull individuals from a population and update the population
#'
#' @param pop Pop
#' @param nInd numeric, number of individuals to pull, if \code{NULL} pull all
#' individuals
#' @param use character, all options provided by \code{\link{selectInd}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' pullInd(basePop, nInd = 2)
#' pullInd(basePop, nInd = 3)
#' pullInd(basePop)
#'
#' @return Pop
#'
#' @export
pullInd <- function(pop, nInd = NULL, use = "rand") {
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  if (is.null(nInd)) {
    nInd <- nInd(pop)
  }
  pulled <- selectInd(pop = pop, nInd = nInd, use = use)
  sel <- pop@id %in% pulled@id
  remainder <- pop[!sel]
  ret <- list(pulled = pulled, remainder = remainder)
  return(ret)
}

#' @rdname pullDroneGroupsFromDCA
#' @title Pulls drone groups from a Drone Congregation Area (DCA)
#'
#' @description Pulls drone groups from a Drone Congregation Area (DCA) to use
#' them later in mating. Number of drones per group is sampled from a Poisson
#' distribution with average group size. Pulled drones are removed from the DCA.
#'
#' @param DCA Pop, population of drones
#' @param nGroups Integer, number of drone groups to be created
#' @param avgGroupSize Numeric, average number of drones per group
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#' apiary <- c(colony1, colony2)
#' DCA <- createDCA(apiary)
#' pullDroneGroupsFromDCA(DCA, nGroup = 4, avgGroupSize = 5)
#'
#' @return list of Pop
#'
#' @export
pullDroneGroupsFromDCA <- function(DCA, nGroup, avgGroupSize = 17) {
  if (!isPop(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  nDrones <- rpois(n = nGroup, lambda = avgGroupSize)
  if (sum(nDrones) > nInd(DCA)) {
    stop("Not enough drones in the DCA!")
  }
  ret <- vector(mode = "list", length = nGroup)
  for (group in 1:nGroup) {
    tmp <- pullInd(pop = DCA, nInd = nDrones[group])
    ret[[group]] <- tmp$pulled
    DCA <- tmp$remainder
  }
  return(ret)
}

#' @rdname pullCaste
#' @title Pull individuals from a caste in a colony
#'
#' @description Pull individuals from a caste in a colony. These individuals are
#' removed from the colony.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to pull, if \code{NULL} all individuals
#' are pulled
#' @param use character, all options provided by \code{\link{selectInd}}
#'
#' @seealso \code{\link{pullQueen}}, \code{\link{pullVirginQueens}},
#' \code{\link{pullWorkers}}, and \code{\link{pullDrones}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addVirginQueens(colony1, nInd = 10)
#' colony2 <- addVirginQueens(colony1, nInd = 5)
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 5)
#' colony2 <- addDrones(colony2, nInd = 6)
#' pullCaste(colony1, caste = "queen")
#' pullQueen(colony1)
#'
#' pullCaste(colony1, caste = "virgin_queens")
#' pullCaste(colony1, caste = "virgin_queens", nInd = 2)
#' pullVirginQueens(colony1)
#' pullVirginQueens(colony1, nInd = 2)
#'
#' pullCaste(colony1, caste = "workers")
#' pullCaste(colony1, caste = "workers", nInd = 5)
#' pullWorkers(colony1)
#' pullWorkers(colony1, nInd = 5)
#'
#' pullCaste(colony1, caste = "drones")
#' pullCaste(colony1, caste = "drones", nInd = 5)
#' pullDrones(colony1)
#' pullDrones(colony1, nInd = 5)
#'
#' apiary <- c(colony1, colony2)
#'
#' pullCaste(apiary, caste = "queen")
#' pullQueen(apiary)
#' nQueen(apiary)
#' nQueen(pullQueen(apiary)$colonies)
#'
#' pullCaste(apiary, caste = "virgin_queens")
#' pullVirginQueens(apiary)
#' nVirginQueens(apiary)
#' nVirginQueens(pullVirginQueens(apiary)$colonies)
#' nVirginQueens(pullVirginQueens(apiary, nInd = 5)$colonies)
#'
#' pullCaste(apiary, caste = "workers")
#' pullWorkers(apiary)
#' nWorkers(apiary)
#' nWorkers(pullWorkers(apiary)$colonies)
#' nWorkers(pullWorkers(apiary, nInd = 5)$colonies)
#'
#' pullCaste(apiary, caste = "drones")
#' pullDrones(apiary)
#' nDrones(apiary)
#' nDrones(pullDrones(apiary)$colonies)
#' nDrones(pullDrones(apiary, nInd = 5)$colonies)
#'
#' @return list of Pop and Colony when \code{x} is Colony and list of (a list of
#' Pop (named by colony id) and Colonies) when \code{x} is Colonies
#'
#' @export
pullCaste <- function(x, caste, nInd = NULL, use = "rand") {
  if (isColony(x)) {
    if (is.null(slot(x, caste))) {
      ret <- list(pulled = NULL, colony = x)
    } else {
      if (is.null(nInd)) {
        nInd <- nInd(slot(x, caste))
      }
      tmp <- pullInd(pop = slot(x, caste), nInd = nInd, use = use)
      slot(x, caste) <- tmp$remainder
      ret <- list(pulled = tmp$pulled, colony = x)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("pulled", "colonies")
    ret$pulled <- vector(mode = "list", length = nCol)
    names(ret$pulled) <- getId(x)
    ret$colonies <- x
    for (colony in 1:nCol) {
      tmp = pullCaste(x = x@colonies[[colony]], caste = caste, nInd = nInd, use = use)
      ret$pulled[[colony]] <- tmp$pulled
      ret$colonies@colonies[[colony]] <- tmp$colony
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn pullCaste Pull queen from a colony
#' @export
pullQueen <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- pullCaste(x, caste = "queen")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn pullCaste Pull virgin queens from a colony
#' @export
pullVirginQueens <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- pullCaste(x, caste = "virgin_queens", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn pullCaste Pull workers from a colony
#' @export
pullWorkers <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- pullCaste(x, caste = "workers", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn pullCaste Pull drones from a colony
#' @export
pullDrones <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- pullCaste(x, caste = "drones", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname crossVirginQueen
#' @title Crosses a virgin queen to a group drones
#'
#' @description Crosses a virgin queen to a group of drones
#'
#' @param virginQueen AlphaSimR population object
#' @param fathers AlphaSimR population class.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderPop)
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
#' droneGroups <- pullDroneGroupsFromDCA(DCA, nGroup = 7, avgGroupSize = 19)
#'
#'# Cross the virgin queen
#'
#' virginQueen <- colony1@virgin_queens
#' queen <- crossVirginQueen(virginQueen, fathers = droneGroups[[1]])
#'
#' @return AlphaSim population object of a mated queen
#'
#' @export
crossVirginQueen <- function(virginQueen, fathers) {
  if (!isPop(virginQueen)) {
    stop("Argument virginQueen must be a Pop class object!")
  }
  if (!isPop(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  if (isQueenMated(virginQueen)) {
    stop("The queen is mated already!")
  }
  if (is.null(fathers)) {
    stop("Missing fathers!")
  }
  if (nInd(virginQueen) > 1) {
    stop("#TODO: Expand the function to mate multiple virgin queens at once!")
  }
  virginQueen@misc$fathers <- fathers
  return(virginQueen)
}

#' @rdname setQueensYOB
#' @title Set the queen's year of birth
#'
#' @description Set the queen's year of birth
#'
#' @param x Pop, Colony, or Colonies
#' @param year integer, the year of the birth of the queen
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#'
#' getQueensYOB(getQueen(colony1))
#' getQueensYOB(colony1)
#' getQueensYOB(apiary)
#'
#' queen <- getQueen(colony1)
#' queen <- setQueensYOB(queen, year = 2021)
#' getQueensYOB(queen)
#'
#' colony1 <- setQueensYOB(colony1, year = 2021)
#' getQueensYOB(colony1)
#'
#' apiary <- setQueensYOB(apiary, year = 2021)
#' getQueensYOB(apiary)
#'
#' queen <- setQueensYearOfBirth(queen, year = 2022)
#' getQueensYearOfBirth(queen)
#'
#' colony1 <- setQueensYearOfBirth(colony1, year = 2022)
#' getQueensYearOfBirth(colony1)
#'
#' apiary <- setQueensYearOfBirth(apiary, year = 2022)
#' getQueensYearOfBirth(apiary)
#'
#' @return Pop, Colony, or Colonies
#'
#' @export
setQueensYearOfBirth <- setQueensYOB <- function(x, year) {
  if (isPop(x)) {
    x@misc$yearOfBirth <- year
  } else if (isColony(x)) {
    if (!is.null(x@queen)) {
      x@queen@misc$yearOfBirth <- year
    } else {
      stop("Missing queen!") # TODO: should this be a warning?
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in 1:nCol) {
      x@colonies[[colony]]@queen@misc$yearOfBirth <- year
    }
  } else {
    stop("Argument x must be a Pop, Colony or Colonies class object!")
  }
  return(x)
}
