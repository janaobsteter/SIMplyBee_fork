# Level 1 Pop Functions

#' @rdname getCaste
#' @title Access individuals of a caste
#'
#' @description Access individuals of a caste. These individuals stay in the
#' colony.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#'
#' @seealso \code{\link{getQueen}}, \code{\link{getFathers}}, \code{\link{getVirginQueens}},
#' \code{\link{getWorkers}}, and \code{\link{getDrones}}
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
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' getCaste(colony1, caste = "queen")
#' getCaste(colony1, caste = "fathers")
#' getCaste(colony1, caste = "fathers", nInd = 2)
#' getCaste(colony1, caste = "fathers", nInd = 2)@id
#' getCaste(colony1, caste = "fathers", nInd = 2)@id
#' getCaste(colony1, caste = "virgin_queens")
#' getCaste(colony1, caste = "workers")
#' getCaste(colony1, caste = "drones")
#'
#' apiary <- c(colony1, colony2)
#' getCaste(apiary, caste = "queen")
#' getCaste(apiary, caste = "queen")[[1]]@id
#' getCaste(apiary, caste = "queen")[[2]]@id
#' getCaste(apiary, caste = "fathers")
#' getCaste(apiary, caste = "virgin_queens")
#' getCaste(apiary, caste = "workers")
#' getCaste(apiary, caste = "drones")
#'
#' @return Pop when \code{x} is Colony and list of Pop when \code{x} is Colonies,
#' named by colony id when \code{x} is Colonies
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
    # Could have called Colony method for every colony of x, but the code below will be faster
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
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getQueen
#' @title Access the queen
#'
#' @description Access the queen of a colony. The queen stays in the colony.
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
#' @return Pop when \code{x} is Colony and list of Pop when \code{x} is Colonies,
#' named by colony id when \code{x} is Colonies
#'
#' @export
getQueen <- function(x) {
  ret <- getCaste(x, caste = "queen")
  return(ret)
}

#' @rdname getFathers
#' @title Access fathers
#'
#' @description Access fathers (drones the queen mated with). These fathers stay
#' in the colony.
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
#' @return Pop when \code{x} is Colony and list of Pop when \code{x} is Colonies,
#' named by colony id when \code{x} is Colonies
#'
#' @export
getFathers <- function(x, nInd = NULL) {
  ret <- getCaste(x, caste = "fathers", nInd = nInd)
  return(ret)
}

#' @rdname getVirginQueens
#' @title Access virgin queens
#'
#' @description Access virgin queens. These virgin queens stay in the colony.
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
#' colony2 <- addVirginQueens(colony2, nVirginQueens = 10)
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
#' @return Pop when \code{x} is Colony and list of Pop when \code{x} is Colonies,
#' named by colony id when \code{x} is Colonies
#'
#' @export
getVirginQueens <- function(x, nInd = NULL) {
  ret <- getCaste(x, caste = "virgin_queens", nInd = nInd)
  return(ret)
}

#' @rdname getWorkers
#' @title Access workers
#'
#' @description Access workers. These workers stay in the colony.
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
#' @return Pop when \code{x} is Colony and list of Pop when \code{x} is Colonies,
#' named by colony id when \code{x} is Colonies
#'
#' @export
getWorkers <- function(x, nInd = NULL) {
  ret <- getCaste(x, caste = "workers", nInd = nInd)
  return(ret)
}

#' @rdname getDrones
#' @title Access drones
#'
#' @description Access drones. These drones stay in the colony.
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
#' @return Pop when \code{x} is Colony and list of Pop when \code{x} is Colonies,
#' named by colony id when \code{x} is Colonies
#'
#' @export
getDrones <- function(x, nInd = NULL) {
  ret <- getCaste(x, caste = "drones", nInd = nInd)
  return(ret)
}

#' @rdname crateFounderDrones
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
#'
#' @description Creates the specified number of workers in the colony
#'       \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#'
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
#'
#' @description Creates the specified number of drones in the colony
#'       \as double haploids from the current queen \code{colony@queen}.
#'
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
#' @return AlphaSim population object of created drones
#'
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
#'
#' @description Creates the specified number of virgin queens in the colony
#'        \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#'
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
#' @return AlphaSim population object of created virgin queens
#'
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
#' @return Pop
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
#' @return Pop
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
#' @return list of Pop
#'
#' @export
pullDroneGroupsFromDCA <- function(DCA, nGroup, avgGroupSize) {
  if (!"Pop" %in% class(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  nDrones <- rpois(n = nGroup, lambda = avgGroupSize)
  if (sum(nDrones) > nInd(DCA)) {
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

#' @rdname pullCaste
#' @title Pull individuals from a caste in a colony
#'
#' @description Pull individuals from a caste in a colony. These individuals are
#' removed from the colony.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to pull, if NULL all individuals
#' are pulled, otherwise a random sample
#'
#' @seealso \code{\link{pullQueen}}, \code{\link{pullVirginQueens}},
#' \code{\link{pullWorkers}}, and \code{\link{pullDrones}}
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
#' colony2 <- addDrones(colony2, nInd = 5)
#' pullCaste(colony1, caste = "queen")
#' pullCaste(colony1, caste = "virgin_queens")
#' pullCaste(colony1, caste = "workers")
#' pullCaste(colony1, caste = "workers", nInd = 5)
#' pullCaste(colony1, caste = "drones")
#' pullCaste(colony2, caste = "drones")
#'
#' apiary <- c(colony1, colony2)
#' pullCaste(apiary, caste = "queen")
#' pullCaste(apiary, caste = "virgin_queens")
#' pullCaste(apiary, caste = "workers")
#' nWorkers(pullCaste(apiary, caste = "workers")$colonies)
#' nWorkers(pullCaste(apiary, caste = "workers", nInd = 5)$colonies)
#' pullCaste(apiary, caste = "drones")
#' nDrones(pullCaste(apiary, caste = "drones")$colonies)
#'
#' @return list of Pop and Colony when \code{x} is Colony and list of (a list of
#' Pop (named by colony id) and Colonies) when \code{x} is Colonies
#'
#' @export
pullCaste <- function(x, caste, nInd = NULL) {
  if ("Colony" %in% class(x)) {
    if (is.null(slot(x, caste))) {
      ret <- list(pulled = NULL, colony = x)
    } else {
      if (is.null(nInd)) {
        nInd <- nInd(slot(x, caste))
      }
      tmp <- pullIndFromPop(pop = slot(x, caste), nInd = nInd)
      slot(x, caste) <- tmp$remainder
      ret <- list(pulled = tmp$pulled, colony = x)
    }
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("pulled", "colonies")
    ret$pulled <- vector(mode = "list", length = nCol)
    names(ret$pulled) <- getId(x)
    ret$colonies <- x
    for (colony in 1:nCol) {
      tmp = pullCaste(x = x@colonies[[colony]], caste = caste, nInd = nInd)
      ret$pulled[[colony]] <- tmp$pulled
      ret$colonies@colonies[[colony]] <- tmp$colony
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname pullQueen
#' @title Pull queen from a colony
#'
#' @description Pull queen from a colony. The queen is removed from the colony.
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
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony2 <- addDrones(colony2, nInd = 5)
#' pullQueen(colony1)
#'
#' apiary <- c(colony1, colony2)
#' pullQueen(apiary)
#' nQueen(apiary)
#' nQueen(pullQueen(apiary)$colonies)
#'
#' @return list of Pop and Colony when \code{x} is Colony and list of (a list of
#' Pop (named by colony id) and Colonies) when \code{x} is Colonies
#'
#' @export
pullQueen <- function(x) {
  ret <- pullCaste(x, caste = "queen")
  return(ret)
}

#' @rdname pullVirginQueens
#' @title Pull virgin queens from a colony
#'
#' @description Pull virgin queen from a colony. These virgin queens are
#' removed from the colony.
#'
#' @param x Colony or Colonies
#' @param nInd numeric, number of virgin queens to pull, if NULL all virgin queens
#' are pulled, otherwise a random sample
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
#' colony1 <- addVirginQueens(colony1, nInd = 10)
#' pullVirginQueens(colony1)
#' pullVirginQueens(colony1, nInd = 2)
#' pullVirginQueens(colony2)
#'
#' apiary <- c(colony1, colony2)
#' pullVirginQueens(apiary)
#' nVirginQueens(apiary)
#' nVirginQueens(pullVirginQueens(apiary)$colonies)
#'
#' @return list of Pop and Colony when \code{x} is Colony and list of (a list of
#' Pop (named by colony id) and Colonies) when \code{x} is Colonies
#'
#' @export
pullVirginQueens <- function(x, nInd = NULL) {
  ret <- pullCaste(x, caste = "virgin_queens", nInd = nInd)
  return(ret)
}


#' @rdname pullWorkers
#' @title Pull workers from a colony
#'
#' @description Pull workers from a colony. These workers are removed from the
#' colony.
#'
#' @param x Colony or Colonies
#' @param nInd numeric, number of workers to pull, if NULL all workers
#' are pulled, otherwise a random sample
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
#' pullWorkers(colony1)
#' pullWorkers(colony1, nInd = 5)
#' pullWorkers(colony2)
#'
#' apiary <- c(colony1, colony2)
#' pullWorkers(apiary)
#' nWorkers(apiary)
#' nWorkers(pullWorkers(apiary)$colonies)
#'
#' @return list of Pop and Colony when \code{x} is Colony and list of (a list of
#' Pop (named by colony id) and Colonies) when \code{x} is Colonies
#'
#' @export
pullWorkers <- function(x, nInd = NULL) {
  ret <- pullCaste(x, caste = "workers", nInd = nInd)
  return(ret)
}

#' @rdname pullDrones
#' @title Pull drones from a colony
#'
#' @description Pull drones from a colony. These drones are removed from the
#' colony.
#'
#' @param x Colony or Colonies
#' @param nInd numeric, number of drones to pull, if NULL all drones
#' are pulled, otherwise a random sample
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
#' pullDrones(colony1)
#' pullDrones(colony1, nInd = 5)
#' pullDrones(colony2)
#'
#' apiary <- c(colony1, colony2)
#' pullDrones(apiary)
#' nDrones(apiary)
#' nDrones(pullDrones(apiary)$colonies)
#'
#' @return list of Pop and Colony when \code{x} is Colony and list of (a list of
#' Pop (named by colony id) and Colonies) when \code{x} is Colonies
#'
#' @export
pullDrones <- function(x, nInd = NULL) {
  ret <- pullCaste(x, caste = "drones", nInd = nInd)
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
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
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
  if ("Pop" %in% class(x)) {
    x@misc$yearOfBirth <- year
  } else if ("Colony" %in% class(x)) {
    if (!is.null(x@queen)) {
      x@queen@misc$yearOfBirth <- year
    } else {
      stop("Missing queen!") # TODO: should this be a warning?
    }
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    for (colony in 1:nCol) {
      x@colonies[[colony]]@queen@misc$yearOfBirth <- year
    }
  } else {
    stop("Argument x must be a Pop, Colony or Colonies class object!")
  }
  return(x)
}
