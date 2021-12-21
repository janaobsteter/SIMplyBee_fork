# Level 1 Pop Functions

#' @rdname getCaste
#' @title Access individuals of a caste
#'
#' @description Level 1 function that returns individuals of a caste. These
#'   individuals stay in the colony (compared to \code{\link{pullCaste}}).
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed; if there are less individuals than requested,
#'   we return the ones available - this can also be just \code{NULL}
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
#'
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 4)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addWorkers(colony2, nInd = 20)
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
#' getFathers(getQueen(colony1))
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
#' getVirginQueens(apiary, nInd = 2)
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
#' getDrones(apiary)[[2]]
#' getDrones(apiary, nInd = 2)
#'
#' @return Pop when \code{x} is Colony, but \code{NULL} if caste is not present,
#'   and list of Pop when \code{x} is Colonies, named by colony id when \code{x}
#'   is Colonies
#'
#' @export
getCaste <- function(x, caste, nInd = NULL, use = "rand") {
  if (isColony(x)) {
    if (caste == "fathers") {
      pop <- x@queen@misc[[1]]$fathers
    } else {
      pop <- slot(x, caste)
    }
    if (is.null(pop)) {
      ret <- NULL
    } else {
      if (is.null(nInd)) {
        nInd <- nInd(pop)
      }
      nIndRequested <- nInd
      nIndAvailable <- nInd(pop)
      if (nIndRequested > nIndAvailable) {
        nIndRequested <- nIndAvailable
      }
      ret <- selectInd(pop = pop, nInd = nIndRequested, use = use)
    }
  } else if (isColonies(x)) {
    ret <- lapply(X = x@colonies, FUN = getCaste, caste = caste, nInd = nInd, use = use)
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
    ret <- getCaste(x, caste = "queen", nInd = 1)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCaste Access fathers (drones the queen mated with)
#' @export
getFathers <- function(x, nInd = NULL, use = "rand") {
  if (isPop(x)) {
    ret <- lapply(X = x@misc,
                  FUN = function(z) {
                    if (is.null(z$fathers)) {
                      ret <- NULL
                    } else {
                      if (is.null(nInd)) {
                        n <- nInd(z$fathers)
                      }
                      ret <- selectInd(pop = z$fathers, nInd = n, use = use)
                    }
                    return(ret)
                  })
    if (nInd(x) == 1) {
      ret <- ret[[1]]
    }
  } else if (isColony(x) | isColonies(x)) {
    ret <- getCaste(x, caste = "fathers", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
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

#' @rdname createWorkers
#' @title Creates workers from the colony
#'
#' @description Level 1 function that creates the specified number of workers
#'   from the colony by mating the colony queen and the fathers. If csd locus is
#'   defined, it takes it into account and any csd homozygotes are removed and
#'   counted as homozygous drones.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of workers to create
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two nodes named \code{workers} (a \code{\link{Pop-class}})
#'   and \code{pHomBrood} (an integer)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' createWorkers(colony, nInd = 10)
#'
#' @export
createWorkers <- function(colony, nInd, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (!isQueenPresent(colony)) {
    stop("Missing queen!")
  }
  if (!isQueenMated(colony)) {
    stop("Unmated queen!")
  }
  ret <- vector(mode = "list", length = 2)
  names(ret) <- c("workers", "pHomBrood")
  workers <- randCross2(females = getQueen(colony),
                        males = getFathers(colony),
                        nCrosses = nInd)
  if (is.null(simParamBee$csdChr)) {
    ret$workers <- workers
    ret$pHomBrood <- 0
  } else {
    sel <- isCsdHeterozygous(pop = workers, simParamBee = simParamBee)
    ret$workers <- workers[sel]
    ret$pHomBrood <- as.integer(nInd - sum(sel)) / workers@nInd
  }
  return(ret)
}

#' @describeIn createWorkers Create virgin queens
# TODO: explore options for implementing difference between workers' and queens'
#       patrilines - see https://github.com/HighlanderLab/SIMplyBee/issues/78
#' @export
createVirginQueens <- function(colony, nInd, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  ret <- createWorkers(colony = colony, nInd = nInd, simParamBee = simParamBee)
  names(ret) <- c("virgin_queens", "pHomBrood")
  return(ret)
}

#' @rdname createFounderDrones
#' @title Creates drones from a founding (base) population
#'
#' @description Level 1 function that creates drones from a founding (base)
#'   population by doubling recombined genomes of each founding individual -
#'   mimicking a queen laying drones. Such drones are usually used as fathers
#'   when creating colonies.
#'
#' @param pop \code{\link{Pop-class}}
#' @param nDronesPerQueen integer, number of drones to create per founding
#'   individual (a substitute for a queen).
#'
#' @details Note that this function creates \code{nDronesPerQueen} for each
#'   individual in the \code{pop}, which will amount to
#'   \code{nInd(pop) * nDronesPerQueen} drones - this can be slow, so tune both
#'   numbers according to your needs.
#'   TODO The drones will eventually be made properly haploid!
#'        Follow https://github.com/HighlanderLab/SIMplyBee/issues/24
#'
#' @return \code{\link{Pop-class}} with drones
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' createWorkers(colony, nInd = 10)
#' createDrones(colony, nInd = 2)
#'
#' @export
createFounderDrones <- function(pop, nDronesPerQueen = 100) {
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  drones <- makeDH(pop, nDH = nDronesPerQueen)
  return(drones)
}

#' @rdname createDrones
#' @title Creates drones from the colony
#'
#' @description Level 1 function that creates the specified number of drones
#'   from the colony. Currently this is done by creating doubled-haploids from a
#'   queen (generating recombinant gametes and doubling them).
#'   TODO The drones will eventually be made properly haploid!
#'        Follow https://github.com/HighlanderLab/SIMplyBee/issues/24
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, the number of drones to create
#'
#' @return \code{\link{Pop-class}} with drones
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' createWorkers(colony, nInd = 10)
#' createDrones(colony, nInd = 2)
#'
#' @export
createDrones <- function(colony, nInd) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (!isQueenPresent(colony)) {
    stop("Missing queen!")
  }
  drones <- makeDH(pop = getQueen(colony), nDH = nInd)
  return(drones)
}

#' @rdname createDCA
#' @title Create a drone congregation area (DCA)
#'
#' @description Level 1 function that creates a population of drones from one or
#'   multiple colonies. Such a population is often referred to as a drone
#'   congregation area (DCA).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param nInd numeric, number of random drones to pull from each colony,
#'   if \code{NULL} all drones in a colony are pulled
#'
#' @return \code{\link{Pop-class}}
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
#' @description Level 1 function that pulls individuals from a population and
#'   update the population (these individuals don't stay in a population).
#'
#' @param pop \code{\link{Pop-class}}
#' @param nInd numeric, number of individuals to pull, if \code{NULL} pull all
#'   individuals
#' @param use character, all options provided by \code{\link{selectInd}}
#'
#' @return list with a node \code{pulled} holding \code{\link{Pop-class}} of
#'   pulled individuals and a node \code{remainder)} holding \code{\link{Pop-class}}
#'   of remaining individuals
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
#' @description Level 1 function that pulls drone groups from a Drone
#'   Congregation Area (DCA) to use them later in mating. Number of drones per
#'   group is sampled from a truncated Poisson distribution parameterised with
#'   an average group size and truncation at zero (= zeroes are excluded).
#'   Drones are pulled (removed) from the DCA to reflect the fact that drones
#'   die after mating, so they can't be present in the DCA anymore.
#'
#' @param DCA \code{\link{Pop-class}}, population of drones
#' @param nGroup integer, number of drone groups to be created
#' @param avgGroupSize numeric, average number of drones per group
#'
#' @return list of \code{\link{Pop-class}}
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
#' @export
pullDroneGroupsFromDCA <- function(DCA, nGroup, avgGroupSize = 17) {
  if (!isPop(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  nDrones <- extraDistr::rtpois(n = nGroup, lambda = avgGroupSize, a = 0)
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
#' @description Level 1 function that pulls individuals from a caste in a
#'   colony. These individuals are removed from the colony (compared to
#'   \code{\link{getCaste}}).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to pull, if \code{NULL} all
#'   individuals are pulled
#' @param use character, all options provided by \code{\link{selectInd}}
#'
#' @seealso \code{\link{pullQueen}}, \code{\link{pullVirginQueens}},
#'   \code{\link{pullWorkers}}, and \code{\link{pullDrones}}
#'
#' @return list of \code{\link{Pop-class}} and \code{\link{Colony-class}}
#'   when \code{x} is \code{\link{Colony-class}} and list of (a list of
#'   \code{\link{Pop-class}} named by colony id) and
#'   \code{\link{Colonies-class}} when \code{x} is
#'   \code{\link{Colonies-class}}
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
#' nQueens(apiary)
#' nQueens(pullQueen(apiary)$colonies)
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
#' @title Cross (mate) a virgin queen to a group drones
#'
#' @description Level 1 function that crossses (mates) a virgin queen to a group
#'   of drones. This function does not create any progeny, it only stores the
#'   mated drones (fathers) so we can later create progeny as needed.
#'
#' @param pop \code{\link{Pop-class}}, one or more virgin queens to be mated
#' @param fathers \code{\link{Pop-class}}, a group of drones that will be mated
#'   with virgin queen(s); if there is more than one virgin queen, then the
#'   \code{fathers} are partitioned into multiple groups of average size of
#'   \code{nAvgFathers} using \code{\link{pullDroneGroupsFromDCA}}
#' @param nAvgFathers numeric, average number of drones (fathers) used in mating
#'   the virgin queen(s) - currently active only when multiple virgin queens
#'   provided
#'
#' @seealso \code{\link{Colony-class}} on how we store the fathers along the
#'   queen.
#'
#' @return \code{\link{Pop-class}} with mated queen(s)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' (matedQueen1 <- crossVirginQueen(pop = basePop[2], fathers = drones[1:5]))
#' isQueenMated(basePop[2])
#' isQueenMated(matedQueen1)
#' nFathers(matedQueen1)
#' getFathers(matedQueen1)@id
#' (matedQueen2 <- crossVirginQueen(pop = basePop[3], fathers = drones[6:10]))
#' isQueenMated(basePop[3])
#' isQueenMated(matedQueen2)
#' nFathers(matedQueen2)
#' getFathers(matedQueen2)@id
#'
#' matedQueens <- crossVirginQueen(pop = basePop[2:3],
#'                                 fathers = drones[1:10], nAvgFathers = 2)
#' matedQueens
#' isQueenMated(matedQueens)
#' nFathers(matedQueens)
#' getFathers(matedQueens)
#'
#' @export
crossVirginQueen <- function(pop, fathers, nAvgFathers) {
  # TODO: set nAvgFathers to NULL by default and then grab the value from
  #       SimParamBee
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  if (any(isQueenMated(pop))) {
    stop("One of the queens in pop is already mated!")
  }
  if (!isPop(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  nVirginQueen <- nInd(pop)
  if (nVirginQueen == 1) {
    # TODO: do we take all provided fathers, specified nAvgFathers, or default nAvgFathers from SimParam when nAvgFathers = NULL?
    pop@misc[[1]]$fathers <- fathers
  } else {
    fathers <- pullDroneGroupsFromDCA(DCA = fathers,
                                      nGroup = nVirginQueen,
                                      avgGroupSize = nAvgFathers)
    for (queen in 1:nVirginQueen) {
      pop@misc[[queen]]$fathers <- fathers[[queen]]
    }
  }
  return(pop)
}

#' @rdname setQueensYOB
#' @title Set the queen's year of birth
#'
#' @description Level 1 function that sets the queen's year of birth.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#' @param year integer, the year of the birth of the queen
#'
#' @return \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
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
#' @export
setQueensYearOfBirth <- setQueensYOB <- function(x, year) {
  if (isPop(x)) {
    # TODO: expand to more than 1 queen
    x@misc[[1]]$yearOfBirth <- year
  } else if (isColony(x)) {
    if (isQueenPresent(x)) {
      x@queen@misc[[1]]$yearOfBirth <- year
    } else {
      stop("Missing queen!") # TODO: should this be a warning?
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in 1:nCol) {
      x@colonies[[colony]]@queen@misc[[1]]$yearOfBirth <- year
    }
  } else {
    stop("Argument x must be a Pop, Colony or Colonies class object!")
  }
  return(x)
}
