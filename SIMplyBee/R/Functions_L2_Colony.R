# Level 2 Colony Functions

#' @rdname createColony
#' @title Create a new Colony
#'
#' @description Level 2 function that creates a new \code{\link{Colony-class}}
#'   to initiate simulations.
#'
#' @param location numeric, location of the colony as \code{c(x, y)}
#' @param queen \code{\link{Pop-class}} with one individual that will be the
#'   queen of the colony; error is thrown if more than one individual is given
#' @param yearOfBirth numeric, year of birth of the queen
#' @param fathers \code{\link{Pop-class}} with drones that the queen will mate
#'   with as part of this function (if she is already mated, a warning is given
#'   and the fathers argument is ignored)
#' @param virginQueens \code{\link{Pop-class}} with one or more individuals of
#'   which one will become the queen of the colony in the future
#'   TODO: think and explain what happens if we provide both a queen and virgin
#'   queens (possibly of different origin)!
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details When a mated queen is given or a queen alongside with fathers, the
#'   function also generates one virgin queen.
#'   TODO: discuss this - see below
#'
#' @return new \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones)
#' colony1
#'
#' colony2 <- createColony(virginQueens = basePop[3])
#' colony2
#'
#' @export
createColony <- function(location = NULL, queen = NULL, yearOfBirth = NULL,
                         fathers = NULL, virginQueens = NULL,
                         simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(queen)) {
    id <- as.character(NA)
  } else {
    if (!isPop(queen)) {
      stop("Argument queen must be a Pop class object!")
    }
    if (nInd(queen) > 1) {
      stop("You must provide just one queen for the colony!")
    }
    id <- queen@id
    if (!is.null(yearOfBirth)) {
      if (is.null(queen@misc[[1]]$yearOfBirth)) {
        queen <- setQueensYearOfBirth(x = queen, year = yearOfBirth)
      } else {
        warning("The queen already has the year of birth set - ignoring the yearOfBirth argument!")
      }
    }
    if (!is.null(fathers)) {
      if (!isPop(fathers)) {
        stop("Argument fathers must be a Pop class object!")
      }
      if (isQueenMated(queen)) {
        warning("The queen is already mated - ignoring the fathers argument!")
      } else {
        queen <- crossVirginQueen(pop = queen, fathers = fathers,
                                  simParamBee = simParamBee)
      }
    }
    if (!is.null(virginQueens)) {
      warning("You are providing the queen and virgin queen(s) at the same time.")
      warning("Are they properly related?")
    }
  }
  colony <- new(Class = "Colony",
                id = id,
                location = location,
                queen = queen,
                virginQueens = virginQueens)
  colony <- resetEvents(colony)
  validObject(colony)
  return(colony)
}

#' @rdname reQueenColony
#' @title Re-queen a colony
#'
#' @description Level 2 function that re-queens a colony adds a mated or a
#'   virgin queen, removes the previous queen, and changes colony id to the new
#'   mated queen.
#'
#' @param colony \code{\link{Colony-class}}
#' @param queen \code{\link{Pop-class}} with one individual that will be the
#'   queen of the colony; if she is not mated, she will be added as a virgin
#'   queen that will have to be mated later.
#' @param removeVirginQueens logical, remove existing virgin queens, default is
#'   \code{\link{TRUE}} since bee-keepers tend to remove any virgin queen cells
#'   to ensure the provided queen prevails. See also details.
#'
#' @details If the provided queen is mated, then she is saved in the queen slot
#'   of the colony. If she is not mated, then she is saved in the virgin queen
#'   slot (replacing any existing virgin queens) and once she is mated will be
#'   promoted to the queen of the colony.
#'
#' @return \code{\link{Colony-class}} with a new queen (see also details)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addVirginQueens(colony)
#' colony
#'
#' virginQueen <- basePop[3]
#' reQueenColony(colony, queen = virginQueen)
#'
#' matedQueen <- crossVirginQueen(pop = basePop[3], fathers = drones[6:10])
#' reQueenColony(colony, queen = matedQueen)
#'
#' @export
reQueenColony <- function(colony, queen, removeVirginQueens = TRUE) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if  (!isPop(queen)) {
    stop("Argument queen must be a Pop class object!")
  }
  if (isQueenMated(queen)) {
    if (nInd(queen) > 1) {
      stop("You must provide just one queen for the colony!")
    }
    colony@queen <- queen
    colony@id <- queen@id
    if (removeVirginQueens) {
      colony <- removeVirginQueens(colony)
    }
  } else {
    colony <- removeQueen(colony)
    colony@virginQueens <- queen
  }
  validObject(colony)
  return(colony)
}

#' @rdname addVirginQueens
#' @title Add (raise) virgin queens in the colony
#'
#' @description Level 2 function that adds (raises) the specified number of
#'   virgin queens in the colony by crossing the current queen and the fathers.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param nInd numeric or function, number of virgin queens to add; if
#'   \code{NULL} then \code{simParamBee$nVirginQueens} is used
#' @param new logical, should the virgin queens be added a fresh (ignoring
#'   currently present virgin queens in the colony)
#' @param year numeric, year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details \code{addVirginQueens} replaces any currently present virgin queens.
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   virgin queens added
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
#' addVirginQueens(colony1, nInd = 20)
#' nVirginQueens(addVirginQueens(apiary, nInd = 20))
#'
#' # Using a default in SP$nVirginQueens
#' # (just to have some workers - change this to your needs!)
#' addVirginQueens(colony1)
#' nVirginQueens(addVirginQueens(apiary))
#'
#' SP$nVirginQueens <- 15
#' addVirginQueens(colony1)
#' nVirginQueens(addVirginQueens(apiary))
#'
#' nVirginQueensFun <- function(colony) { rpois(n = 1, lambda = 15) }
#' addVirginQueens(colony1, nInd = nVirginQueensFun)
#' nVirginQueens(addVirginQueens(apiary, nInd = nVirginQueensFun))
#'
#' SP$nVirginQueens <- nVirginQueensFun
#' addVirginQueens(colony1)
#' nVirginQueens(addVirginQueens(apiary))
#'
#' @export
addVirginQueens <- function(x, nInd = NULL, new = FALSE, year = NULL,
                            simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (!isQueenPresent(x)) {
      stop("Missing queen!")
    }
    if (!isQueenMated(x)) {
      stop("Unmated queen!")
    }
    if (is.null(nInd)) {
      nInd <- simParamBee$nVirginQueens
    }
    if (is.function(nInd)) {
      nInd <- nInd(x)
    }
    if (nInd > 0) {
      newVirginQueens <- createVirginQueens(x = x, nInd = nInd, year = year,
                                            simParamBee = simParamBee)
      if (is.null(x@virginQueens) | new) {
        x@virginQueens <- newVirginQueens$virginQueens
        x@queen@misc[[1]]$pHomBrood <- newVirginQueens$pHomBrood
      } else {
        x@virginQueens <- c(x@virginQueens, newVirginQueens$virginQueens)
        # TODO: we need some scaling of the pHomBrood here and sticking pHomBrood into
        #       colony!
        #       see https://github.com/HighlanderLab/SIMplyBee/issues/104
        #           https://github.com/HighlanderLab/SIMplyBee/issues/80
        x@queen@misc[[1]]$pHomBrood <- (x@queen@misc[[1]]$pHomBrood + newVirginQueens$pHomBrood) / 2
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- addVirginQueens(x = x[[colony]], nInd = nInd, new = new,
                                     year = year, simParamBee = simParamBee)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname addWorkers
#' @title Add (raise) workers in the colony
#'
#' @description Level 2 function that adds (raises) the specified number of
#'   workers in the colony by crossing the current queen and the fathers. If
#'   there are already some workers present, new and present workers are
#'   combined.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param nInd numeric or function, number of workers; if \code{NULL} then
#'   \code{simParamBee$nWorkers} is used
#' @param new logical, should the workers be added a fresh (ignoring currently
#'   present workers in the colony)
#' @param exact logical, if the csd locus is turned on and exact is TRUE,
#' add the exact specified number of only
#' viable workers (heterozygous on the csd locus)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   workers added
#'
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' addWorkers(colony1, nInd = 20)
#' nWorkers(addWorkers(apiary, nInd = 20))
#'
#' # Using a default in SP$nWorkers
#' # (just to have some workers - change this to your needs!)
#' addWorkers(colony1)
#' nWorkers(addWorkers(apiary))
#'
#' SP$nWorkers <- 15
#' addWorkers(colony1)
#' nWorkers(addWorkers(apiary))
#'
#' nWorkersFun <- function(colony) { rpois(n = 1, lambda = 15) }
#' addWorkers(colony1, nInd = nWorkersFun)
#' nWorkers(addWorkers(apiary, nInd = nWorkersFun))
#'
#' SP$nWorkers <- nWorkersFun
#' addWorkers(colony1)
#' nWorkers(addWorkers(apiary))
#'
#' @export
addWorkers <- function(x, nInd = NULL, new = FALSE, exact = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.null(nInd)) {
      nInd <- simParamBee$nWorkers
    }
    if (is.function(nInd)) {
      nInd <- nInd(x)
    }
    if (nInd > 0) {
      newWorkers <- createWorkers(x, nInd, exact = exact, simParamBee = simParamBee)
      if (is.null(x@workers) | new) {
        x@workers <- newWorkers$workers
        x@queen@misc[[1]]$pHomBrood <- newWorkers$pHomBrood
      } else {
        x@workers <- c(x@workers, newWorkers$workers)
        # TODO: we need some scaling of the pHomBrood here and sticking pHomBrood into
        #       colony!
        #       see https://github.com/HighlanderLab/SIMplyBee/issues/104
        #           https://github.com/HighlanderLab/SIMplyBee/issues/80
        x@queen@misc[[1]]$pHomBrood <- (x@queen@misc[[1]]$pHomBrood + newWorkers$pHomBrood) / 2
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- addWorkers(x = x[[colony]], nInd = nInd, new = new,
                                exact = exact, simParamBee = simParamBee)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname addDrones
#' @title Add (raise) drones in the colony
#'
#' @description Level 2 function that adds (raises) the specified number of
#'   drones in the colony by crossing the current queen and the fathers. If
#'   there are already some drones present, new and present drones are combined.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param nInd numeric or function, number of drones; if \code{NULL} then
#'   \code{simParamBee$nDrones} is used
#' @param new logical, should the drones be added a fresh (ignoring currently
#'   present drones)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   drones added
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
#' addDrones(colony1, nInd = 20)
#' nDrones(addDrones(apiary, nInd = 20))
#'
#' # Using a default in SP$nWorkers
#' # (just to have some workers - change this to your needs!)
#' addDrones(colony1)
#' nDrones(addDrones(apiary))
#'
#' SP$nDrones <- 15
#' addDrones(colony1)
#' nDrones(addDrones(apiary))
#'
#' nDronesFun <- function(colony) { rpois(n = 1, lambda = 15) }
#' addDrones(colony1, nInd = nDronesFun)
#' nDrones(addDrones(apiary, nInd = nDronesFun))
#'
#' SP$nDrones <- nDronesFun
#' addDrones(colony1)
#' nDrones(addDrones(apiary))
#'
#' @export
addDrones <- function(x, nInd = NULL, new = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.null(nInd)) {
      nInd <- simParamBee$nDrones
    }
    if (is.function(nInd)) {
      nInd <- nInd(x)
    }
    if (nInd > 0) {
      newDrones <- createDrones(x, nInd)
      if (is.null(x@drones) | new) {
        x@drones <- newDrones
      } else {
        x@drones <- c(x@drones, newDrones)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- addDrones(x = x[[colony]], nInd = nInd, new = new,
                               simParamBee = simParamBee)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname buildUpColony
#' @title Build up colony by adding (raising) workers and drones
#'
#' @description Level 2 function that builds up colony by adding (raising)
#'   workers and drones usually in spring or after events such as split or
#'   swarming.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nWorkers numeric or function, number of worker; if \code{NULL} then
#'   \code{simParamBee$nWorkers} is used (unless \code{new = TRUE}, currently
#'   present workers are taken into account and only the missing difference is
#'   added)
#' @param nDrones numeric or function, number of drones; if \code{NULL} then
#'   \code{simParamBee$nDrones} is used (unless \code{new = TRUE}, currently
#'   present drones are taken into account so only the missing difference is
#'   added)
#' @param new logical, should the workers and drones be added a
#'   fresh (ignoring currently present workers and drones)
#' @param exact logical, if the csd locus is turned on and exact is TRUE,
#' create the exact specified number of only
#' viable workers (heterozygous on the csd locus)
#' @param resetEvents logical, call \code{\link{resetEvents}} as part of the
#'   build up
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details This function turns production of the colony to \code{TRUE}.
#'
#' @return \code{\link{Colony-class}} with workers and drones added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' isProductive(colony)
#'
#' # Using defaults in SP
#' # (just to have some bees - change this to your needs!)
#' (colony <- buildUpColony(colony))
#' isProductive(colony)
#'
#' colony <- buildUpColony(colony, nWorkers = 100)
#' colony # we are already at the target
#' colony <- buildUpColony(colony, nWorkers = 150)
#' colony # increasing the target
#' colony <- buildUpColony(colony, nWorkers = 100)
#' colony # we are already at the target
#' colony <- buildUpColony(colony, nWorkers = 100, new = TRUE)
#' colony # adding completely new workers & drones
#'
#' # Using functions
#' nWorkersFun <- function(colony) { rpois(n = 1, lambda = 100) }
#' nDronesFun <- function(colony) { rpois(n = 1, lambda = 15) }
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' buildUpColony(colony, nWorkers = nWorkersFun, nDrones = nDronesFun)
#' buildUpColony(colony, nWorkers = nWorkersFun, nDrones = nDronesFun)
#'
#' # Using functions in simParamBee
#' SP$nWorkers <- nWorkersFun
#' SP$nDrones <- nDronesFun
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' buildUpColony(colony)
#' buildUpColony(colony)
#'
#' @export
buildUpColony <- function(colony, nWorkers = NULL, nDrones = NULL,
                          new = FALSE, exact = FALSE, resetEvents = FALSE,
                          simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }

  # Workers
  if (is.null(nWorkers)) {
    nWorkers <- simParamBee$nWorkers
  }
  if (is.function(nWorkers)) {
    nWorkers <- nWorkers(colony)
  }
  if (new) {
    n <- nWorkers
  } else {
    n <- nWorkers - nWorkers(colony)
  }
  if (n > 0) {
    colony <- addWorkers(x = colony, nInd = n, new = new,
                         exact = exact, simParamBee = simParamBee)
  }

  # Drones
  if (is.null(nDrones)) {
    nDrones <- simParamBee$nDrones
  }
  if (is.function(nDrones)) {
    nDrones <- nDrones(colony)
  }
  if (new) {
    n <- nDrones
  } else {
    n <- nDrones - nDrones(colony)
  }
  if (n > 0) {
    colony <- addDrones(x = colony, nInd = n, new = new,
                        simParamBee = simParamBee)
  }

  # Events
  if (resetEvents) {
    colony <- resetEvents(colony)
  }
  colony@production <- TRUE
  # TODO: call some sort of finalise function that will guide what happens next
  #       with the colony?

  validObject(colony)
  return(colony)
}

#' @rdname replaceVirginQueens
#' @title Replace a proportion of virgin queens with new ones
#'
#' @description Level 2 function that replaces a proportion of virgin queens with new
#'   virgin queens from the colony. Useful after events like season change, swarming,
#'   supersedure, etc. due to the short life span of the virgin queens.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion of virgin queens to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of virgin queens that stay when \code{p < 1}
#' @param year numeric, year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or  or \code{\link{Colonies-class}} with
#'   replaced virgin queens
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
#' colony1 <- addVirginQueens(colony1, nInd = 20)
#' apiary <- addVirginQueens(apiary, nInd = 20)
#'
#' getVirginQueens(colony1)@id
#' colony1 <- replaceVirginQueens(colony1, p = 0.5)
#' getVirginQueens(colony1)@id
#' colony1 <- replaceVirginQueens(colony1, p = 1.5)
#' getVirginQueens(colony1)@id
#'
#' lapply(getVirginQueens(apiary), FUN = function(x) x@id)
#' apiary <- replaceVirginQueens(apiary, p = 0.5)
#' lapply(getVirginQueens(apiary), FUN = function(x) x@id)
#' apiary <- replaceVirginQueens(apiary, p = 1.5)
#' lapply(getVirginQueens(apiary), FUN = function(x) x@id)
#'
#' @export
replaceVirginQueens <- function(x, p = 1, use = "rand", year = NULL,
                                simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    nVirginQueens <- nVirginQueens(x)
    if (nVirginQueens > 0) {
      nVirginQueensReplaced <- round(nVirginQueens * p)
      if (nVirginQueensReplaced < nVirginQueens) {
        nVirginQueensStay <- nVirginQueens - nVirginQueensReplaced
        tmp <- createVirginQueens(x, nInd = nVirginQueensReplaced,
                                  year = year, simParamBee = simParamBee)
        x@virginQueens <- c(selectInd(x@virginQueens, nInd = nVirginQueensStay, use = use),
                            tmp$virginQueens)
        # TODO: we need some scaling of the pHomBrood here and sticking pHomBrood into
        #       colony!
        #       see https://github.com/HighlanderLab/SIMplyBee/issues/104
        #           https://github.com/HighlanderLab/SIMplyBee/issues/80
        x@queen@misc[[1]]$pHomBrood <- (x@queen@misc[[1]]$pHomBrood + tmp$pHomBrood) / 2
      } else {
        x <- addVirginQueens(x = x, nInd = nVirginQueensReplaced, new = TRUE,
                             year = year, simParamBee = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- replaceVirginQueens(x = x[[colony]], p = p,
                                         use = use, year = year,
                                         simParamBee = simParamBee)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname replaceWorkers
#' @title Replace a proportion of workers with new ones
#'
#' @description Level 2 function that replaces a proportion of workers with new
#'   workers from the colony. Useful after events like season change, swarming,
#'   supersedure, etc. due to the short life span of the workers.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion of workers to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of workers that stay when \code{p < 1}
#' @param exact logical, if the csd locus is turned on and exact is TRUE,
#' replace the workers with the exact specified number of only
#' viable workers (heterozygous on the csd locus)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   replaced workers
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
#' colony1 <- addWorkers(colony1, nInd = 20)
#' apiary <- addWorkers(apiary, nInd = 20)
#'
#' getWorkers(colony1)@id
#' colony1 <- replaceWorkers(colony1, p = 0.5)
#' getWorkers(colony1)@id
#' colony1 <- replaceWorkers(colony1, p = 1.5)
#' getWorkers(colony1)@id
#'
#' lapply(getWorkers(apiary), FUN = function(x) x@id)
#' apiary <- replaceWorkers(apiary, p = 0.5)
#' lapply(getWorkers(apiary), FUN = function(x) x@id)
#' apiary <- replaceWorkers(apiary, p = 1.5)
#' lapply(getWorkers(apiary), FUN = function(x) x@id)
#'
#' @export
replaceWorkers <- function(x, p = 1, use = "rand", exact = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    nWorkers <- nWorkers(x)
    if (nWorkers > 0) {
      nWorkersReplaced <- round(nWorkers * p)
      if (nWorkersReplaced < nWorkers) {
        nWorkersStay <- nWorkers - nWorkersReplaced
        tmp <- createWorkers(x, nInd = nWorkersReplaced, exact = exact, simParamBee = simParamBee)
        x@workers <- c(selectInd(x@workers, nInd = nWorkersStay, use = use),
                            tmp$workers)
        # TODO: we need some scaling of the pHomBrood here and sticking pHomBrood into
        #       colony!
        #       see https://github.com/HighlanderLab/SIMplyBee/issues/104
        #           https://github.com/HighlanderLab/SIMplyBee/issues/80
        x@queen@misc[[1]]$pHomBrood <- (x@queen@misc[[1]]$pHomBrood + tmp$pHomBrood) / 2
      } else {
        x <- addWorkers(x = x, nInd = nWorkersReplaced, new = TRUE,
                        exact = exact, simParamBee = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- replaceWorkers(x = x[[colony]], p = p, use = use,
                                    exact = exact, simParamBee = simParamBee)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname replaceDrones
#' @title Replace a proportion of drones with new ones
#'
#' @description Level 2 function that replaces a proportion of drones with new
#'   drones from the colony. Useful after events like season change, swarming,
#'   supersedure, etc. due to the short life span of the drones.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion of drones to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of drones that stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   replaced drones
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
#' colony1 <- addDrones(colony1, nInd = 20)
#' apiary <- addDrones(apiary, nInd = 20)
#'
#' getDrones(colony1)@id
#' colony1 <- replaceDrones(colony1, p = 0.5)
#' getDrones(colony1)@id
#' colony1 <- replaceDrones(colony1, p = 1.5)
#' getDrones(colony1)@id
#'
#' lapply(getDrones(apiary), FUN = function(x) x@id)
#' apiary <- replaceDrones(apiary, p = 0.5)
#' lapply(getDrones(apiary), FUN = function(x) x@id)
#' apiary <- replaceDrones(apiary, p = 1.5)
#' lapply(getDrones(apiary), FUN = function(x) x@id)
#'
#' @export
replaceDrones <- function(x, p = 1, use = "rand") {
  if (isColony(x)) {
    nDrones <- nDrones(x)
    if (nDrones > 0) {
      nDronesReplaced <- round(nDrones * p)
      if (nDronesReplaced < nDrones) {
        nDronesStay <- nDrones - nDronesReplaced
        x@drones <- c(selectInd(x@drones, nInd = nDronesStay, use = use),
                           createDrones(x, nInd = nDronesReplaced))
      } else {
        x <- addDrones(x = x, nInd = nDronesReplaced, new = TRUE)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- replaceDrones(x = x[[colony]], p = p, use = use)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname removeQueen
#' @title Remove queen
#'
#' @description Level 2 function that removes the queen of a colony.
#'
#' @param colony \code{\link{Colony-class}}
#'
#' @return \code{\link{Colony-class}} without the queen
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony
#' getQueen(colony)
#'
#' colony <- removeQueen(colony)
#' colony
#' getQueen(colony)
#'
#' @export
removeQueen <- function(colony) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colony@queen <- NULL
  colony@id <- NULL
  validObject(colony)
  return(colony)
}

#' @rdname removeVirginQueens
#' @title Remove a proportion of virgin queens
#'
#' @description Level 2 function that removes a proportion of virgin queens of a
#'   colony.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of virgins queens that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}} without virgin queens
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addVirginQueens(colony, nInd = 10)
#' getVirginQueens(colony)
#'
#' colony <- removeVirginQueens(colony)
#' colony
#' getVirginQueens(colony)
#'
#' @export
removeVirginQueens <- function(colony, p = 1, use = "rand") {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (p > 1) {
    stop("p can not be higher than 1!")
  } else if (p < 0) {
    stop("p can not be less than 0!")
  } else if (p == 1) {
    colony@virginQueens <- NULL
  } else {
    n <- round(nVirginQueens(colony) * (1 - p))
    colony@virginQueens <- selectInd(pop = colony@virginQueens,
                                     nInd = n, use = use)
  }
  validObject(colony)
  return(colony)
}

#' @rdname removeWorkers
#' @title Remove a proportion of workers
#'
#' @description Level 2 function that removes a proportion of workers, for
#'   example, as a preparation for winter.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of workers that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}} without workers
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#'
#' colony <- addWorkers(colony, nInd = 10)
#' colony
#' getWorkers(colony)@id
#'
#' colony <- removeWorkers(colony, p = 0.5)
#' colony
#' getWorkers(colony)@id
#'
#' colony <- removeWorkers(colony, p = 1.0)
#' colony
#' getWorkers(colony)
#'
#' @export
removeWorkers <- function(colony, p = 1, use = "rand") {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (p > 1) {
    stop("p can not be higher than 1!")
  } else if (p < 0) {
    stop("p can not be less than 0!")
  } else if (p == 1) {
    colony@workers <- NULL
  } else {
    nWorkersNew <- round(nWorkers(colony) * (1 - p))
    colony@workers <- selectInd(pop = colony@workers,
                                nInd = nWorkersNew, use = use)
  }
  validObject(colony)
  return(colony)
}

#' @rdname removeDrones
#' @title Remove a proportion of drones
#'
#' @description Level 2 function that removes a proportion of drones, for
#'   example, at the end of summer.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of drones that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}} without drones
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' colony <- addDrones(colony, nInd = 10)
#' colony
#' getDrones(colony)@id
#'
#' colony <- removeDrones(colony, p = 0.5)
#' colony
#' getDrones(colony)@id
#'
#' colony <- removeDrones(colony, p = 1.0)
#' colony
#' getDrones(colony)
#'
#' @export
removeDrones <- function(colony, p = 1, use = "rand") {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (p > 1) {
    stop("p can not be higher than 1!")
  } else if (p < 0) {
    stop("p can not be less than 0!")
  } else if (p == 1) {
    colony@drones <- NULL
  } else {
    nDronesNew <- round(nDrones(colony) * (1 - p))
    colony@drones <- selectInd(pop = colony@drones,
                               nInd = nDronesNew, use = use)
  }
  validObject(colony)
  return(colony)
}

#' @rdname resetEvents
#' @title Reset colony events
#'
#' @description Level 2 function that resets the slots swarm, split,
#'   supersedure, collapsed, and production to FALSE. Useful at the end of a
#'   yearly cycle to reset the events, allowing the user to track new events in
#'   a new year.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param collapse logical, reset the collapse event (only sensible in setting
#'   up a new colony, which the default of \code{NULL} caters for; otherwise, a
#'   collapsed colony should be left collapsed forever, unless you force
#'   resetting this event with \code{collapse = TRUE})
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   events reset
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony1 <- createColony(queen = basePop[2], fathers = drones)
#' colony2 <- createColony(queen = basePop[3], fathers = drones)
#' colony1
#' apiary <- c(colony1, colony2)
#'
#' (colony1 <- buildUpColony(colony1, nWorkers = 100))
#' resetEvents(colony1)
#' apiary <- buildUpColonies(apiary, nWorkers = 100)
#' apiary[[1]]
#' resetEvents(apiary)[[1]]
#'
#' tmp <- splitColony(colony1)
#' (split <- tmp$split)
#' resetEvents(split)
#' (remnant <- tmp$remnant)
#' resetEvents(remnant)
#'
#' tmp <- splitColonies(apiary)
#' (splits <- tmp$splits)
#' splits[[1]]
#' resetEvents(splits)[[1]]
#' (remnants <- tmp$remnants)
#' remnants[[1]]
#' resetEvents(remnants)[[1]]
#'
#' tmp <- swarmColony(colony1)
#' (swarm <- tmp$swarm)
#' resetEvents(swarm)
#' (remnant <- tmp$remnant)
#' resetEvents(remnant)
#'
#' tmp <- swarmColonies(apiary)
#' (swarms <- tmp$swarms)
#' swarms[[1]]
#' resetEvents(swarms)[[1]]
#' (remnants <- tmp$remnants)
#' remnants[[1]]
#' resetEvents(remnants)[[1]]
#'
#' (tmp <- supersedeColony(colony1)) # TODO: do we still get production if we have supersedure?
#' resetEvents(tmp)
#'
#' (tmp <- supersedeColonies(apiary)) # TODO: do we still get production if we have supersedure?
#' tmp[[1]]
#' resetEvents(tmp)[[1]]
#'
#' (tmp <- collapseColony(colony1))
#' resetEvents(tmp)
#' resetEvents(tmp, collapse = TRUE)
#'
#' (tmp <- collapseColonies(apiary))
#' tmp[[1]]
#' resetEvents(tmp)[[1]]
#' resetEvents(tmp, collapse = TRUE)[[1]]
#'
#' @export
resetEvents <- function(x, collapse = NULL) {
  if (isColony(x)) {
    x@swarm <- FALSE
    x@split <- FALSE
    x@supersedure <- FALSE
    # Reset collapse only if asked (!is.null(collapse)) or if it was not yet
    #   turned on (is.null(x@collapse))
    if (is.null(collapse)) {
      collapse <- is.null(x@collapse)
    }
    if (collapse) {
      x@collapse <- FALSE
    }
    x@production <- FALSE
    validObject(x)
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- resetEvents(x = x[[colony]],
                                 collapse = collapse)
    }
    validObject(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(x)
}

#' @rdname crossColony
#' @title Cross (mate) a virgin queen of a colony to a group of drones
#'
#' @description Level 2 function that crosses (mates) a virgin queen of colony
#'   to a group of drones. When there are multiple virgin queens in the colony,
#'   one is selected at random, mated, and promoted to the queen of the colony.
#'   Other virgin queens are destroyed. Mated drones (fathers) are stored for
#'   producing progeny at a later stage.
#'   TODO: is this correct (about destroy)?
#'
#' @param colony \code{\link{Colony-class}}
#' @param fathers \code{\link{Pop-class}}, drones
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{Colony-class}} on how we store the fathers along the
#'   queen.
#'
#' @return \code{\link{Colony-class}} with a mated queen
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#'
#' colony <- createColony(virginQueen = basePop[2])
#' colony
#' colony <- crossColony(colony, fathers = drones)
#' colony
#'
#' @export
crossColony <- function(colony, fathers, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (isQueenPresent(colony)) {
    stop("Queen already present in the colony!")
  }
  if (!areVirginQueensPresent(colony)) {
    stop("No virgin queen(s) in the colony to cross!")
  }
  if (!isPop(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  # Pick one virgin queen that will prevail
  # TODO: should this use argument be really random? Do we want to make it into argument of this function?
  virginQueen <- selectInd(colony@virginQueens, nInd = 1, use = "rand")
  # TODO: do we take all fathers or just a 'default/nAvgFathers' or some other number?
  #       imagine someone providing 100 or 1000 fathers - should we just take them all?
  #       maybe add argument nFathers = NULL and in that case pull value from simParamBee,
  #       but throw a warning if a user provided more fathers? If the user specifies
  #       nAvgFathers, then we take as many as he/she wants
  queen <- crossVirginQueen(pop = virginQueen, fathers,
                            simParamBee = simParamBee)
  colony <- reQueenColony(colony, queen)
  colony <- removeVirginQueens(colony)
  validObject(colony)
  return(colony)
}

#' @rdname collapseColony
#' @title Collapse colony
#'
#' @description Level 2 function that collapses colony by setting the collapse
#'   event slot to \code{TRUE}. The production status slot is also changed (to
#'   \code{FALSE}).
#'
#' @param colony \code{\link{Colony-class}}
#'
#' @return \code{\link{Colony-class}} with the collapse event set to \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' (colony <- buildUpColony(colony, nWorkers = 100))
#' collapseColony(colony)
#'
#' @export
collapseColony <- function(colony) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colony@collapse <- TRUE
  colony@production <- FALSE
  validObject(colony)
  return(colony)
}

#' @rdname swarmColony
#' @title Swarm colony
#'
#' @description Level 2 function that swarms colony - an event where the queen
#'   leaves with a proportion of workers to create a new colony (the swarm). The
#'   remnant colony retains the other proportion of workers and all drones, and
#'   the workers raise virgin queens, of which only one prevails. Location of
#'   the swarm is the same as for the remnant (for now).
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of workers that will leave with the swarm colony
#' @param year numeric, year of birth for virgin queens
#'
#' @return list with two \code{\link{Colony-class}}, the \code{swarm} and the
#'   \code{remnant} (see the description what each colony holds!); both colonies
#'   have the swarm event set to \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' (colony <- buildUpColony(colony, nWorkers = 100))
#'
#' tmp <- swarmColony(colony)
#' tmp$swarm
#' tmp$remnant
#'
#' @export
swarmColony <- function(colony, p = 0.5, year = NULL) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!!")
  }

  nWorkers <- nWorkers(colony)
  nWorkersSwarm <- round(nWorkers * p)
  # TODO: pulling is random by default, should we make type/use of pulling an argument?
  tmp <- pullWorkers(x = colony, nInd = nWorkersSwarm)
  currentLocation <- getLocation(colony)

  swarmColony <- createColony()
  # It's not re-queening, but the function also sets the colony id
  swarmColony <- reQueenColony(colony = swarmColony,
                               queen = colony@queen)
  swarmColony@workers <- tmp$pulled
  swarmColony <- setLocation(x = swarmColony, location = currentLocation)

  remnantColony <- createColony()
  remnantColony@workers <- getWorkers(tmp$colony)
  remnantColony@drones <- getDrones(colony)
  # Workers raise virgin queens from eggs laid by the queen and one random
  #   virgin queen prevails, so we create just one
  # Could consider that a non-random one prevails (say the more aggressive one),
  #   by creating many virgin queens and then picking the one with highest
  #   gv/pheno for competition or some other criteria (patri-lineage)
  # TODO: add the exact = 1 argument in createVirginQueens() once available
  remnantColony@virginQueens <- createVirginQueens(x = colony, nInd = 1,
                                                   year = year)$virginQueens
  remnantColony <- setLocation(x = remnantColony, location = currentLocation)

  remnantColony@last_event <- "remnant"
  swarmColony@last_event <- "swarm"

  remnantColony@swarm <- TRUE
  swarmColony@swarm <- TRUE
  remnantColony@production <- FALSE
  swarmColony@production <- FALSE

  ret <- list(swarm = swarmColony, remnant = remnantColony)
  validObject(ret$swarmColony)
  validObject(ret$remnantColony)
  return(ret)
}

#' @rdname supersedeColony
#' @title Supersede colony
#'
#' @description Level 2 function that supersedes colony - an event where the
#'   queen dies. The workers and drones stay unchanged, but workers raise virgin
#'   queens, of which only one prevails.
#'
#' @param colony \code{\link{Colony-class}}
#' @param year numeric, year of birth for virgin queens
#'
#' @return \code{\link{Colony-class}} with the supersede event set to
#'   \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' (colony <- buildUpColony(colony, nWorkers = 100))
#'
#' supersedeColony(colony)
#'
#' @export
supersedeColony <- function(colony, year = NULL) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (!isQueenPresent(colony)) {
    stop("No queen present in the colony!")
  }
  # The bilogical order is: 1) queen dies and 2) workers raise virgin queens
  #   from laid eggs from the queen
  # The code below does 2) and then 1) since we don't store eggs
  # Workers raise virgin queens from eggs laid by the queen and one random
  #   virgin queen prevails, so we create just one
  # Could consider that a non-random one prevails (say the more aggressive one),
  #   by creating many virgin queens and then picking the one with highest
  #   gv/pheno for competition or some other criteria (patri-lineage)
  # TODO: add the exact = 1 argument in createVirginQueens() once available
  colony@virginQueens <- createVirginQueens(x = colony, nInd = 1,
                                            year = year)$virginQueens
  colony <- removeQueen(colony)
  colony@last_event <- "superseded"
  colony@supersedure <- TRUE
  validObject(colony)
  return(colony)
}

#' @rdname splitColony
#' @title Split colony in two colonies
#'
#' @description Level 2 function that splits colony into two new colonies to
#'   prevent swarming (in managed situation). The remnant colony retains the
#'   queen and a proportion of the workers and all drones. The split colony gets
#'   the other part of the workers, which raise virgin queens, of which only one
#'   prevails. Location of the split is the same as for the remnant.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of workers that will go to the split colony
#' @param year numeric, year of birth for virgin queens
#'
#' @return list with two \code{\link{Colony-class}}, the \code{split} and the
#'   \code{remnant} (see the description what each colony holds!); both colonies
#'   have the split even slot set do \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' (colony <- buildUpColony(colony, nWorkers = 100))
#'
#' tmp <- splitColony(colony)
#' tmp$split
#' tmp$remnant
#'
#' @export
splitColony <- function(colony, p = 0.3, year = NULL) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (p < 0 | p > 1) {
    stop("pSplit must be between 0 and 1!")
  }
  nWorkers <- nWorkers(colony)
  nWorkersSplit <- round(nWorkers * p)
  # TODO: pulling is random by default, should we make the type/use of pulling an argument?
  tmp <- pullWorkers(x = colony, nInd = nWorkersSplit)

  remnantColony <- tmp$colony

  splitColony <- createColony()
  splitColony@workers <- tmp$pulled
  # Workers raise virgin queens from eggs laid by the queen (assuming) that
  #   a frame of brood is also provided to the split and then one random virgin
  #   queen prevails, so we create just one
  # Could consider that a non-random one prevails (say the more aggressive one),
  #   by creating many virgin queens and then picking the one with highest
  #   gv/pheno for competition or some other criteria (patri-lineage)
  # TODO: add the exact = 1 argument in createVirginQueens() once available
  splitColony@virginQueens <- createVirginQueens(x = colony, nInd = 1,
                                                 year = year)$virginQueens
  splitColony <- setLocation(x = splitColony, location = getLocation(splitColony))

  remnantColony@last_event <- "remnant"
  splitColony@last_event <- "split"

  remnantColony@split <- TRUE
  splitColony@split <- TRUE

  remnantColony@production <- TRUE
  splitColony@production <- FALSE

  ret <- list(split = splitColony, remnant = remnantColony)
  validObject(ret$splitColony)
  validObject(ret$remnantColony)
  return(ret)
}

#' @rdname setLocation
#' @title Set colony location
#'
#' @description Level 2 function that to set colony location to (x, y)
#'   coordinates.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param location numeric or list, location to be set for the
#'   \code{\link{Colony-class}} or for \code{\link{Colonies-class}}; when
#'   numeric the same location will be set for all colonies; when list different
#'   locations will be set for each colony - the list has to have the same
#'   length at there are colonies in \code{x})
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with set
#'   location
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
#' getLocation(colony1)
#' getLocation(colony2)
#' getLocation(apiary)
#'
#' loc1 <- c(512, 722)
#' colony1 <- setLocation(colony1, location = loc1)
#' getLocation(colony1)
#'
#' loc2 <- c(189, 357)
#' colony2 <- setLocation(colony2, location = loc2)
#' getLocation(colony2)
#'
#' getLocation(c(colony1, colony2))
#'
#' # Assuming one location (as in bringing colonies to one place!)
#' apiary <- setLocation(apiary, location = loc1)
#' getLocation(apiary)
#'
#' # Assuming different locations (so tmp is not an apiary in one location!)
#' tmp <- setLocation(c(colony1, colony2), location = list(loc1, loc2))
#' getLocation(tmp)
#'
#' @export
setLocation <- function(x, location) {
  if (isColony(x)) {
    x@location <- location
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    if (is.list(location)) {
      if (length(location) != nCol) {
        stop("The length of location list and the number of colonies must match!")
      }
      for (colony in seq_len(nCol)) {
        x[[colony]]@location <- location[[colony]]
      }
    } else {
      for (colony in seq_len(nCol)) {
        x[[colony]]@location <- location
      }
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname setPhenoColony
#' @title Set colony phenotype
#'
#' @description Level 2 function that sets phenotypes for all individuals in
#'   the castes as well as the overall colony.
#'   THIS FUNCTION IS UNDER DEVELOPMENT - BEST IF YOU DON'T USE IT JUST YET!
#'
#' @param colony \code{\link{Colony-class}}
#' @param FUN function, any function that can be aplied on \code{colony} and
#'   can return phenotypes for defined traits via \code{\link{SimParamBee}}
#' @param ... all parameters of \code{\link{setPheno}}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation
#'   parameters
#'
#' @return \code{\link{Colony-class}} with phenotypes
#'
#' @examples
#' # TODO
#'
# TODO: Set pheno to virgin queens as well? Add caste argument here, similarly as
#       in getColonyGv()?
# TODO: what if caste phenos have already been set? need a sensible default!!!
# TODO: while ... will work for all arguments of setPheno() (such as h2, H2, ...)
#       it will not work for simParam - so best to add all these arguments directly?
# See
#     https://github.com/HighlanderLab/SIMplyBee/issues/26
#     https://github.com/HighlanderLab/SIMplyBee/issues/28
#     https://github.com/HighlanderLab/SIMplyBee/issues/32
#     https://github.com/HighlanderLab/SIMplyBee/issues/44
setPhenoColony <- function(colony, FUN = NULL, ..., simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colony@queen <- setPheno(colony@queen, ...)
  colony@workers <- setPheno(colony@workers, ...)
  colony@drones <- setPheno(colony@drones, ...)
  if (!is.null(FUN)) {
    colony@pheno <- FUN(colony, ...)
  }
  validObject(colony)
  return(colony)
}
