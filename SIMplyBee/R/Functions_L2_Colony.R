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
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return new \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones)
#' colony1
#'
#' colony2 <- createColony(virginQueens = basePop[3])
#' colony2
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
    if (!(isVirginQueen(queen) | isQueen(queen))) {
      stop("Individual in queen must be a virgin queen or a queen!")
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
      if (any(!isDrone(fathers))) {
        stop("Individuals in fathers must be drones!")
      }
      if (isQueenMated(queen)) {
        warning("The queen is already mated - ignoring the fathers argument!")
      } else {
        queen <- crossVirginQueen(
          pop = queen, fathers = fathers,
          simParamBee = simParamBee
        )
      }
    }
    if (!is.null(virginQueens)) {
      stop("You can provide only queen or virgin queen(s).")
    }
  }
  colony <- new(
    Class = "Colony",
    id = id,
    location = location,
    queen = queen,
    virginQueens = virginQueens
  )
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
#'   queen that will have to be mated later; test will be run if the individual
#'   \code{\link{isVirginQueen}} or \code{\link{isQueen}}
#' @param removeVirginQueens logical, remove existing virgin queens, default is
#'   \code{\link{TRUE}} since bee-keepers tend to remove any virgin queen cells
#'   to ensure the provided queen prevails (see details)
#'
#' @details If the provided queen is mated, then she is saved in the queen slot
#'   of the colony. If she is not mated, then she is saved in the virgin queen
#'   slot (replacing any existing virgin queens) and once she is mated will be
#'   promoted to the queen of the colony.
#'
#' @return \code{\link{Colony-class}} with a new queen (see details)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addVirginQueens(colony)
#' colony
#'
#' virginQueen <- basePop[3]
#' reQueenColony(colony, queen = virginQueen)
#'
#' matedQueen <- crossVirginQueen(pop = basePop[3], fathers = drones[6:10])
#' reQueenColony(colony, queen = matedQueen)
#' @export
reQueenColony <- function(colony, queen, removeVirginQueens = TRUE) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (!isPop(queen)) {
    stop("Argument queen must be a Pop class object!")
  }
  if (!(isVirginQueen(queen) | isQueen(queen))) {
    stop("Individual in queen must be a virgin queen or a queen!")
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' nVirginQueensFun <- function(colony) {
#'   rpois(n = 1, lambda = 15)
#' }
#' addVirginQueens(colony1, nInd = nVirginQueensFun)
#' nVirginQueens(addVirginQueens(apiary, nInd = nVirginQueensFun))
#'
#' SP$nVirginQueens <- nVirginQueensFun
#' addVirginQueens(colony1)
#' nVirginQueens(addVirginQueens(apiary))
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
      newVirginQueens <- createVirginQueens(
        x = x, nInd = nInd, year = year,
        simParamBee = simParamBee
      )
      if (is.null(x@virginQueens) | new) {
        x@virginQueens <- newVirginQueens
      } else {
        x@virginQueens <- c(x@virginQueens, newVirginQueens)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- addVirginQueens(
        x = x[[colony]], nInd = nInd, new = new,
        year = year, simParamBee = simParamBee
      )
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
#' @param nInd numeric or function, number of workers to be added, but see
#'   \code{new}; if \code{NULL} then \code{simParamBee$nWorkers} is used
#' @param new logical, should the number of workers be added anew or should we
#'   only top-up the existing number of workers to \code{nInd}
#' @param exact logical, if the csd locus is turned on and exact is \code{TRUE},
#'   we add the exact specified number of viable workers (heterozygous at the
#'   csd locus)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   workers added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' # Specify own number
#' SP$nWorkers <- 15
#' addWorkers(colony1)
#' nWorkers(addWorkers(apiary))
#'
#' # Specify a function that will give a number
#' nWorkersFun <- function(colony) {
#'   rpois(n = 1, lambda = 15)
#' }
#' addWorkers(colony1, nInd = nWorkersFun)
#' nWorkers(addWorkers(apiary, nInd = nWorkersFun))
#'
#' # Store a function or a value in the SP object
#' SP$nWorkers <- nWorkersFun
#' addWorkers(colony1)
#' nWorkers(addWorkers(apiary))
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
        x@queen@misc[[1]]$nWorkers <- nInd
        x@queen@misc[[1]]$nHomBrood <- newWorkers$nHomBrood
      } else {
        x@workers <- c(x@workers, newWorkers$workers)
        x@queen@misc[[1]]$nWorkers <- x@queen@misc[[1]]$nWorkers + nInd
        x@queen@misc[[1]]$nHomBrood <- x@queen@misc[[1]]$nHomBrood + newWorkers$nHomBrood
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- addWorkers(
        x = x[[colony]], nInd = nInd, new = new,
        exact = exact, simParamBee = simParamBee
      )
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
#'   \code{simParamBee$nDrones} is used
#' @param nInd numeric or function, number of drones to be added, but see
#'   \code{new}; if \code{NULL} then \code{simParamBee$nDrones} is used
#' @param new logical, should the number of drones be added anew or should we
#'   only top-up the existing number of drones to \code{nInd}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   drones added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' addDrones(colony1, nInd = 20)
#' nDrones(addDrones(apiary, nInd = 20))
#'
#' # Using defaults in SP$nWorkers
#' # (just to have some workers - change this to your needs!)
#' addDrones(colony1)
#' nDrones(addDrones(apiary))
#'
#' # Specifying own number
#' SP$nDrones <- 15
#' addDrones(colony1)
#' nDrones(addDrones(apiary))
#'
#' # Specify a function that will give a number
#' nDronesFun <- function(colony) {
#'   rpois(n = 1, lambda = 15)
#' }
#' addDrones(colony1, nInd = nDronesFun)
#' nDrones(addDrones(apiary, nInd = nDronesFun))
#'
#' # Store a function or a value in the SP object
#' SP$nDrones <- nDronesFun
#' addDrones(colony1)
#' nDrones(addDrones(apiary))
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
        x@queen@misc[[1]]$nDrones <- nInd
      } else {
        x@drones <- c(x@drones, newDrones)
        x@queen@misc[[1]]$nDrones <- x@queen@misc[[1]]$nDrones + nInd
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- addDrones(
        x = x[[colony]], nInd = nInd, new = new,
        simParamBee = simParamBee
      )
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
#' @param nWorkers numeric or function, number of worker to add to the colony,
#'   but see \code{new}; if \code{NULL} then \code{simParamBee$nWorkers} is used
#' @param nDrones numeric or function, number of drones to add to the colony,
#'   but see \code{new}; if \code{NULL} then \code{simParamBee$nDrones} is used
#' @param new logical, should the number of workers and drones be added anew or
#'   should we only top-up the existing number of workers and drones to
#'   \code{nWorkers} and \code{nDrones} (see details)
#' @param exact logical, if the csd locus is turned on and exact is \code{TRUE},
#'   create the exact specified number of only viable workers (heterozygous on
#'   the csd locus)
#' @param resetEvents logical, call \code{\link{resetEvents}} as part of the
#'   build up
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details Argument \code{new} enables simulation of two common cases. First,
#'   if you are modelling year-to-year cycle, you will likely want
#'   \code{new = TRUE}, so that, say, in spring you will replace old (from last
#'   year) workers and drones with the new ones. This is the case that we are
#'   targeting and hence \code{new = TRUE} is default. Second, if you are
#'   modelling shorter period cycles, you will likely want \code{new = FALSE} to
#'   just top up the current workers and drones - you might also want to look at
#'   \code{\link{replaceWorkers}} and \code{\link{replaceDrones}}.
#'
#' TODO: Discuss on how to model day-to-day variation with \code{new = FALSE}.
#'   We are not sure this is easy to achieve with current implementation just
#'   now, but could be expanded.
#'   https://github.com/HighlanderLab/SIMplyBee/issues/176
#'
#' This function turns on production in the colony.
#'
#' @return \code{\link{Colony-class}} with workers and drones replaced or added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' isProductive(colony)
#'
#' # Using defaults in SP$nWorkers & SP$nDrones
#' (colony <- buildUpColony(colony))
#' isProductive(colony)
#' getWorkers(colony)@id
#'
#' # Specifying own number
#' colony <- buildUpColony(colony, nWorkers = 100)
#' getWorkers(colony)@id
#' # we got new workers since new = TRUE
#'
#' colony <- buildUpColony(colony, nWorkers = 100, new = FALSE)
#' getWorkers(colony)@id
#' # we did NOT get new workers since new = FALSE and we were at the target of 100
#'
#' colony <- buildUpColony(colony, nWorkers = 150, new = FALSE)
#' getWorkers(colony)@id
#' # we got additional workers since new = FALSE and we were NOT at the target of 150
#'
#' # Specify a function that will give a number
#' nWorkersFun <- function(colony) {
#'   rpois(n = 1, lambda = 100)
#' }
#' nDronesFun <- function(colony) {
#'   rpois(n = 1, lambda = 15)
#' }
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' buildUpColony(colony, nWorkers = nWorkersFun, nDrones = nDronesFun)
#' buildUpColony(colony, nWorkers = nWorkersFun, nDrones = nDronesFun)
#'
#' # Store a function or a value in the SP object
#' SP$nWorkers <- nWorkersFun
#' SP$nDrones <- nDronesFun
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' buildUpColony(colony)
#' buildUpColony(colony)
#' @export
buildUpColony <- function(colony, nWorkers = NULL, nDrones = NULL,
                          new = TRUE, exact = FALSE, resetEvents = FALSE,
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
    colony <- addWorkers(
      x = colony, nInd = n, new = new,
      exact = exact, simParamBee = simParamBee
    )
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
    colony <- addDrones(
      x = colony, nInd = n, new = new,
      simParamBee = simParamBee
    )
  }

  # Events
  if (resetEvents) {
    colony <- resetEvents(colony)
  }
  colony@production <- TRUE
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
        tmp <- createVirginQueens(x,
          nInd = nVirginQueensReplaced,
          year = year, simParamBee = simParamBee
        )
        x@virginQueens <- c(
          selectInd(x@virginQueens, nInd = nVirginQueensStay, use = use),
          tmp
        )
      } else {
        x <- addVirginQueens(
          x = x, nInd = nVirginQueensReplaced, new = TRUE,
          year = year, simParamBee = simParamBee
        )
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- replaceVirginQueens(
        x = x[[colony]], p = p,
        use = use, year = year,
        simParamBee = simParamBee
      )
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
#' @param exact logical, if the csd locus is turned on and exact is \code{TRUE},
#'   replace the workers with the exact specified number of only viable workers
#'   (heterozygous on the csd locus)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   replaced workers
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' @export
replaceWorkers <- function(x, p = 1, use = "rand", exact = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (!(p > 0)) {
      stop("p must be greater than 0")
    }
    nWorkers <- nWorkers(x)
    if (nWorkers > 0) {
      nWorkersReplaced <- round(nWorkers * p)
      if (nWorkersReplaced < nWorkers) {
        nWorkersStay <- nWorkers - nWorkersReplaced
        tmp <- createWorkers(x, nInd = nWorkersReplaced, exact = exact, simParamBee = simParamBee)
        x@workers <- c(
          selectInd(x@workers, nInd = nWorkersStay, use = use),
          tmp$workers
        )
        x@queen@misc[[1]]$nWorkers <- x@queen@misc[[1]]$nWorkers + nWorkersReplaced
        x@queen@misc[[1]]$nHomBrood <- x@queen@misc[[1]]$nHomBrood + tmp$nHomBrood
      } else {
        x <- addWorkers(
          x = x, nInd = nWorkersReplaced, new = TRUE,
          exact = exact, simParamBee = simParamBee
        )
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- replaceWorkers(
        x = x[[colony]], p = p, use = use,
        exact = exact, simParamBee = simParamBee
      )
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' @export
replaceDrones <- function(x, p = 1, use = "rand") {
  if (isColony(x)) {
    if (!(p > 0)) {
      stop("p must be greater than 0")
    }
    nDrones <- nDrones(x)
    if (nDrones > 0) {
      nDronesReplaced <- round(nDrones * p)
      if (nDronesReplaced < nDrones) {
        nDronesStay <- nDrones - nDronesReplaced
        x@drones <- c(
          selectInd(x@drones, nInd = nDronesStay, use = use),
          createDrones(x, nInd = nDronesReplaced)
        )
        x@queen@misc[[1]]$nDrones <- x@queen@misc[[1]]$nDrones + nDronesReplaced
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony
#' getQueen(colony)
#'
#' colony <- removeQueen(colony)
#' colony
#' getQueen(colony)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addVirginQueens(colony, nInd = 10)
#' getVirginQueens(colony)
#'
#' colony <- removeVirginQueens(colony)
#' colony
#' getVirginQueens(colony)
#' @export
removeVirginQueens <- function(colony, p = 1, use = "rand") {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (p > 1) {
    stop("p must not be higher than 1!")
  } else if (p < 0) {
    stop("p must not be less than 0!")
  } else if (p == 1) {
    colony@virginQueens <- NULL
  } else {
    n <- round(nVirginQueens(colony) * (1 - p))
    colony@virginQueens <- selectInd(
      pop = colony@virginQueens,
      nInd = n, use = use
    )
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
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
#' @export
removeWorkers <- function(colony, p = 1, use = "rand") {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (p > 1) {
    stop("p must not be higher than 1!")
  } else if (p < 0) {
    stop("p must not be less than 0!")
  } else if (p == 1) {
    colony@workers <- NULL
  } else {
    nWorkersNew <- round(nWorkers(colony) * (1 - p))
    colony@workers <- selectInd(
      pop = colony@workers,
      nInd = nWorkersNew, use = use
    )
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
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
#' @export
removeDrones <- function(colony, p = 1, use = "rand") {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (p > 1) {
    stop("p must not be higher than 1!")
  } else if (p < 0) {
    stop("p must not be less than 0!")
  } else if (p == 1) {
    colony@drones <- NULL
  } else {
    nDronesNew <- round(nDrones(colony) * (1 - p))
    colony@drones <- selectInd(
      pop = colony@drones,
      nInd = nDronesNew, use = use
    )
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
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
#' (tmp <- supersedeColony(colony1))
#' resetEvents(tmp)
#'
#' (tmp <- supersedeColonies(apiary))
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
      x[[colony]] <- resetEvents(
        x = x[[colony]],
        collapse = collapse
      )
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
#'
#' @param colony \code{\link{Colony-class}}
#' @param fathers \code{\link{Pop-class}}, drones; \code{\link{isDrone}} test
#'   will be run on these individuals
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
#'
#' colony <- createColony(virginQueen = basePop[2])
#' colony
#' colony <- crossColony(colony, fathers = drones)
#' colony
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
  if (any(!isDrone(fathers))) {
    stop("Individuals in fathers must be drones!")
  }
  # TODO: Choosing the queen in supersedure: at random or something else
  #   https://github.com/HighlanderLab/SIMplyBee/issues/178
  virginQueen <- selectInd(colony@virginQueens, nInd = 1, use = "rand")
  # TODO: do we take all fathers or just a 'default/nFathers' or some other number?
  #       imagine someone providing 100 or 1000 fathers - should we just take them all?
  #       maybe add argument nFathers = NULL and in that case pull value from simParamBee,
  #       but throw a warning if a user provided more fathers? If the user specifies
  #       nFathers, then we take as many as he/she wants
  #       https://github.com/HighlanderLab/SIMplyBee/issues/157
  #       https://github.com/HighlanderLab/SIMplyBee/issues/98
  queen <- crossVirginQueen(
    pop = virginQueen, fathers,
    simParamBee = simParamBee
  )
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' (colony <- buildUpColony(colony, nWorkers = 100))
#' collapseColony(colony)
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
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two \code{\link{Colony-class}}, the \code{swarm} and the
#'   \code{remnant} (see the description what each colony holds!); both colonies
#'   have the swarm event set to \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' (colony <- buildUpColony(colony, nWorkers = 100))
#'
#' tmp <- swarmColony(colony)
#' tmp$swarm
#' tmp$remnant
#' @export
swarmColony <- function(colony, p = NULL, year = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!!")
  }
  if (is.null(p)) {
    p <- simParamBee$pSwarm
  }
  if (is.function(p)) {
    p <- p(colony)
  }
  if (p < 0 | p > 1) {
    stop("p must be between 0 and 1 (inclusive)!")
  }

  nWorkers <- nWorkers(colony)
  nWorkersSwarm <- round(nWorkers * p)

  # TODO: Add use="something" to select pWorkers that swarm
  #   https://github.com/HighlanderLab/SIMplyBee/issues/160
  tmp <- pullWorkers(x = colony, nInd = nWorkersSwarm)
  currentLocation <- getLocation(colony)

  swarmColony <- createColony()
  # It's not re-queening, but the function also sets the colony id
  swarmColony <- reQueenColony(
    colony = swarmColony,
    queen = colony@queen
  )
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
  remnantColony@virginQueens <- createVirginQueens(
    x = colony, nInd = 1,
    year = year
  )
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' (colony <- buildUpColony(colony, nWorkers = 100))
#'
#' supersedeColony(colony)
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
  colony@virginQueens <- createVirginQueens(
    x = colony, nInd = 1,
    year = year
  )
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
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two \code{\link{Colony-class}}, the \code{split} and the
#'   \code{remnant} (see the description what each colony holds!); both colonies
#'   have the split even slot set do \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' (colony <- buildUpColony(colony, nWorkers = 100))
#'
#' tmp <- splitColony(colony)
#' tmp$split
#' tmp$remnant
#' @export
splitColony <- function(colony, p = NULL, year = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.null(p)) {
    p <- simParamBee$pSplit
  }
  if (is.function(p)) {
    p <- p(colony)
  }
  if (p < 0 | p > 1) {
    stop("p must be between 0 and 1 (inclusive)!")
  }
  nWorkers <- nWorkers(colony)
  nWorkersSplit <- round(nWorkers * p)
  # TODO: Split colony splits at random by default, but we could make it as a function of some parameters
  #   https://github.com/HighlanderLab/SIMplyBee/issues/179
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
  splitColony@virginQueens <- createVirginQueens(
    x = colony, nInd = 1,
    year = year
  )
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
# TODO:
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

#' @rdname combineColony
#' @title Combine two colony
#'
#' @description Level 2 function that combines two colony objects into one.
#'   For example, to combine a weak and a strong colony. Workers and drones
#'   of the weak colony are added to the strong. User has to remove the weak
#'.  colony.
#'
#' @param strong \code{\link{Colony-class}}
#' @param weak \code{\link{Colony-class}}
#'
#' @return a colony \code{\link{Colony-class}} that combines workers and drones
#'   of both colony.
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = base[1], nInd = 30)
#' col1 <- createColony(queen = base[2], fathers = drones[1:15] )
#' col2 <- createColony(queen = base[3], fathers = drones[16:30])
#'
#' col2 <- combineColony(strong = col1, weak = col2)
#' rm(col1)
#' @export
combineColony <- function(strong, weak) {
  if (!isColony(strong)) {
    stop("Argument strong must be a Colony class object!")
  }
  if (!isColony(weak)) {
    stop("Argument weak must be a Colony class object!")
  }
  strong@workers <- c(strong@workers, weak@workers)
  strong@drones <- c(strong@drones, weak@drones)
  return(strong)
}
