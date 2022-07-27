# Level 2 Colony Functions

#' @rdname createColony
#' @title Create a new Colony
#'
#' @description Level 2 function that creates a new \code{\link{Colony-class}}
#'   to initiate simulations.
#'
#' @param x \code{\link{Pop-class}}, one queen or virgin queen(s)
#' @param location numeric, location of the colony as \code{c(x, y)}
#'
#' @return new \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 50)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones, nFathers = 15)
#' colony1
#'
#' colony2 <- createColony(x = basePop[3:4])
#' colony2
#' @export
createColony <- function(x, location = NULL) {
  if (!isPop(x)) {
    stop("Argument x must be a Pop class object!")
  }
  if (all(isQueen(x))) {
    if (1 < nInd(x)) {
      stop("You must provide just one queen for the colony!")
    }
    queen <- x
    id <- queen@id
    virginQueens <- NULL
  } else if (all(isVirginQueen(x))) {
    queen <- NULL
    id <- as.character(NA)
    virginQueens <- x
  } else {
    stop("Argument x must hold one queen or virgin queen(s)!")
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

#' @rdname reQueen
#' @title Re-queen
#'
#' @description Level 2 function that re-queens a colony or colonies by adding a mated or a
#'   virgin queen, removing the previous queen, and changing the colony id to
#'   the new mated queen.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
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
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with new queen(s) (see details)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 12, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 200)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 7, nFathers = nFathersPoisson)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' colony <- addVirginQueens(colony)
#' apiary <- createColonies(basePop[3:4], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[2:3])
#' apiary <- addVirginQueens(apiary)
#'
#' # Check queen and virgin queens IDs
#' getCasteId(colony, caste = "queen")
#' getCasteId(colony, caste = "virginQueens")
#' getCasteId(apiary, caste = "queen")
#' getCasteId(apiary, caste = "virginQueens")
#'
#' # Requeen with virgin queens
#' virginQueens <- basePop[5:8]
#' # Requeen a Colony class
#' colony <- reQueen(colony, queen = virginQueens[1])
#' # Check queen and virgin queens IDs
#' getCasteId(colony, caste = "queen")
#' getCasteId(colony, caste = "virginQueens")
#' # Requeen a MultiColony class
#' apiary <- reQueen(apiary, queen = virginQueens[2:3])
#' # Check queen and virgin queens IDs
#' getCasteId(apiary, caste = "queen")
#' getCasteId(apiary, caste = "virginQueens")
#'
#' # Requeen with mated queens
#' matedQueens <- cross(x = basePop[9:12], fathers = fatherGroups[4:7])
#' # Requeen a Colony class
#' colony <- reQueen(colony, queen = matedQueens[1])
#' # Check queen and virgin queens IDs
#' getCasteId(colony, caste = "queen")
#' getCasteId(colony, caste = "virginQueens")
#' # Requeen a MultiColony class
#' apiary <- reQueen(apiary, queen = matedQueens[2:3])
#' # Check queen and virgin queens IDs
#' getCasteId(apiary, caste = "queen")
#' getCasteId(apiary, caste = "virginQueens")
#' getCasteId(apiary, caste = "queen")
#' getCasteId(apiary, caste = "virginQueens")
#'
#' @export
reQueen <- function(x, queen, removeVirginQueens = TRUE) {
  if (!isPop(queen)) {
    stop("Argument queen must be a Pop class object!")
  }
  if (!all(isVirginQueen(queen) | isQueen(queen))) {
    stop("Individual in queen must be a virgin queen or a queen!")
  }
  if (isColony(x)) {
    if (isQueenMated(queen)) {
      if (nInd(queen) > 1) {
        stop("You must provide just one queen for the colony!")
      }
      x@queen <- queen
      x@id <- queen@id
      if (removeVirginQueens) {
        x <- removeVirginQueens(x)
      }
    } else {
      x <- removeQueen(x)
      x@virginQueens <- queen
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    if (nInd(queen) < nCol) {
      stop("Not enough queens provided!")
    }
    for (colony in seq_len(nCol)) {
      x[[colony]] <- reQueen(x = x[[colony]],
                             queen = queen[colony])
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname addVirginQueens
#' @title Add (raise) virgin queens in the colony
#'
#' @description Level 2 function that adds (raises) the specified number of
#'   virgin queens in the colony by crossing the current queen and the fathers.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param nInd numeric or function, number of virgin queens to add; if
#'   \code{NULL} then \code{\link{SimParamBee}$nVirginQueens} is used
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
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' apiary <- c(colony1, colony2)
#' addVirginQueens(colony1, nInd = 20)
#' nVirginQueens(addVirginQueens(apiary, nInd = 20))
#'
#' # Using a default in SP$nVirginQueens
#' # (just to have some workers - change this to your needs!)
#' addVirginQueens(colony1)
#' nVirginQueens(addVirginQueens(apiary))
#'
#' # Specify own number
#' SP$nVirginQueens <- 15
#' addVirginQueens(colony1)
#' nVirginQueens(addVirginQueens(apiary))
#' # nVirginQueens will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' addVirginQueens(colony1, nInd = nVirginQueensPoisson)
#' nVirginQueens(addVirginQueens(apiary, nInd = nVirginQueensPoisson))
#' # nVirginQueens will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nVirginQueens <- nVirginQueensPoisson
#' addVirginQueens(colony1)
#' nVirginQueens(addVirginQueens(apiary))
#' # nVirginQueens will vary between function calls when a function is used
#' @export
addVirginQueens <- function(x, nInd = NULL, new = FALSE, year = NULL,
                            simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nInd)) {
    nInd <- simParamBee$nVirginQueens
  }
  # doing "if (is.function(nInd))" below
  if (isColony(x)) {
    if (!isQueenPresent(x)) {
      stop("Missing queen!")
    }
    if (!isQueenMated(x)) {
      stop("Unmated queen!")
    }
    if (is.function(nInd)) {
      nInd <- nInd(colony = x) # see nVirginQueensPoissonColonyStrength
    }
    if (0 < nInd) {
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
#'   \code{new}; if \code{NULL} then \code{\link{SimParamBee}$nWorkers} is used
#' @param new logical, should the number of workers be added anew or should we
#'   only top-up the existing number of workers to \code{nInd}
#' @param exact logical, if the csd locus is turned on and exact is \code{TRUE},
#'   we add the exact specified number of viable workers (heterozygous at the
#'   csd locus)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details This function increases queen's \code{nWorkers} and \code{nHomBrood}
#'   counters.
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   workers added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
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
#' # nWorkers will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' addWorkers(colony1, nInd = nWorkersPoisson)
#' nWorkers(addWorkers(apiary, nInd = nWorkersPoisson))
#' # nWorkers will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nWorkers <- nWorkersPoisson
#' addWorkers(colony1)
#' nWorkers(addWorkers(apiary))
#' # nWorkers will vary between function calls when a function is used
#'
#' # Queen's counters
#' getMisc(getQueen(addWorkers(colony1)))
#' getMisc(getQueen(addWorkers(colony2)))
#' @export
addWorkers <- function(x, nInd = NULL, new = FALSE, exact = FALSE,
                       simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nInd)) {
    nInd <- simParamBee$nWorkers
  }
  # doing "if (is.function(nInd))" below
  if (isColony(x)) {
    if (is.function(nInd)) {
      nInd <- nInd(colony = x) # see nWorkersPoissonQueenFecundity
    }
    if (0 < nInd) {
      newWorkers <- createWorkers(x, nInd, exact = exact, simParamBee = simParamBee)
      if (is.null(x@workers) | new) {
        x@workers <- newWorkers$workers
      } else {
        x@workers <- c(x@workers, newWorkers$workers)
      }
      x@queen@misc[[1]]$nWorkers <- x@queen@misc[[1]]$nWorkers + nInd(newWorkers$workers)
      x@queen@misc[[1]]$nHomBrood <- x@queen@misc[[1]]$nHomBrood + newWorkers$nHomBrood
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
#' @param nInd numeric or function, number of drones to be added, but see
#'   \code{new}; if \code{NULL} then \code{\link{SimParamBee}$nDrones} is used
#' @param new logical, should the number of drones be added anew or should we
#'   only top-up the existing number of drones to \code{nInd}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details This function increases queen's \code{nDrones} counter.
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with
#'   drones added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
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
#' # nDrones will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' addDrones(colony1, nInd = nDronesPoisson)
#' nDrones(addDrones(apiary, nInd = nDronesPoisson))
#' # nDrones will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nDrones <- nDronesPoisson
#' addDrones(colony1)
#' nDrones(addDrones(apiary))
#' # nDrones will vary between function calls when a function is used
#'
#' # Queen's counters
#' getMisc(getQueen(addDrones(colony1)))
#' getMisc(getQueen(addDrones(colony2)))
#' @export
addDrones <- function(x, nInd = NULL, new = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nInd)) {
    nInd <- simParamBee$nDrones
  }
  # doing "if (is.function(nInd))" below
  if (isColony(x)) {
    if (is.function(nInd)) {
      nInd <- nInd(x = x) # see nDronesPoissonQueenFecundity
    }
    if (0 < nInd) {
      newDrones <- createDrones(x, nInd)
      if (is.null(x@drones) | new) {
        x@drones <- newDrones
      } else {
        x@drones <- c(x@drones, newDrones)
      }
      x@queen@misc[[1]]$nDrones <- x@queen@misc[[1]]$nDrones + nInd
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

#' @rdname buildUp
#' @title Build up colony or colonies by adding (raising) workers and drones
#'
#' @description Level 2 function that builds up colony by adding (raising)
#'   workers and drones usually in spring or after events such as split or
#'   swarming.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param nWorkers numeric or function, number of worker to add to the colony,
#'   but see \code{new}; if \code{NULL} then \code{\link{SimParamBee}$nWorkers}
#'   is used
#' @param nDrones numeric or function, number of drones to add to the colony,
#'   but see \code{new}; if \code{NULL} then \code{\link{SimParamBee}$nDrones}
#'   is used
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
#' @details This function increases queen's \code{nWorkers}, \code{nHomBrood},
#'   and \code{nDrones} counters. It also turns production on.
#'
#'   Argument \code{new} enables simulation of two common cases. First,
#'   if you are modelling year-to-year cycle, you will likely want
#'   \code{new = TRUE}, so that, say, in spring you will replace old (from last
#'   year) workers and drones with the new ones. This is the case that we are
#'   targeting and hence \code{new = TRUE} is default. Second, if you are
#'   modelling shorter period cycles, you will likely want \code{new = FALSE} to
#'   just top up the current workers and drones - you might also want to look at
#'   \code{\link{replaceWorkers}} and \code{\link{replaceDrones}}.
#'
#' TODO: Discuss on how to model day-to-day variation with \code{new = FALSE}.
#'       We are not sure this is easy to achieve with current implementation
#'       just now, but could be expanded.
#'       https://github.com/HighlanderLab/SIMplyBee/issues/176
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with workers and
#'    drones replaced or added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' isProductive(colony)
#' apiary <- createColonies(basePop[3:4], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(2,3)])
#' isProductive(apiary)
#'
#' # Using defaults in SP$nWorkers & SP$nDrones
#' (colony <- buildUp(colony))
#' isProductive(colony)
#' getWorkers(colony)@id
#' # Build-up a MultiColony class
#' (apiary <- buildUp(apiary))
#' isProductive(apiary)
#'
#' # Specifying own number
#' colony <- buildUp(colony, nWorkers = 100)
#' getWorkers(colony)@id
#' # we got new workers since new = TRUE
#' # Build up a MultiColony class
#' apiary <- buildUp(apiary, nWorkers = 250)
#'
#' colony <- buildUp(colony, nWorkers = 100, new = FALSE)
#' getWorkers(colony)@id
#' # we did NOT get new workers since new = FALSE and we were at the target of 100
#'
#' colony <- buildUp(colony, nWorkers = 150, new = FALSE)
#' getWorkers(colony)@id
#' # we got additional workers since new = FALSE and we were NOT at the target of 150
#'
#' # The user can also specify a function that will give a number
#' colony <- createColony(x = basePop[5])
#' colony <- cross(colony, fathers = fatherGroups[[4]])
#' buildUp(colony, nWorkers = nWorkersPoisson, nDrones = nDronesPoisson)
#' buildUp(colony, nWorkers = nWorkersPoisson, nDrones = nDronesPoisson)
#' # nWorkers and nDrones will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nWorkers <- nWorkersPoisson
#' SP$nDrones <- nDronesPoisson
#' # Create new drones and a new colony
#' colony <- createColony(x = basePop[6])
#' colony <- cross(colony, fathers = fatherGroups[[5]])
#' buildUp(colony)
#' buildUp(colony)
#' # nWorkers and nDrones will vary between function calls when a function is used
#' # Same for MultiColony class
#' apiary <- createColonies(basePop[7:8], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(6,7)])
#' buildUp(apiary)[[1]]
#' buildUp(apiary)[[1]]
#'
#' # Queen's counters
#' getMisc(getQueen(buildUp(colony)))
#'
#' @export
buildUp <- function(x, nWorkers = NULL, nDrones = NULL,
                    new = TRUE, exact = FALSE, resetEvents = FALSE,
                    simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }

  if (isColony(x)) {
    # Workers
    if (is.null(nWorkers)) {
      nWorkers <- simParamBee$nWorkers
    }
    if (is.function(nWorkers)) {
      nWorkers <- nWorkers(colony = x) # see nWorkersPoissonQueenFecundity
    }
    if (new) {
      n <- nWorkers
    } else {
      n <- nWorkers - nWorkers(x)
    }
    if (n > 0) {
      x <- addWorkers(
        x = x, nInd = n, new = new,
        exact = exact, simParamBee = simParamBee
      )
    }

    # Drones
    if (is.null(nDrones)) {
      nDrones <- simParamBee$nDrones
    }
    if (is.function(nDrones)) {
      nDrones <- nDrones(x = x) # see nDronesPoissonQueenFecundity
    }
    if (new) {
      n <- nDrones
    } else {
      n <- nDrones - nDrones(x)
    }
    if (n > 0) {
      x <- addDrones(
        x = x, nInd = n, new = new,
        simParamBee = simParamBee
      )
    }

    # Events
    if (resetEvents) {
      x <- resetEvents(x)
    }
    x@production <- TRUE
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- buildUp(
        x = x[[colony]],
        nWorkers = nWorkers,
        nDrones = nDrones,
        new = new,
        exact = exact,
        resetEvents = resetEvents,
        simParamBee = simParamBee
      )
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }

  validObject(x)
  return(x)
}

#' @rdname downsize
#' @title Reduce number of workers and remove all drones and virgin queens from a colony
#' or colonies
#'
#' @description Level 2 function that downsizes colony by removing a proportion
#'   of workers, all drones and all virgin queens. Usually in the autumn, such
#'   an event occurs in preparation for the winter months.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion of workers to be removed from the colony; if
#'   \code{NULL} then \code{\link{SimParamBee}$downsizeP} is used
#' @param use character, all the options provided by \code{\link{selectInd}};
#'   it guides the selection of workers that will be removed
#' @param new logical, should we remove all current workers and add a targeted
#'   proportion anew (say, create winter workers)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with workers reduced and
#'   drones/virgin queens removed
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 100)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 3, nFather= 12)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' colony <- buildUp(colony)
#' colony <- addVirginQueens(x = colony, nInd = 10)
#' colony
#' apiary <- createColonies(basePop[3:4], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(2,3)])
#' apiary <- buildUp(apiary)
#' apiary <- addVirginQueens(apiary, nInd = 10)
#' apiary
#'
#' # Downsize
#' colony <- downsize(x = colony, new = TRUE, use = "rand")
#' colony
#' apiary <- downsize(x = apiary, new = TRUE, use = "rand")
#' apiary[[1]]
#'
#' @export
downsize <- function(x, p = NULL, use = "rand", new = FALSE,
                     simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.null(p)) {
      p <- simParamBee$downsizeP
    }
    if (is.function(p)) {
      p <- p(x)
    }
    if (new == TRUE) {
      n <- round(nWorkers(x) * (1 - p))
      x <- addWorkers(x = x, nInd = n, new = TRUE)
    } else {
      x <- removeWorkers(x = x, p = p, use = use)
    }
    x <- removeDrones(x = x, p = 1)
    x <- removeVirginQueens(x = x, p = 1)
    x@production <- FALSE
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- downsize(
        x = x[[colony]],
        p = p,
        use = use,
        new = new,
        simParamBee = simParamBee
      )
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }

  validObject(x)
  return(x)
}

#' @rdname replaceVirginQueens
#' @title Replace a proportion of virgin queens with new ones
#'
#' @description Level 2 function that replaces a proportion of virgin queens
#'   with new virgin queens from the colony. Useful after events like season
#'   change, swarming, supersedure, etc. due to the short life span of the
#'   virgin queens.
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
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
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
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
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
replaceWorkers <- function(x, p = 1, use = "rand", exact = FALSE,
                           simParamBee = NULL) {
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
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
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
    if (!(0 < p)) {
      stop("Argument p must be greater than 0")
    }
    nDrones <- nDrones(x)
    if (0 < nDrones) {
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
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} without the queen
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones[1:5], nFathers = 5)
#' colony
#' getQueen(colony)
#'
#' colony <- removeQueen(colony)
#' colony
#' getQueen(colony)
#' @export
removeQueen <- function(x) {
  if (isColony(x)) {
    x@queen <- NULL
    x@id <- NULL
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- removeQueen(x = x[[colony]], p = p, use = use)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }

  validObject(x)
  return(x)
}

#' @rdname removeVirginQueens
#' @title Remove a proportion of virgin queens
#'
#' @description Level 2 function that removes a proportion of virgin queens of a
#'   colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of virgins queens that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} without virgin queens
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones[1:5], nFathers = 5)
#' colony <- addVirginQueens(colony, nInd = 10)
#' getVirginQueens(colony)
#'
#' colony <- removeVirginQueens(colony)
#' colony
#' getVirginQueens(colony)
#' @export
removeVirginQueens <- function(x, p = 1, use = "rand") {
  if (isColony(x)) {
    if (1 < p) {
      stop("p must not be higher than 1!")
    } else if (p < 0) {
      stop("p must not be less than 0!")
    } else if (p == 1) {
      x@virginQueens <- NULL
    } else {
      nVirginQueensNew <- round(nVirginQueens(x) * (1 - p))
      x@virginQueens <- selectInd(
        pop = x@virginQueens,
        nInd = nVirginQueensNew, use = use
      )
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- removeVirginQueens(x = x[[colony]], p = p, use = use)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname removeWorkers
#' @title Remove a proportion of workers
#'
#' @description Level 2 function that removes a proportion of workers, for
#'   example, as a preparation for winter.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of workers that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} without workers
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones, nFathers = 5)
#' colony
#'
#' colony <- addWorkers(colony, nInd = 10)
#' colony
#' getWorkers(colony)@id
#'
#' colony <- removeWorkers(x = colony, p = 0.5)
#' colony
#' getWorkers(colony)@id
#'
#' colony <- removeWorkers(x = colony, p = 1.0)
#' colony
#' getWorkers(colony)
#' @export
removeWorkers <- function(x, p = 1, use = "rand") {
  if (isColony(x)) {
    if (1 < p) {
      stop("p must not be higher than 1!")
    } else if (p < 0) {
      stop("p must not be less than 0!")
    } else if (p == 1) {
      x@workers <- NULL
    } else {
      nWorkersNew <- round(nWorkers(x) * (1 - p))
      x@workers <- selectInd(
        pop = x@workers,
        nInd = nWorkersNew, use = use
      )
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- removeWorkers(x = x[[colony]], p = p, use = use)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname removeDrones
#' @title Remove a proportion of drones
#'
#' @description Level 2 function that removes a proportion of drones, for
#'   example, at the end of summer.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of drones that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} without drones
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones, nFathers = 5)
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
removeDrones <- function(x, p = 1, use = "rand") {
  if (isColony(x)) {
    if (1 < p) {
      stop("p must not be higher than 1!")
    } else if (p < 0) {
      stop("p must not be less than 0!")
    } else if (p == 1) {
      x@drones <- NULL
    } else {
      nDronesNew <- round(nDrones(x) * (1 - p))
      x@drones <- selectInd(
        pop = x@drones,
        nInd = nDronesNew, use = use
      )
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- removeDrones(x = x[[colony]], p = p, use = use)
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
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
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' colony1
#' apiary <- c(colony1, colony2)
#'
#' (colony1 <- buildUp(colony1, nWorkers = 100))
#' resetEvents(colony1)
#' apiary <- buildUp(apiary, nWorkers = 100)
#' apiary[[1]]
#' resetEvents(apiary)[[1]]
#'
#' tmp <- split(colony1)
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
#' tmp <- swarm(colony1)
#' (swarm <- tmp$swarm)
#' resetEvents(swarm)
#' (remnant <- tmp$remnant)
#' resetEvents(remnant)
#'
#' tmp <- swarm(apiary)
#' (swarms <- tmp$swarms)
#' swarms[[1]]
#' resetEvents(swarms)[[1]]
#' (remnants <- tmp$remnants)
#' remnants[[1]]
#' resetEvents(remnants)[[1]]
#'
#' (tmp <- supersede(colony1))
#' resetEvents(tmp)
#'
#' (tmp <- supersedeColonies(apiary))
#' tmp[[1]]
#' resetEvents(tmp)[[1]]
#'
#' (tmp <- collapse(colony1))
#' resetEvents(tmp)
#' resetEvents(tmp, collapse = TRUE)
#'
#' (tmp <- collapse(apiary))
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

#' @rdname collapse
#' @title Collapse
#'
#' @description Level 2 function that collapses colony by setting the collapse
#'   event slot to \code{TRUE}. The production status slot is also changed (to
#'   \code{FALSE}).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with the collapse
#'   event set to \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones = createDrones(basePop[1], n = 1000)
#' fatherGroups = pullDroneGroupsFromDCA(drones, n = 10, nFathers = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[1])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' apiary <- createColonies(x = basePop[2:10], n = 9)
#' apiary <- cross(apiary, fathers = fatherGroups[2:10])
#'
#' # Collapse
#' hasCollapsed(colony)
#' colony <- collapse(colony)
#' hasCollapsed(colony)
#'
#' hasCollapsed(apiary)
#' tmp <- pullColonies(apiary, n = 2)
#' tmp
#' apiaryLost <- collapse(tmp$pulledColonies)
#' hasCollapsed(apiaryLost)
#' apiaryLeft <- tmp$remainingColonies
#' hasCollapsed(apiaryLeft)
#'
#' @export
collapse <- function(x) {
  if (isColony(x)) {
    x@collapse <- TRUE
    x@production <- FALSE
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- collapse(x = x[[colony]])
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname swarm
#' @title Swarm
#'
#' @description Level 2 function that swarms colony - an event where the queen
#'   leaves with a proportion of workers to create a new colony (the swarm). The
#'   remnant colony retains the other proportion of workers and all drones, and
#'   the workers raise virgin queens, of which only one prevails. Location of
#'   the swarm is the same as for the remnant (for now).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion of workers that will leave with the swarm colony;
#'   if \code{NULL} then \code{\link{SimParamBee}$swarmP} is used
#' @param year numeric, year of birth for virgin queens
#' @param nVirginQueens integer, the number of virgin queens to be created in the
#'   colony; of these one is randomly selected as the new virgin queen of the
#'   remnant colony. If \code{NULL}, the value from \code{simParamBee$nVirginQueens}
#'   is used
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two \code{\link{Colony-class}} or \code{\link{Colonies-class}},
#' the \code{swarm} and the \code{remnant} (see the description what each colony holds!); both
#' outputs have the swarm event set to \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones = createDrones(basePop[1], n = 1000)
#' fatherGroups = pullDroneGroupsFromDCA(drones, n = 10, nFathers = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' (colony <- buildUp(colony, nWorkers = 100))
#' apiary <- createColonies(basePop[3:8], n = 6)
#' apiary <- cross(apiary, fathers = fatherGroups[2:7])
#' apiary <- buildUp(apiary, nWorkers = 100)
#'
#' # Swarm a colony
#' tmp <- swarm(colony)
#' tmp$swarm
#' tmp$remnant
#'
#' # Swarm all colonies in the apiary with p = 0.6 (60% of workers leave)
#' tmp <- swarm(apiary, p = 0.6)
#' tmp$swarm[[1]]
#' tmp$remnant[[1]]
#'
#' # Sample colonies from the apiary that will swarm (sample with probability of 0.2)
#' tmp <- pullColonies(apiary, p = 0.2)
#' # Swarm only the pulled colonies
#' (swarm(tmp$pulledColonies, p = 0.6))
#'
#' #' @export
swarm <- function(x, p = NULL, year = NULL, nVirginQueens = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(p)) {
    p <- simParamBee$swarmP
  }
  if (is.null(nVirginQueens)) {
    nVirginQueens <- simParamBee$nVirginQueens
  }

  if (isColony(x)) {
    if  (is.function(p)) {
      p <- p(colony)
    }
    if (p < 0 | 1 < p) {
      stop("p must be between 0 and 1 (inclusive)!")
    }
    if (is.function(nVirginQueens)) {
      nVirginQueens <- nVirginQueens()
    }
    nWorkers <- nWorkers(x)
    nWorkersSwarm <- round(nWorkers * p)

    # TODO: Add use="something" to select pWorkers that swarm
    #       https://github.com/HighlanderLab/SIMplyBee/issues/160
    tmp <- pullWorkers(x = x, nInd = nWorkersSwarm)
    currentLocation <- getLocation(colony)

    swarmColony <- createColony(x = x@queen)
    # It's not re-queening, but the function also sets the colony id

    swarmColony@workers <- tmp$pulled
    swarmColony <- setLocation(x = swarmColony, location = currentLocation)

    tmpVirginQueen <- createVirginQueens(
      x = x, nInd = nVirginQueens,
      year = year
    )
    tmpVirginQueen <- selectInd(tmpVirginQueen, nInd = 1, use = "rand")

    remnantColony <- createColony(x = tmpVirginQueen)
    remnantColony@workers <- getWorkers(tmp$colony)
    remnantColony@drones <- getDrones(x)
    # Workers raise virgin queens from eggs laid by the queen and one random
    #   virgin queen prevails, so we create just one
    # Could consider that a non-random one prevails (say the more aggressive one),
    #   by creating many virgin queens and then picking the one with highest
    #   gv/pheno for competition or some other criteria (patri-lineage)

    remnantColony <- setLocation(x = remnantColony, location = currentLocation)

    remnantColony@last_event <- "remnant"
    swarmColony@last_event <- "swarm"

    remnantColony@swarm <- TRUE
    swarmColony@swarm <- TRUE
    remnantColony@production <- FALSE
    swarmColony@production <- FALSE

    ret <- list(swarm = swarmColony, remnant = remnantColony)
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    if (nCol == 0) {
      ret <- list(
        swarms = createColonies(),
        remnants = createColonies()
      )
    } else {
      ret <- list(
        swarms = createColonies(n = nCol),
        remnants = createColonies(n = nCol)
      )
      for (colony in seq_len(nCol)) {
        tmp <- swarm(x[[colony]],
                    p = p, year = year,
                    nVirginQueens = nVirginQueens,
                    simParamBee = simParamBee
        )
        ret$swarms[[colony]] <- tmp$swarm
        ret$remnants[[colony]] <- tmp$remnant
      }
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }

  validObject(ret$swarmColony)
  validObject(ret$remnantColony)
  return(ret)
}

#' @rdname supersede
#' @title Supersede
#'
#' @description Level 2 function that supersedes colony - an event where the
#'   queen dies. The workers and drones stay unchanged, but workers raise virgin
#'   queens, of which only one prevails.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param year numeric, year of birth for virgin queens
#' @param nVirginQueens integer, the number of virgin queens to be created in the
#'   colony; of these one is randomly selected as the new virgin queen of the
#'   remnant colony. If \code{NULL}, the value from \code{simParamBee$nVirginQueens}
#'   is used
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return  \code{\link{Colony-class}} or \code{\link{Colonies-class}} with the
#' supersede event set to \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones = createDrones(basePop[1], n = 1000)
#' fatherGroups = pullDroneGroupsFromDCA(drones, n = 10, nFathers = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' (colony <- buildUp(colony, nWorkers = 100))
#' apiary <- createColonies(basePop[3:8], n = 6)
#' apiary <- cross(apiary, fathers = fatherGroups[2:7])
#' apiary <- buildUp(apiary, nWorkers = 100)
#'
#' # Supersede a colony
#' getQueen(colony)
#' getVirginQueens(colony)
#' colony <- supersede(colony)
#' getQueen(colony)
#' getVirginQueens(colony)
#'
#' # Supersede all colonies in the apiary
#' getId(mergePops(getQueen(apiary)))
#' getId(mergePops(getVirginQueens(apiary))) # No virgin queens
#' apiary1 <- supersede(apiary)
#' getId(mergePops(getQueen(apiary1))) # No queens
#' getId(mergePops(getVirginQueens(apiary1))) # No virgin queens
#'
#' # Sample colonies from the apiary that will supersede (sample with probability of 0.2)
#' tmp <- pullColonies(apiary, p = 0.2)
#' # Swarm only the pulled colonies
#' (supersede(tmp$pulledColonies))
#'
#' @export
supersede <- function(x, year = NULL, nVirginQueens = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nVirginQueens)) {
    nVirginQueens <- simParamBee$nVirginQueens
  }
  if (isColony(x)) {
    if (!isQueenPresent(x)) {
      stop("No queen present in the colony!")
    }
    if (is.function(nVirginQueens)) {
      nVirginQueens <- nVirginQueens()
    }

    tmpVirginQueen <- createVirginQueens(
      x = x, nInd = nVirginQueens,
      year = year
    )
    x@virginQueens <- selectInd(tmpVirginQueen, nInd = 1, use = "rand")
    x <- removeQueen(x)
    x@last_event <- "superseded"
    x@supersedure <- TRUE
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    if (nCol == 0) {
      colonies <- createColonies()
    } else {
      for (colony in seq_len(nCol)) {
        x[[colony]] <- supersede(x[[colony]],
                                 year = year,
                                 nVirginQueens = nVirginQueens,
                                 simParamBee = simParamBee)
      }
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  validObject(x)
  return(x)

  # The biological order is: 1) queen dies and 2) workers raise virgin queens
  #   from eggs laid by the queen
  # The code below does 2) and then 1) since we don't store eggs
  # Workers raise multiple virgin queens out of which one prevails, so we create
  #   just one
  # TODO: We could consider that a non-random one prevails (say the most
  #       aggressive one), by creating many virgin queens and then picking the
  #       one with highest pheno for competition or some other criteria
  #       https://github.com/HighlanderLab/SIMplyBee/issues/239
}

#' @rdname split
#' @title Split colony in two colonies
#'
#' @description Level 2 function that splits colony into two new colonies to
#'   prevent swarming (in managed situation). The remnant colony retains the
#'   queen and a proportion of the workers and all drones. The split colony gets
#'   the other part of the workers, which raise virgin queens, of which only one
#'   prevails. Location of the split is the same as for the remnant.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param p numeric, proportion of workers that will go to the split colony; if
#'   \code{NULL} then \code{\link{SimParamBee}$splitP} is used
#' @param year numeric, year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two  \code{\link{Colony-class}} or \code{\link{Colonies-class}},
#' the \code{split} and the \code{remnant} (see the description what each colony holds!);
#' both outputs have the split even slot set do \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones = createDrones(basePop[1], n = 1000)
#' fatherGroups = pullDroneGroupsFromDCA(drones, n = 10, nFathers = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' (colony <- buildUp(colony, nWorkers = 100))
#' apiary <- createColonies(basePop[3:8], n = 6)
#' apiary <- cross(apiary, fathers = fatherGroups[2:7])
#' apiary <- buildUp(apiary, nWorkers = 100)
#'
#' # Split a colony
#' tmp <- split(colony)
#' tmp$split
#' tmp$remnant
#'
#' # Split all colonies in the apiary with p = 0.5 (50% of workers in each split)
#' tmp <- split(apiary, p = 0.5)
#' tmp$split[[1]]
#' tmp$remnant[[1]]
#'
#' # Split only specific colonies in the apiary
#' tmp <- pullColonies(apiary, ID = c(4, 5))
#' # Split only the pulled colonies
#' (split(tmp$pulledColonies, p = 0.5))
#'
#' @export
split <- function(x, p = NULL, year = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(p)) {
    p <- simParamBee$splitP
  }

  if (isColony(x)) {
    if (is.function(p)) {
      p <- p(colony)
    }
    if (p < 0 | 1 < p) {
      stop("p must be between 0 and 1 (inclusive)!")
    }
    nWorkers <- nWorkers(x)
    nWorkersSplit <- round(nWorkers * p)
    # TODO: Split colony splits at random by default, but we could make it as a
    #       function of some parameters
    #       https://github.com/HighlanderLab/SIMplyBee/issues/179
    tmp <- pullWorkers(x = x, nInd = nWorkersSplit)

    remnantColony <- tmp$colony

    tmpVirginQueens <- createVirginQueens(
      x = x, nInd = 1,
      year = year
    )
    splitColony <- createColony(x = tmpVirginQueens)
    splitColony@workers <- tmp$pulled
    # Workers raise virgin queens from eggs laid by the queen (assuming) that
    #   a frame of brood is also provided to the split and then one random virgin
    #   queen prevails, so we create just one
    # TODO: Could consider that a non-random one prevails (say the most aggressive
    #       one), by creating many virgin queens and then picking the one with
    #       highest pheno for competition or some other criteria
    #       https://github.com/HighlanderLab/SIMplyBee/issues/239

    splitColony <- setLocation(x = splitColony, location = getLocation(splitColony))

    remnantColony@last_event <- "remnant"
    splitColony@last_event <- "split"

    remnantColony@split <- TRUE
    splitColony@split <- TRUE

    remnantColony@production <- TRUE
    splitColony@production <- FALSE

    ret <- list(split = splitColony, remnant = remnantColony)
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    if (nCol == 0) {
      ret <- list(
        splits = createColonies(),
        remnants = createColonies()
      )
    } else {
      ret <- list(
        splits = createColonies(n = nCol),
        remnants = createColonies(n = nCol)
      )
      for (colony in seq_len(nCol)) {
        tmp <- split(x[[colony]],
                     p = p, year = year,
                     simParamBee = simParamBee
        )
        ret$splits[[colony]] <- tmp$split
        ret$remnants[[colony]] <- tmp$remnant
      }
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }

  validObject(ret$splitColony)
  validObject(ret$remnantColony)
  return(ret)
}

#' @rdname combine
#' @title Combine two colony objects
#'
#' @description Level 2 function that combines two colony objects into one or
#'   two colonies objects of the same length to one. For example, to combine a
#'   weak and a strong colony (or colonies). Workers and drones of the weak
#'   colony are added to the strong. User has to remove the weak colony (or
#'   colonies) from the workspace.
#'
#' @param strong \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param weak \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return a combined \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 70)
#' col1 <- createColony(x = basePop[2])
#' col1 <- crossColony(col1, drones = drones[1:10], nFathers = 5)
#' col2 <- createColony(x = basePop[3])
#' col2 <- crossColony(col2, drones = drones[11:20], nFathers = 5)
#' col1 <- buildUp(colony = col1, nWorkers = 100, nDrones = 10)
#' col1
#' col2 <- buildUp(colony = col2, nWorkers = 20, nDrones = 2)
#' col2
#' col1 <- combine(strong = col1, weak = col2)
#' col1
#' rm(col2)
#'
#' drones <- createDrones(x = basePop[1], nInd = 70)
#' col1 <- createColony(x = basePop[4])
#' col1 <- crossColony(col1, drones = drones[1:10], nFathers = 5)
#' col2 <- createColony(x = basePop[5])
#' col2 <- crossColony(col2, drones = drones[11:20], nFathers = 5)
#' col3 <- createColony(x = basePop[6])
#' col3 <- crossColony(col3, drones = drones[21:30], nFathers = 5)
#' col4 <- createColony(x = basePop[7])
#' col4 <- crossColony(col4, drones = drones[31:40], nFathers = 5)
#' col1 <- buildUp(colony = col1, nWorkers = 100, nDrones = 10)
#' col2 <- buildUp(colony = col2, nWorkers = 20, nDrones = 2)
#' col3 <- buildUp(colony = col3, nWorkers = 100, nDrones = 10)
#' col4 <- buildUp(colony = col4, nWorkers = 20, nDrones = 2)
#' colsStrong <- c(col1, col3)
#' colsStrong[[1]]
#' colsStrong[[2]]
#' colsWeak <- c(col2, col4)
#' cols <- combine(strong = colsStrong, weak = colsWeak)
#' cols[[1]]
#' cols[[2]]
#' rm(colsWeak)
#' @export
combine <- function(strong, weak) {
  if (isColony(strong) & isColony(weak)) {
    strong@workers <- c(strong@workers, weak@workers)
    strong@drones <- c(strong@drones, weak@drones)
  } else if (isColonies(strong) & isColonies(weak)) {
    if (nColonies(weak) == nColonies(strong)) {
      nCol <- nColonies(weak)
      for (colony in seq_len(nCol)) {
        strong[[colony]] <- combine(strong = strong[[colony]], weak = weak[[colony]])
      }
    } else {
      stop("Weak and strong colonies objects must be of the same length!")
    }
  } else {
    stop("Argument strong and weak must both be either a Colony or Colonies class objects!")
  }
  return(strong)
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
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 20)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:10], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[11:20], nFathers = 5)
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

#' @rdname setColonyPheno
#' @title Set colony phenotype
#'
#' @description Level 2 function that sets phenotypes for all colony individuals
#'   (queen, workers, drones, and virgin queens) and for the colony or each colony in
#'   the colonies.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param colonyFUN function, any function that can be run on \code{colony} and
#'   returns colony phenotypes; if \code{NULL} then
#'   \code{\link{SimParamBee}$phenoColony} is used - if even this is \code{NULL},
#'   then colony phenotype is not set, but phenotypes of colony individuals are
#' @param ... all arguments of \code{\link{setPheno}} and \code{colonyFUN}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details When this function is called on a colony, phenotypes for all colony
#'   individuals is set or reset if phenotypes already exist.
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}} with phenotypes
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#'
#' # Define two traits that collectively affect colony honey yield:
#' # 1) queen's effect on colony honey yield
#' # 2) workers' effect on colony honey yield
#' # The traits will have negative genetic correlation and heritability of 0.25
#' meanP <- c(20, 0)
#' varA <- c(1, 1 / 10)
#' corA <- matrix(data = c(
#'   1.0, -0.5,
#'   -0.5, 1.0
#' ), nrow = 2, byrow = TRUE)
#' varE <- c(3, 3 / 10)
#' varA / (varA + varE)
#' SP$addTraitA(nQtlPerChr = 100, mean = meanP, var = varA, corA = corA)
#' SP$setVarE(varE = varE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 100)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 5, nFathers = 14)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' colony <- buildUp(colony, nWorkers = 10)
#' apiary <- createColonies(basePop[3:5], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(2,3)])
#' apiary <- buildUp(apiary, nWorkers = 10)
#'
#' # Set phenotypes for all colony individuals
#' colony <- setColonyPheno(colony)
#' apiary <- setColonyPheno(apiary)
#'
#' # Queen's phenotype for both traits
#' pheno(getQueen(colony))
#' lapply(getQueen(apiary), FUN = pheno)
#' # TODO: use getQueensPheno(colony, caste = "queen")
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/26
#'
#' # Workers' phenotype for both traits
#' pheno(getWorkers(colony))
#' lapply(getWorkers(apiary), FUN = pheno)
#' # TODO: use getWorkersPheno(colony, caste = "queen")
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/26
#'
#' # TODO: use getColonyPheno(colony) for all individuals
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/26
#'
#' # Set phenotypes for all colony individuals AND colony
#' colony <- setColonyPheno(colony, colonyFUN = phenoQueenPlusSumOfWorkers)
#' pheno(colony)
#' # Set phenotypes for all colony individuals AND colonies
#' apiary <- setColonyPheno(apiary, colonyFUN = phenoQueenPlusSumOfWorkers)
#' lapply(apiary@colonies, FUN = pheno)
#' # TODO: use getColonyPheno(colony) for all individuals and/or colony
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/26
#'
#' # Colony phenotype - store the colony function into the SP object
#' SP$phenoColony <- phenoQueenPlusSumOfWorkers
#' pheno(setColonyPheno(colony))
#' pheno(setColonyPheno(colony))
#' lapply(setColonyPheno(apiary)@colonies, FUN = pheno)
#' lapply(setColonyPheno(apiary)@colonies, FUN = pheno)
#' # phenotype will vary between function calls
#'
#' # TODO:
#' # See
#' #     https://github.com/HighlanderLab/SIMplyBee/issues/26
#' #     https://github.com/HighlanderLab/SIMplyBee/issues/28
#' #     https://github.com/HighlanderLab/SIMplyBee/issues/32
#' @export
setColonyPheno <- function(x, colonyFUN = NULL, ..., simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(colonyFUN)) {
    colonyFUN <- simParamBee$phenoColony
  }
  # TODO: how should we handle the creation of phenotypes when residual variance
  #       is set (then we get phenotypes automatically and we should not call
  #       setPheno() below - this overwrittes previous phenotypes), but when the
  #       residual variance is not set, we have to call setPheno()
  #       https://github.com/HighlanderLab/SIMplyBee/issues/235
  if (isColony(x)) {
    if (!is.null(x@queen)) {
      x@queen <- setPheno(x@queen, ...)
    }
    if (!is.null(x@workers)) {
      x@workers <- setPheno(x@workers, ...)
    }
    if (!is.null(x@drones)) {
      x@drones <- setPheno(x@drones, ...)
    }
    if (!is.null(x@virginQueens)) {
      x@virginQueens <- setPheno(x@virginQueens, ...)
    }
    if (!is.null(colonyFUN)) {
      x@pheno <- colonyFUN(x, ...)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- setColonyPheno(x[[colony]],
                                    colonyFUN = colonyFUN, ...,
                                    # TODO: is ... really passed on to setPheno?
                                    #       https://github.com/HighlanderLab/SIMplyBee/issues/240
                                    simParamBee = simParamBee
      )
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }

  validObject(x)
  return(x)
}
