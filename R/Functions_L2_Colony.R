# ---- Level 2 Colony Functions ----

#' @rdname createColony
#' @title Create a new Colony
#'
#' @description Level 2 function that creates a new \code{\link{Colony-class}}
#'   to initiate simulations.
#'
#' @param x \code{\link{Pop-class}}, one queen or virgin queen(s)
#' @param location numeric, location of the colony as \code{c(x, y)}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return new \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 15)
#'
#' # Create an empty Colony class with one or multiple virgin queens
#' colony1 <- createColony(x = basePop[2])
#' colony1
#' colony2 <- createColony(x = basePop[3:4])
#' colony2
#'
#' # Create a mated Colony
#' colony1 <- cross(colony1, drones = drones)
#' colony1
#' @export
createColony <- function(x = NULL, location = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  simParamBee$updateLastColonyId()
  if (is.null(x)) {
    colony <- new(
      Class = "Colony",
      id = simParamBee$lastColonyId,
      location = location
    )
  } else {
    if (!isPop(x)) {
      stop("Argument x must be a Pop class object!")
    }
    if (all(isQueen(x))) {
      if (1 < nInd(x)) {
        stop("You must provide just one queen for the colony!")
      }
      queen <- x
      virginQueens <- NULL
    } else if (all(isVirginQueen(x))) {
      queen <- NULL
      virginQueens <- x
    } else {
      stop("Argument x must hold one queen or virgin queen(s)!")
    }

    colony <- new(
      Class = "Colony",
      id = simParamBee$lastColonyId,
      location = location,
      queen = queen,
      virginQueens = virginQueens
    )
  }
  colony <- resetEvents(colony)
  validObject(colony)
  return(colony)
}

#' @rdname reQueen
#' @title Re-queen
#'
#' @description Level 2 function that re-queens a Colony or
#'   MultiColony object by adding a mated or a virgin queen, removing the
#'   previous queen, and changing the colony id to the new mated queen.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
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
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}} with new queen(s) (see details)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 12, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 200)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 7, nDrones = nFathersPoisson)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- addVirginQueens(colony)
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[2:3])
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
#' matedQueens <- cross(x = basePop[9:12], drones = droneGroups[4:7])
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
#' @export
reQueen <- function(x, queen, removeVirginQueens = TRUE) {
  if (!isPop(queen)) {
    stop("Argument queen must be a Pop class object!")
  }
  if (!all(isVirginQueen(queen) | isQueen(queen))) {
    stop("Individual in queen must be a virgin queen or a queen!")
  }
  if (isColony(x)) {
    if (all(isQueen(queen))) {
      if (nInd(queen) > 1) {
        stop("You must provide just one queen for the colony!")
      }
      x@queen <- queen
      if (removeVirginQueens) {
        x <- removeVirginQueens(x)
      }
    } else {
      x <- removeQueen(x, addVirginQueens = FALSE)
      x@virginQueens <- queen
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    if (nInd(queen) < nCol) {
      stop("Not enough queens provided!")
    }
    for (colony in seq_len(nCol)) {
      x[[colony]] <- reQueen(
        x = x[[colony]],
        queen = queen[colony]
      )
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname addCastePop
#' @title Add caste individuals to the colony
#'
#' @description Level 2 function that adds (raises) the specified number of
#'   a specific caste individuals to a Colony or MultiColony object by producing
#'   offspring from a mated queen. If there are already some individuals present
#'   in the caste, new and present individuals are combined.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "workers", "drones", or "virginQueens"
#' @param nInd numeric or function, number of workers to be added, but see
#'   \code{new}; if \code{NULL} then \code{\link{SimParamBee}$nWorkers} is used.
#'   If input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be used for all the colonies.
#' @param new logical, should the number of individuals be added to the caste population
#'   anew or should we only top-up the existing number of individuals to \code{nInd}
#' @param exact logical, only relevant when adding workers - if the csd locus is turned
#'  on and exact is \code{TRUE}, we add the exact specified number of viable workers
#'  (heterozygous at the csd locus)
#' @param year numeric, only relevant when adding virgin queens - year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... additional arguments passed to \code{nInd} when this argument is a function
#'
#' @details This function increases queen's \code{nWorkers} and \code{nHomBrood}
#'   counters.
#'
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}} with
#'   workers added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[4:5], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[3:4])
#'
#' # Add virgin queens
#' addCastePop(colony, caste = "virginQueens", nInd = 20)
#' # Or use a alias function
#' addVirginQueens(colony, nInd = 20)
#' nVirginQueens(addVirginQueens(apiary, nInd = 20))
#'
#' # Add workers
#' addCastePop(colony, caste = "workers", nInd = 20)
#' # Or use a alias function
#' addWorkers(colony, nInd = 20)
#' nWorkers(addWorkers(apiary, nInd = 20))
#'
#' # Add drones
#' addCastePop(colony, caste = "drones", nInd = 20)
#' # Or use a alias function
#' addDrones(colony, nInd = 20)
#' nDrones(addDrones(apiary, nInd = 20))
#'
#' # Using a default in SP$nWorkers
#' # (just to have some individuals - change this to your needs!)
#' addVirginQueens(colony)
#' addDrones(colony)
#' addWorkers(colony)
#' nWorkers(addWorkers(apiary))
#'
#' # Specify own number
#' SP$nVirginQueens <- 2
#' SP$nWorkers <- 15
#' SP$nDrones <- 3
#' nVirginQueens(addVirginQueens(colony))
#' nWorkers(addWorkers(colony))
#' nDrones(addDrones(colony))
#' # nVirginQueens/nWorkers/nDrones will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' nVirginQueens(addVirginQueens(colony, nInd = nVirginQueensPoisson))
#' nVirginQueens(addVirginQueens(colony, nInd = nVirginQueensPoisson))
#' nWorkers(addWorkers(colony, nInd = nWorkersPoisson))
#' nWorkers(addWorkers(colony, nInd = nWorkersPoisson))
#' nDrones(addDrones(colony, nInd = nDronesPoisson))
#' nDrones(addDrones(colony, nInd = nDronesPoisson))
#' # nVirginQueens/nWorkers/nDrones will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nVirginQueens <- nVirginQueensPoisson
#' SP$nWorkers <- nWorkersPoisson
#' SP$nDrones <- nDronesPoisson
#' (addVirginQueens(colony))
#' (addWorkers(colony))
#' (addDrones(colony))
#' # nVirginQueens/nWorkers/nDrones will vary between function calls when a function is used
#'
#' # Queen's counters
#' getMisc(getQueen(addWorkers(colony)))
#'
#' # Add individuals to a MultiColony object
#' nWorkers(addWorkers(apiary, nInd = 100, new = TRUE))
#' nDrones(addDrones(apiary, nInd = 10))
#' # Add different number of workers to colonies
#' nWorkers(addWorkers(apiary, nInd = c(50, 100)))
#' nDrones(addDrones(apiary, nInd = c(100, 10)))
#'
#' @export
addCastePop <- function(x, caste = NULL, nInd = NULL, new = FALSE,
                        exact = FALSE, year = NULL, simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) != 1) {
    stop("Argument caste must be of length 1!")
  }
  if (is.null(nInd)) {
    if (caste == "workers") {
      nInd <- simParamBee$nWorkers
    } else if (caste == "drones") {
      nInd <- simParamBee$nDrones
    }  else if (caste == "virginQueens") {
      nInd <- simParamBee$nVirginQueens
    }
  }
  # doing "if (is.function(nInd))" below
  if (isColony(x)) {
    if (hasCollapsed(x)) {
      stop(paste0("The colony ", getId(x), " collapsed, hence you can not add individuals (from the queen) to it!"))
    }
    if (!isQueenPresent(x)) {
      stop("Missing queen!")
    }
    if (length(nInd) > 1) {
      warning("More than one value in the nInd argument, taking only the first value!")
      p <- p[1]
    }
    if (is.function(nInd)) {
      nInd <- nInd(x, ...)
    } else {
      if (!is.null(nInd) && nInd < 0) {
        stop("nInd must be non-negative or NULL!")
      }
    }
    if (0 < nInd) {
      newInds <- createCastePop(x, nInd,
                                caste = caste, exact = exact,
                                year = year, simParamBee = simParamBee
      )
      if (caste == "workers") {
        homInds <- newInds$nHomBrood
        newInds <- newInds$workers
        x@queen@misc[[1]]$nWorkers <- x@queen@misc[[1]]$nWorkers + nInd(newInds)
        x@queen@misc[[1]]$nHomBrood <- x@queen@misc[[1]]$nHomBrood + homInds
      }
      if (caste == "drones") {
        x@queen@misc[[1]]$nDrones <- x@queen@misc[[1]]$nDrones + nInd(newInds)
      }
      if (is.null(slot(x, caste)) | new) {
        slot(x, caste) <- newInds
      } else {
        slot(x, caste) <- c(slot(x, caste), newInds)
      }
    } else {
      warning("The number of individuals to add is less than 0, hence adding nothing.")
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nNInd <- length(nInd)
    if (nNInd > 1 && nNInd < nCol) {
      stop("Too few values in the nInd argument!")
    }
    if (nNInd > 1 && nNInd > nCol) {
      warning(paste0("Too many values in the nInd argument, taking only the first ", nCol, "values!"))
      nInd <- nInd[1:nCol]
    }
    for (colony in seq_len(nCol)) {
      if (is.null(nInd)) {
        nIndColony <- NULL
      } else {
        nIndColony <- ifelse(nNInd == 1, nInd, nInd[colony])
      }
      x[[colony]] <- addCastePop(
        x = x[[colony]], caste = caste,
        nInd = nIndColony,
        new = new,
        exact = exact, simParamBee = simParamBee, ...
      )
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  validObject(x)
  return(x)
}

#' @describeIn addCastePop Add workers to a colony
#' @export
addWorkers <- function(x, nInd = NULL, new = FALSE,
                       exact = FALSE, simParamBee = NULL, ...) {
  ret <- addCastePop(
    x = x, caste = "workers", nInd = nInd, new = new,
    exact = exact, simParamBee = simParamBee, ...
  )
  return(ret)
}

#' @describeIn addCastePop Add drones to a colony
#' @export
addDrones <- function(x, nInd = NULL, new = FALSE, simParamBee = NULL, ...) {
  ret <- addCastePop(
    x = x, caste = "drones", nInd = nInd, new = new,
    simParamBee = simParamBee, ...
  )
  return(ret)
}

#' @describeIn addCastePop Add virgin queens to a colony
#' @export
addVirginQueens <- function(x, nInd = NULL, new = FALSE,
                            year = NULL, simParamBee = NULL, ...) {
  ret <- addCastePop(
    x = x, caste = "virginQueens", nInd = nInd, new = new,
    year = year, simParamBee = simParamBee, ...
  )
  return(ret)
}

#' @rdname buildUp
#' @title Build up Colony or MultiColony object by adding (raising) workers and drones
#'
#' @description Level 2 function that builds up a Colony or MultiColony object by adding
#'   (raising) workers and drones usually in spring or after events such as split or
#'   swarming.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param nWorkers numeric or function, number of worker to add to the colony,
#'   but see \code{new}; if \code{NULL} then \code{\link{SimParamBee}$nWorkers}
#'   is used. If input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies.
#' @param nDrones numeric or function, number of drones to add to the colony,
#'   but see \code{new}; if \code{NULL} then \code{\link{SimParamBee}$nDrones}
#'   is used. If input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies.
#' @param new logical, should the number of workers and drones be added anew or
#'   should we only top-up the existing number of workers and drones to
#'   \code{nWorkers} and \code{nDrones} (see details)
#' @param exact logical, if the csd locus is turned on and exact is \code{TRUE},
#'   create the exact specified number of only viable workers (heterozygous on
#'   the csd locus)
#' @param resetEvents logical, call \code{\link{resetEvents}} as part of the
#'   build up
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... additional arguments passed to \code{nWorkers} or \code{nDrones}
#'   when these arguments are a function
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
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}} with workers and
#'    drones replaced or added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' isProductive(colony)
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' isProductive(apiary)
#'
#' # Build up
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
#' # Build up with different numbers
#' apiary <- buildUp(apiary, nWorkers = c(1000, 2000), nDrones = c(100, 150))
#' nWorkers(apiary)
#' nDrones(apiary)
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
#' colony <- cross(colony, drones = droneGroups[[4]])
#' buildUp(colony, nWorkers = nWorkersPoisson, nDrones = nDronesPoisson)
#' buildUp(colony, nWorkers = nWorkersPoisson, nDrones = nDronesPoisson)
#' # nWorkers and nDrones will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nWorkers <- nWorkersPoisson
#' SP$nDrones <- nDronesPoisson
#' # Create new drones and a new colony
#' colony <- createColony(x = basePop[6])
#' colony <- cross(colony, drones = droneGroups[[5]])
#' buildUp(colony)
#' buildUp(colony)
#' # nWorkers and nDrones will vary between function calls when a function is used
#' # Same for MultiColony class
#' apiary <- createMultiColony(basePop[7:8], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(6, 7)])
#' buildUp(apiary)[[1]]
#' buildUp(apiary)[[1]]
#'
#' # Queen's counters
#' getMisc(getQueen(buildUp(colony)))
#' @export
buildUp <- function(x, nWorkers = NULL, nDrones = NULL,
                    new = TRUE, exact = FALSE, resetEvents = FALSE,
                    simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (hasCollapsed(x)) {
      stop(paste0("The colony ", getId(x), " collapsed, hence you can not build it up!"))
    }
    # Workers
    if (is.null(nWorkers)) {
      nWorkers <- simParamBee$nWorkers
    }
    if (is.function(nWorkers)) {
      nWorkers <- nWorkers(colony = x, ...)
    }
    if (length(nWorkers) > 1) {
      warning("More than one value in the nWorkers argument, taking only the first value!")
      nWorkers <- nWorkers[1]
    }
    if (new) {
      n <- nWorkers
    } else {
      n <- nWorkers - nWorkers(x)
    }

    x <- addWorkers(
      x = x, nInd = n, new = new,
      exact = exact, simParamBee = simParamBee
    )

    # Drones
    if (is.null(nDrones)) {
      nDrones <- simParamBee$nDrones
    }
    if (is.function(nDrones)) {
      nDrones <- nDrones(x = x, ...)
    }
    if (length(nDrones) > 1) {
      warning("More than one value in the nDrones argument, taking only the first value!")
      nDrones <- nDrones[1]
    }
    if (new) {
      n <- nDrones
    } else {
      n <- nDrones - nDrones(x)
    }
    x <- addDrones(
      x = x, nInd = n, new = new,
      simParamBee = simParamBee
    )

    # Events
    if (resetEvents) {
      x <- resetEvents(x)
    }
    x@production <- TRUE
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nNWorkers <- length(nWorkers)
    nNDrones <- length(nDrones)
    if (nNWorkers > 1 && nNWorkers < nCol) {
      stop("Too few values in the nWorkers argument!")
    }
    if (nNDrones > 1 && nNDrones < nCol) {
      stop("Too few values in the nDrones argument!")
    }
    if (nNWorkers > 1 && nNWorkers > nCol) {
      warning(paste0("Too many values in the nWorkers argument, taking only the first ", nCol, "values!"))
      nWorkers <- nWorkers[1:nCol]
    }
    if (nNDrones > 1 && nNDrones > nCol) {
      warning(paste0("Too many values in the nDrones argument, taking only the first ", nCol, "values!"))
      nNDrones <- nNDrones[1:nCol]
    }
    for (colony in seq_len(nCol)) {
      if (is.null(nWorkers)) {
        nWorkersColony <- NULL
      } else {
        nWorkersColony <- ifelse(nNWorkers == 1, nWorkers, nWorkers[colony])
      }
      if (is.null(nDrones)) {
        nDronesColony <- NULL
      } else {
        nDronesColony <- ifelse(nNDrones == 1, nDrones, nDrones[colony])
      }
      x[[colony]] <- buildUp(
        x = x[[colony]],
        nWorkers = nWorkersColony,
        nDrones = nDronesColony,
        new = new,
        exact = exact,
        resetEvents = resetEvents,
        simParamBee = simParamBee, ...
      )
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }

  validObject(x)
  return(x)
}

#' @rdname downsize
#' @title Reduce number of workers and remove all drones and virgin queens from
#'   a Colony or MultiColony object
#'
#' @description Level 2 function that downsizes a Colony or MultiColony object
#'   by removing a proportion of workers, all drones and all virgin queens.
#'   Usually in the autumn, such an event occurs in preparation for the winter months.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param p numeric, proportion of workers to be removed from the colony; if
#'   \code{NULL} then \code{\link{SimParamBee}$downsizeP} is used.
#'   If input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies
#' @param use character, all the options provided by \code{\link{selectInd}};
#'   it guides the selection of workers that will be removed
#' @param new logical, should we remove all current workers and add a targeted
#'   proportion anew (say, create winter workers)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... additional arguments passed to \code{p} when this argument is a
#'   function
#'
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}} with workers reduced and
#'   drones/virgin queens removed
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 12)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(colony)
#' colony <- addVirginQueens(x = colony, nInd = 10)
#' colony
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(apiary)
#' nWorkers(apiary); nDrones(apiary)
#' apiary <- addVirginQueens(apiary, nInd = 10)
#' apiary
#'
#' # Downsize
#' colony <- downsize(x = colony, new = TRUE, use = "rand")
#' colony
#' apiary <- downsize(x = apiary, new = TRUE, use = "rand")
#' apiary[[1]]
#' # Downsize with different numbers
#' nWorkers(apiary); nDrones(apiary)
#' apiary <- downsize(x = apiary, p = c(0.5, 0.1), new = TRUE, use = "rand")
#' nWorkers(apiary); nDrones(apiary)
#' @export
downsize <- function(x, p = NULL, use = "rand", new = FALSE,
                     simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!is.logical(new)) {
    stop("Argument new must be logical!")
  }
  if (any(1 < p)) {
    stop("p must not be higher than 1!")
  } else if (any(p < 0)) {
    stop("p must not be less than 0!")
  }
  if (isColony(x)) {
    if (hasCollapsed(x)) {
      stop(paste0("The colony ", getId(x), " collapsed, hence you can not downsize it!"))
    }
    if (is.null(p)) {
      p <- simParamBee$downsizeP
    }
    if (is.function(p)) {
      p <- p(x, ...)
    }
    if (length(p) > 1) {
      warning("More than one value in the p argument, taking only the first value!")
      p <- p[1]
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
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nP <- length(p)
    if (nP > 1 && nP < nCol) {
      stop("Too few values in the p argument!")
    }
    if (nP > 1 && nP > nCol) {
      warning(paste0("Too many values in the p argument, taking only the first ", nCol, "values!"))
      p <- p[1:nCol]
    }
    for (colony in seq_len(nCol)) {
      if (is.null(p)) {
        pColony <- NULL
      } else {
        pColony <- ifelse(nP == 1, p, p[colony])
      }
      x[[colony]] <- downsize(
        x = x[[colony]],
        p = pColony,
        use = use,
        new = new,
        simParamBee = simParamBee, ...
      )
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }

  validObject(x)
  return(x)
}

#' @rdname replaceCastePop
#' @title Replace a proportion of caste individuals with new ones
#'
#' @description Level 2 function that replaces a proportion of caste individuals
#'   with new individuals from a Colony or MultiColony object. Useful after
#'   events like season change, swarming, supersedure, etc. due to the short life span
#'   honeybees.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "workers", "drones", or "virginQueens"
#' @param p numeric, proportion of caste individuals to be replaced with new ones;
#'   if input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of caste individuals that stay when \code{p < 1}
#' @param exact logical, only relevant when adding workers - if the csd locus is turned
#'  on and exact is \code{TRUE}, we replace the exact specified number of viable workers
#'  (heterozygous at the csd locus). You probably want this set to TRUE since you want to
#'  replace with the same number of workers.
#' @param year numeric, only relevant when replacing virgin queens,
#'   year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or  or \code{\link{MultiColony-class}} with
#'   replaced virgin queens
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[4:5], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[3:4])
#'
#' # Add individuals
#' colony <- buildUp(colony, nWorkers = 10, nDrones = 10)
#' colony <- addVirginQueens(colony, nInd = 5)
#' apiary <- buildUp(apiary, nWorkers = 10, nDrones = 10)
#' apiary <- addVirginQueens(apiary, nInd = 20)
#'
#' # Replace individuals in a colony
#' getVirginQueens(colony)@id
#' colony <- replaceCastePop(colony, caste = "virginQueens", p = 0.5)
#' # or alias: replaceVirginQueens(colony, p = 0.5)
#' getVirginQueens(colony)@id
#' getWorkers(colony)@id
#' colony <- replaceCastePop(colony, caste = "workers", p = 0.5)
#' # or alias: replaceWorkers(colony, p = 0.5)
#' getWorkers(colony)@id
#' getDrones(colony)@id
#' colony <- replaceCastePop(colony, caste = "drones", p = 0.5)
#' # or alias: replaceDrones(colony, p = 0.5)
#' getDrones(colony)@id
#'
#' lapply(getVirginQueens(apiary), FUN = function(x) x@id)
#' apiary <- replaceVirginQueens(apiary, p = 0.5)
#' lapply(getVirginQueens(apiary), FUN = function(x) x@id)
#' apiary <- replaceWorkers(apiary, p = 1)
#' lapply(getWorkers(apiary), FUN = function(x) x@id)
#' apiary <- replaceDrones(apiary, p = 1)
#' lapply(getDrones(apiary), FUN = function(x) x@id)
#' # Replace with different proportions
#' apiary <- replaceWorkers(apiary, p = c(1, 0.7))
#' lapply(getWorkers(apiary), FUN = function(x) x@id)
#' apiary <- replaceDrones(apiary, p = c(1, 0.1))
#' lapply(getDrones(apiary), FUN = function(x) x@id)
#' @export
replaceCastePop <- function(x, caste = NULL, p = 1, use = "rand", exact = TRUE,
                            year = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) != 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(1 < p)) {
    stop("p must not be higher than 1!")
  } else if (any(p < 0)) {
    stop("p must not be less than 0!")
  }
  if (isColony(x)) {
    if (hasCollapsed(x)) {
      stop(paste0("The colony ", getId(x), " collapsed, hence you can not replace individuals in it!"))
    }
    if (!isQueenPresent(x)) {
      stop("Missing queen!")
    }
    if (length(p) > 1) {
      warning("More than one value in the p argument, taking only the first value!")
      p <- p[1]
    }
    nInd <- nCaste(x, caste)
    if (nInd > 0) {
      nIndReplaced <- round(nInd * p)
      if (nIndReplaced < nInd) {
        nIndStay <- nInd - nIndReplaced
        if (nIndReplaced > 0) {
          tmp <- createCastePop(x,
                                caste = caste,
                                nInd = nIndReplaced, exact = exact,
                                year = year, simParamBee = simParamBee
          )
          if (caste == "workers") {
            x@queen@misc[[1]]$nWorkers <- x@queen@misc[[1]]$nWorkers + nIndReplaced
            x@queen@misc[[1]]$nHomBrood <- x@queen@misc[[1]]$nHomBrood + tmp$nHomBrood
            tmp <- tmp$workers
          }
          if (caste == "drones") {
            x@queen@misc[[1]]$nDrones <- x@queen@misc[[1]]$nDrones + nIndReplaced
          }

          slot(x, caste) <- c(
            selectInd(slot(x, caste), nInd = nIndStay, use = use),
            tmp
          )
        }
      } else {
        x <- addCastePop(
          x = x, caste = caste, nInd = nIndReplaced, new = TRUE,
          year = year, simParamBee = simParamBee
        )
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nP <- length(p)
    if (nP > 1 && nP < nCol) {
      stop("Too few values in the p argument!")
    }
    if (nP > 1 && nP > nCol) {
      warning(paste0("Too many values in the p argument, taking only the first ", nCol, "values!"))
      p <- p[1:nCol]
    }
    for (colony in seq_len(nCol)) {
      if (is.null(p)) {
        pColony <- NULL
      } else {
        pColony <- ifelse(nP == 1, p, p[colony])
      }
      x[[colony]] <- replaceCastePop(
        x = x[[colony]], caste = caste,
        p = pColony,
        use = use, year = year,
        simParamBee = simParamBee
      )
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  validObject(x)
  return(x)
}

#' @describeIn replaceCastePop Replaces some workers in a colony
#' @export
replaceWorkers <- function(x, p = 1, use = "rand", exact = TRUE, simParamBee = NULL) {
  ret <- replaceCastePop(
    x = x, caste = "workers", p = p,
    use = use, exact = exact,
    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn replaceCastePop Replaces some drones in a colony
#' @export
replaceDrones <- function(x, p = 1, use = "rand", simParamBee = NULL) {
  ret <- replaceCastePop(
    x = x, caste = "drones", p = p,
    use = use, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn replaceCastePop Replaces some virgin queens in a colony
#' @export
replaceVirginQueens <- function(x, p = 1, use = "rand", simParamBee = NULL) {
  ret <- replaceCastePop(
    x = x, caste = "virginQueens", p = p,
    use = use, simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname removeCastePop
#' @title Remove a proportion of caste individuals from a colony
#'
#' @description Level 2 function that removes a proportion of virgin queens of
#'   a Colony or MultiColony object
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "workers", "drones", or "virginQueens"
#' @param p numeric, proportion to be removed; if input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of virgins queens that will stay when \code{p < 1}
#' @param addVirginQueens logical, whether virgin queens should be added; only
#'   used when removing the queen from the colony
#' @param nVirginQueens integer, the number of virgin queens to be created in the
#'   colony; only used when removing the queen from the colony. If \code{0}, no virgin
#'   queens are added; If \code{NULL}, the value from \code{simParamBee$nVirginQueens}
#'   is used
#' @param year numeric, only relevant when adding virgin queens - year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}} without virgin queens
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(colony)
#' apiary <- createMultiColony(basePop[4:5], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[3:4])
#' apiary <- buildUp(apiary)
#'
#' # Add virgin queens
#' colony <- addVirginQueens(colony, nInd = 10)
#' apiary <- addVirginQueens(apiary, nInd = 10)
#'
#' # Remove virgin queens
#' nVirginQueens(colony)
#' colony <- removeCastePop(colony, caste = "virginQueens", p = 0.5)
#' # or alias: removeVirginQueens(colony, p = 0.5)
#' nVirginQueens(colony)
#' colony <- removeCastePop(colony, caste = "virginQueens")
#' # or alias: colony <- removeVirginQueens(colony)
#' nVirginQueens(colony)
#' nWorkers(colony)
#' nDrones(colony)
#' colony <- removeCastePop(colony, caste = "workers", p = 0.3)
#' # or alias: colony <- removeWorkers(colony, p = 0.3)
#' colony <- removeCastePop(colony, caste = "drones", p = 0.3)
#' # or alias: colony <- removeDrones(colony, p = 0.3)
#'
#' nVirginQueens(apiary)
#' nWorkers(apiary)
#' nDrones(apiary)
#' apiary <- removeCastePop(apiary, caste = "virginQueens", p = 0.3)
#' # or alias: removeVirginQueens(apiary, p = 0.3)
#' nVirginQueens(apiary)
#' apiary <- removeCastePop(apiary, caste = "workers", p = 0.3)
#' # or alias: removeWorkers(apiary, p = 0.3)
#' nWorkers(apiary)
#' #' apiary <- removeCastePop(apiary, caste = "drones", p = 0.3)
#' # or alias: removeDrones(apiary, p = 0.3)
#' nDrones(apiary)
#' # Remove different proportions
#' apiary <- buildUp(apiary)
#' nWorkers(apiary); nDrones(apiary)
#' nWorkers(removeWorkers(apiary, p = c(0.1, 0.5)))
#' nDrones(removeDrones(apiary, p = c(0.1, 0.9)))
#' @export
removeCastePop <- function(x, caste = NULL, p = 1, use = "rand",
                           addVirginQueens = FALSE, nVirginQueens = NULL,
                           year = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) != 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(1 < p)) {
    stop("p must not be higher than 1!")
  } else if (any(p < 0)) {
    stop("p must not be less than 0!")
  }
  if (isColony(x)) {
    if (hasCollapsed(x)) {
      warning(paste0("The colony ", getId(x), " collapsed. You can only remove individuals from it!"))
    }
    if (length(p) > 1) {
      warning("More than one value in the p argument, taking only the first value!")
      p <- p[1]
    }
    if (caste == "queen") {
      if (addVirginQueens) {
        if (is.null(nVirginQueens)) {
          nVirginQueens <- simParamBee$nVirginQueens
        }
        x <- addVirginQueens(x, nInd = nVirginQueens, year = year, simParamBee = simParamBee)
      }
    }
    if (p == 1) {
      slot(x, caste) <- NULL
    } else {
      nIndStay <- round(nCaste(x, caste) * (1 - p))
      if (nIndStay > 0) {
        slot(x, caste) <- selectInd(
          pop = slot(x, caste),
          nInd = nIndStay,
          use = use
        )
      } else {
        x <- removeCastePop(x, caste)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nP <- length(p)
    if (nP > 1 && nP < nCol) {
      stop("Too few values in the p argument!")
    }
    if (nP > 1 && nP > nCol) {
      warning(paste0("Too many values in the p argument, taking only the first ", nCol, "values!"))
      p <- p[1:nCol]
    }
    for (colony in seq_len(nCol)) {
      if (is.null(p)) {
        pColony <- NULL
      } else {
        pColony <- ifelse(nP == 1, p, p[colony])
      }
      x[[colony]] <- removeCastePop(
        x = x[[colony]], caste = caste,
        p = pColony,
        use = use
      )
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  validObject(x)
  return(x)
}

#' @describeIn removeCastePop Remove queen from a colony
#' @export
removeQueen <- function(x, addVirginQueens = FALSE, nVirginQueens = NULL, year = NULL, simParamBee = NULL) {
  ret <- removeCastePop(x = x, caste = "queen", p = 1, addVirginQueens = addVirginQueens,
                        nVirginQueens = nVirginQueens, year = year, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn removeCastePop Remove workers from a colony
#' @export
removeWorkers <- function(x, p = 1, use = "rand") {
  ret <- removeCastePop(x = x, caste = "workers", p = p, use = use)
  return(ret)
}

#' @describeIn removeCastePop Remove workers from a colony
#' @export
removeDrones <- function(x, p = 1, use = "rand") {
  ret <- removeCastePop(x = x, caste = "drones", p = p, use = use)
  return(ret)
}

#' @describeIn removeCastePop Remove virgin queens from a colony
#' @export
removeVirginQueens <- function(x, p = 1, use = "rand") {
  ret <- removeCastePop(x = x, caste = "virginQueens", p = p, use = use)
  return(ret)
}

#' @rdname resetEvents
#' @title Reset colony events
#'
#' @description Level 2 function that resets the slots swarm, split,
#'   supersedure, collapsed, and production to FALSE in a Colony or MultiColony object.
#'   Useful at the end of a yearly cycle to reset the events, allowing the user to track
#'   new events in a new year.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param collapse logical, reset the collapse event (only sensible in setting
#'   up a new colony, which the default of \code{NULL} caters for; otherwise, a
#'   collapsed colony should be left collapsed forever, unless you force
#'   resetting this event with \code{collapse = TRUE})
#'
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}} with
#'   events reset
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[4:5], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[3:4])
#'
#' # Build-up - this sets Productive to TRUE
#' (colony <- buildUp(colony, nWorkers = 100))
#' isProductive(colony)
#' resetEvents(colony)
#'
#' apiary <- buildUp(apiary, nWorkers = 100)
#' isProductive(apiary[[1]])
#' resetEvents(apiary)[[1]]
#'
#' # Split - this sets Split to TRUE
#' tmp <- split(colony)
#' (split <- tmp$split)
#' hasSplit(split)
#' resetEvents(split)
#' (remnant <- tmp$remnant)
#' hasSplit(remnant)
#' resetEvents(remnant)
#'
#' tmp <- split(apiary)
#' (splits <- tmp$split)
#' hasSplit(splits[[1]])
#' resetEvents(splits)[[1]]
#' (remnants <- tmp$remnant)
#' hasSplit(remnants[[1]])
#' resetEvents(remnants)[[1]]
#'
#' # Swarm - this sets Swarm to TRUE
#' tmp <- swarm(colony)
#' (swarm <- tmp$swarm)
#' hasSwarmed(swarm)
#' resetEvents(swarm)
#' (remnant <- tmp$remnant)
#' hasSwarmed(remnant)
#' resetEvents(remnant)
#'
#' tmp <- swarm(apiary)
#' (swarms <- tmp$swarm)
#' hasSwarmed(swarms[[1]])
#' resetEvents(swarms)[[1]]
#' (remnants <- tmp$remnant)
#' hasSwarmed(remnants[[1]])
#' resetEvents(remnants)[[1]]
#'
#' # Supersede - this sets Supersede to TRUE
#' (tmp <- supersede(colony))
#' hasSuperseded(tmp)
#' resetEvents(tmp)
#'
#' (tmp <- supersede(apiary))
#' hasSuperseded(tmp[[1]])
#' resetEvents(tmp)[[1]]
#'
#' # Collapse - this sets Collapse to TRUE
#' (tmp <- collapse(colony))
#' hasCollapsed(tmp)
#' resetEvents(tmp)
#' resetEvents(tmp, collapse = TRUE)
#'
#' (tmp <- collapse(apiary))
#' hasCollapsed(tmp[[1]])
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
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- resetEvents(
        x = x[[colony]],
        collapse = collapse
      )
    }
    validObject(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(x)
}

#' @rdname collapse
#' @title Collapse
#'
#' @description Level 2 function that collapses a Colony or MultiColony object
#'   by setting the collapse event slot to \code{TRUE}. The production status
#'   slot is also changed (to \code{FALSE}).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}} with the collapse
#'   event set to \code{TRUE}
#'
#' @details You should use this function in an edge-case when you
#'  want to indicate that the colony has collapsed, but you still want to
#'  collect some values from the colony for a retrospective analysis.
#'  It resembles a situation where the colony has collapsed, but dead
#'  bees are still in the hive.
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(basePop[1], n = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[1])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(x = basePop[2:10], n = 9)
#' apiary <- cross(apiary, drones = droneGroups[2:10])
#'
#' # Collapse
#' hasCollapsed(colony)
#' colony <- collapse(colony)
#' hasCollapsed(colony)
#'
#' hasCollapsed(apiary)
#' tmp <- pullColonies(apiary, n = 2)
#' tmp
#' apiaryLost <- collapse(tmp$pulled)
#' hasCollapsed(apiaryLost)
#' apiaryLeft <- tmp$remnant
#' hasCollapsed(apiaryLeft)
#' @export
collapse <- function(x) {
  if (isColony(x)) {
    x@collapse <- TRUE
    x@production <- FALSE
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]] <- collapse(x = x[[colony]])
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname swarm
#' @title Swarm
#'
#' @description Level 2 function that swarms a Colony or MultiColony object -
#'   an event where the queen
#'   leaves with a proportion of workers to create a new colony (the swarm). The
#'   remnant colony retains the other proportion of workers and all drones, and
#'   the workers raise virgin queens, of which only one prevails. Location of
#'   the swarm is the same as for the remnant (for now).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param p numeric, proportion of workers that will leave with the swarm colony;
#'   if \code{NULL} then \code{\link{SimParamBee}$swarmP} is used.
#'   If input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies
#' @param year numeric, year of birth for virgin queens
#' @param nVirginQueens integer, the number of virgin queens to be created in the
#'   colony; of these one is randomly selected as the new virgin queen of the
#'   remnant colony. If \code{NULL}, the value from \code{simParamBee$nVirginQueens}
#'   is used
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... additional arguments passed to \code{p} or \code{nVirginQueens}
#'   when these arguments are functions
#'
#' @return list with two \code{\link{Colony-class}} or \code{\link{MultiColony-class}},
#' the \code{swarm} and the \code{remnant} (see the description what each colony holds!); both
#' outputs have the swarm event set to \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(basePop[1], n = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' (colony <- buildUp(colony, nWorkers = 100))
#' apiary <- createMultiColony(basePop[3:8], n = 6)
#' apiary <- cross(apiary, drones = droneGroups[2:7])
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
#' # Swarm with different proportions
#' nWorkers(apiary)
#' tmp <- swarm(apiary, p = c(0.4, 0.6, 0.5, 0.5, 0.34, 0.56))
#' nWorkers(tmp$swarm)
#' nWorkers(tmp$remnant)
#'
#' # Sample colonies from the apiary that will swarm (sample with probability of 0.2)
#' tmp <- pullColonies(apiary, p = 0.2)
#' # Swarm only the pulled colonies
#' (swarm(tmp$pulled, p = 0.6))
#' @export
swarm <- function(x, p = NULL, year = NULL, nVirginQueens = NULL, simParamBee = NULL, ...) {
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
    if (hasCollapsed(x)) {
      stop(paste0("The colony ", getId(x), " collapsed, hence it can not swarm!"))
    }
    if (!isQueenPresent(x)) {
      stop("No queen present in the colony!")
    }
    if (!isWorkersPresent(x)) {
      stop("No workers present in the colony!")
    }
    if (is.function(p)) {
      p <- p(x, ...)
    } else  {
      if (p < 0 | 1 < p) {
        stop("p must be between 0 and 1 (inclusive)!")
      }
      if (length(p) > 1) {
        warning("More than one value in the p argument, taking only the first value!")
        p <- p[1]
      }
    }
    if (is.function(nVirginQueens)) {
      nVirginQueens <- nVirginQueens(x, ...)
    }
    nWorkers <- nWorkers(x)
    nWorkersSwarm <- round(nWorkers * p)

    # TODO: Add use="something" to select pWorkers that swarm
    #       https://github.com/HighlanderLab/SIMplyBee/issues/160
    tmp <- pullWorkers(x = x, nInd = nWorkersSwarm)
    currentLocation <- getLocation(x)

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
    remnantColony@workers <- getWorkers(tmp$remnant)
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
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nP <- length(p)
    if (nP > 1 && nP < nCol) {
      stop("Too few values in the p argument!")
    }
    if (nP > 1 && nP > nCol) {
      warning(paste0("Too many values in the p argument, taking only the first ", nCol, "values!"))
      p <- p[1:nCol]
    }
    if (nCol == 0) {
      ret <- list(
        swarm = createMultiColony(),
        remnant = createMultiColony()
      )
    } else {
      ret <- list(
        swarm = createMultiColony(n = nCol),
        remnant = createMultiColony(n = nCol)
      )
      for (colony in seq_len(nCol)) {
        if (is.null(p)) {
          pColony <- NULL
        } else {
          pColony <- ifelse(nP == 1, p, p[colony])
        }
        tmp <- swarm(x[[colony]],
                     p = pColony,
                     year = year,
                     nVirginQueens = nVirginQueens,
                     simParamBee = simParamBee, ...
        )
        ret$swarm[[colony]] <- tmp$swarm
        ret$remnant[[colony]] <- tmp$remnant
      }
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }

  validObject(ret$swarmColony)
  validObject(ret$remnantColony)
  return(ret)
}

#' @rdname supersede
#' @title Supersede
#'
#' @description Level 2 function that supersedes a Colony or MultiColony object -
#'   an event where the
#'   queen dies. The workers and drones stay unchanged, but workers raise virgin
#'   queens, of which only one prevails.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param year numeric, year of birth for virgin queens
#' @param nVirginQueens integer, the number of virgin queens to be created in the
#'   colony; of these one is randomly selected as the new virgin queen of the
#'   remnant colony. If \code{NULL}, the value from \code{simParamBee$nVirginQueens}
#'   is used
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... additional arguments passed to \code{nVirginQueens} when this
#'   argument is a function
#'
#' @return  \code{\link{Colony-class}} or \code{\link{MultiColony-class}} with the
#' supersede event set to \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(basePop[1], n = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' (colony <- buildUp(colony, nWorkers = 100))
#' apiary <- createMultiColony(basePop[3:8], n = 6)
#' apiary <- cross(apiary, drones = droneGroups[2:7])
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
#' try(getId(mergePops(getVirginQueens(apiary)))) # No virgin queens
#' apiary1 <- supersede(apiary)
#' try(getId(mergePops(getQueen(apiary1)))) # No queens
#' getId(mergePops(getVirginQueens(apiary1))) # No virgin queens
#'
#' # Sample colonies from the apiary that will supersede (sample with probability of 0.2)
#' tmp <- pullColonies(apiary, p = 0.2)
#' # Swarm only the pulled colonies
#' (supersede(tmp$pulled))
#' @export
supersede <- function(x, year = NULL, nVirginQueens = NULL, simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nVirginQueens)) {
    nVirginQueens <- simParamBee$nVirginQueens
  }
  if (isColony(x)) {
    if (hasCollapsed(x)) {
      stop(paste0("The colony ", getId(x), " collapsed, hence it can not supresede!"))
    }
    if (!isQueenPresent(x)) {
      stop("No queen present in the colony!")
    }
    if (is.function(nVirginQueens)) {
      nVirginQueens <- nVirginQueens(x, ...)
    }
    x <- removeQueen(x, addVirginQueens = TRUE, nVirginQueens = nVirginQueens,
                     year = year, simParamBee = simParamBee)
    x@virginQueens <- selectInd(x@virginQueens, nInd = 1, use = "rand")
    # TODO: We could consider that a non-random virgin queen prevails (say the most
    #       aggressive one), by creating many virgin queens and then picking the
    #       one with highest pheno for competition or some other criteria
    #       https://github.com/HighlanderLab/SIMplyBee/issues/239
    x@last_event <- "superseded"
    x@supersedure <- TRUE
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    if (nCol == 0) {
      x <- createMultiColony()
    } else {
      for (colony in seq_len(nCol)) {
        x[[colony]] <- supersede(x[[colony]],
                                 year = year,
                                 nVirginQueens = nVirginQueens,
                                 simParamBee = simParamBee, ...
        )
      }
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  validObject(x)
  return(x)
}

#' @rdname split
#' @title Split colony in two MultiColony
#'
#' @description Level 2 function that splits a Colony or MultiColony object
#'   into two new colonies to
#'   prevent swarming (in managed situation). The remnant colony retains the
#'   queen and a proportion of the workers and all drones. The split colony gets
#'   the other part of the workers, which raise virgin queens, of which only one
#'   prevails. Location of the split is the same as for the remnant.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param p numeric, proportion of workers that will go to the split colony; if
#'   \code{NULL} then \code{\link{SimParamBee}$splitP} is used.
#'   If input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies
#' @param year numeric, year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... additional arguments passed to \code{p} when this argument is a
#'   function
#'
#' @return list with two  \code{\link{Colony-class}} or \code{\link{MultiColony-class}},
#' the \code{split} and the \code{remnant} (see the description what each colony holds!);
#' both outputs have the split even slot set do \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(basePop[1], n = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' (colony <- buildUp(colony, nWorkers = 100))
#' apiary <- createMultiColony(basePop[3:8], n = 6)
#' apiary <- cross(apiary, drones = droneGroups[2:7])
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
#' # Split with different proportions
#' nWorkers(apiary)
#' tmp <- split(apiary, p = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
#' nWorkers(tmp$split)
#' nWorkers(tmp$remnant)
#'
#' # Split only specific colonies in the apiary
#' tmp <- pullColonies(apiary, ID = c(4, 5))
#' # Split only the pulled colonies
#' (split(tmp$pulled, p = 0.5))
#' @export
split <- function(x, p = NULL, year = NULL, simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(p)) {
    p <- simParamBee$splitP
  }
  if (isColony(x)) {
    if (hasCollapsed(x)) {
      stop(paste0("The colony ", getId(x), " collapsed, hence you can not split it!"))
    }
    if (!isQueenPresent(x)) {
      stop("No queen present in the colony!")
    }
    if (!isWorkersPresent(x)) {
      stop("No workers present in the colony!")
    }
    if (is.function(p)) {
      p <- p(x, ...)
    } else  {
      if (p < 0 | 1 < p) {
        stop("p must be between 0 and 1 (inclusive)!")
      }
      if (length(p) > 1) {
        warning("More than one value in the p argument, taking only the first value!")
        p <- p[1]
      }
    }
    nWorkers <- nWorkers(x)
    nWorkersSplit <- round(nWorkers * p)
    # TODO: Split colony at random by default, but we could make it as a
    #       function of some parameters
    #       https://github.com/HighlanderLab/SIMplyBee/issues/179
    tmp <- pullWorkers(x = x, nInd = nWorkersSplit)
    remnantColony <- tmp$remnant
    tmpVirginQueens <- createVirginQueens(
      x = x, nInd = 1,
      year = year
    )
    # Workers raise virgin queens from eggs laid by the queen (assuming) that
    #   a frame of brood is also provided to the split and then one random virgin
    #   queen prevails, so we create just one
    # TODO: Could consider that a non-random one prevails (say the most aggressive
    #       one), by creating many virgin queens and then picking the one with
    #       highest pheno for competition or some other criteria
    #       https://github.com/HighlanderLab/SIMplyBee/issues/239

    splitColony <- createColony(x = tmpVirginQueens)
    splitColony@workers <- tmp$pulled
    splitColony <- setLocation(x = splitColony, location = getLocation(splitColony))

    remnantColony@last_event <- "remnant"
    splitColony@last_event <- "split"

    remnantColony@split <- TRUE
    splitColony@split <- TRUE

    remnantColony@production <- TRUE
    splitColony@production <- FALSE

    ret <- list(split = splitColony, remnant = remnantColony)
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nP <- length(p)
    if (nP > 1 && nP < nCol) {
      stop("Too few values in the p argument!")
    }
    if (nP > 1 && nP > nCol) {
      warning(paste0("Too many values in the nInd argument, taking only the first ", nCol, "values!"))
      p <- p[1:nCol]
    }
    if (nCol == 0) {
      ret <- list(
        split = createMultiColony(),
        remnant = createMultiColony()
      )
    } else {
      ret <- list(
        split = createMultiColony(n = nCol),
        remnant = createMultiColony(n = nCol)
      )
      for (colony in seq_len(nCol)) {
        if (is.null(p)) {
          pColony <- NULL
        } else {
          pColony <- ifelse(nP == 1, p, p[colony])
        }
        tmp <- split(x[[colony]],
                     p = pColony,
                     year = year,
                     simParamBee = simParamBee, ...
        )
        ret$split[[colony]] <- tmp$split
        ret$remnant[[colony]] <- tmp$remnant
      }
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }

  validObject(ret$splitColony)
  validObject(ret$remnantColony)
  return(ret)
}

#' @rdname combine
#' @title Combine two colony objects
#'
#' @description Level 2 function that combines two Colony or MultiColony objects
#'   into one or
#'   two colonies objects of the same length to one. For example, to combine a
#'   weak and a strong colony (or MultiColony). Workers and drones of the weak
#'   colony are added to the strong. User has to remove the weak colony (or
#'   MultiColony) from the workspace.
#'
#' @param strong \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param weak \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return a combined \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(basePop[1], n = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#'
#' # Create weak and strong Colony and MultiColony class
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- cross(colony1, drones = droneGroups[[1]])
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- cross(colony2, drones = droneGroups[[2]])
#' apiary1 <- createMultiColony(basePop[4:6], n = 3)
#' apiary1 <- cross(apiary1, drones = droneGroups[3:5])
#' apiary2 <- createMultiColony(basePop[7:9], n = 3)
#' apiary2 <- cross(apiary2, drones = droneGroups[6:8])
#'
#' # Build-up
#' colony1 <- buildUp(x = colony1, nWorkers = 100, nDrones = 20)
#' colony2 <- buildUp(x = colony2, nWorkers = 20, nDrones = 5)
#' apiary1 <- buildUp(x = apiary1, nWorkers = 100, nDrones = 20)
#' apiary2 <- buildUp(x = apiary2, nWorkers = 20, nDrones = 5)
#'
#' # Combine
#' nWorkers(colony1)
#' nWorkers(colony2)
#' nDrones(colony1)
#' nDrones(colony2)
#' colony1 <- combine(strong = colony1, weak = colony2)
#' nWorkers(colony1)
#' nWorkers(colony2)
#' nDrones(colony1)
#' nDrones(colony2)
#' getQueen(colony1)@id
#' getQueen(colony2)@id
#' rm(colony2)
#'
#' nWorkers(apiary1)
#' nWorkers(apiary2)
#' nDrones(apiary1)
#' nDrones(apiary2)
#' apiary1 <- combine(strong = apiary1, weak = apiary2)
#' nWorkers(apiary1)
#' nWorkers(apiary2)
#' nDrones(apiary1)
#' nDrones(apiary2)
#' rm(apiary2)
#' @export
combine <- function(strong, weak) {
  if (isColony(strong) & isColony(weak)) {
    if (hasCollapsed(strong)) {
      stop(paste0("The colony ", getId(strong), " (strong) has collapsed, hence you can not combine it!"))
    }
    if (hasCollapsed(weak)) {
      stop(paste0("The colony ", getId(weak), " (weak) has collapsed, hence you can not combine it!"))
    }
    strong@workers <- c(strong@workers, weak@workers)
    strong@drones <- c(strong@drones, weak@drones)
  } else if (isMultiColony(strong) & isMultiColony(weak)) {
    if (nColonies(weak) == nColonies(strong)) {
      nCol <- nColonies(weak)
      for (colony in seq_len(nCol)) {
        strong[[colony]] <- combine(strong = strong[[colony]], weak = weak[[colony]])
      }
    } else {
      stop("Weak and strong MultiColony objects must be of the same length!")
    }
  } else {
    stop("Argument strong and weak must both be either a Colony or MultiColony class objects!")
  }
  return(strong)
}

#' @rdname setLocation
#' @title Set colony location
#'
#' @description Level 2 function that to set a Colony or MultiColony object
#'   location to (x, y) coordinates.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param location numeric or list, location to be set for the
#'   \code{\link{Colony-class}} or for \code{\link{MultiColony-class}}; when
#'   numeric the same location will be set for all colonies; when list different
#'   locations will be set for each colony - the list has to have the same
#'   length at there are colonies in \code{x})
#'
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}} with set
#'   location
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(basePop[1], n = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#'
#' # Create Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[3:8], n = 6)
#' apiary <- cross(apiary, drones = droneGroups[2:7])
#'
#' getLocation(colony)
#' getLocation(apiary)
#'
#' loc1 <- c(512, 722)
#' colony <- setLocation(colony, location = loc1)
#' getLocation(colony)
#'
#' # Assuming one location (as in bringing colonies to one place!)
#' apiary <- setLocation(apiary, location = loc1)
#' getLocation(apiary)
#' @export
setLocation <- function(x, location) {
  if (isColony(x)) {
    x@location <- location
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    if (is.list(location)) {
      if (length(location) != nCol) {
        stop("The length of location list and the number of colonies must match!")
      }
      for (colony in seq_len(nCol)) {
        if (!is.null(x[[colony]])) {
          x[[colony]]@location <- location[[colony]]
        }
      }
    } else {
      for (colony in seq_len(nCol)) {
        if (!is.null(x[[colony]])) {
          x[[colony]]@location <- location
        }
      }
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  validObject(x)
  return(x)
}
