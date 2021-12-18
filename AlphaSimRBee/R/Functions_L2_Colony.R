# Level 2 Colony Functions

#' @rdname createColony
#' @title Create a new Colony
#'
#' @description Creates a new \code{\link{Colony-class}} to initiate
#'   simulations.
#'
#' @param location numeric, location of the colony as \code{c(x, y)}
#' @param queen \code{\link{Pop-class}} with one individual that will be the
#'   queen of the colony; error is thrown if more than one individual is given
#' @param yearOfBirth numeric, year of birth of the queen
#' @param fathers \code{\link{Pop-class}} with drones that the queen will mate
#'   with as part of this function (if she is already mated, a warning is given
#'   and the fathers argument is ignored)
#' @param virgin_queens \code{\link{Pop-class}} with one or more individuals of
#'   which one will become the queen of the colony in the future
#'   TODO: think and explain what happens if we provide both a queen and virgin
#'   queens (possibly of different origin)!
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details When a mated queen is given or a queen alongside with fathers, the
#'   function also generates one virgin queen.
#'   TODO: discuss this - see below
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 12, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' colony1 <- createColony(queen = basePop[1], fathers = basePop[2:11])
#' colony1
#'
#' colony2 <- createColony(virgin_queens = basePop[12])
#' colony2
#'
#' @export
createColony <- function(location = NULL, queen = NULL, yearOfBirth = NULL,
                         fathers = NULL, virgin_queens = NULL,
                         simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!is.null(queen)) {
    if (nInd(queen) > 1) {
      stop("More than one individual given for a queen!")
    }
    if (isQueenMated(queen) & !is.null(fathers)) {
      warning("The queen is already mated - ignoring fathers!")
      queen@misc <- list(yearOfBirth = yearOfBirth, fathers = queen@misc$fathers)
    } else {
      queen@misc <- list(yearOfBirth = yearOfBirth, fathers = fathers)
    }
  }
  colony <- new("Colony",
                location = location,
                queen = queen,
                virgin_queens = virgin_queens)
  colony <- resetEvents(colony)
  # TODO: do we really want to add virgin queen(s) automatically? Fells like
  #       we don't want this - we should have then also added workers and drones
  # TODO: should then buildUpColony add virginQueens?
  if (!is.null(colony@queen)) {
    colony@id <- colony@queen@id
    if (isQueenMated(colony)) {
      # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
      colony <- addVirginQueens(colony = colony, nInd = 1, simParamBee = simParamBee)
    }
  }
  return(colony)
}

#' @rdname reQueenColony
#' @title Re-queen a colony
#'
#' @description Re-queen a colony adds a mated or a virgin queen, removes the
#'   previous queen, and changes colony id to the new mated queen.
#'
#' @param colony \code{\link{Colony-class}}
#' @param queen \code{\link{Pop-class}} with one individual that will be the
#'   queen of the colony; if she is not mated, then she will be added as a
#'   virgin queen that will have to be mated later
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony
#'
#' virginQueen <- basePop[3]
#' reQueenColony(colony, queen = virginQueen)
#'
#' matedQueen <- crossVirginQueen(virginQueen = basePop[3], fathers = drones[6:10])
#' reQueenColony(colony, queen = matedQueen)
#'
#' @export
reQueenColony <- function(colony, queen) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if  (!isPop(queen)) {
    stop("Argument queen must be a Pop class object!")
  }
  if (isQueenMated(queen)) {
    colony@queen <- queen
    colony@id <- queen@id
  } else {
    colony@id <- NULL
    colony@queen <- NULL
    colony@virgin_queens <- queen
  }
  return(colony)
}

#' @rdname addVirginQueens
#' @title Add (raise) virgin queens in the colony
#'
#' @description Adds (raises) the specified number of virgin queens in the
#'   colony by crossing the current queen and the fathers.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of virgin queens to add
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' addVirginQueens(colony, nInd = 10)
#'
#' @export
addVirginQueens <- function(colony, nInd, simParamBee = NULL) {
  # TODO: do we need argument new here like we have it for addWorkers() and addDrones()?
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.null(colony@queen)) {
    stop("Missing queen!")
  }
  if (!isQueenMated(colony)) {
    stop("Unmated queen!")
  }
  tmp <- createVirginQueens(colony = colony, nInd = nInd, simParamBee = simParamBee)
  colony@virgin_queens <- tmp$virgin_queens
  colony@nHomDrones <- colony@nHomDrones + tmp$nHomDrones
  return(colony)
}

#' @rdname addWorkers
#' @title Add (raise) workers in the colony
#'
#' @description Adds (raises) the specified number of workers in the colony by
#'   crossing the current queen and the fathers. If there are already some
#'   workers present, new and present workers are combined.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of workers to add
#' @param new logical, should the workers be added a fresh (ignoring currently
#'   present workers in the colony)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' addWorkers(colony, nInd = 20)
#'
#' @export
addWorkers <- function(colony, nInd, new = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.function(nInd)) {
    nInd <- nInd(colony)
  }
  if (nInd > 0) {
    newWorkers <- createWorkers(colony, nInd, simParamBee = simParamBee)
    if (is.null(colony@workers) | new) {
      colony@workers <- newWorkers$workers
      colony@nHomDrones <- newWorkers$nHomDrones
    } else {
      colony@workers <- c(colony@workers, newWorkers$workers)
      colony@nHomDrones <- colony@nHomDrones + newWorkers$nHomDrones
    }
  }
  return(colony)
}

#' @rdname addDrones
#' @title Add (raise) drones in the colony
#'
#' @description Adds (raises) the specified number of drones in the colony by
#'   crossing the current queen and the fathers. If there are already some
#'   drones present, new and present drones are combined.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of drones to add
#' @param new logical, should the drones be added a fresh (ignoring currently
#'   present drones)
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' addDrones(colony, nInd = 20)
#'
#' @export
addDrones <- function(colony, nInd, new = FALSE) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.function(nInd)) {
    nInd <- nInd(colony)
  }
  if (nInd > 0) {
    newDrones <- createDrones(colony, nInd)
    if (is.null(colony@drones) | new) {
      colony@drones <- newDrones
    } else {
      colony@drones <- c(colony@drones, newDrones)
    }
  }
  return(colony)
}

#' @rdname buildUpColony
#' @title Build up colony by adding (raising) workers and drones
#'
#' @description Build up colony by adding (raising) workers and drones, usually
#'   in spring or after events such as split or swarming.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nWorkers integer, desired number of workers in the colony (currently
#'   present workers are taken into account so only the difference is added)
#' @param nDrones integer, desired number of drones in the colony (currently
#'   present drones are taken into account so only the difference is added)
#' @param new logical, should the workers and drones be added a fresh (ignoring
#'   currently present workers and drones)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details This function turns production of the colony to \code{TRUE}.
#'
#' @return \code{\link{Colony-class}}
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
#' (colony <- buildUpColony(colony, nWorkers = 100))
#' isProductive(colony)
#'
#' (colony <- buildUpColony(colony, nWorkers = 100)) # we are already at the target
#' (colony <- buildUpColony(colony, nWorkers = 150)) # increasing the target
#' (colony <- buildUpColony(colony, nWorkers = 100)) # we are already at the target
#' (colony <- buildUpColony(colony, nWorkers = 100, new = TRUE))
#'
#' @export
buildUpColony <- function(colony, nWorkers, nDrones = nWorkers * 0.1,
                          new = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (new) {
    n <- nWorkers
  } else {
    n <- nWorkers - nWorkers(colony)
  }
  if (n > 0) {
    colony <- addWorkers(colony, nInd = n, new = new, simParamBee = simParamBee)
  }
  if (new) {
    n <- nDrones
  } else {
    n <- nDrones - nDrones(colony)
  }
  if (n > 0) {
    colony <- addDrones(colony, nInd = n, new = new)
  }
  colony@production <- TRUE
  return(colony)
}

#' @rdname replaceWorkers
#' @title Replaces a proportion of workers with new workers
#'
#' @description Replaces a proportion of workers with new workers from the
#'   colony. Useful after events like season change, swarming, supersedure, etc.
#'   due to the short life span of the workers.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of workers to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of workers that stay when \code{p < 1}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters

#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' colony <- addWorkers(colony, nInd = 10)
#' colony
#' getWorkers(colony)@id
#' colony <- replaceWorkers(colony, p = 0.5)
#' getWorkers(colony)@id
#'
#' colony <- replaceWorkers(colony, p = 1.0)
#' getWorkers(colony)@id
#'
#' colony <- replaceWorkers(colony, p = 1.5)
#' getWorkers(colony)@id
#'
#' @export
replaceWorkers <- function(colony, p = 1, use = "rand", simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  nWorkers <- nWorkers(colony)
  nWorkersReplaced <- round(nWorkers * p)
  if (nWorkersReplaced < nWorkers) {
    nWorkersStay <- nWorkers - nWorkersReplaced
    tmp <- createWorkers(colony, nInd = nWorkersReplaced, simParamBee = simParamBee)
    colony@workers <- c(selectInd(colony@workers, nInd = nWorkersStay, use = use),
                        tmp$workers)
    # TODO: we need some scaling of the nHomDrones here, right? Is this OK?
    colony@nHomDrones <- as.integer(round(colony@nHomDrones * nWorkersStay / nWorkers) + tmp$nHomDrones)
  } else {
    colony <- addWorkers(colony, nInd = nWorkersReplaced, new = TRUE, simParamBee = simParamBee)
  }
  return(colony)
}

#' @rdname replaceDrones
#' @title Replaces a proportion drones with new drones
#'
#' @description Replaces a proportion drones with new drones from the colony.
#'   Useful after events like season change, swarming, supersedure, etc. due to
#'   the short life span of the drones.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of drones to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of drones that stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}}
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
#' colony <- replaceDrones(colony, p = 0.5)
#' getDrones(colony)@id
#'
#' colony <- replaceDrones(colony, p = 1.0)
#' getDrones(colony)@id
#'
#' colony <- replaceDrones(colony, p = 1.5)
#' getDrones(colony)@id
#'
#' @export
replaceDrones <- function(colony, p = 1, use = "rand") {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  nDrones <- nDrones(colony)
  nDronesReplaced <- round(nDrones * p)
  if (nDronesReplaced < nDrones) {
    nDronesStay <- nDrones - nDronesReplaced
    colony@drones <- c(selectInd(colony@drones, nInd = nDronesStay, use = use),
                       createDrones(colony, nInd = nDronesReplaced))
  } else {
    colony <- addDrones(colony, nInd = nDronesReplaced, new = TRUE)
  }
  return(colony)
}

#' @rdname removeQueen
#' @title Remove queen
#'
#' @description Remove the queen of a colony
#'
#' @param colony \code{\link{Colony-class}}
#'
#' @return \code{\link{Colony-class}}
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
  return(colony)
}

#' @rdname removeVirginQueens
#' @title Remove a proportion of virgin queens
#'
#' @description Remove a proportion of virgin queens of a colony
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of virgins queens that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}}
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
    colony@virgin_queens <- NULL
  } else {
    n <- round(nVirginQueens(colony) * (1 - p))
    colony@virgin_queens <- selectInd(pop = colony@virgin_queens,
                                      nInd = n, use = use)
  }
  return(colony)
}

#' @rdname removeWorkers
#' @title Remove a proportion of workers
#'
#' @description Remove a proportion of workers, for example, as a preparation
#'   for winter.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of workers that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}}
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
    colony@nHomDrones <- 0
  } else {
    nWorkersNew <- round(nWorkers(colony) * (1 - p))
    colony@workers <- selectInd(pop = colony@workers,
                                nInd = nWorkersNew, use = use)
    colony@nHomDrones <- round(colony@nHomDrones * (1 - p))
  }
  return(colony)
}

#' @rdname removeDrones
#' @title Remove a proportion of drones
#'
#' @description Remove a proportion of drones, for example, at the end of summer.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion to be removed
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of drones that will stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}}
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
  return(colony)
}

#' @rdname resetEvents
#' @title Reset colony events
#'
#' @description Resets the slots swarm, split, supersedure, collapsed, and
#'   production to FALSE. Useful at the end of a yearly cycle to reset the
#'   events, allowing the user to track new events in a new year.
#'
#' @param colony \code{\link{Colony-class}}
#'
#' @return \code{\link{Colony-class}}
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
#' (colony <- buildUpColony(colony, nWorkers = 100))
#' resetEvents(colony)
#'
#' tmp <- splitColony(colony, pSplit = 0.5)
#' (split <- tmp$split)
#' resetEvents(split)
#' (remnant <- tmp$remnant)
#' resetEvents(remnant)
#'
#' tmp <- swarmColony(colony, pSwarm = 0.5)
#' (swarm <- tmp$swarm)
#' resetEvents(swarm)
#' (remnant <- tmp$remnant)
#' resetEvents(remnant)
#'
#' (tmp <- supersedeColony(colony)) # TODO: do we still get production if we have supersedure?
#' resetEvents(tmp)
#'
#' (tmp <- collapseColony(colony))
#' resetEvents(tmp)
#'
#' @export
resetEvents <- function(colony) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colony@swarm <- FALSE
  colony@split <- FALSE
  colony@supersedure <- FALSE
  colony@collapse <- FALSE
  colony@production <- FALSE
  return(colony)
}

#' @rdname crossColony
#' @title Cross (mate) a virgin queen of a colony to a group of drones
#'
#' @description Cross (mate) a virgin queen of colony to a group of drones. When
#'   there are multiple virgin queen in the colony, one is selected at random,
#'   mated and promoted to the queen of the colony. Mated drones (fathers) are
#'   stored for producing progeny within this function or later.
#'
#' @param colony \code{\link{Colony-class}}
#' @param fathers \code{\link{Pop-class}}, drones
#' @param nWorkers integer, number of workers to create
#' @param nDrones integer, number of drones to create
#' @param new logical, should all the \code{nWorkers} and \code{nDrones} be from
#'   the new queen or should we reach these numbers only by topping up workers
#'   and drones
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{Colony-class}} on how we store the fathers along the
#'   queen.
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#'
#' colony <- createColony(virgin_queen = basePop[2])
#' colony
#' colony <- crossColony(colony, fathers = drones)
#' colony
#'
#' colony <- createColony(virgin_queen = basePop[2])
#' colony
#' colony <- crossColony(colony, fathers = drones, nWorkers = 20)
#' colony
#'
#' @export
crossColony <- function(colony, fathers = NULL, nWorkers = 0,
                        nDrones = nWorkers * 0.1, new = FALSE,
                        simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (!isPop(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  if (is.null(colony@virgin_queens)) {
    stop("No virgin queen(s)!")
  }
  if (!is.null(colony@queen)) {
    stop("Mated queen already present!")
  }
  if (is.null(fathers)) {
    stop("Missing fathers!")
  }
  # TODO: should this use argument be really random? Do we want to make it into argument of this function?
  virginQueen <- selectInd(colony@virgin_queens, nInd = 1, use = "rand")
  colony@queen <- crossVirginQueen(virginQueen = virginQueen, fathers = fathers)
  colony@id <- colony@queen@id
  # TODO: should we add virgin queens here by default?
  # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
  colony <- addVirginQueens(colony = colony, nInd = 1, simParamBee = simParamBee)
  # TODO: should we remove addWorkers() and addDrones() and leave this for buildUpColony()
  colony <- addWorkers(colony, nInd = nWorkers, new = new, simParamBee = simParamBee)
  colony <- addDrones(colony, nInd = nDrones, new = new)
  return(colony)
}

#' @rdname collapseColony
#' @title Collapses colony
#'
#' @description Collapses colony by setting the collapse event slot to
#'   \code{TRUE}. The production status slot is also changed (to \code{FALSE}).
#'
#' @param colony \code{\link{Colony-class}}
#'
#' @return \code{\link{Colony-class}}
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
  return(colony)
}

#' @rdname swarmColony
#' @title Replicates the swarming process and produces two colonies
#'
#' @description List. Replicates the swarming of the colony - the process in which
#' a part of the workers leave with the old queen and creates a new colony (the swarm),
#' while a part of the workers stay with a new queen and the old drones.
#' The swarming colony contains the old mated queen,
#'  a percentage (pSwarm) of the original colonies workers, no drones and a virgin queen is created from the worker population.
#'  A new location must be given to the new swarm colony.
#'  The colony that stays contains the remaining workers and drones. A virgin queen is selected from the workers and mated if fathers are present.
#'
#' @param colony \code{\link{Colony-class}}
#' @param pSwarm Integer. Percentage of colony that will swarm
#' @param crossVirginQueen Logical. Whether a virgin queen is to be mated
#' @param fathers AlphaSimR population object. Number of fathers pulled from the DCA
#' @param pWorkers Numeric, proportion of workers that are replaced with the workers from the new queen in the remnant colony
#' @param pDrones Numeric, proportion of drones that are replaced with the drones from the new queen in the remnant colony
#' @param swarm Location Integer. X,Y coordinates of newly made swarmed hive
#'
#' @examples inst/examples/examples_swarm.R
#'
#' @return list with two \code{\link{Colony-class}}, swarm with the old queen and a proportion of workers,
#' and remnant with a new queen and a proportion of workers
#'
#' @export
swarmColony <- function(colony, pSwarm = 0.5, crossVirginQueen = FALSE,
                        fathers = NULL, pWorkers = 1, pDrones = 1,
                        swarmLocation = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!!")
  }
  if (!is.null(fathers) && !isPop(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  if (is.null(colony@virgin_queens)) {
    stop("Virgin queen not present in the colony, cannot swarm!")
  }

  nWorkers <- nWorkers(colony)
  nWorkersSwarm <- round(nWorkers * pSwarm, 0)
  nWorkersStay <- nWorkers - nWorkersSwarm
  workersSwarmId <- sample(x = colony@workers@id, size = nWorkersSwarm, replace = FALSE)
  workersStayId <- colony@workers@id[!colony@workers@id %in% workersSwarmId]
  # TODO: use pullWorkers() here

  remnantColony <- createColony()
  remnantColony@virgin_queens <- selectInd(colony@virgin_queens, 1, use = "rand")
  remnantColony@workers <- colony@workers[workersStayId]
  remnantColony@drones <- colony@drones
  remnantColony@location <- colony@location

  if (crossVirginQueen) {
    if (is.null(fathers)) {
      stop("No fathers provided, cannot mate the queen!")
    }
    remnantColony@queen <- remnantColony@virgin_queens
    remnantColony@id <- remnantColony@queen@id
    remnantColony@queen@misc$fathers <- fathers
    # populate the remnant with virgin queens, workers, and drones from the new queen
    # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
    remnantColony <- addVirginQueens(colony = remnantColony, nInd = 1, simParamBee = simParamBee)
    remnantColony <- replaceWorkers(remnantColony, pWorkers, simParamBee = simParamBee)
    remnantColony <- replaceDrones(remnantColony, pDrones)
  }

  swarm <- colony
  # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
  swarm <- addVirginQueens(colony = swarm, nInd = 1, simParamBee = simParamBee)
  swarm@workers <- colony@workers[workersSwarmId]
  swarm@drones <- NULL
  swarm@location <- swarmLocation

  remnantColony@last_event <- "remnant"
  swarm@last_event <- "swarm"

  remnantColony@swarm <- TRUE
  swarm@swarm <- TRUE
  remnantColony@production <- FALSE
  swarm@production <- FALSE

  ret <- list(swarm = swarm, remnant = remnantColony)
  return(ret)
}

#' @rdname supersedeColony
#' @title Replicates a supersedure of the colony and replaces the queen with a virgin queen
#'
#' @description Replicates the process of supersedure, where the
#' queen is replaced by a new virgin queen. The workers and the drones stay
#' in the colony. If no fathers are present, mating of the virgin queen does not occur.
#' @seealso \code{\link[??????]{supersedure}}
#'
#' @param colony \code{\link{Colony-class}}
#' @param crossVirginQueen Logical. Whether a virgin queen is to be mated
#' @param fathers AlphaSimR population object. Number of fathers pulled from the DCA
#' @param pWorkers Numeric, proportion of workers that are replaced with the workers from the new queen
#' @param pDrones Numeric, proportion of drones that are replaced with the drones from the new queen
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' TODO
#'
#' @export
supersedeColony <- function(colony, crossVirginQueen = FALSE, fathers = NULL,
                            pWorkers = 1, pDrones = 1, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.null(colony@queen)) {
    stop("No queen present in the colony!")
  }
  colony@queen <- NULL
  colony@virgin_queens <- selectInd(colony@virgin_queens, nInd = 1, use = "rand")
  if (crossVirginQueen) {
    if (is.null(fathers)) {
      stop("No fathers provided, cannot mate the queen!")
    }
    if (!isPop(fathers)) {
      stop("Argument fathers must be a Pop class object!")
    }
    colony@queen <- colony@virgin_queens
    colony@queen@misc$fathers <- fathers
    # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
    colony <- addVirginQueens(colony = colony, nInd = 1, simParamBee = simParamBee)
    colony <- replaceWorkers(colony, pWorkers, simParamBee = simParamBee)
    colony <- replaceDrones(colony, pDrones)
  }
  colony@last_event <- "superseded"
  colony@supersedure <- TRUE
  colony@production <- TRUE
  return(colony)
}

#' @rdname splitColony
#' @title Split the colony in two colonies
#'
#' @description Spit the colony into two new colonies to prevent swarming (in managed populations)
#' - one colony is with the old queen and a part of the workers and drones (this is the remaining colony)
#' - the split colony is taken to a new location with part of the workers.
#'  A new mated queen can be introduced to the split colony.
#'  If no new queen is introduced, a virgin queen must be present to mate with fathers from DCA and continue colony
#' @seealso \code{\link[??????]{splitColony}}
#'
#' @param colony \code{\link{Colony-class}}
#' @param pSplit Integer. Percentage of hive to split
#' @param newQueen AlphaSimR population object. A new mated queen is brought into the colony from other source
#' @param crossVirginQueen Logical. If no mated queen is introduced, a virgin queen must be present to mate and continue colony
#' @param fathers AlphaSimR population object. Number of fathers pulled from the DCA
#' @param pWorkers Numeric, proportion of workers that are replaced with the workers from the new queen in the split
#' @param splitLocation Integer. X,Y coordinates of newly made split hive
#'
#' @examples
#' TODO
#'
#' @return list with two \code{\link{Colony-class}}, split with the old queen and a proportion of workers,
#' and remnant with a ??? and a proportion of workers TODO
#'
#' @export
splitColony <- function(colony, pSplit = 0.30, newQueen = NULL,
                        crossVirginQueen = FALSE, fathers = NULL,
                        pWorkers = 1, splitLocation = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (!is.null(newQueen) && !isPop(newQueen)) {
    stop("Argument newQueen must be a Pop class object!")
  }
  if (!is.null(fathers) && !isPop(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  nWorkers <- nWorkers(colony)
  nWorkersSplit <- round(nWorkers * pSplit, 0)
  noWorkersStay <- nWorkers - nWorkersSplit
  workersSplitId <- sample(x = colony@workers@id, size = nWorkersSplit, replace = FALSE)
  workersStayId <- colony@workers@id[!colony@workers@id %in% workersSplitId]
  splitColony <- createColony()
  splitColony@workers <- colony@workers[workersSplitId]
  splitColony@location <- splitLocation
  colony@workers <- colony@workers[workersStayId]
  # TODO: use pullCaste() here?

  if (!is.null(newQueen)) {
    if (!isQueenMated(newQueen)) {
      splitColony@virgin_queens <- newQueen
    }
    if (isQueenMated(newQueen)) {
      splitColony@queen <- newQueen
      splitColony@id <- splitColony@queen@id
    }
    if (crossVirginQueen) {
      if (is.null(fathers)) {
        stop("No fathers provided, cannot mate the queen!")
      }
      splitColony@queen <- splitColony@virgin_queens
      if (isQueenMated(splitColony)) {
        stop("Queen already mated!")
      }
      splitColony@queen@misc$fathers <- fathers
    }
  }

  if (!is.null(splitColony@queen)) {
    # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
    splitColony <- addVirginQueens(colony = splitColony, nInd = 1, simParamBee = simParamBee)
    splitColony <- replaceWorkers(splitColony, pWorkers, simParamBee = simParamBee)
  }

  # Change the status of the colony
  colony@last_event <- "remnant"
  splitColony@last_event <- "split"

  colony@split <- TRUE
  splitColony@split <- TRUE

  colony@production <- TRUE
  splitColony@production <- FALSE

  ret <- list(split = splitColony, remnant = colony)
  return(ret)
}

#' @rdname setLocation
#' @title Set the colony location
#'
#' @description Set the colony location as (x, y) coordinates.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param location numeric or list, location (x, y) to be set for the Colony
#' (numeric) or for Colonies (numeric to set the same location for all colony or
#' list to set different location for each colony - if list, it has to has the
#' same length at there is colonies in \code{x})
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}}
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
#' colony2 <- setLocation(colony1, location = loc2)
#' getLocation(colony2)
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
      for (colony in 1:nCol) {
        x@colonies[[colony]]@location <- location[[colony]]
      }
    } else {
      for (colony in 1:nCol) {
        x@colonies[[colony]]@location <- location
      }
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(x)
}

# TODO: Document
# TODO: Set pheno to virgin queens as well? Add caste argument here, similarly as
#   in getColonyGv()?
# TODO: what if caste phenos have already been set? need a sensible default!!!
# TODO: while ... will work for all arguments of setPheno() (such as h2, H2, ...)
#  it will not work for simParam - so best to add all these arguments directly?
setPhenoColony <- function(colony, FUN = NULL, ...) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colony@queen <- setPheno(colony@queen, ...)
  colony@workers <- setPheno(colony@workers, ...)
  colony@drones <- setPheno(colony@drones, ...)
  if (!is.null(FUN)) {
    colony@pheno <- FUN(colony, ...)
  }
  return(colony)
}
