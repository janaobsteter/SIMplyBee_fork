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
#' @return new \code{\link{Colony-class}}
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
  if (is.null(queen)) {
    id <- as.character(NA)
  } else {
    if (!isPop(queen)) {
      stop("Argument queen must be a Pop class object!")
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
  }
  colony <- new(Class = "Colony",
                id = id,
                location = location,
                queen = queen,
                virgin_queens = virgin_queens)
  colony <- resetEvents(colony)
  # TODO: do we really want to add virgin queen(s) automatically? Fells like
  #       we don't want this - we should have then also added workers and drones
  # TODO: should then buildUpColony add virginQueens?
  if (isQueenPresent(colony)) {
    if (isQueenMated(colony)) {
      # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
      # TODO: warn that if virgin queens are provided in this function but so is
      #       the queen, then we do ... hmm, what? Kill the provided virgin queens
      #       and replace them with virgin queens from the queen (if we will do
      #       this going forward)?
      colony <- addVirginQueens(colony = colony, nInd = 1, simParamBee = simParamBee)
    }
  }
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
#'
#' @return \code{\link{Colony-class}} with new queen
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
#' matedQueen <- crossVirginQueen(pop = basePop[3], fathers = drones[6:10])
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
    colony@queen <- NULL
    colony@id <- NULL
    colony@virgin_queens <- queen
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
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of virgin queens to add
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details \code{addVirginQueens} replaces any currently present virgin queens.
#'
#' @return \code{\link{Colony-class}} with virgin queens added
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
  # TODO: make nInd = NULL and grab default value from simParamBee
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
  tmp <- createVirginQueens(colony = colony, nInd = nInd, simParamBee = simParamBee)
  colony@virgin_queens <- tmp$virgin_queens
  # TODO: update this logic here on queen vs colony pHomBrood
  colony@queen@misc[[1]]$pHomBrood <- mean(c(colony@queen@misc[[1]]$pHomBrood, tmp$pHomBrood))
  validObject(colony)
  return(colony)
}

#' @rdname addWorkers
#' @title Add (raise) workers in the colony
#'
#' @description Level 2 function that adds (raises) the specified number of
#'   workers in the colony by crossing the current queen and the fathers. If
#'   there are already some workers present, new and present workers are
#'   combined.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of workers to add
#' @param new logical, should the workers be added a fresh (ignoring currently
#'   present workers in the colony)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} with workers added
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
  # TODO: make nInd = NULL and grab default value from simParamBee
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
      colony@queen@misc[[1]]$pHomBrood <- newWorkers$pHomBrood
    } else {
      colony@workers <- c(colony@workers, newWorkers$workers)
      # TODO: update this logic here on queen vs colony pHomBrood
      colony@queen@misc[[1]]$pHomBrood <- mean(c(colony@queen@misc[[1]]$pHomBrood, newWorkers$pHomBrood))
    }
  }
  validObject(colony)
  return(colony)
}

#' @rdname addDrones
#' @title Add (raise) drones in the colony
#'
#' @description Level 2 function that adds (raises) the specified number of
#'   drones in the colony by crossing the current queen and the fathers. If
#'   there are already some drones present, new and present drones are combined.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of drones to add
#' @param new logical, should the drones be added a fresh (ignoring currently
#'   present drones)
#'
#' @return \code{\link{Colony-class}} with drones added
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
  # TODO: make nInd = NULL and grab default value from simParamBee
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
  validObject(colony)
  return(colony)
}

#' @rdname buildUpColony
#' @title Build up colony by adding (raising) workers and drones
#'
#' @description Level 2 function that builds up colony by adding (raising)
#'   workers and drones, usually in spring or after events such as split or
#'   swarming.
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
#' (colony <- buildUpColony(colony, nWorkers = 100))
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
#' @export
buildUpColony <- function(colony, nWorkers, nDrones = nWorkers * 0.1,
                          new = FALSE, simParamBee = NULL) {
  # TODO: make n* = NULL and grab default value from simParamBee
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
  validObject(colony)
  return(colony)
}

#' @rdname replaceWorkers
#' @title Replace a proportion of workers with new ones
#'
#' @description Level 2 function that replaces a proportion of workers with new
#'   workers from the colony. Useful after events like season change, swarming,
#'   supersedure, etc. due to the short life span of the workers.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of workers to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of workers that stay when \code{p < 1}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colony-class}} with replaced workers
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
  if (nWorkers > 0) {
    nWorkersReplaced <- round(nWorkers * p)
    if (nWorkersReplaced < nWorkers) {
      nWorkersStay <- nWorkers - nWorkersReplaced
      tmp <- createWorkers(colony, nInd = nWorkersReplaced, simParamBee = simParamBee)
      colony@workers <- c(selectInd(colony@workers, nInd = nWorkersStay, use = use),
                          tmp$workers)
      # TODO: we need some scaling of the pBrood here, right? Is this OK?
      colony@queen@misc[[1]]$pHomBrood <- mean(c(colony@queen@misc[[1]]$pHomBrood, tmp$pHomBrood))
    } else {
      colony <- addWorkers(colony, nInd = nWorkersReplaced, new = TRUE, simParamBee = simParamBee)
    }
  }
  validObject(colony)
  return(colony)
}

#' @rdname replaceDrones
#' @title Replace a proportion of drones with new ones
#'
#' @description Level 2 function that replaces a proportion of drones with new
#'   drones from the colony. Useful after events like season change, swarming,
#'   supersedure, etc. due to the short life span of the drones.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of drones to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of drones that stay when \code{p < 1}
#'
#' @return \code{\link{Colony-class}} with replaced drones
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
  if (nDrones > 0) {
    nDronesReplaced <- round(nDrones * p)
    if (nDronesReplaced < nDrones) {
      nDronesStay <- nDrones - nDronesReplaced
      colony@drones <- c(selectInd(colony@drones, nInd = nDronesStay, use = use),
                         createDrones(colony, nInd = nDronesReplaced))
    } else {
      colony <- addDrones(colony, nInd = nDronesReplaced, new = TRUE)
    }
  }
  validObject(colony)
  return(colony)
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
    colony@virgin_queens <- NULL
  } else {
    n <- round(nVirginQueens(colony) * (1 - p))
    colony@virgin_queens <- selectInd(pop = colony@virgin_queens,
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
#' @param colony \code{\link{Colony-class}}
#'
#' @return \code{\link{Colony-class}} with events reset
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
  # colony@collapse <- FALSE
  colony@production <- FALSE
  validObject(colony)
  return(colony)
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
#' colony <- createColony(virgin_queen = basePop[2])
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
  if (is.null(colony@virgin_queens)) {
    stop("No virgin queen(s)!")
  }
  if (!isPop(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  # Pick one virgin queen that will prevail
  # TODO: should this use argument be really random? Do we want to make it into argument of this function?
  virginQueen <- selectInd(colony@virgin_queens, nInd = 1, use = "rand")
  # TODO: do we take all fathers or just a 'default/nAvgFathers' or some other number?
  colony@queen <- crossVirginQueen(pop = virginQueen, fathers,
                                   simParamBee = simParamBee)
  colony@id <- colony@queen@id
  # TODO: should we really add virgin queens here by default? If we decide not to do this, make sure to set NULL to
  #       colony@virgin_queens since we promoted the prevailed virgin queen to the queen and the virgin
  #       queens slot contains the queen too!
  #       If we decide to do this, then how will we handle the csd issue?
  # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
  colony <- addVirginQueens(colony = colony, nInd = 1, simParamBee = simParamBee)
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
#'   remnant colony retains the other proportion of workers and all drones.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of workers that will leave with the swarm colony
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
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
swarmColony <- function(colony, p = 0.5, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!!")
  }

  nWorkers <- nWorkers(colony)
  nWorkersSwarm <- round(nWorkers * p)
  tmp <- pullWorkers(x = colony, nInd = nWorkersSwarm)
  currentLocation <- getLocation(colony)

  swarmColony <- createColony()
  swarmColony@queen <- colony@queen
  swarmColony@id <- colony@queen@id
  # TODO: should we be building virgin queens here at all? Given that this is a swarm!
  # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
  swarmColony <- addVirginQueens(colony = swarmColony, nInd = 1, simParamBee = simParamBee)
  swarmColony@workers <- tmp$pulled
  swarmColony <- setLocation(x = swarmColony, location = currentLocation)

  remnantColony <- createColony()
  # One virgin queen prevails
  # TODO: should this use argument be really random? Do we want to make it into argument of this function?
  remnantColony@virgin_queens <- selectInd(colony@virgin_queens, nInd = 1, use = "rand")
  remnantColony@workers <- getWorkers(tmp$colony)
  remnantColony@drones <- getDrones(colony)
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
#'   queen dies and one of virgin queens prevails. The workers and drones stay
#'   unchanged.
#'
#' @param colony \code{\link{Colony-class}}
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
supersedeColony <- function(colony) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (!isQueenPresent(colony)) {
    stop("No queen present in the colony!")
  }
  colony@queen <- NULL
  colony@id <- NULL
  # One virgin queen prevails
  # TODO: should this use argument be really random? Do we want to make it into argument of this function?
  colony@virgin_queens <- selectInd(colony@virgin_queens, nInd = 1, use = "rand")
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
#'   the other part of the workers and keeps location of the original colony.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of workers that will go to the split colony
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
splitColony <- function(colony, p = 0.3) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (p < 0 | p > 1) {
    stop("pSplit must be between 0 and 1!")
  }
  nWorkers <- nWorkers(colony)
  nWorkersSplit <- round(nWorkers * p)
  tmp <- pullWorkers(x = colony, nInd = nWorkersSplit)

  remnantColony <- tmp$colony
  # TODO: should really all virgin queens go to the remnant?

  splitColony <- createColony()
  # TODO: should any virgin queen go into the split, will then one prevail as in supersedure or remnant of the swarm?
  splitColony@workers <- tmp$pulled
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
#   in getColonyGv()?
# TODO: what if caste phenos have already been set? need a sensible default!!!
# TODO: while ... will work for all arguments of setPheno() (such as h2, H2, ...)
#  it will not work for simParam - so best to add all these arguments directly?
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
