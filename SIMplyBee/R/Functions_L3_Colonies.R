# Level 3 Colonies Functions

#' @rdname createColonies
#' @title Create colonies
#'
#' @description Level 3 function that creates a set of colonies. Usually to
#'   start a simulation.
#'
#' @param pop \code{\link{Pop-class}}, individuals that will be used as queens
#'   of the created colonies (the queens are selected at random from if there
#'   are more than \code{n} in \code{Pop})
#' @param nCol integer, number of colonies to create (if only \code{nCol} is
#'   given then \code{\link{Colonies-class}} is created with \code{nCol} empty
#'   (\code{NULL}) individual colony) - this is mostly useful for programming)
#' @param mated logical, create mated or unmated (virgin) colonies; if mated,
#'   then \code{nInd(pop)-n} individuals from \code{pop} are used to create
#'   drones with which the queens will mate with
#' @param nAvgFathers integer, number of drones that a queen mates with
#'   TODO nAvgFathers default should go to simParamBee and then we set it to NULL
#'        here and if its NULL we grab value from simParamBee, otherwise use it
#'        from the user
#' @param nDronesPerQueen integer, number of drones to generate per individual
#'   from the pop for mating with the queens
#'   TODO nDronesPerQueen default should go to simParamBee and then we set it to NULL
#'        here and if its NULL we grab value from simParamBee, otherwise use it
#'        from the user
#' @param yearOfBirth numeric, year of birth of the queen
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details When both \code{pop} and \code{nCol} are \code{NULL}, then an empty
#'   \code{NULL} \code{\link{Colonies-class}} is created with 0 colonies.
#'
#' @return \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Create 2 empty (NULL) colonies
#' apiary <- createColonies(nCol = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 mated colonies
#' apiary <- createColonies(pop = basePop, nCol = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 unmated/virgin colonies
#' apiary <- createColonies(pop = basePop, nCol = 2, mated = FALSE)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' @export
createColonies <- function(pop = NULL, nCol = NULL, mated = TRUE,
                           nAvgFathers = 15, nDronesPerQueen = 100,
                           yearOfBirth = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!is.null(pop)) {
    if (!isPop(pop)) {
      stop("Argument pop must be a Pop class object!")
    }
    if (nInd(pop) < nCol) {
      stop("Not enough individuals in the pop to create n colonies!")
    }
    ret <- new("Colonies", colonies = vector(mode = "list", length = nCol))
    if (mated) {
      if (nInd(pop) < (nCol - 1)) {
        stop("You must provide at least n+1 individuals in the pop to create n mated colonies!")
      }
      tmp <- pullInd(pop = pop, nInd = nCol)
      queens <- tmp$pulled
      DCA <- createFounderDrones(pop = tmp$remainder, nDronesPerQueen = nDronesPerQueen)
      fatherPackages <- pullDroneGroupsFromDCA(DCA, nGroup = nCol, avgGroupSize = nAvgFathers)
      for (colony in seq_len(nCol)) {
        ret@colonies[[colony]] <- createColony(queen = queens[colony],
                                               fathers = fatherPackages[[colony]],
                                               yearOfBirth = yearOfBirth,
                                               simParamBee = simParamBee)
      }
    } else {
      virginQueens <- selectInd(pop, nInd = nCol, use = "rand")
      for (colony in seq_len(nCol)) {
        ret@colonies[[colony]] <- createColony(virgin_queens = virginQueens[colony],
                                               yearOfBirth = yearOfBirth,
                                               simParamBee = simParamBee)
      }
    }
  } else if (!is.null(nCol)) {
    ret <- new(Class = "Colonies", colonies = vector(mode = "list", length = nCol))
  } else {
    ret <- new(Class = "Colonies")
  }
  validObject(ret)
  return(ret)
}

#' @rdname assignColonyToColonies
#' @title Assign (replace) a colony to colonies
#'
#' @description Level 3 function that assigns (replaces) a colony among a set of
#'   colonies, for example, to replace an old colony. By defining a position, it
#'   will insert the colony to that position and with this replace the old
#'   colony at that position.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param colony \code{\link{Colony-class}}, colony that will be added
#' @param pos numeric or character, index or ID of the old colony
#'
#' @return \code{\link{Colonies-class}} with a replaced colony
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' founderDrones <- createFounderDrones(pop = basePop[1:3], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[4], fathers = founderDrones[1:10])
#' colony2 <- createColony(queen = basePop[5], fathers = founderDrones[11:20])
#' colony3 <- createColony(queen = basePop[6], fathers = founderDrones[21:30])
#' apiary <- c(colony1, colony2)
#'
#' getId(apiary)
#' apiary <- assignColonyToColonies(apiary, colony3, pos = 1)
#' getId(apiary)
#'
#' @export
assignColonyToColonies <- function(colonies, colony, pos) {
  if (!isColonies(colonies)) {
    stop("Argument Colonies must be a Colonies class object!")
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colonies@colonies[[pos]] <- colony
  validObject(colonies)
  return(colonies)
}

#' @rdname selectColonies
#' @title Select individual colonies
#'
#' @description Level 3 function that selects individual colonies based on
#'   colony ID or random selection.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param ID numeric or character, name of a colony (one or more) to be pulled
#'   out; note that numeric value is converted to character
#' @param n numeric, number of colonies to select
#' @param p numeric, probability of a colony being selected
#'
#' @return \code{\link{Colonies-class}} with selected colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' founderDrones <- createFounderDrones(pop = basePop[1:3], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[4], fathers = founderDrones[1:10])
#' colony2 <- createColony(queen = basePop[5], fathers = founderDrones[11:20])
#' colony3 <- createColony(queen = basePop[6], fathers = founderDrones[21:30])
#' apiary <- c(colony1, colony2, colony3)
#'
#' selectColonies(apiary, ID = 4)
#' selectColonies(apiary, ID = "4")
#' selectColonies(apiary, ID = c(4, 5))
#' selectColonies(apiary, ID = c("4", "5"))
#' # ... alternative
#' apiary[1]
#' apiary[[1]]
#' apiary["4"]
#' apiary[["4"]]
#' getId(apiary[c(1, 2)])
#' getId(apiary[c("4", "5")])
#' getId(apiary[c(2, 1)])
#' getId(apiary[c("5", "4")])
#'
#' getId(selectColonies(apiary, p = 0.5))
#' getId(selectColonies(apiary, p = 0.5))
#' getId(selectColonies(apiary, p = 0.5))
#'
#' @export
selectColonies <- function(colonies, ID = NULL, n = NULL, p = NULL) {
  # TODO: add use and trait argument to this function?
  #       the idea is that we could swarm/supersede/... colonies depending on a trait expression
  #       this will be complicated - best to follow ideas from
  #       https://github.com/HighlanderLab/SIMplyBee/issues/105
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!is.null(ID)) {
    ret <- colonies[getId(colonies) %in% ID]
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(colonies)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    lSel <- sample.int(n = nCol, size = n)
    message(paste0("Randomly selecting colonies: ", n))
    if (length(lSel) > 0) {
      ret <- colonies[lSel]
    } else {
      ret <- NULL
    }
  } else {
    stop("Provide either ID, n, or p!")
  }
  validObject(ret)
  return(ret)
}

#' @rdname pullColonies
#' @title Pull out some colonies
#'
#' @description Level 3 function that pulls out some colonies based on colony
#'   ID or random selection.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param ID numeric or character, name of a colony (one or more) to be pulled
#'   out; note that numeric value is converted to character
#' @param n numeric, number of colonies to select
#' @param p numeric, probability of a colony being pulled out
#'
#' @return list with two \code{\link{Colonies-class}}, the \code{pulledColonies}
#'   and the \code{remainingColonies}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' apiary <- createColonies(pop = basePop, nCol = 3)
#' (names <- getId(apiary))
#'
#' tmp <- pullColonies(apiary, ID = names[1])
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, n = 1)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, p = 0.5)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' @export
pullColonies <- function(colonies, ID = NULL, n = NULL, p = NULL) {
  # TODO: add use and trait argument to this function that would be passed to selectColonies()?
  #       the idea is that we could swarm/supersede/... colonies depending on a trait expression
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!is.null(ID)) {
    pulledColonies <- selectColonies(colonies, ID)
    remainingColonies <- removeColonies(colonies, ID)
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(colonies)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    lPull <- sample.int(n = nCol, size = n)
    message(paste0("Randomly pulling colonies: ", n))
    if (length(lPull) > 0) {
      ids <- getId(colonies)
      pulledColonies <- selectColonies(colonies, ids[lPull])
      remainingColonies <- removeColonies(colonies, ids[lPull])
    } else {
      pulledColonies <- createColonies()
      remainingColonies <- colonies
    }
  } else {
    stop("You must provide either ID, n, or p!")
  }
  ret <- list(pulledColonies = pulledColonies, remainingColonies = remainingColonies)
  validObject(ret$pulledColonies)
  validObject(ret$remainingColonies)
  return(ret)
}

#' @rdname removeColonies
#' @title Remove some colonies
#'
#' @description Level 3 function that removes some colonies based on their ID.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param ID numeric or character, name of a colony (one or more) to be removed;
#'   note that numeric value is converted to character
#'
#' @return \code{\link{Colonies-class}} with some colonies removed
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' apiary <- createColonies(pop = basePop, nCol = 3)
#' (names <- getId(apiary))
#'
#' getId(removeColonies(apiary, ID = names[1]))
#' getId(removeColonies(apiary, ID = names[c(2, 3)]))
#'
#' @export
removeColonies <- function(colonies, ID) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  ret <- colonies[!getId(colonies) %in% ID]
  validObject(ret)
  return(ret)
}

#' @rdname buildUpColonies
#' @title Build up colony by adding (raising) workers, drones, and virgin queens
#'   for all given colonies
#'
#' @description Level 3 function that does the same as
#'   \code{\link{buildUpColony}} but for all given colonies.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param nWorkers numeric or function, number of worker; if \code{NULL} then
#'   \code{simParamBee$nWorkers} is used (unless \code{new = TRUE}, currently
#'   present workers are taken into account and only the missing difference is
#'   added)
#' @param nDrones numeric or function, number of drones; if \code{NULL} then
#'   \code{simParamBee$nDrones} is used (unless \code{new = TRUE}, currently
#'   present drones are taken into account so only the missing difference is
#'   added)
#' @param nVirginQueens numeric or function, number of virgin queens; if
#'   \code{NULL} then \code{simParamBee$nVirginQueens} is used (unless \code{new
#'   = TRUE}, currently present virgin queens are taken into account so only the
#'   missing difference is added)
#' @param new logical, should the workers, drones, and virgin queens be added a
#'   fresh (ignoring currently present workers, drones, and virgin queens)
#' @param resetEvents logical, call \code{\link{resetEvents}} as part of the
#'   build up
#' @param year numeric, year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colonies-class}} with workers and drones added
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
#' isProductive(apiary)
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Using defaults in SP
#' # (just to have some bees - change this to your needs!)
#' (apiary <- buildUpColonies(apiary))
#' isProductive(apiary)
#' apiary[[1]]
#' apiary[[2]]
#'
#' apiary <- buildUpColonies(apiary, nWorkers = 100)
#' apiary[[1]] # we are already at the target
#' apiary <- buildUpColonies(apiary, nWorkers = 150)
#' apiary[[1]] # increasing the target
#' apiary <- buildUpColonies(apiary, nWorkers = 100)
#' apiary[[1]] # we are already at the target
#' apiary <- buildUpColonies(apiary, nWorkers = 100, new = TRUE)
#' apiary[[1]] # adding completely new workers & drones
#'
#' # Using functions
#' nWorkersFun <- function(colony) { rpois(n = 1, lambda = 100) }
#' nDronesFun <- function(colony) { rpois(n = 1, lambda = 15) }
#' nVirginQueensFun <- function(colony) { rpois(n = 1, lambda = 15) }
#' apiary <- c(colony1, colony2)
#' tmp <- buildUpColonies(apiary, nWorkers = nWorkersFun, nDrones = nDronesFun,
#'                        nVirginQueens = nVirginQueensFun)
#' tmp[[1]]
#' tmp[[2]]
#' tmp <- buildUpColonies(apiary, nWorkers = nWorkersFun, nDrones = nDronesFun,
#'                        nVirginQueens = nVirginQueens)
#' tmp[[1]]
#' tmp[[2]]
#'
#' # Using functions in simParamBee
#' SP$nWorkers <- nWorkersFun
#' SP$nDrones <- nDronesFun
#' SP$nVirginQueens <- nVirginQueens
#' apiary <- c(colony1, colony2)
#' tmp <- buildUpColonies(apiary)
#' tmp[[1]]
#' tmp[[2]]
#' tmp <- buildUpColonies(apiary)
#' tmp[[1]]
#' tmp[[2]]
#'
#' @export
buildUpColonies <- function(colonies, nWorkers = NULL, nDrones = NULL,
                            nVirginQueens = NULL, new = FALSE,
                            resetEvents = FALSE, year = NULL,
                            simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in seq_len(nCol)) {
    colonies@colonies[[colony]] <- buildUpColony(colony = colonies[[colony]],
                                                 nWorkers = nWorkers,
                                                 nDrones = nDrones,
                                                 nVirginQueens = nVirginQueens,
                                                 new = new,
                                                 resetEvents = resetEvents,
                                                 year = year,
                                                 simParamBee = simParamBee)
  }
  validObject(colonies)
  return(colonies)
}

#' @rdname replaceWorkersColonies
#' @title Replace a proportion of workers with new ones for all given colonies
#'
#' @description Level 3 function that does the same as
#'   \code{\link{replaceWorkers}} but for all given colonies.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param p numeric, proportion of workers to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of workers that stay when \code{p < 1}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colonies-class}} with replaced workers
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
#' apiary <- buildUpColonies(apiary, nWorkers = 10, nDrones = 10)
#' apiary
#' apiary[[1]]
#' getWorkers(apiary[[1]])@id
#' apiary[[2]]
#' getWorkers(apiary[[2]])@id
#' lapply(X = getWorkers(apiary), FUN = function(z) z@id)
#'
#' apiary <- replaceWorkersColonies(apiary)
#' lapply(X = getWorkers(apiary), FUN = function(z) z@id)
#'
#' @export
replaceWorkersColonies <- function(colonies, p = 1, use = "rand",
                                   simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in seq_len(nCol)) {
    colonies@colonies[[colony]] <- replaceWorkers(colony = colonies[[colony]],
                                                  p = p, use = use,
                                                  simParamBee = simParamBee)
  }
  validObject(colonies)
  return(colonies)
}

#' @rdname replaceDronesColonies
#' @title Replace a proportion of drones with new ones for all given colonies
#'
#' @description Level 3 function that does the same as
#'   \code{\link{replaceDrones}} but for all given colonies.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param p numeric, proportion of drones to be replaced with new ones
#' @param use character, all the options provided by \code{\link{selectInd}} -
#'   guides selection of drones that stay when \code{p < 1}
#'
#' @return \code{\link{Colonies-class}} with replaced drones
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
#' apiary <- buildUpColonies(apiary, nWorkers = 10, nDrones = 10)
#' apiary
#' apiary[[1]]
#' getDrones(apiary[[1]])@id
#' apiary[[2]]
#' getDrones(apiary[[2]])@id
#' lapply(X = getDrones(apiary), FUN = function(z) z@id)
#'
#' apiary <- replaceDronesColonies(apiary)
#' lapply(X = getDrones(apiary), FUN = function(z) z@id)
#'
#' @export
replaceDronesColonies <- function(colonies, p = 1, use = "rand") {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in seq_len(nCol)) {
    colonies@colonies[[colony]] <- replaceDrones(colony = colonies[[colony]],
                                                 p = p, use = use)
  }
  validObject(colonies)
  return(colonies)
}

#' @rdname reQueenColonies
#' @title Re-queen a colony for all given colonies
#'
#' @description Level 3 function that does the same as
#'   \code{\link{reQueenColony}} but for all given colonies.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param queens \code{\link{Pop-class}}, queens to be added to colonies, one
#'   for each colony
#'
#' @return \code{\link{Colonies-class}} with re-queened colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 20)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' virginQueens <- basePop[4:5]
#' apiary2 <- reQueenColonies(apiary, queen = virginQueens)
#' apiary2
#' apiary2[[1]]
#' apiary2[[2]]
#'
#' matedQueens <- crossVirginQueen(pop = basePop[4:5],
#'                                 fathers = drones[11:20], nAvgFathers = 2)
#' apiary3 <- reQueenColonies(apiary, queen = matedQueens)
#' apiary3
#' apiary3[[1]]
#' apiary3[[2]]
#'
#' @export
reQueenColonies <- function(colonies, queens) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!isPop(queens)) {
    stop("Argument queens must be a Pop class object!")
  }
  nCol <- nColonies(colonies)
  if (nInd(queens) < nCol) {
    stop("Not enough queens provided!")
  }
  for (colony in seq_len(nCol)) {
    colonies@colonies[[colony]] <- reQueenColony(colony = colonies[[colony]],
                                                 queen = queens[colony])
  }
  validObject(colonies)
  return(colonies)
}

#' @rdname crossColonies
#' @title Cross colony for all given colonies
#'
#' @description Level 3 function that does the same as \code{\link{crossColony}}
#'   but for all given colonies. To ease the use, \code{crossColonies} takes in
#'   a group of drones (Drone Congregation Area - DCA), partitions it so that
#'   each colony virgin queen mates with one group/partition of drones.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param DCA \code{\link{Pop-class}}, Drone Congregation Area
#' @param nAvgFathers numeric, average number of drones (fathers) to used in
#'   matings (see \code{\link{crossColony}})
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colonies-class}} with mated colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 30)
#' colony1 <- createColony(virgin_queen = basePop[2])
#' colony2 <- createColony(virgin_queen = basePop[3])
#' apiary <- c(colony1, colony2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' apiary <- crossColonies(colonies = apiary, DCA = drones, nAvgFathers = 10)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' @export
crossColonies <- function(colonies, DCA, nAvgFathers, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!isPop(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    ret <- createColonies()
  } else {
    ret <- createColonies(nCol = nCol)
    fatherGroups <- pullDroneGroupsFromDCA(DCA, nGroup = nCol,
                                           avgGroupSize = nAvgFathers)
    for (colony in seq_len(nCol)) {
      ret@colonies[[colony]] <- crossColony(colonies[[colony]],
                                            fathers = fatherGroups[[colony]],
                                            simParamBee = simParamBee)
    }
  }
  validObject(ret)
  return(ret)
}

#' @rdname collapseColonies
#' @title Collapse colony for all given colonies
#'
#' @description Level 3 function that does the same as
#'   \code{\link{collapseColony}} but for all given colonies.
#'
#' @param colonies \code{\link{Colonies-class}}
#'
#' @return \code{\link{Colonies-class}} with the collapse event set to
#'   \code{TRUE}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' founderDrones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 30)
#' colony1 <- createColony(queen = basePop[2], fathers = founderDrones[ 1:10])
#' colony2 <- createColony(queen = basePop[3], fathers = founderDrones[11:20])
#' colony3 <- createColony(queen = basePop[4], fathers = founderDrones[21:30])
#' apiary <- c(colony1, colony2, colony3)
#' apiary
#' hasCollapsed(apiary)
#'
#' tmp <- pullColonies(apiary, ID = c("2", "3"))
#' tmp
#' apiaryLost <- collapseColonies(tmp$pulledColonies)
#' hasCollapsed(apiaryLost)
#' apiaryLeft <- tmp$remainingColonies
#' hasCollapsed(apiaryLeft)
#'
#' @export
collapseColonies <- function(colonies) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in seq_len(nCol)) {
    colonies@colonies[[colony]] <- collapseColony(colonies@colonies[[colony]])
  }
  validObject(colonies)
  return(colonies)
}

#' @rdname swarmColonies
#' @title Swarm colony for all given colonies
#'
#' @description Level 3 function that does the same as \code{\link{swarmColony}}
#'   but for all given colonies.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param p numeric, proportion of workers that will leave with the swarm colony
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two \code{\link{Colonies-class}}, the \code{swarms} and the
#'   \code{remnants} (see the description of \code{\link{swarmColony}} what each
#'   node holds!)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony1 <- buildUpColony(colony1, nWorkers = 100)
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony2 <- buildUpColony(colony2, nWorkers = 100)
#' apiary <- c(colony1, colony2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' tmp <- swarmColonies(apiary)
#' tmp$swarms
#' tmp$swarms[[1]]
#' tmp$swarms[[2]]
#' hasSwarmed(tmp$swarms)
#' tmp$remnants
#' tmp$remnants[[1]]
#' tmp$remnants[[2]]
#' hasSwarmed(tmp$remnants)
#'
#' @export
swarmColonies <- function(colonies, p = 0.5, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (p < 0 | p > 1) {
    stop("p must be between 0 and 1!")
  }
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    ret <- list(swarms = createColonies(),
                remnants = createColonies())
  } else {
    ret <- list(swarms = createColonies(nCol = nCol),
                remnants = createColonies(nCol = nCol))
    for (colony in seq_len(nCol)) {
      tmp <- swarmColony(colonies[[colony]], p = p, simParamBee = simParamBee)
      ret$swarms@colonies[[colony]] <- tmp$swarm
      ret$remnants@colonies[[colony]] <- tmp$remnant
    }
  }
  validObject(ret$swarms)
  validObject(ret$remnants)
  return(ret)
}

#' @rdname supersedeColonies
#' @title Supersede colony for all given colonies
#'
#' @description Level 3 function that does the same as
#'   \code{\link{supersedeColony}} but for all given colonies.
#'
#' @param colonies \code{\link{Colonies-class}}
#'
#' @return \code{\link{Colonies-class}} with superseded colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony1 <- buildUpColony(colony1, nWorkers = 100)
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony2 <- buildUpColony(colony2, nWorkers = 100)
#' apiary <- c(colony1, colony2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' apiary <- supersedeColonies(apiary)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#' hasSuperseded(apiary)
#'
#' @export
supersedeColonies <- function(colonies) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    colonies <- createColonies()
  } else {
    for (colony in seq_len(nCol)) {
      colonies@colonies[[colony]] <- supersedeColony(colonies[[colony]])
    }
  }
  validObject(colonies)
  return(colonies)
}

#' @rdname splitColonies
#' @title Split colony in two colonies for all given colonies
#'
#' @description Level 3 function that does the same as \code{\link{splitColony}}
#'   but for all given colonies.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param p numeric, percentage of workers that will go to the split colony
#'
#' @return list with two \code{\link{Colonies-class}}, the \code{splits} and the
#'   \code{remnants} (see the description of \code{\link{splitColony}} what each
#'   node holds!)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony1 <- buildUpColony(colony1, nWorkers = 100)
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony2 <- buildUpColony(colony2, nWorkers = 100)
#' apiary <- c(colony1, colony2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' tmp <- splitColonies(apiary)
#' tmp$splits
#' tmp$splits[[1]]
#' tmp$splits[[2]]
#' hasSplit(tmp$splits)
#' tmp$remnants
#' tmp$remnants[[1]]
#' tmp$remnants[[2]]
#' hasSplit(tmp$remnants)
#'
#' @export
splitColonies <- function(colonies, p = 0.3) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (p < 0 | p > 1) {
    stop("p must be between 0 and 1!")
  }
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    ret <- list(splits = createColonies(),
                remnants = createColonies())
  } else {
    ret <- list(splits = createColonies(nCol = nCol),
                remnants = createColonies(nCol = nCol))
    for (colony in seq_len(nCol)) {
      tmp <- splitColony(colonies[[colony]], p = p)
      ret$splits@colonies[[colony]] <- tmp$split
      ret$remnants@colonies[[colony]] <- tmp$remnant
    }
  }
  validObject(ret$splits)
  validObject(ret$remnants)
  return(ret)
}

#' @rdname setPhenoColonies
#' @title TODO
#'
#' @description Level 3 function that TODO
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param FUN TODO
#' @param ... TODO
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colonies-class}} with phenotypes
#'
#' @examples
#' # TODO
#'
#' @export
# See setPhenoColony() and
#     https://github.com/HighlanderLab/SIMplyBee/issues/26
#     https://github.com/HighlanderLab/SIMplyBee/issues/28
#     https://github.com/HighlanderLab/SIMplyBee/issues/32
#     https://github.com/HighlanderLab/SIMplyBee/issues/44
setPhenoColonies <- function(colonies, FUN = NULL, ..., simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in seq_len(nCol)) {
    colonies@colonies[[colony]] <- setPhenoColony(colonies[[colony]],
                                                  FUN = FUN, ...,
                                                  simParamBee = simParamBee)
  }
  validObject(colonies)
  return(colonies)
}
