# Level 3 Colonies Functions

#' @rdname createColonies
#' @title Create colonies
#'
#' @description Level 3 function that creates a set of colonies. Usually to
#'   start a simulation.
#'
#' @param pop \code{\link{Pop-class}}, virgin queens or queens for the colonies
#'   (selected at random if there are more than \code{n} in \code{Pop});
#'   \code{\link{isVirginQueen}} and \code{\link{isQueen}} test will be run on
#'   individuals
#' @param n integer, number of colonies to create (if only \code{nCol} is
#'   given then \code{\link{Colonies-class}} is created with \code{nCol} empty
#'   (\code{NULL}) individual colony) - this is mostly useful for programming)
#' @param mated logical, create mated or unmated (virgin) colonies; if mated,
#'   then \code{nInd(pop)-n} individuals from \code{pop} are used to create
#'   drones with which the queens will mate with
#' @param nFathers integer, number of drones that a queen mates with
#'   TODO nFathers default should go to simParamBee and then we set it to NULL
#'        here and if its NULL we grab value from simParamBee, otherwise use it
#'        from the user
#'        https://github.com/HighlanderLab/SIMplyBee/issues/98
#' @param nDronesPerQueen integer, number of drones to generate per individual
#'   in the \code{pop} for mating with the queens
#'   TODO nDronesPerQueen default should go to simParamBee and then we set it to NULL
#'        here and if its NULL we grab value from simParamBee, otherwise use it
#'        from the user
#'        ttps://github.com/HighlanderLab/SIMplyBee/issues/26
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
#' basePop <- createVirginQueens(founderGenomes)
#'
#' # Create 2 empty (NULL) colonies
#' apiary <- createColonies(n = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 mated colonies
#' apiary <- createColonies(pop = basePop, n = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 unmated/virgin colonies
#' apiary <- createColonies(pop = basePop, n = 2, mated = FALSE)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' @export
createColonies <- function(pop = NULL, n = NULL, mated = TRUE,
                           nFathers = NULL, nDronesPerQueen = 100,
                           yearOfBirth = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!is.null(pop)) {
    if (!isPop(pop)) {
      stop("Argument pop must be a Pop class object!")
    }
    if (any(!(isVirginQueen(pop) | isQueen(pop)))) {
      stop("Individuals in pop must be virgin queens or queens!")
    }
    if (nInd(pop) < n) {
      stop("Not enough individuals in the pop to create n colonies!")
    }
    if (is.null(nFathers)) {
      nFathers <- simParamBee$nFathers
    }
    ret <- new("Colonies", colonies = vector(mode = "list", length = n))
    if (mated) {
      if (nInd(pop) < (n - 1)) {
        stop("You must provide at least n+1 individuals in the pop to create n mated colonies!")
      }
      tmp <- pullInd(pop = pop, nInd = n)
      queens <- tmp$pulled
      DCA <- createDrones(x = tmp$remainder, nInd = nDronesPerQueen)
      fatherPackages <- pullDroneGroupsFromDCA(DCA, n = n, nFathers = nFathers)
      for (colony in seq_len(n)) {
        ret[[colony]] <- createColony(queen = queens[colony],
                                      fathers = fatherPackages[[colony]],
                                      yearOfBirth = yearOfBirth,
                                      simParamBee = simParamBee)
      }
    } else {
      virginQueens <- selectInd(pop, nInd = n, use = "rand")
      for (colony in seq_len(n)) {
        ret[[colony]] <- createColony(virginQueens = virginQueens[colony],
                                      yearOfBirth = yearOfBirth,
                                      simParamBee = simParamBee)
      }
    }
  } else if (!is.null(n)) {
    ret <- new(Class = "Colonies", colonies = vector(mode = "list", length = n))
  } else {
    ret <- new(Class = "Colonies")
  }
  validObject(ret)
  return(ret)
}

#' @rdname selectColonies
#' @title Select individual colonies
#'
#' @description Level 3 function that selects individual colonies based on
#'   colony ID or random selection.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param ID character or numeric, name of a colony (one or more) to be
#'   selected; if character (numeric) colonies are selected based on their name
#'   (position)
#' @param n numeric, number of colonies to select
#' @param p numeric, probability of a colony being selected (takes precedence
#'   over \code{n})
#'
#' @return \code{\link{Colonies-class}} with selected colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' founderDrones <- createDrones(x = basePop[1:4], nInd = 10)
#' colony1 <- createColony(queen = basePop[5], fathers = founderDrones[1:10])
#' colony2 <- createColony(queen = basePop[6], fathers = founderDrones[11:20])
#' colony3 <- createColony(queen = basePop[7], fathers = founderDrones[21:30])
#' colony4 <- createColony(queen = basePop[8], fathers = founderDrones[31:40])
#' apiary <- c(colony1, colony2, colony3, colony4)
#' getId(apiary)
#'
#' getId(selectColonies(apiary, ID = 1))
#' getId(selectColonies(apiary, ID = "5"))
#' getId(selectColonies(apiary, ID = c(1, 2)))
#' getId(selectColonies(apiary, ID = c("5", "6")))
#' getId(selectColonies(apiary, ID = 5))
#' getId(selectColonies(apiary, ID = "9"))
#' # ... alternative
#' getId(apiary[1])
#' getId(apiary[[1]])
#' getId(apiary["5"])
#' getId(apiary[["5"]])
#' getId(apiary[c(1, 2)])
#' getId(apiary[c("5", "6")])
#' getId(apiary[5])
#' getId(apiary["9"])
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
    # Testing because a logical vector recycles on colonies[ID]
    if (!(is.character(ID) | is.numeric(ID))) {
      stop("ID must be character or numeric!")
    }
    ret <- colonies[ID]
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
#' @param ID character or numeric, name of a colony (one or more) to be pulled
#'   out; if character (numeric) colonies are pulled out based on their name
#'   (position)
#' @param n numeric, number of colonies to select
#' @param p numeric, probability of a colony being pulled out (takes precedence
#'   over \code{n})
#'
#' @return list with two \code{\link{Colonies-class}}, the \code{pulledColonies}
#'   and the \code{remainingColonies}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' founderDrones <- createDrones(x = basePop[1:4], nInd = 10)
#' colony1 <- createColony(queen = basePop[5], fathers = founderDrones[1:10])
#' colony2 <- createColony(queen = basePop[6], fathers = founderDrones[11:20])
#' colony3 <- createColony(queen = basePop[7], fathers = founderDrones[21:30])
#' colony4 <- createColony(queen = basePop[8], fathers = founderDrones[31:40])
#' apiary <- c(colony1, colony2, colony3, colony4)
#' getId(apiary)
#'
#' tmp <- pullColonies(apiary, ID = c(1, 2))
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, ID = c("5", "6"))
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, ID = 5)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, ID = "9")
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, n = 2)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, p = 0.75)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' @export
pullColonies <- function(colonies, ID = NULL, n = NULL, p = NULL) {
  # TODO: add use and trait argument to this function?
  #       the idea is that we could swarm/supersede/... colonies depending on a trait expression
  #       this will be complicated - best to follow ideas from
  #       https://github.com/HighlanderLab/SIMplyBee/issues/105
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
#' @param ID character or numeric, name of a colony (one or more) to be
#'   removed; if character (numeric) colonies are removed based on their name
#'   (position)
#'
#' @return \code{\link{Colonies-class}} with some colonies removed
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' founderDrones <- createDrones(x = basePop[1:4], nInd = 10)
#' colony1 <- createColony(queen = basePop[5], fathers = founderDrones[1:10])
#' colony2 <- createColony(queen = basePop[6], fathers = founderDrones[11:20])
#' colony3 <- createColony(queen = basePop[7], fathers = founderDrones[21:30])
#' colony4 <- createColony(queen = basePop[8], fathers = founderDrones[31:40])
#' apiary <- c(colony1, colony2, colony3, colony4)
#' getId(apiary)
#'
#' getId(removeColonies(apiary, ID = 1))
#' getId(removeColonies(apiary, ID = "5"))
#'
#' getId(removeColonies(apiary, ID = c(1, 2)))
#' getId(removeColonies(apiary, ID = c("5", "6")))
#'
#' getId(removeColonies(apiary, ID = 5))
#' getId(removeColonies(apiary, ID = "9"))
#'
#' @export
removeColonies <- function(colonies, ID) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (is.character(ID)) {
    ret <- colonies[!getId(colonies) %in% ID]
  } else if (is.numeric(ID)) {
    ret <- colonies[-ID]
  } else {
    stop("ID must be character or numeric!")
  }
  validObject(ret)
  return(ret)
}

#' @rdname buildUpColonies
#' @title Build up colony by adding (raising) workers and drones for all given
#'   colonies
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
#' @param exact logical, if the csd locus is turned on and exact is TRUE,
#' create the exact specified number of only
#' viable workers (heterozygous on the csd locus)
#' @param new logical, should the workers and drones be added a fresh (ignoring
#'   currently present workers and drones)
#' @param resetEvents logical, call \code{\link{resetEvents}} as part of the
#'   build up
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colonies-class}} with workers and drones added
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' apiary <- c(colony1, colony2)
#' tmp <- buildUpColonies(apiary, nWorkers = nWorkersFun, nDrones = nDronesFun)
#' tmp[[1]]
#' tmp[[2]]
#' tmp <- buildUpColonies(apiary, nWorkers = nWorkersFun, nDrones = nDronesFun)
#' tmp[[1]]
#' tmp[[2]]
#'
#' # Using functions in simParamBee
#' SP$nWorkers <- nWorkersFun
#' SP$nDrones <- nDronesFun
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
                            new = FALSE, exact = FALSE,
                            resetEvents = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (is.null(nWorkers)) {
    nWorkers <- simParamBee$nWorkers
  }
  if (is.null(nDrones)) {
    nDrones <- simParamBee$nDrones
  }
  nCol <- nColonies(colonies)
  for (colony in seq_len(nCol)) {
    colonies[[colony]] <- buildUpColony(colony = colonies[[colony]],
                                        nWorkers = nWorkers,
                                        nDrones = nDrones,
                                        new = new,
                                        exact = exact,
                                        resetEvents = resetEvents,
                                        simParamBee = simParamBee)
  }
  validObject(colonies)
  return(colonies)
}

#' @rdname downsizeColonies
#' @title Reduce number of workers and remove all drones and virgin queens from
#'   colonies
#'
#' @description Level 3 function that downsizes colonies by removing a percentage
#'   of workers, all drones and all virgin queens. Usually in the autumn, such
#'   an event occurs in preparation for the winter months.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param p numeric, percentage of workers to remove from the colonies
#' @param use character, all the options provided by \code{\link{selectInd}};
#'   it guides the selection of workers that will be removed

#' @return \code{\link{Colonies-class}} with workers reduced and drones/virgin queens removed
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' apiary <- createColonies(pop = basePop, n = 2)
#' apiary <- buildUpColonies(apiary)
#' apiary <- addVirginQueens(x = apiary, nInd = 20)
#' apiary[[1]]
#' apiary[[2]]
#'
#' apiary <- downsizeColonies(colonies = apiary)
#' apiary[[1]]
#' apiary[[2]]
#'
#' # TODO: FIND REFERENCES for defaults
#' #   https://github.com/HighlanderLab/SIMplyBee/issues/197
#' @export
downsizeColonies <- function(colonies, p = 0.85, use = "rand") {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  colonies <- removeWorkers(x = colonies, p = p, use = "rand")
  colonies <- removeDrones(x = colonies, p = 1)
  colonies <- removeVirginQueens(x = colonies, p = 1)
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
#'   for each colony; \code{\link{isVirginQueen}} and \code{\link{isQueen}} test
#'   will be run on to ensure these are proper queens
#'
#' @return \code{\link{Colonies-class}} with re-queened colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 20)
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
#'                                 fathers = drones[11:20], nFathers = 2)
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
  if (any(!(isVirginQueen(queens) | isQueen(queens)))) {
    stop("Individuals in queens must be virgin queens or queens!")
  }
  nCol <- nColonies(colonies)
  if (nInd(queens) < nCol) {
    stop("Not enough queens provided!")
  }
  for (colony in seq_len(nCol)) {
    colonies[[colony]] <- reQueenColony(colony = colonies[[colony]],
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
#' @param DCA \code{\link{Pop-class}}, Drone Congregation Area;
#'   \code{\link{isDrone}} test will be run on these to ensure these are drones
#' @param nFathers numeric, average number of drones (fathers) to used in
#'   matings (see \code{\link{crossColony}})
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colonies-class}} with mated colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 30)
#' colony1 <- createColony(virginQueen = basePop[2])
#' colony2 <- createColony(virginQueen = basePop[3])
#' apiary <- c(colony1, colony2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' apiary <- crossColonies(colonies = apiary, DCA = drones, nFathers = 10)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' @export
crossColonies <- function(colonies, DCA, nFathers, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!isPop(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  if (any(!isDrone(DCA))) {
    stop("Individuals in DCA must be drones!")
  }
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    ret <- createColonies()
  } else {
    ret <- createColonies(n = nCol)
    fatherGroups <- pullDroneGroupsFromDCA(DCA, n = nCol,
                                           nFathers = nFathers)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- crossColony(colonies[[colony]],
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
#' basePop <- createVirginQueens(founderGenomes)
#'
#' founderDrones <- createDrones(x = basePop[1], nInd = 30)
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
    colonies[[colony]] <- collapseColony(colonies[[colony]])
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
#' @param year numeric, year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two \code{\link{Colonies-class}}, the \code{swarms} and the
#'   \code{remnants} (see the description of \code{\link{swarmColony}} what each
#'   node holds!)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
swarmColonies <- function(colonies, p = NULL, year = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (is.null(p)) {
    p <- simParamBee$pSwarm
  }
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    ret <- list(swarms = createColonies(),
                remnants = createColonies())
  } else {
    ret <- list(swarms = createColonies(n = nCol),
                remnants = createColonies(n = nCol))
    for (colony in seq_len(nCol)) {
      tmp <- swarmColony(colonies[[colony]], p = p, year = year)
      ret$swarms[[colony]] <- tmp$swarm
      ret$remnants[[colony]] <- tmp$remnant
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
#' @param year numeric, year of birth for virgin queens
#'
#' @return \code{\link{Colonies-class}} with superseded colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
supersedeColonies <- function(colonies, year = NULL) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    colonies <- createColonies()
  } else {
    for (colony in seq_len(nCol)) {
      colonies[[colony]] <- supersedeColony(colonies[[colony]],
                                            year = year)
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
#' @param year numeric, year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two \code{\link{Colonies-class}}, the \code{splits} and the
#'   \code{remnants} (see the description of \code{\link{splitColony}} what each
#'   node holds!)
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
splitColonies <- function(colonies, p = NULL, year = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (is.null(p)) {
    p <- simParamBee$pSplit
  }
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    ret <- list(splits = createColonies(),
                remnants = createColonies())
  } else {
    ret <- list(splits = createColonies(n = nCol),
                remnants = createColonies(n = nCol))
    for (colony in seq_len(nCol)) {
      tmp <- splitColony(colonies[[colony]], p = p, year = year)
      ret$splits[[colony]] <- tmp$split
      ret$remnants[[colony]] <- tmp$remnant
    }
  }
  validObject(ret$splits)
  validObject(ret$remnants)
  return(ret)
}

#' @rdname setPhenoColonies
#' @title TODO
#' https://github.com/HighlanderLab/SIMplyBee/issues/158
#'
#' @description Level 3 function that TODO
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param FUN function, any function that can be applied on \code{colony} and
#'   can return phenotypes for defined traits via \code{\link{SimParamBee}}
#' @param ... all parameters of \code{\link{setPheno}}
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
    colonies[[colony]] <- setPhenoColony(colonies[[colony]],
                                         FUN = FUN, ...,
                                         simParamBee = simParamBee)
  }
  validObject(colonies)
  return(colonies)
}
