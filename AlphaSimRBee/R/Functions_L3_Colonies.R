# Level 3 Colonies Functions

#' @rdname createColonies
#' @title Create colonies
#'
#' @description Level 3 function that creates a set of colonies.
#'
#' @param ... one or more \code{\link{Colony-class}}, \code{\link{Colonies-class}},
#' or \code{NULL} objects
#' @param n numeric, number of colonies to create; this argument takes precedence
#' over \code{...}
#'
#' @return A \code{\link{Colonies-class}} object
#'
#' @examples
#' # Create 10 empty (NULL) colonies
#' apiary <- createColonies(n = 10)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create an apiary from two existing colonies
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[2], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[1], fathers = drones)
#' colony2 <- createColony(virgin_queens = basePop[3])
#' apiary <- createColonies(colony1, colony2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # ... an alternative
#' apiary <- c(colony1, colony2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' @export
createColonies <- function(..., n = NULL) {
  if (is.null(n)) {
    input <- list(...)
    class <- ifelse(length(input) == 0, "NULL", sapply(input, "class"))
    if (!all("NULL" %in% class | "Colony" %in% class | "Colonies" %in% class)) {
      stop("Arguments have to be a NULL, Colony, or Colonies class object!")
    }
    output <- new("Colonies", colonies = input)
  } else {
    output <- new("Colonies", colonies = vector(mode = "list", length = n))
  }
  return(output)
}

#' @rdname createColonies2
#' @title Create colonies (a version that will encompass createColonies soon) TODO
#'
#' @description Level 3 function that creates a set of colonies. Usually to
#'   start a simulation.
#'
#' @param pop \code{\link{Pop-class}}, individuals that will be used to seed
#'   queens and drones in colonies (these individuals are selected at random if
#'   there are more than \code{n})
#' @param n integer, number of colonies to create (if only \code{n} is given
#'   then empty (\code{NULL}) colonies are created - mostly useful for
#'   programming)
#' @param mated logical, create mated or unmated (virgin) colonies
#' @param nAvgFathers integer, number of drones that a queen mates with
#'   TODO nAvgFathers default should go to simParamBee and then we set it to NULL
#'        here and if its NULL we grab value from simParamBee, otherwise use it
#'        from the user
#' @param nDronesPerQueen integer, number of drones that a queen generates
#'   TODO nDronesPerQueen default should go to simParamBee and then we set it to NULL
#'        here and if its NULL we grab value from simParamBee, otherwise use it
#'        from the user
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Create 2 empty (NULL) colonies
#' apiary <- createColonies2(n = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 mated colonies
#' apiary <- createColonies2(pop = basePop, n = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 unmated/virgin colonies
#' apiary <- createColonies2(pop = basePop, n = 2, mated = FALSE)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' @export
createColonies2 <- function(pop = NULL, n, mated = TRUE,
                            nAvgFathers = 15, nDronesPerQueen = 100, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!is.null(pop)) {
    if (!isPop(pop)) {
      stop("Argument pop must be a Pop class object!")
    }
    ret <- new("Colonies", colonies = vector(mode = "list", length = n))
    if (mated) {
      tmp <- pullInd(pop = pop, nInd = n)
      queens <- tmp$pulled
      DCA <- createFounderDrones(pop = tmp$remainder, nDronesPerQueen = nDronesPerQueen)
      fatherPackages <- pullDroneGroupsFromDCA(DCA, nGroup = n, avgGroupSize = nAvgFathers)
      for (colony in 1:n) {
        ret@colonies[[colony]] <- createColony(queen = queens[colony],
                                               fathers = fatherPackages[[colony]],
                                               simParamBee = simParamBee)
      }
    } else {
      virginQueens <- selectInd(pop, nInd = n, use = "rand")
      for (colony in 1:n) {
        ret@colonies[[colony]] <- createColony(virgin_queens = virginQueens[colony],
                                               simParamBee = simParamBee)
      }
    }
  } else if (!is.null(n)) {
    ret <- new("Colonies", colonies = vector(mode = "list", length = n))
  } else {
    stop("You must provide either pop and n or at least n!")
  }
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
#' @param colony Colony, colony that will be added
#' @param colonies Colonies, set of colonies that will be expanded
#' @param pos numeric or character, position of the old colony
#'
#' @return Expanded \code{\link{Colonies-class}} object
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
#' apiary <- assignColonyToColonies(apiary, colony3, pos = 2)
#' getId(apiary)
#'
#' @return Expanded Colonies object
#'
#' @export
# TODO: this should be converted to a <- method
assignColonyToColonies <- function(colonies, colony, pos) {
  if (!isColonies(colonies)) {
    stop("Argument Colonies must be a Colonies class object!")
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colonies@colonies[[pos]] <- colony
  return(colonies)
}

#' @rdname selectColonies
#' @title Select individual colonies from a set of colonies
#'
#' @description Level 3 function that selects individual colonies from a set of
#'   colonies based on colony index or name.
#'
#' @param colonies Colonies, a set of colonies
#' @param ID numeric or character, name of a colony (one or more) in
#' \code{colonies}; note that numeric value is effectively converted to character
#' - see examples how to select by index
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
#' @return Colonies
#'
#' @export
selectColonies <- function(colonies, ID = NULL, p = NULL) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!is.null(ID)) {
    ret <- colonies[getId(colonies) %in% ID]
  } else if (!is.null(p)) {
    lPull <- as.logical(rbinom(n = nColonies(colonies), size = 2, p = p))
    if (any(lPull)) {
      ret <- colonies[lPull]
    } else {
      ret <- NULL
    }
  } else {
    stop("Provide either ID or p!")
  }
  return(ret)
}

#' @rdname pullColonies
#' @title Pull some colonies from a larger set of colonies
#'
#' @description Level 3 function that pulls the colonies from the list of all
#'   colonies based on colony IDs and return two lists: a list of selected
#'   colonies and updated original colonies.
#'
#' @param colonies Colonies, a set of colonies
#' @param ID numeric or character, name of a colony (one or more) in
#' \code{colonies}; note that numeric value is converted to character
#' @param p numeric, probability of a colony being pulled
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' apiary <- createColonies2(pop = basePop, n = 3)
#' (names <- getId(apiary))
#'
#' tmp <- pullColonies(apiary, ID = names[1])
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, p = 0.5)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' @return Colonies
#'
#' @export
pullColonies <- function(colonies, ID = NULL, p = NULL) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!is.null(ID)) {
    pulledColonies <- selectColonies(colonies, ID)
    remainingColonies <- removeColonies(colonies, ID)
  } else if (!is.null(p)) {
    lPull <- as.logical(rbinom(n = nColonies(colonies), size = 2, p = p))
    message(paste0("Pulling out ", sum(lPull), " colonies."))
    if (any(lPull)) {
      ids <- getId(colonies)
      pulledColonies <- selectColonies(colonies, ids[lPull])
      remainingColonies <- removeColonies(colonies, ids[lPull])
    } else {
      pulledColonies <- createColonies()
      remainingColonies <- colonies
    }
  } else {
    stop("You must provide either ID or p!")
  }
  ret <- list(pulledColonies = pulledColonies, remainingColonies = remainingColonies)
  return(ret)
}

#' @rdname removeColonies
#' @title Remove the colonies from the colony list based on IDs
#'
#' @description Level 3 function that removes the colonies from the list of all
#'   colonies based on colony IDs and return a list of remaining colonies.
#'
#' @param colonies Colonies
#' @param ID character, IDs of colony(ies) in \code{colonies}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' apiary <- createColonies2(pop = basePop, n = 3)
#' (names <- getId(apiary))
#'
#' getId(removeColonies(apiary, ID = names[1]))
#' getId(removeColonies(apiary, ID = names[c(2, 3)]))
#'
#' @return Colonies
#'
#' @export
removeColonies <- function(colonies, ID) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  ret <- colonies[!getId(colonies) %in% ID]
  return(ret)
}

#' @rdname buildUpColonies
#' @title  Build up Colonies by adding workers and drones
#'
#' @description Level 3 function that TODO Workers and drones are added to the colonies to build them
#' up to the number of desired workers and drones (nWorkers and nDrones).
#' For example: a user may build up colonies in the Period 1 and if events such as split or swarming
#' occur.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param nWorkers Desired number of workers wanted in the colonies
#' @param nDrones Desired number of drones wanted in the colonies
#'
#' @examples
#' #Create founder haplotypes
#' founderGenomes <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderGenomes)
#'
#' #Create population
#' base <- newPop(founderGenomes)
#'
#' #Create 10 virgin queen colonies
#'  apiary1 <- createMultipleMatedColonies(founderGenomes = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 <- buildUpColonies(apiary1, nWorkers = 1000)
#'
#' @return An updated AlphaSimRBee Colonies object
#'
#' @export
buildUpColonies <- function(colonies, nWorkers, nDrones = nWorkers * 0.1,
                            new = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in 1:nCol) {
    colonies@colonies[[colony]] <- buildUpColony(colony = colonies[[colony]],
                                                 nWorkers = nWorkers,
                                                 nDrones = nDrones,
                                                 new = new,
                                                 simParamBee = simParamBee)
  }
  return(colonies)
}

#' @rdname replaceWorkersColonies
#' @title  Replace workers in all the colonies of a Colonies object
#'
#' @description Level 3 function that ... The function replaces a given proportion of workers in all the colonies
#' of a Colonies object with new workers from the same mated queen. The default percentage
#' is set to 1, hence replacing all workers If some colonies do not have workers, the
#' function does not add them.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#'
#' @examples
#' #Create founder haplotypes
#' founderGenomes <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderGenomes)
#'
#' #Create population
#' base <- newPop(founderGenomes)
#'
#' #Create 10 virgin queen colonies
#'  apiary1 <- createMultipleMatedColonies(founderGenomes = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 <- buildUpColonies(apiary1, nWorkers = 1000)
#'
#' #Replace the workers
#'   apiary1 <- replaceWorkers(apiary1)
#'
#' @return An updated AlphaSimRBee Colonies object
#'
#' @export
replaceWorkersColonies <- function(colonies, p = 1, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in 1:nCol) {
    if (!is.null(colonies[[colony]]@workers)) { # TODO: do we need this if here given what we do in replaceWorkers?
      colonies@colonies[[colony]] <- replaceWorkers(colony = colonies[[colony]],
                                                    p = p,
                                                    simParamBee = simParamBee)
    }
  }
  return(colonies)
}

#' @rdname replaceDronesColonies
#' @title  Replace drones in all the colonies of a Colonies object
#'
#' @description Level 3 function that ... The function replaces a given proportion of drones in all the colonies
#' of a Colonies object with new drones from the same queen. The default percentage
#' is set to 1, hence replacing all drones. If some colonies do not have drones, the
#' function does not add them.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#'
#' @examples
#' #Create founder haplotypes
#' founderGenomes <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderGenomes)
#'
#' #Create population
#' base <- newPop(founderGenomes)
#'
#' #Create 10 virgin queen colonies
#'  apiary1 <- createMultipleMatedColonies(founderGenomes = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 <- buildUpColonies(apiary1, nWorkers = 1000)
#'
#' #Replace the drones
#'   apiary1 <- replaceDrones(apiary1)
#'
#' @return An updated AlphaSimRBee Colonies object
#'
#' @export
replaceDronesColonies <- function(colonies, p = 1) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in 1:nCol) {
    if (!is.null(colonies[[colony]]@drones)) { # TODO: do we need this if here given what we do in replaceDrones?
      colonies@colonies[[colony]] <- replaceDrones(colony = colonies[[colony]],
                                                   p = p)
    }
  }
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
#' @return \code{\link{Colonies-class}}
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
  for (colony in 1:nCol) {
    colonies@colonies[[colony]] <- reQueenColony(colony = colonies[[colony]],
                                                 queen = queens[colony])
  }
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
#' @return \code{\link{Colonies-class}}
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
    ret <- createColonies(n = nCol)
    fatherGroups <- pullDroneGroupsFromDCA(DCA, nGroup = nCol,
                                           avgGroupSize = nAvgFathers)
    for (colony in 1:nCol) {
      ret@colonies[[colony]] <- crossColony(colonies[[colony]],
                                            fathers = fatherGroups[[colony]],
                                            simParamBee = simParamBee)
    }
  }
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
#' @return \code{\link{Colonies-class}}
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
  for (colony in 1:nCol) {
    colonies@colonies[[colony]] <- collapseColony(colonies@colonies[[colony]])
  }
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
    ret <- list(swarms = createColonies(n = 0),
                remnants = createColonies(n = 0))
  } else {
    ret <- list(swarms = createColonies(n = nCol),
                remnants = createColonies(n = nCol))
    for (colony in 1:nCol) {
      tmp <- swarmColony(colonies[[colony]], p = p, simParamBee = simParamBee)
      ret$swarms@colonies[[colony]] <- tmp$swarm
      ret$remnants@colonies[[colony]] <- tmp$remnant
    }
  }
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
#' @return \code{\link{Colonies-class}}
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
    for (colony in 1:nCol) {
      colonies@colonies[[colony]] <- supersedeColony(colonies[[colony]])
    }
  }
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
    ret <- list(splits = createColonies(n = 0),
                remnants = createColonies(n = 0))
  } else {
    ret <- list(splits = createColonies(n = nCol),
                remnants = createColonies(n = nCol))
    for (colony in 1:nCol) {
      tmp <- splitColony(colonies[[colony]], p = p)
      ret$splits@colonies[[colony]] <- tmp$split
      ret$remnants@colonies[[colony]] <- tmp$remnant
    }
  }
  return(ret)
}

#' @rdname setPhenoColonies
#' @title TODO
#'
#' @description Level 3 function that TODO
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param FUN TODO
#' @param ... TODO
#'
#' @examples
#' TODO
#'
#' @return An updated AlphaSimRBee Colonies object
#'
#' @export
setPhenoColonies <- function(colonies, FUN = NULL, ...) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  nCol <- nColonies(colonies)
  for (colony in 1:nCol) {
    colonies@colonies[[colony]] <- setPhenoColony(colonies[[colony]],
                                                  FUN = FUN, ...)
  }
  return(colonies)
}
