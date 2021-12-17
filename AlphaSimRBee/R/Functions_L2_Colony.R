# Level 2 Colony Functions

#' @rdname createColony
#' @title Create a new Colony
#'
#' @description Creates a new \code{\link{Colony}}.
#' The function is intended for creating initial colonies from
#' 'FOUNDERPOP' created by \code{\link{runMacs}}.
#'
#' @param id Character, the ID of the colony, which equals the ID of the queen of not stated otherwise.
#' @param location Numeric, location of the colony (x, y).
#' @param queen AlphaSimR population object to become the queen of the colony.
#' @param drones AlphaSimR population object to become the drones of the colony.
#' @param workers AlphaSimR population object to become the workers of the colony.
#' @param virgin_queens AlphaSimR individual or population object to become the virgin queen(s) of the colony.
#' @param pheno A matrix of the phenotypes of the colony
#' @param swarm Logical, whether the colony has swarmed
#' @param split Logical, whether the colony has split
#' @param supersedure Logical, whether the colony has superseded
#' @param collapse Logical, whether the colony has collapsed
#' @param production Logical, whether the colony produces hive products
#' @param last_event Character, the last event of the colony #TODO: WE probably don't need this
#' @param misc A list, normally empty and exists solely as an open slot available for uses to store extra information about individuals.
#'
#' @examples
#' # Create founder genomes
#' founderGenomes <- quickHaplo(nInd = 200, nChr = 1, segSites = 100)
#'
#' # Set simulation parameters
#' SP <- SimParamBee$new(founderGenomes)
#'
#' # Create a base population
#' basePop <- newPop(founderGenomes)
#'
#' # Create two colonies
#' colony1 <- createColony(queen = basePop[1], fathers = basePop[2:15])
#' colony1
#'
#' colony2 <- createColony(virgin_queens = basePop[16])
#' colony2
#'
#' @return Colony
#'
#' @export
createColony <- function(id = NULL, location = NULL,
                         queen = NULL, yearOfBirth = NULL, fathers = NULL,
                         virgin_queens = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(id)) {
    if (!is.null(queen)) {
      id <- queen@id
    } else {
      id <- NA
    }
  }
  if (!is.null(queen)) {
    queen@misc <- list(yearOfBirth = yearOfBirth, fathers = fathers)
  }
  colony <- new("Colony",
                id = as.character(id),
                location = location,
                queen = queen)
  if (!is.null(queen) & !is.null(fathers)) {
    # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
    colony <- addVirginQueens(colony = colony, nInd = 1, simParamBee = simParamBee)
  }
  return(colony)
}

#' @rdname addWorkers
#' @title Add (raise) workers to the colony
#'
#' @description Adds (raises) the specified number of workers in the colony by
#'   crossing the current queen and the fathers. If there are already some
#'   workers present, new and present workers are combined.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of workers to add
#' @param new logical, should the workers be added a fresh (ignoring current
#'   workers)
#' @param simParamBee \code{\link{SimParamBee}}
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
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
#' @title Add (raise) drones to the colony
#'
#' @description Adds (raises) the specified number of drones in the colony by
#'   crossing the current queen and the fathers. If there are already some
#'   drones present, new and present drones are combined.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, number of drones to add
#' @param new logical, should the drones be added a fresh (ignoring current
#'   drones)
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
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

#' @rdname addVirginQueens
#' @title Add (raise) virgin queens to the colony
#'
#' @description Adds (raises) the specified number of virgin queens in the
#'   colony by crossing the current queen and the fathers.
#'
#' @param colony \code{\link{Colony-class}}
#' @param nInd integer, Number of virgin queens to add
#' @param simParamBee \code{\link{SimParamBee}}
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
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

#' @rdname reQueenColony
#' @title TODO
#'
#' @description Re-queens a colony/colonies that have no queen or virgin queens present.
#' This function allows users to insert a virgin queen or a previously mated queen
#' into the queen-less colony.
#'
#' @param colony \code{\link{Colony-class}}
#' @param queen  Pop/number of queens to add to the queen-less colony/colonies
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=100)
#'
#' #Set simulation parameters
#' SP <- SimParamBee$new(founderPop)
#'
#' #Create population
#' base <- newPop(founderPop)
#'
#' #Create 10 mated colonies from the base population
#' apiary1 <- createMultipleMatedColonies(base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build-up the colonies
#' apiary1 <- buildUpColonies(apiary1, nWorkers = colonyFullSize)
#'
#' #Split all the colonies
#' tmp <- splitColonies(apiary1)
#' apiary1 <- tmp$remnants
#' apiary0 <- tmp$splits
#'
#' #Create 10 virgin queens from the base population
#' virginQueens <- base[1]
#'
#' #Repopulate the split colonies with virgin queens taken from the base population
#' apiary0 <- reQueenColonies(apiary0, queen = virginQueens)
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
    colony@virgin_queens <- queen
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
#' @param nWorkers integer, desired number of workers in the colony
#' @param nDrones integer, desired number of drones in the colony
#' @param new logical, should the workers and drones be added a fresh (ignoring
#'   current workers and drones)
#' @param simParamBee \code{\link{SimParamBee}}
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' buildUpColony(colony, nWorkers = 100)
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
  n <- nWorkers - nWorkers(colony)
  if (n > 0) {
    colony <- addWorkers(colony, nInd = n, new = new, simParamBee = simParamBee)
  }
  n <- nDrones - nDrones(colony)
  if (n > 0) {
    colony <- addDrones(colony, nInd = n, new = new)
  }
  colony@production <- TRUE
  return(colony)
}

#' @rdname replaceWorkers
#' @title Replaces a proportion workers with new workers
#'
#' @description Replaces a proportion workers with new workers. Useful after
#'   events like season change, swarming, supersedure etc. due to the short-life
#'   span of the workers.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of workers to be replaced with new ones
#' @param simParamBee \code{\link{SimParamBee}}

#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
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
#' @export
replaceWorkers <- function(colony, p = 1, simParamBee = NULL) {
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
    colony@workers <- c(selectInd(colony@workers, nInd = nWorkersStay, use = "rand"),
                        tmp$workers)
    # TODO: we need some scaling of the nHomDrones here, right? Is this OK?
    colony@nHomDrones <- as.integer(round(colony@nHomDrones * nWorkersStay / nWorkers) + tmp$nHomDrones)
  } else {
    colony <- addWorkers(colony, nInd = nWorkersReplaced, new = TRUE, simParamBee = simParamBee)
  }
  return(colony)
}

#' @rdname replaceDrones
#' @title Replaces a proportion drones with new workers
#'
#' @description Replaces a proportion drones. with new drones. Useful after
#'   events like season change, swarming, supersedure etc. due to the short-life
#'   span of the drones.
#'
#' @param colony \code{\link{Colony-class}}
#' @param p numeric, proportion of drones. to be replaced with new ones
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
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
#' @export
replaceDrones <- function(colony, p = 1) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  nDrones <- nDrones(colony)
  nDronesReplaced <- round(nDrones * p)
  if (nDronesReplaced < nDrones) {
    nDronesStay <- nDrones - nDronesReplaced
    colony@drones <- c(selectInd(colony@drones, nInd = nDronesStay, use = "rand"),
                       createDrones(colony, nInd = nDronesReplaced))
  } else {
    colony <- addDrones(colony, nInd = nDronesReplaced, new = TRUE)
  }
  return(colony)
}

#' @rdname removeQueen
#' @title Remove queen
#'
#' @description Remove queen of a colony
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
#' @title Remove virgin queens
#'
#' @description Remove virgin queens of a colony
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
#' getVirginQueens(colony)
#'
#' colony <- removeVirginQueens(colony)
#' colony
#' getVirginQueens(colony)
#'
#' @export
removeVirginQueens <- function(colony) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colony@virgin_queens <- NULL
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
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' colony <- addWorkers(colony, nInd = 10)
#' colony
#' getWorkers(colony)@id
#' colony <- removeWorkers(colony, p = 0.5)
#' getWorkers(colony)@id
#'
#' @export
removeWorkers <- function(colony, p) {
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
    nWorkers <- nWorkers(colony)
    nWorkersNew <- round(nWorkers * (1 - p))
    colony@workers <- selectInd(colony@workers, nInd = nWorkersNew, use = "rand")
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
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony <- createColony(queen = basePop[2], fathers = drones)
#' colony
#' colony <- addDrones(colony, nInd = 10)
#' colony
#' getDrones(colony)@id
#' colony <- removeDrones(colony, p = 0.5)
#' getDrones(colony)@id
#'
#' @export
removeDrones <- function(colony, p) {
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
    nDrones <- nDrones(colony)
    nDronesNew <- round(nDrones * (1 - p))
    colony@drones <- selectInd(colony@drones, nInd = nDronesNew, use = "rand")
  }
  return(colony)
}

#' @rdname resetEvents
#' @title Reset the swarm, split, supersedure events
#'
#' @description Reset the slots swarm, split and supersedure to FALSE.
#' A user will use this function at the end of a yearly cycle to reset the events,
#' allowing the user to track the events of the current year without overlap.
#'
#' @param colony \code{\link{Colony-class}}
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' TODO
#'
#' @export
resetEvents <- function(colony) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colony@swarm <- FALSE
  colony@split <- FALSE
  colony@supersedure <- FALSE
  return(colony)
}

#' @rdname crossColony
#' @title Crosses a colony with a virgin queen to a group of drones
#'
#' @description Crosses a colony with a virgin queen to a group of drones and
#'   potentially builds up the colony.
#'
#' @param colony \code{\link{Colony-class}}
#' @param fathers \code{\link{Pop-class}}, drones
#' @param nWorkers integer, number of workers to create
#' @param nDrones integer, number of drones to create
#' @param simParamBee \code{\link{SimParamBee}}
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
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
                        nDrones = nWorkers * 0.1, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (!isPop(fathers)) {
    stop("Argument fathers must be a Pop class object!")
  }
  if (is.null(fathers)) {
    stop("Missing fathers!")
  }
  if (is.null(colony@virgin_queens)) {
    stop("No virgin queen!")
  }
  if (!is.null(colony@queen)) {
    stop("Mated queen already present!")
  }
  colony@queen <- selectInd(colony@virgin_queens, nInd = 1, use = "rand")
  colony@id <- colony@queen@id
  colony@queen@misc$fathers <- fathers
  # TODO: bump the number of virgin queens to ~10 or some default from simParamBee
  colony <- addVirginQueens(colony = colony, nInd = 1, simParamBee = simParamBee)
  colony <- addWorkers(colony, nInd = nWorkers, new = TRUE, simParamBee = simParamBee)
  colony <- addDrones(colony, nInd = nDrones, new = TRUE)
  return(colony)
}

#' @rdname collapseColony
#' @title Replicates colony collapse
#'
#' @description Replicates the collapse of a colony. This can be due to winter losses, disease or other factors.
#' @seealso \code{\link[??????]{collapseColony}}
#'
#' @param colony \code{\link{Colony-class}}
#'
#' @return \code{\link{Colony-class}}
#'
#' @examples
#' TODO
#'
#' @export
collapseColony <- function(colony) {
  if (!isColony(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  colony@collapse <- TRUE
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
