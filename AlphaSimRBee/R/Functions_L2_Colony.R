# Level 2 Colony Functions

#' @rdname createColony
#' @title Create a new Colony
#'
#' @description
#' Creates a new \code{\link{Colony}}.
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
#'
#' @return Returns an object of \code{\link{Colony}}
#'
#' @examples
#' #Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 = createColony(queen = base[1], fathers = base[2:15])
#' colony2 = createColony(virgin_queens = base[16])
#'
#' @return AlphaSim Colony object of class "Colony"
#'
#' @export
createColony <- function(id = NULL, location = NULL, queen = NULL, drones = NULL,
                         workers = NULL, virgin_queens = NULL, fathers = NULL,
                         pheno = NULL, swarm = FALSE, split = FALSE, supersedure = FALSE,
                         collapse = FALSE, # rob = FALSE,
                         production = FALSE,
                         last_event = NULL, yearOfBirth = NULL, misc = NULL,
                         simParam = NULL) {
  if (is.null(simParam)) {
    simParam <- get("SP", envir = .GlobalEnv)
  }
  if (is.null(id)) {
    if (!is.null(queen)) {
      id <- queen@id
    }
  }
  if (!is.null(queen) & !is.null(fathers)) {
    virgin_queens <- randCross2(
      females = queen,
      males = fathers,
      nCrosses = 1)
  }
  if (!is.null(queen)) {
    queen@misc <- list(fathers = fathers, yearOfBirth = yearOfBirth)
  }
  output <- new("Colony",
    id = as.character(id),
    location = location,
    queen = queen,
    drones = drones,
    workers = workers,
    virgin_queens = virgin_queens,
    pheno = matrix(),
    # ncol=simParam@nTraits),
    swarm = swarm,
    split = split,
    supersedure = supersedure,
    collapse = collapse,
    # rob=rob,
    production = production,
    last_event = "new_colony",
    misc = list()
  )
  return(output)
}

#' @rdname setQueensYOB
#' @title Set the queen's year of birth
#' @usage \method{setQueenYOB}(colony)
#' @description Set the year of birth of the queen in the \code{colony@queen@misc$yearOfBirth} slot
#' @param x Undefined argument. Can be a "Pop" class or "Colony" class
#' @param year Integer, the year of the birth of the queen
#'
#' @examples
#' #Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 = createColony(queen = base[1], fathers = base[2:15])
#' setQueenAge(colony, 1)
#'
#' @return AlphaSimRBee Colony object
#'
#' @export
setQueenYOB <- function(x, year) {
  if ("Pop" %in% class(x)) {
    x@misc$yearOfBirth <- year
    return(x)
  } else if ("Colony" %in% class(x)) {
    if (!is.null(x@queen)) {
      x@queen@misc$yearOfBirth <- year
      return(x)
  } else {
    stop("Argument x must be of class Pop or Colony")
  }
}

#' @rdname addWorkers
#' @title Add workers to the colony
#' @usage \method{addWorkers}(colony, nInd)
#'
#' @description Create workers and store them in the \code{colony@workers} slot. If there is
#' already some workers present in the hive, the function will not overwrite them but instead
#' combine the newly created and the existing workers. The function returns the updated colony.
#'
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, number of workers to add.
#'
#' @examples
#' Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 = createColony(queen = base[1], fathers = base[2:15])
#' colony1 = addWorkers(colony1, nInd = 2000)
#'
#' @return Updated AlphaSimRBee Colony object
#'
#' @export
#'
addWorkers <- function(colony, nInd = NULL, ...) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  if (is.function(nInd)) {
    nInd = nInd(colony, ...)
    }
  newWorkers <- createWorkers(colony, nInd)
  if (!is.null(colony@workers)) {
    colony@workers <- mergePops(list(colony@workers, newWorkers))
     }
  else {
    colony@workers <- newWorkers
  }
  print(paste0(nInd, "workers added to the colony"))
  return(colony)
}

#' @rdname addDrones
#' @title
#' @usage \method{addDrones}(colony, nInd)
#'
#' @description Create drones and store them in the \code{colony@drones} slot. If there is
#' already some drones present in the hive, the function will not overwrite them but instead
#' combine the newly created and the existing drones The function returns the updated colony.
#'
#' @param colony AlphaSimRBee Colony object
#' @param nInd Integer, number of drones to add.
#'
#' @examples
#' Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony with queen and fathers
#' colony1 = createColony(queen = base[1], fathers = base[2:15])
#'
#' #Insert workers and drones into the colony
#' colony1 = addWorkers(colony1, nInd = 2000)
#' colony1 = addDrones(colony, nInd = 100)
#'
#' @return Updated AlphaSimRBee Colony object
#'
#' @export
addDrones <- function(colony, nInd) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  if (is.function(nInd)) {
    nWorkers = nWorkers(colony, ...)
  }
  newDrones <- createDrones(colony, nInd)
  if (!is.null(colony@drones)) {
    colony@drones <- mergePops(list(colony@drones, newDrones))
  }
  else {
    colony@drones <- newDrones
  }
  print(paste0(nInd, " drones added to the colony"))
  return(colony)
}

#' @rdname addVirginQueens
#' @title Create additional virgin queens
#' @usage \method{createVirginQueens}(colony, nVirginQueens)
#'
#' @description Creates the specified number of virgin queens in the colony
#'       \by crossing the current queen and the fathers and adds them in
#'       \ the \code{colony@virgin_queens} slot.
#' @param colony AlphaSimRBee Colony object
#' @param nVirginQueens Numeric. Number of virgin queens to create
#'
#' @examples
#' #Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony with queen and fathers
#' colony1 = createColony(queen = base[1], fathers = base[2:15])
#'
#' #Insert workers and drones into the colony
#' colony1 = addWorkers(colony1, nInd = 2000)
#' colony1 = addDrones(colony, nInd = 100)
#'
#' #Insert 10  virgin queens into the colony
#' colony1 = addVirginQueens(colony1, nVirginQueen = 10)
#'
#' @return Updated AlphaSimRBee Colony object
#' @export
addVirginQueens <- function(colony, nVirginQueens) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  if (is.null(colony@queen)) {
    stop("Missing queen!")
  }
  if (is.null(colony@queen@misc$fathers)) {
    stop("Missing fathers!")
  }
  virginQueenPop <- randCross2(
    females = colony@queen,
    males = colony@queen@misc$fathers,
    nCrosses = nVirginQueens
  )
  colony@virgin_queens <- virginQueenPop
  return(colony)
}

#' @rdname reQueenColony
#' @title  reQueenColony
#' @usage \method{reQueenColony}(colony, queen)
#'
#' @description Re-queens a colony/colonies that have no queen or virgin queens present.
#' This function allows users to insert a virgin queen or a previously mated queen
#' into the queen-less colony.
#'
#' @param colony AlphaSimRBee Colony object
#' @param queen  Pop/number of queens to add to the queen-less colony/colonies
#'
#' @examples
#' Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' base = newPop(founderPop, simParam=SP)
#'
#' #Create 10 mated colonies from the base population
#' apiary1 = createMultipleMatedColonies(base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build-up the colonies
#' apiary1 = buildUpColonies(apiary1, nWorkers = colonyFullSize, nDrones = colonyFullSize * 0.1)
#'
#' #Split all the colonies
#' tmp <- splitColonies(apiary1)
#' apiary1 <- tmp$remnants
#' apiary0 <- tmp$splits
#'
#' #Create 10 virgin queens from the base population
#' virginQueens = base[1]
#'
#' #Repopulate the split colonies with virgin queens taken from the base population
#' apiary0 <- reQueenColonies(apiary0, queen = virginQueens)
#'
#' @return Updated AlphaSimRBee Colony object
#' @export
reQueenColony <- function(colony, queen) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
  if (!"Pop" %in% class(queen)) {
    stop("Argument queen must be an object of the class Pop")
  }
  if (!isQueenMated(queen)) {
    colony@virgin_queens <- queen
  } else {
    colony@queen <- queen
    colony@id <- queen@id
  }
  return(colony)
}

#' @rdname buildUpColony
#' @title  buildUpColony
#' @usage \method{buildUpColony}(colony, nWorkers, nDrones)
#'
#' @description Workers and drones are added to a colony/colonies to build it/them
#' up to the number of desired workers and drones (nWorkers and nDrones).
#' A user would want to build up a colony in the Period 1 and if events such as split or swarming
#' occur.
#'
#' @param colony AlphaSimRBee Colony object
#' @param nWorkers Desired number of workers wanted in the colony
#' @param nDrones Desired number of drones wanted in the colony
#'
#' @examples
#' #Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' base = newPop(founderPop, simParam=SP)
#'
#' #Create 10 mated colonies from the base population
#' apiary1 = createMultipleMatedColonies(base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build-up the colonies
#' colonyFullSize = 500
#' apiary1 = buildUpColonies(apiary1, nWorkers = colonyFullSize, nDrones = colonyFullSize * 0.1)
#'
#' @return Updated AlphaSimRBee Colony object
#'
#' @export
buildUpColony <- function(colony, nWorkers, nDrones) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
  n <- nWorkers - nWorkers(colony)
  if (n > 0) {
    colony <- addWorkers(colony, nInd = n)
  }
  n <- nDrones - nDrones(colony)
  if (n > 0) {
    colony <- addDrones(colony, nInd = n)
  }
  colony@production <- TRUE

  return(colony)
}

#' @rdname replaceWorkers
#' @title Replaces a proportion workers with new workers with new genetic information
#' @usage \method{replaceWorkers}(colony, p)
#'
#' @description Replace a proportion of workers in the new with new workers from the same queen and same fathers.
#' A user would want to replace a proportion (or all) of workers after swarming and supersedure or
#' due to the short-life span of the workers.
#'
#' @param colony AlphaSimRBee Colony object.
#' @param p Numeric, proportion of workers to be replaced with new ones.
#'
#' @examples
#' #Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 = createColony(queen = base[1], fathers = base[2:15])
#' colony1 = addWorkers(colony1, nInd = 2000)
#' colony1 = replaceWorkers(colony1, p = 0.2)
#'
#' @return Updated AlphaSimRBee Colony object
#'
#' @export
replaceWorkers <- function(colony, p = 1) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  nWorkers <- colony@workers@nInd
  nWorkersReplaced <- round(nWorkers * p)
  if (nWorkersReplaced < nWorkers) {
    nWorkersStay <- nWorkers - nWorkersReplaced
    colony@workers <- c(
      selectInd(colony@workers, nInd = nWorkersStay, use = "rand"),
      createWorkers(colony, nInd = nWorkersReplaced)
    )
  } else {
    (
      colony@workers <- createWorkers(colony, nWorkersReplaced)
    )
  }
  return(colony)
}

#' @rdname replaceDrones
#' @title Replaces drone with new drone with new genetic information
#' @usage \method{replaceWorkers}(colony, p)
#'
#' @description Replace a proportion of drones in the new with new drones from the same queen.
#' A user would want to replace a proportion (or all) of drones after swarming and supersedure or
#' due to the short-life span of the drones.
#'
#' @param colony AlphaSimRBee Colony object.
#' @param p Numeric, proportion of drones to be replaced with new ones.
#'
#' @examples
#' #Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 = createColony(queen = base[1], fathers = base[2:15])
#' colony1 = addDrones(colony1, nInd = 2000)
#' colony1 = replaceDrones(colony1, p = 0.2)
#'
#' @return Updated AlphaSimRBee Colony object
#'
#' @export
replaceDrones <- function(colony, p = 1) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  nDrones <- colony@drones@nInd
  nDronesReplaced <- round(nDrones * p)
  if (nDronesReplaced < nDrones) {
    nDronesStay <- nDrones - nDronesReplaced
    colony@drones <- c(
      selectInd(colony@drones, nInd = nDronesStay, use = "rand"),
      createDrones(colony, nInd = nDronesReplaced)
    )
  } else {
    (
      colony@drones <- createDrones(colony, nDronesReplaced)
    )
  }
  return(colony)
}

#' @rdname removeWorkers
#' @title Remove selected percentage of workers
#' @usage \method{removeWorkers}(colony, p)
#'
#' @description To decrease the number of workers (for example, in winter)
#'
#' @param colony Colony class. AlphaSimR Colony object from the \code{createColony(...)} call
#' @param p Numeric. 0<=p>=1 .
#'
#' @examples inst/examples/examples_removeWorkers.R
#' @return Updated AlphaSimRBee Colony object
#' @export
removeWorkers <- function(colony, p) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  if (p > 1) {
    stop("p can not be higher than 1")
  } else if (p < 0) {
    stop("p can not be less than 0")
  } else if (p == 1) {
    colony@workers <- NULL
    warning("All workers removed!")
  } else {
    nWorkers <- colony@workers@nInd
    nWorkesNew <- round(nWorkers * (1 - p))
    colony@workers <- selectInd(colony@workers, nInd = nWorkersNew, use = "rand")
  }
  return(colony)
}

#' @rdname removeDrones
#' @title Remove selected percentage of drones
#' @usage \method{removeWorkers}(colony, nWorkers)
#'
#' @description To decrease the number of drones (for example, in winter)
#'
#' @param colony Colony class. AlphaSimR Colony object from the \code{createColony(...)} call
#' @param p Numeric. 0<=p>=1 .
#'
#' @examples inst/examples/examples_removeDrones.R
#' #' @return Updated AlphaSimRBee Colony object
#' @export
removeDrones <- function(colony, p) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  if (p > 1) {
    stop("p can not be higher than 1")
  } else if (p < 0) {
    stop("p can not be less than 0")
  } else if (p == 1) {
    colony@drones <- NULL
    warning("All drones removed!")
  } else {
    nDrones <- colony@drones@nInd
    nDronesNew <- round(nDrones * (1 - p))
    colony@drones <- selectInd(colony@drones, nInd = nDronesNew, use = "rand")
  }
  return(colony)
}

#' @rdname resetEvents
#' @title Reset the swarm, split, supersedure events
#' @usage \method{resetEvents}(colony)
#'
#' @description Reset the slots swarm, split and supersedure to FALSE.
#' A user will use this function at the end of a yearly cycle to reset the events,
#' allowing the user to track the events of the current year without overlap.
#'
#' @param colony AlphaSimRBee Colony object.
#'
#' @examples
# TODO----
# make reset events' example
#'
#' @return An updated AlphaSimRBee Colony object
#'
#' @export
resetEvents <- function(colony) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  colony@swarm <- FALSE
  colony@split <- FALSE
  colony@supersedure <- FALSE
  return(colony)
}

#' @rdname crossColony
#' @title Crosses a colony with a virgin queen to a group of fathers pulled from the DCA.
#' @usage \method{crossColony}(colony, fathers, nWorkers, nDrones)
#'
#' @description Crosses a colony with a virgin queen to a group of fathers pulled from the DCA
#' \creates workers, drones and a new virgin queen and write them to the corresponding
#' \slots of the colony object.
#' #IF the colony is queenless - select a queen from the virgin queen - if not, mate the current queen!!!
#' @seealso \code{\link[??????]{createColony}}
#'
#' @param colony AlphaSimRBee Colony object with a non-mated virgin queen
#' @param fathers Pop Class. Father group pulled from the DCA.
#' @param nWorkers Integer.Number of workers to create
#' @param nDrones Integer. Number of drones to create
#' @examples inst/examples/examples_crossColony.R
#' @return Single AlphaSim population object of a mated colony
#' @export
crossColony <- function(colony, fathers = NULL, nWorkers = 0, nDrones = 0) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  if (!"Pop" %in% class(fathers)) {
    stop("Argument fathers must be an object of the class Pop")
  }
  if (is.null(fathers)) {
    stop("Missing fathers!")
  }
  if (is.null(colony@virgin_queens)) {
    stop("No virgin queen!")
  }
  if (!is.null(colony@queen)) {
    stop("Mated queen present!")
  }
  colony@queen <- selectInd(colony@virgin_queens, nInd = 1, use = "rand")
  colony@id <- colony@queen@id
  colony@queen@misc$fathers <- fathers

  if (nWorkers != 0) {
    colony@workers <- createWorkers(colony, nWorkers)
  }
  if (nDrones != 0) {
    colony@drones <- createDrones(colony, nDrones)
  }
  colony@virgin_queens <- selectInd(colony@workers, nInd = 1, use = "rand")

  return(colony)
}

#' @rdname collapseColony
#' @title Replicates colony collapse
#' @usage \method{collapseColony}(colony)
#'
#' @description Replicates the collapse of a colony. This can be due to winter losses, disease or other factors.
#' @seealso \code{\link[??????]{collapseColony}}
#'
#' @param colony Colony class. AlphaSimRBee Colony object from the \code{createColony(...)} call
#'
#' @examples inst/examples/examples_collapseColony.R
#' @return Single AlphaSim population object of collapsed colony
#' @export
#'
collapseColony <- function(colony) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  colony@collapse <- TRUE
  return(colony)
}

#' @rdname swarmColony
#' @title Replicates the swarming process and produces two colonies.
#' @usage \method{swarmColony}(colony, pSwarm, crossVirginQueen. fathers, nWorkers, nDrones, swarmLocation)
#'
#' @description List. Replicates the swarming of the colony - the process in which
#' a part of the workers leave with the old queen and creates a new colony (the swarm),
#' while a part of the workers stay with a new queen and the old drones.
#' The swarming colony contains the old mated queen,
#'  a percentage (pSwarm) of the original colonies workers, no drones and a virgin queen is created from the worker population.
#'  A new location must be given to the new swarm colony.
#'  The colony that stays contains the remaining workers and drones. A virgin queen is selected from the workers and mated if fathers are present.
#'
#' @seealso \code{\link[??????]{createColony}}
#' @param colony Colony class. AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param pSwarm Integer. Percentage of colony that will swarm
#' @param crossVirginQueen Logical. Whether a virgin queen is to be mated
#' @param fathers AlphaSimR population object. Number of fathers pulled from the DCA
#' @param pWorkers Numeric, proportion of workers that are replaced with the workers from the new queen in the remnant colony
#' @param pDrones Numeric, proportion of drones that are replaced with the drones from the new queen in the remnant colony
#' @param swarm Location Integer. X,Y coordinates of newly made swarmed hive
#'
#' @examples inst/examples/examples_swarm.R
#' @return Two colonies, one with the new queen and proportion of workers and
#' one with the old queen and proportion of workers.
#' @export
swarmColony <- function(colony, pSwarm = 0.5, crossVirginQueen = FALSE, fathers = NULL,
                        pWorkers = 1, pDrones = 1, swarmLocation = NULL) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
  if (!"Pop" %in% class(fathers)) {
    stop("Argument fathers must be an object of the class Pop")
  }
  if (is.null(colony@virgin_queens)) {
    stop("Virgin queen not present in the colony, cannot swarm")
  }
  if (is.null(colony@drones)) {
    warning("No drones present in the colony!")
  }

  nWorkersSwarm <- round(colony@workers@nInd * pSwarm, 0)
  nWorkersStay <- colony@workers@nInd - nWorkersSwarm
  workersSwarmId <- sample(x = colony@workers@id, size = nWorkersSwarm, replace = FALSE)
  workersStayId <- colony@workers@id[!colony@workers@id %in% workersSwarmId]

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
    remnantColony <- replaceWorkers(remnantColony, pWorkers)
    remnantColony <- replaceDrones(remnantColony, pDrones)
    remnantColony@virgin_queens <- createWorkers(remnantColony, 1)
  }

  swarm <- colony
  swarm@workers <- colony@workers[workersSwarmId]
  swarm@virgin_queens <- selectInd(swarm@workers, nInd = 1, use = "rand")
  swarm@drones <- NULL
  swarm@location <- swarmLocation


  remnantColony@last_event <- "remnant"
  swarm@last_event <- "swarm"

  remnantColony@swarm <- TRUE
  swarm@swarm <- TRUE
  remnantColony@production <- FALSE
  swarm@production <- FALSE

  message("Created two colonies.")

  return(list(remnant = remnantColony, swarm = swarm))
}

#' @rdname supersedeColony
#' @title Replicates a supersedure of the colony and replaces the queen with a virgin queen.
#' @usage \method{supersedureColony}(colony, crossVirginQueen, fathers, nWorkers, nDrones)
#'
#' @description Replicates the process of supersedure, where the
#' queen is replaced by a new virgin queen. The workers and the drones stay
#' in the colony. If no fathers are present, mating of the virgin queen does not occur.
#' @seealso \code{\link[??????]{supersedure}}
#'
#' @param colony Colony class. AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param crossVirginQueen Logical. Whether a virgin queen is to be mated
#' @param fathers AlphaSimR population object. Number of fathers pulled from the DCA
#' @param pWorkers Numeric, proportion of workers that are replaced with the workers from the new queen
#' @param pDrones Numeric, proportion of drones that are replaced with the drones from the new queen
#'
#' @examples inst/examples/examples_supersedeColony.R
#' @return Single AlphaSim population object of superseded colony
#' @export
supersedeColony <- function(colony, crossVirginQueen = FALSE, fathers = NULL,
                            pWorkers = 1, pDrones = 1) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
  if (!"Pop" %in% class(fathers)) {
    stop("Argument fathers must be an object of the class Pop")
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
    colony@queen <- colony@virgin_queens
    colony@queen@misc$fathers <- fathers
    colony <- replaceWorkers(colony, pWorkers)
    colony <- replaceDrones(colony, pDrones)
    colony@virgin_queens <- createWorkers(colony, 1)
  }
  colony@last_event <- "superseded"
  colony@supersedure <- TRUE
  colony@production <- TRUE

  return(colony)
}

#' @rdname splitColony
#' @title Split the colony in two colonies.
#' @usage \method{splitColony}(colony, pSplit, newQueen, crossVirginQueen, fathers, nWorkers, nDrones, splitLocation)
#'
#' @description Spit the colony into two new colonies to prevent swarming (in managed populations)
#' - one colony is with the old queen and a part of the workers and drones (this is the remaining colony)
#' - the split colony is taken to a new location with part of the workers.
#'  A new mated queen can be introduced to the split colony.
#'  If no new queen is introduced, a virgin queen must be present to mate with fathers from DCA and continue colony
#' @seealso \code{\link[??????]{splitColony}}
#'
#' @param colony Colony class. AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param pSplit Integer. Percentage of hive to split
#' @param newQueen AlphaSimR population object. A new mated queen is brought into the colony from other source
#' @param crossVirginQueen Logical. If no mated queen is introduced, a virgin queen must be present to mate and continue colony
#' @param fathers AlphaSimR population object. Number of fathers pulled from the DCA
#' @param pWorkers Numeric, proportion of workers that are replaced with the workers from the new queen in the split
#' @param splitLocation Integer. X,Y coordinates of newly made split hive
#'
#' @examples inst/examples/examples_splitColony.R
#' @return Two AlphaSim population objects of the split colony and the remaining colony
#' @export
splitColony <- function(colony, pSplit = 0.30, newQueen = NULL, crossVirginQueen = FALSE, fathers = NULL,
                        pWorkers = 1, splitLocation = NULL) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  if (!"Pop" %in% class(newQueen)) {
    stop("Argument newQueen must be an object of the class Pop")
  }
  if (!"Pop" %in% class(fathers)) {
    stop("Argument fathers must be an object of the class Pop")
  }
  nWorkersSplit <- round(colony@workers@nInd * pSplit, 0)
  noWorkersStay <- colony@workers@nInd - nWorkersSplit
  workersSplitId <- sample(x = colony@workers@id, size = nWorkersSplit, replace = FALSE)
  workersStayId <- colony@workers@id[!colony@workers@id %in% workersSplitId]
  splitColony <- createColony()
  splitColony@workers <- colony@workers[workersSplitId]
  splitColony@location <- splitLocation

  colony@workers <- colony@workers[workersStayId]

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
    splitColony <- replaceWorkers(splitColony, pWorkers)
    splitColony@virgin_queens <- createWorkers(splitColony, 1)
  }

  # Change the status of the colony
  colony@last_event <- "remnant"
  splitColony@last_event <- "split"

  colony@split <- TRUE
  splitColony@split <- TRUE

  colony@production <- TRUE
  splitColony@production <- FALSE

  message("Created two colonies.")
  return(list(remnant = colony, split = splitColony))
}

# TODO
setPhenoColony <- function(colony, FUN = NULL, ...) {
  if (!"Colony" %in% class(colony)) {
    stop("colony must be an object of the class Colony")
  }
  colony@queen <- setPheno(colony@queen, ...)
  colony@workers <- setPheno(colony@workers, ...)
  colony@drones <- setPheno(colony@drones, ...)
  if (!is.null(FUN)) {
    colony@pheno <- FUN(colony, ...)
  }
  return(colony)
}
