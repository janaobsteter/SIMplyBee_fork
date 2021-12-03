# Level 3 Colonies Functions

#' @rdname createColonies
#' @title Create Colonies
#' @usage \method {createColonies}(..., n)
#'
#' @description
#' Creates a new \code{\link{Colonies-class}} from one or more
#' \code{\link{Colony-class}} and/or \code{\link{Colonies-class}}
#' objects.
#'
#' @param ... one or more \code{\link{Colony-class}} and/or
#' \code{\link{Colonies-class}} objects.
#' @param n Number of colony-class objects to add to the colonies-class object
#'
#' @return Returns an empty object of \code{\link{Colonies-class}}
#'
#' @examples
#' # Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' # Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' # Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' # Create colonies
#' apiary1 <- createColonies(n = 10)
#' @export
createColonies <- function(..., n = NULL) {
  if (is.null(n)) {
    input <- list(...)
    class <- sapply(input, "class")
    stopifnot(all(class == "Colony" | class == "Colonies") | all(class == "NULL"))
    output <- new("Colonies", colonies = input)
  } else {
    output <- new("Colonies", colonies = vector(mode = "list", length = n))
  }

  return(output)
}

#' @rdname addColonyToTheColonies
#' @title Adds a colony to the group of colonies
#' @usage \method{addColonyToTheColonies}(colony, colonies)
#'
#' @description  Adds a colony object into a grouped colonies object.
#' For example : A user can use this function to add a new colony to their apiary.
#'
#' @param colony AlphaSimRBee Colony object
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @return Updated AlphaSimRBee Colonies object
#'
#' @examples
#' # Create founder haplotypes
#' founderPop = quickHaplo(nInd=300, nChr=1, segSites=10)
#'
#' # Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' # Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' # Creates founder drones
#' DCA = createFounderDrones(pop[1:100], nDronesPerQueen = 17)
#'
#' # Create colony
#' colony1 <- createColony(queen = pop[201], fathers = DCA[1:27])
#'
#' # Create apiary
#' apiary1 <- createMultipleMatedColonies(pop[101:200], nColonies = 10, nAvgFathers = 15)
#'
#' # Add colony to the apiary
#' apiary1 = addColonyToTheColonies(colony1, apiary1)
#'
#' @return Updated AlphaSimRBee Colonies object
#' @export
addColonyToTheColonies <- function(colony, Colonies) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
  if (!"Colonies" %in% class(Colonies)) {
    stop("Argument Colonies must be an object of the class Colonies")
  }
  Colonies@colonies <- append(Colonies@colonies, list(colony))
  return(Colonies)
}

#' @rdname selectColonies
#' @title Select the colonies from the colony list based on IDs.
#' @usage \method{selectColonies}(colonies, IDs)
#'
#' @description Select the colonies from the list of all colonies based
#' on colony IDs and return a list of selected colonies.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param ID IDs of "colony" class objects listed in the "colonies" object

#' @examples inst/examples/examples_selectColonies.R
#'  # Create founder haplotypes
#'
#' founderPop = quickHaplo(nInd=300, nChr=1, segSites=10)
#'
#' # Set simulation parameters
#'
#' SP = SimParam$new(founderPop)
#'
#' # Create population
#'
#' pop = newPop(founderPop, simParam=SP)
#'
#' # Create colonies
#'
#' founderDrones = createFounderDrones(pop[13:300], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 = createColony(queen = pop[2], fathers = founderDrones[18:37])
#' colony3 = createColony(queen = pop[3], fathers = founderDrones[37:51])
#'
#' # Put the colonies together to the apiary
#'
#' apiary = c(colony1, colony2, colony3)
#'
#' # Select colonies
#'
#' selectedColonies = selectColonies(apiary, ID = c(1,2))
#'
#' @return A list of selected colonies.
#' @export
selectColonies <- function(colonies, ID = NULL, p = NULL) {
  if (!"Colonies" %in% class(colonies)) {
    stop("Argument colonies must be an object of the class Colonies")
  }
  if (!is.null(ID)) {
    ret <- colonies[sapply(colonies@colonies, FUN = function(x) x@id %in% ID)]
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
#' @title Pull the colonies from the colony list based on IDs.
#' @usage \method{pullColonies}(colonies,ID, p)
#'
#' @description Pull the colonies from the list of all colonies based
#' on colony IDs and return two lists: a list of selected colonies and
#' updated original colonies
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param ID IDs of "colony" class objects listed in the "colonies" object
#' @param p Percentage of colonies to pull out of the "colonies" list object
#'
#' @examples
#'  # Create founder haplotypes
#'
#' founderPop = quickHaplo(nInd=300, nChr=1, segSites=10)
#'
#' # Set simulation parameters
#'
#' SP = SimParam$new(founderPop)
#'
#' # Create population
#'
#' pop = newPop(founderPop, simParam=SP)
#'
#' # Create colonies
#'
#' founderDrones = createFounderDrones(pop[13:300], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 = createColony(queen = pop[2], fathers = founderDrones[18:37])
#' colony3 = createColony(queen = pop[3], fathers = founderDrones[37:51])
#'
#' # Put the colonies together to the apiary
#'
#' apiary = c(colony1, colony2, colony3)
#'
#' # Pull the selected colonies
#'
#' pulledColonies = pullColonies(colonies = apiary, ID = c(1,2))
#'
#' apiary = pulledColonies$remainingColonies
#' pull = pulledColonies$pulledColonies
#'
#' @return Two lists: a list of selected colonies and an updated input colonies
#' @export
pullColonies <- function(colonies, ID = NULL, p = NULL) {
  if (!"Colonies" %in% class(colonies)) {
    stop("Argument colonies must be an object of the class Colonies")
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
    stop("Provide either ID or p!")
  }
  return(list(pulledColonies = pulledColonies, remainingColonies = remainingColonies))
}

#' @rdname removeColonies
#' @title Remove the colonies from the colony list based on IDs.
#' @usage \method{removeColonies}(colonies, ID)
#'
#' @description Remove the colonies from the list of all colonies based
#' on colony IDs and return a list of remaining colonies.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param ID IDs of "colony" class objects listed in the "colonies" object
#'
#' @examples
#' # Create founder haplotypes
#'
#' founderPop = quickHaplo(nInd=300, nChr=1, segSites=10)
#'
#' # Set simulation parameters
#'
#' SP = SimParam$new(founderPop)
#'
#' # Create population
#'
#' pop = newPop(founderPop, simParam=SP)
#'
#' # Create colonies
#'
#' founderDrones = createFounderDrones(pop[13:300], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 = createColony(queen = pop[2], fathers = founderDrones[18:37])
#' colony3 = createColony(queen = pop[3], fathers = founderDrones[37:51])
#'
#' # Put the colonies together to the apiary
#'
#' apiary = c(colony1, colony2, colony3)
#'
#' # Remove colonies
#'
#' apiary = removeColonies(apiary, ID = c(1,2))
#'
#' @return A list of remaining colonies. Updated AlphaSimRBee Colonies object
#' @export
removeColonies <- function(colonies, ID) {
  if (!"Colonies" %in% class(colonies)) {
    stop("Argument colonies must be an object of the class Colonies")
  }
  if (!"Pop" %in% class(ID)) {
    stop("Argument ID must be an object of the class Pop")
  }
  return(colonies[!sapply(colonies@colonies, FUN = function(x) x@id %in% ID)])
}

#' @rdname createMultipleVirginColonies
#' @title Create a list object of class "colonies" containing only unmated virgin queens
#' @usage \method{createMultipleVirginColonies}(colonies, ID)
#'
#' @description The function is intended for creating initial colonies from
#' 'FOUNDERPOP'. The user can create a list containing their desired number of colonies (nColonies).
#' All colonies created are populated with un-mated virgin queens, no other members of the caste are
#' present and must be populated in further steps.
#'
#' @param founderPop The initial founder population
#' @param nColonies Number of colonies the use wants to create
#'
#' @examples
#' #' #Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' base = newPop(founderPop, simParam=SP)
#'
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleVirginColonies(founderPop = base, nColonies = 10)
#'
#' @return A AlphaSimRBee Colonies object
#' @export
createMultipleVirginColonies <- function(founderPop, nColonies) {
  ret <- createColonies(n = nColonies)
  virginQueens <- selectInd(founderPop, nInd = nColonies, use = "rand")
  for (colony in 1:nColonies) {
    ret@colonies[[colony]] <- createColony(virgin_queens = virginQueens[colony])
  }
  return(ret)
}

#' @rdname createMultipleMatedColonies
#' @title Create a list object of class "colonies" containing mated queens, virgin queens and fathers
#' @usage \method{createMultipleMatedColonies}(founderPop, nColonies, nAvgFathers)
#'
#' @description The function is intended for creating initial colonies from
#' 'FOUNDERPOP'. The user can create a list containing their desired number of colonies (nColonies).
#' The colonies created contain a mated queen, a virgin queen and fathers (varying number surrounding the nAvgFathers)
#'
#' @param founderPop The initial founder population
#' @param nColonies Number of colonies the use wants to create
#' @param nAvgFathers Average number of fathers that mates with a queen
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
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' @return A AlphaSimRBee Colonies object
#' @export
createMultipleMatedColonies <- function(founderPop, nColonies, nAvgFathers) {
  ret <- createColonies(n = nColonies)
  queensID <- sample(founderPop@id, size = nColonies, replace = FALSE)
  queenMatch <- founderPop@id[founderPop@id %in% queensID]
  queens <- founderPop[queenMatch]
  DPQMatch <- founderPop@id[!founderPop@id %in% queensID]
  DPQs <- founderPop[DPQMatch]
  DCA <- createFounderDrones(pop = DPQs, nDronesPerQueen = 10)
  fatherPackages <- pullDroneGroupsFromDCA(DCA, n = nColonies, nAvgFathers = nAvgFathers)
  for (colony in 1:nColonies) {
    ret@colonies[[colony]] <- createColony(
      queen = queens[colony],
      fathers = fatherPackages[[colony]]
    )
  }
  return(ret)
}

#' @rdname buildUpColonies
#' @title  Build up Colonies by adding workers and drones
#' @usage \method{buildUpColonies}(colonies, nWorkers, nDrones)
#'
#' @description Workers and drones are added to the colonies to build them
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
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' base = newPop(founderPop, simParam=SP)
#'
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 = buildUpColonies(apiary1, nWorkers = 1000, nDrones = 100)
#'
#' @return An updated AlphaSimRBee Colonies object
#' @export
#
buildUpColonies <- function(colonies, nWorkers, nDrones) {
  nCol <- nColonies(colonies)
  for (colony in 1:nCol) {
    colonies@colonies[[colony]] <- buildUpColony(
      colony = colonies[[colony]],
      nWorkers = nWorkers,
      nDrones = nDrones
    )
  }
  return(colonies)
}

#' @rdname replaceWorkersColonies
#' @title  Replace workers in all the colonies of a Colonies object
#' @usage replaceWorkersColonies(colonies)
#'
#' @description The function replaces a given proportion of workers in all the colonies
#' of a Colonies object with new workers from the same mated queen. The default percentage
#' is set to 1, hence replacing all workers If some colonies do not have workers, the
#' function does not add them.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
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
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 = buildUpColonies(apiary1, nWorkers = 1000, nDrones = 100)
#'
#' #Replace the workers
#'   apiary1 <- replaceWorkers(apiary1)
#'
#' @return An updated AlphaSimRBee Colonies object
#' @export
replaceWorkersColonies <- function(colonies, p = 1) {
  nCol <- nColonies(colonies)
  for (colony in 1:nCol) {
    if (!is.null(colonies[[colony]]@workers)) {
      colonies@colonies[[colony]] <- replaceWorkers(
        colony = colonies[[colony]],
        p = p
      )
    }
  }
  return(colonies)
}

#' @rdname replaceDronesColonies
#' @title  Replace drones in all the colonies of a Colonies object
#' @usage replaceDronesColonies(colonies)
#'
#' @description The function replaces a given proportion of drones in all the colonies
#' of a Colonies object with new drones from the same queen. The default percentage
#' is set to 1, hence replacing all drones. If some colonies do not have drones, the
#' function does not add them.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
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
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 = buildUpColonies(apiary1, nWorkers = 1000, nDrones = 100)
#'
#' #Replace the drones
#'   apiary1 <- replaceDrones(apiary1)
#'
#' @return An updated AlphaSimRBee Colonies object
#' @export
replaceDronesColonies <- function(colonies, p = 1) {
  nCol <- nColonies(colonies)
  for (colony in 1:nCol) {
    if (!is.null(colonies[[colony]]@drones)) {
      colonies@colonies[[colony]] <- replaceDrones(
        colony = colonies[[colony]],
        p = p
      )
    }
  }
  return(colonies)
}

#' @rdname reQueenColonies
#' @title  reQueen Colonies
#' @usage \method{reQueenColonies}(colonies, queens)
#'
#' @description: Add a new queen/virgin queen into the queens slot of the colonies.
#' For example: this can be used to re-queen swarmed or split colonies where no queens are present.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param queens Selected individuals to insert into the queen slot of the colony object
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
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 = buildUpColonies(apiary1, nWorkers = 1000, nDrones = 100)
#'
#'  #Split all the colonies
#'  tmp = splitColonies(apiary1)
#'  apiary1 = tmp$remnants
#'  splitcolonies = tmp$splits
#'
#'  #Create 10 virgin queens
#'  virginQueens = createVirginQueens(apiary1[[10]], nColonies(splitcolonies))
#'
#'  # Requeen the splits
#'  splitcolonies = reQueenColonies(splitcolonies, queens = virginQueens)

#'
#' @return An updated AlphaSimRBee Colonies object
#' @export
reQueenColonies <- function(colonies, queens) {
  nCol <- nColonies(colonies)
  if (nInd(queens) < nCol) {
    stop("Not enough queens!")
  }
  for (colony in 1:nCol) {
    colonies@colonies[[colony]] <- reQueenColony(
      colony = colonies[[colony]],
      queen = queens[colony]
    )
  }
  return(colonies)
}

#' @rdname collapseColonies
#' @title  collapseColonies
#' @usage \method{collapseColonies}(colonies, ID)
#'
#' @description:
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param ID IDs of "colony" class objects listed in the "colonies" object
#'
#' @examples
#'# Create founder haplotypes
#'
#' founderPop = quickHaplo(nInd=300, nChr=1, segSites=10)
#'
#' # Set simulation parameters
#'
#' SP = SimParam$new(founderPop)
#'
#' # Create population
#'
#' pop = newPop(founderPop, simParam=SP)
#'
#' # Create colonies
#'
#' founderDrones = createFounderDrones(pop[13:300], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 = createColony(queen = pop[2], fathers = founderDrones[18:37])
#' colony3 = createColony(queen = pop[3], fathers = founderDrones[37:51])
#'
#' # Put the colonies together to the apiary
#'
#' apiary = c(colony1, colony2, colony3)
#'
#' # Collapse colonies
#'
#' apiary = collapseColonies(apiary, ID = c(1,2))
#'
#' @return An updated AlphaSimRBee Colonies object
#' @export
collapseColonies <- function(colonies, ID) {
  return(removeColonies(colonies, ID))
}

#' @rdname supersedeColonies
#' @title  supersedeColonies
#' @usage \method{supersedeColonies}(colonies, crossVirginQueen, fathers, pWorkers, pDrones)
#'
#' @description:  Replicates the process of supersedure, where the
#' queen is replaced by a new virgin queen. The workers and the drones stay
#' in the colonies. If no fathers are present, mating of the virgin queen does not occur.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param crossVirginQueen
#' @param fathers
#' @param pWorkers
#' @param pDrones
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
#' TODO NOT WORKING EXAMPLE
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' # Select colonies for supersedure
#'
#' coloniesForSupersedure = pullColonies(apiary1, p = 0.1)
#'
#' # Supersedure the colonies
#'
#' tmp = supersedeColonies(coloniesForSupersedure)
#'
#' # Put superseded colonies back to the apiary
#'
#' apiary1 = c(apiary1, tmp)
#'
#' @return An updated AlphaSimRBee Colonies object
#' @export
supersedeColonies <- function(colonies
                              # , crossVirginQueen = FALSE, fathers = NULL, pWorkers = 1, pDrones = 1
) {
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    return(createColonies())
  }
  for (colony in 1:nCol) {
    colonies@colonies[[colony]] <- supersedeColony(colonies[[colony]])
  }
  return(colonies)
}

#' @rdname swarmColonies
#' @title  swarmColonies
#' @usage \method{swarmColonies}(colonies, crossVirginQueen, fathers, pWorkers, pDrones)
#'
#' @description: Replicates the swarming of the colonies - the process in which
#' a part of the workers leave with the old queen and creates a new colony (the swarm),
#' while a part of the workers stay with a new queen and the old drones.
#' The swarming colony contains the old mated queen, a percentage (pSwarm)
#' of the original colony's workers, no drones and a virgin queen is created from the worker population.
#' A new location must be given to the new swarm colony.
#' The colony that stays contains the remaining workers and drones.
#' A virgin queen is selected from the workers and mated if fathers are present.
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param crossVirginQueen
#' @param fathers
#' @param pWorkers
#' @param pDrones
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
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 = buildUpColonies(apiary1, nWorkers = 1000, nDrones = 100)
#'
#' @return Two AlphaSim population objects of the swarmed colonies and the remaining colonies
#' @export
swarmColonies <- function(colonies
                          # , crossVirginQueen = FALSE, fathers = NULL, pWorkers = 1, pDrones = 1
) {
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    return(list(swarms = createColonies(n = 0), remnants = createColonies(n = 0)))
  }
  # if (length(fathers) < nCol) {
  #   stop("Not enought fathers)
  # }
  ret <- list(swarms = createColonies(n = nCol), remnants = createColonies(n = nCol))

  for (colony in 1:nCol) {
    tmp <- swarmColony(colonies[[colony]])
    ret$swarms@colonies[[colony]] <- tmp$swarm
    ret$remnants@colonies[[colony]] <- tmp$remnant
  }
  return(ret)
}

#' @rdname splitColonies
#' @title  splitColonies
#' @usage \method{splitColonies}(colonies, crossVirginQueen, fathers, pWorkers, pDrones)
#'
#' @description: Two AlphaSim population objects of the split colonies and the remaining colonies are created.
#' Spit the colony into two new colonies to prevent swarming (in managed populations)
#' - one colony is with the old queen and a part of the workers and drones (this is the remaining colony)
#' - the split colony is taken to a new location with part of the workers.
#'  A new mated queen can be introduced to the split colony.
#'  If no new queen is introduced, a virgin queen must be present to mate with fathers from DCA and continue colony
#'
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param crossVirginQueen
#' @param fathers
#' @param pWorkers
#' @param pDrones
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
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 = buildUpColonies(apiary1, nWorkers = 1000, nDrones = 100)
#'
#'  TODO FINISH EXAMPLE
#'
#' @return Two AlphaSim population objects of the split colonies and the remaining colonies
#' @export
splitColonies <- function(colonies
                          # , crossVirginQueen = FALSE, fathers = NULL, pWorkers = 1, pDrones = 1
) {
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    return(list(splits = createColonies(n = 0), remnants = createColonies(n = 0)))
  }
  # if (length(fathers) < nCol) {
  #   stop("Not enought fathers)
  # }
  ret <- list(splits = createColonies(n = nCol), remnants = createColonies(n = nCol))

  for (colony in 1:nCol) {
    tmp <- splitColony(colonies[[colony]])
    ret$splits@colonies[[colony]] <- tmp$split
    ret$remnants@colonies[[colony]] <- tmp$remnant
  }
  return(ret)
}

#' @rdname crossColonies
#' @title  crossColonies
#' @usage \method{crossColonies}(colonies, DCA, nAvgFathers, crossVirginQueen, fathers, pWorkers, pDrones)
#'
#' @description:  Crosses colonies with a virgin queen to a group of fathers pulled from the DCA
#' \creates workers, drones and a new virgin queen and write them to the corresponding
#' \slots of the colonies object.
#' #IF the colonies are queen-less - select a queen from the virgin queen - if not, mate the current queen!!!
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param crossVirginQueen
#' @param fathers
#' @param pWorkers
#' @param pDrones
#' @param DCA
#' @param nAvgFathers
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
#' #Create 10 virgin queen colonies
#'  apiary1 = createMultipleMatedColonies(founderPop = base, nColonies = 10, nAvgFathers = 15)
#'
#' #Build up colonies by adding 1000 workers and 100 drones to each colony in the "colonies" list
#'  apiary1 = buildUpColonies(apiary1, nWorkers = 1000, nDrones = 100)
#'
#'  TODO FINISH
#'
#' @return An updated AlphaSimRBee Colonies object
#' @export
crossColonies <- function(colonies, DCA, nAvgFathers
                          # , crossVirginQueen = FALSE, fathers = NULL, pWorkers = 1, pDrones = 1
) {
  nCol <- nColonies(colonies)
  if (nCol == 0) {
    return(createColonies())
  }

  # if (length(fathers) < nCol) {
  #   stop("Not enought fathers)
  # }
  ret <- createColonies(n = nCol)
  nFathers <- rpois(n = nCol, lambda = nAvgFathers)
  fatherPackages <- pullDroneGroupsFromDCA(DCA, n = nCol, nAvgFathers = nAvgFathers)

  for (colony in 1:nCol) {
    ret@colonies[[colony]] <- crossColony(colonies[[colony]],
      fathers = fatherPackages[[colony]]
    )
  }
  return(ret)
}

#' @rdname setPhenoColonies
#' @title  setPhenoColonies
#' @usage \method{setPhenoColonies}(colonies, FUN, ...)
#'
#' @description:
#'
#' @param colonies AlphaSimRBee Colonies object containing a list of colonies
#' @param FUN
#' @param ...
#'
#' @examples
#' @return An updated AlphaSimRBee Colonies object
#' @export
setPhenoColonies <- function(colonies, FUN = NULL, ...) {
  nCol <- nColonies(colonies)
  for (colony in 1:nCol) {
    colonies@colonies[[colony]] <- setPhenoColony(colonies[[colony]],
      FUN = FUN, ...
    )
  }
  return(colonies)
}
