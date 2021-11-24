#Level 1 Population Fuctions----

# CreateFounderDrones---- NOT FINISED YET

#' @rdname crateFounderDrones
#' @method createFounderDrones
#' @title Creates drones from base population
#' @usage \method{createFounderDrones}(queenPop, nDronesPerQueen)
#' @description Creates the specified number of drones that are used for
#'       \by mating the founder queen in the \code{colony@queen@misc$fathers} slot.
#' @param queenPop AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nDronesPerQueen Integer, number of drones to create
#'
#' @example 
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
#' colony1@workers = createWorkers(colony1, nInd = 1000)
#' 
#' @return AlphaSim population object of created workers.
#' 
#' @export
# createFounderDrones----

createFounderDrones <- function(queenPop, nDronesPerQueen) {
  return(makeDH(queenPop, nDH = nDronesPerQueen))
}


# createWorkers----

#' @rdname createWorkers
#' @method createWorkers
#' @title Creates workers of the colony
#' @usage \method{createWorkers}(colony, nInd)
#' @description Creates the specified number of workers in the colony
#'       \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, number of workers to create
#'
#' @example 
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
#' colony1@workers = createWorkers(colony1, nInd = 1000)
#' 
#' @return AlphaSim population object of created workers.
#' 
#' @export

createWorkers = function(colony, nInd){
  if (is.null(colony@queen)) {
    stop("Missing queen!") 
  }
  if (!isQueenMated(colony)) {
    stop("Missing fathers!")
  }
  
  workerPop = randCross2(females = colony@queen,
                         males = colony@queen@misc$fathers,
                         nCrosses = nInd)
  return(workerPop)
}


# createDrones----

#' @rdname createDrones
#' @method createDrones
#' @title Creates drones of the colony as double haploids
#' @usage \method{createDrones}(colony, nInd)
#' @description Creates the specified number of drones in the colony
#'       \as double haploids from the current queen \code{colony@queen}.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, the umber of drones to create.
#'
#' @example
#' #' colony1 <- createColony(queen = base[1], fatehrs )#' #Create founder haplotypes
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
#' colony1@workers = createWorkers(colony1, nInd = 1000)
#' colony1@drones = createDrones(colony, nInd = 200)
#' 
#' @return AlphaSim population object of created drones.
#' @export

createDrones = function(colony, nInd){
  if (is.null(colony@queen)) {
    stop("Missing queen!") 
  }
  dronePop = makeDH(pop = colony@queen, nDH = nInd)
  return(dronePop)
}



# createVirginQueens----

#' @rdname createVirginQueens
#' @method createVirginQueens
#' @title Creates virgin queen of the colony as double haploids
#' @usage \method{createVirginQueens}(colony, nInd)
#' @description Creates the specified number of virgin queen in the colony
#'       \as double haploids from the current queen.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, the number of virgin queens to create.
#'
#' @example
#' #' colony1 <- createColony(queen = base[1], fatehrs )#' #Create founder haplotypes
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
#' colony1@workers = createWorkers(colony1, nInd = 1000)
#' colony1@drones = createDrones(colony, nInd = 200)
#' 
#' @return AlphaSim population object of created drones.
#' @export

createVirginQueens = function(colony, nInd){
  return(createWorkers(colony, nInd = nInd))
}



# Create DCA----

#' @rdname createDCA
#' @method createDCA
#' @title Creates a drone congregation area (DCA) from the list of colonies
#' @usage \method{createDCA}(list(colonies))
#' @description Creates a drone congregation area (DCA) from selected colonies.
#' The function takes a vector of the colonies and returns a combined population of drones.
#' @seealso \code{\link[??????]{select_colonies}}
#' @param colonies A list of colonies, each of AlphaSimRBee Colony object
#'
#' @example
#'#' #Create founder haplotypes
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
#' DCA = createDCA(c(colony1, colony2))
#' 
#' @return Single AlphaSim population object of drones from the provided colonies.
#' @export

createDCA = function(colonies) {
  if ("Colonies" %in% class(colonies)) {
    DCA = lapply(X = colonies@colonies, FUN = function(z) z@drones) 
    DCA = mergePops(popList = DCA)
  } else if ("Colony" %in% class(colonies)) {
    DCA = colonies@drones
  } else {
    stop("Argument colonies must be of class Colonies or Colony")
  }
  
  print(paste0("Created a DCA with ", DCA@nInd, " drones."))
  return(DCA)
}



# pullDronesFromDCA----

#' @rdname pullDronesFromDCA
#' @method pullDronesFromDCA
#' @title Pulls the drones from the DCA
#' @usage \method{pullDronesFromDCA}(DCA, nInd)
#' @description  Pulls a specified number of drones from the DCA and updates the DCA
#' @param DCA AlphaSimR population object created with \code{createDCA(...)} call
#' @param nInd Integer, the number of drones to pull from the DCA
#'
#' @example 
#' #'#' #Create founder haplotypes
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
#' DCA = createDCA(c(colony1, colony2))
#' fathers = pullDronesTheDCA(DCA, n = 14)
#' 
#' @return A list with two elements. The first element is the AlphaSimR population object of
#' selected drones. The second element is the updated DCA with the selected drones removed.
#' 
#' @export

pullDronesFromDCA = function(DCA, nInd) {
  selectedDronesID = sample(DCA@id, size = nInd, replace = FALSE)
  sel = DCA@id %in% selectedDronesID
  selectedDrones = DCA[sel]
  updatedDCA = DCA[!sel]
  message(paste0("Selected ", nInd, " fathers from DCA"))
  return(list(selectedDrones = selectedDrones, DCA = updatedDCA))
}



# Pull Drone Packages from DCA---- 

pullDronePackagesFromDCA <- function(DCA, n, nAvgFathers) {
  nFathers = rpois(n = n, lambda = nAvgFathers)
  if (sum(nFathers) > DCA@nInd) {
    stop("Not enough drones in the DCA!")
  }
  ret = vector(mode = "list", length = n)
  for (package in 1:n) {
    DCAresult = pullDronesFromDCA(DCA, nInd = nFathers[package])
    DCA = DCAresult$DCA
    ret[[package]] = DCAresult$selectedDrones
  }
  return(ret)
}

# pull Individuals from the caste----

#' @rdname pullIndFromCaste
#' @method pullIndFromCaste
#' @title Pulls a number of individuals from any caste group 
#' @usage \method{pullIndFromCaste}(colony, caste, nInd)
#' @description Pulls and separates a random number of individuals from any caste group. 
#' Two list groups are created, the group of pulled individuals and the colony.
#' 
#'@seealso \code{\link[??????]{pullIndFromCaste}}
#'@param colony Colony class. AlphaSimRBee Colony object from the \code{createColony(...)} call
#'@param caste Character. Replicating the caste class structure present in the hive (queen, drones, workers etc)
#'@nInd Integer. Number of individuals to be pulled from the caste 
#'
#'@example inst/examples/examples_pullIndFromCaste.R
#'@return Two AlphaSim population objects of the colony and the group of pulled individuals.
#'@export 
#'
pullIndFromCaste = function(colony, caste, nInd) {
  if (nInd > slot(colony, caste)@nInd) {
    stop(paste0("Not enough individuals in ", caste, " ! " ,
                nInd, " required, but ", slot(colony, caste)@nInd, " available."))
  }
  pullId = sample(slot(colony, caste)@id, nInd, replace = F)
  pullMatch = slot(colony, caste)@id %in% pullId
  stayMatch = !slot(colony, caste)@id %in% pullId
  
  indPull = slot(colony, caste)[pullMatch]
  indStay = slot(colony, caste)[stayMatch]
  
  slot(colony, caste) = indStay
  return(list(colony = colony, pulledInd = indPull))
}

# Cross the virgin queen----

#' @rdname crossVirginQueen
#' @method crossVirginQueen
#' @title Crosses a virgin queen to a group drones
#' @usage \method{crossVirginQueen}(virginQueen, fathers)
#' @description Crosses a virgin queen to a group of drones
#' @param virginQueen AlphaSimR population object
#' @param fathers AlphaSimR population class. 
#' 
#' @example
#' @return AlphaSim population object of a mated colony
#' @export

crossVirginQueen = function(virginQueen, fathers) {
  if (isQueenMated(virginQueen)) {
    stop("The queen is mated already!")
  }
  
  if (is.null(fathers)) {
    stop("Missing fathers!")
  }
  
  if (virginQueen@nInd > 1) {
    stop("#TODO: A function to mate multiple virgin queens at once")
  }
  
  virginQueen@misc$fathers = fathers
  
  return(virginQueen)
}