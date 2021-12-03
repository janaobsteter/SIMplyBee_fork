# Level 1 Pop Functions

#' @rdname crateFounderDrones
#' @title Creates drones from base population
#' @usage \method{createFounderDrones}(pop, nDronesPerQueen)
#' @description Creates population of drones from base population.
#'       \Drones are created as double haploids. Founder drones are used when crating colonies to be used as fathers.
#' @param pop AlphaSimRBee Colony object from the \code{createColony(...)}
#' @param nDronesPerQueen Integer, number of drones to create
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
#' founderDrones = createFounderDrones(pop[2:110], nDronesPerQueen = 100)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' @return AlphaSim population object of created drones.
#'
#' @export
createFounderDrones <- function(pop, nDronesPerQueen) {
  if (!("Pop" %in% class(pop))) {
    stop("Argument pop must be a Pop class object!")
  }
  return(makeDH(pop, nDH = nDronesPerQueen))
}

#' @rdname createWorkers
#' @title Creates workers of the colony
#' @usage \method{createWorkers}(colony, nInd)
#' @description Creates the specified number of workers in the colony
#'       \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, number of workers to create
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
#' founderDrones = createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' #Creat workers
#' colony1@workers = createWorkers(colony1, nInd = 1000)
#'
#' @return AlphaSim population object of created workers.
#'
#' @export
createWorkers = function(colony, nInd){
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
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

#' @rdname createDrones
#' @title Creates drones of the colony as double haploids
#' @usage \method{createDrones}(colony, nInd)
#' @description Creates the specified number of drones in the colony
#'       \as double haploids from the current queen \code{colony@queen}.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, the umber of drones to create.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones = createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' #Create drones
#' colony1@drones = createDrones(colony1, nInd = 200)
#'
#' @return AlphaSim population object of created drones.
#' @export
createDrones = function(colony, nInd){
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
  if (is.null(colony@queen)) {
    stop("Missing queen!")
  }
  dronePop = makeDH(pop = colony@queen, nDH = nInd)
  return(dronePop)
}

#' @rdname createVirginQueens
#' @title Creates virgin queen
#' @usage \method{createVirginQueens}(colony, nInd)
#' @description Creates the specified number of virgin queens in the colony
#'        \by mating the current queen and the fathers in the \code{colony@queen@misc$fathers} slot.
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param nInd Integer, the number of virgin queens to create.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones = createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' #Crate virgin queens
#' colony1@virgin_queens = createVirginQueens(colony1, nInd = 17)
#'
#' @return AlphaSim population object of created virgin queens.
#' @export
createVirginQueens = function(colony, nInd){
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
  return(createWorkers(colony, nInd = nInd))
}

#' @rdname createDCA
#' @title Creates a drone congregation area (DCA) from the list of colonies
#' @usage \method{createDCA}(list(colonies))
#' @description Creates a drone congregation area (DCA) from selected colonies.
#' The function takes a vector of the colonies and returns a combined population of drones.
#' @seealso \code{\link[??????]{select_colonies}}
#' @param colonies A list of colonies, each of AlphaSimRBee Colony object
#'
#' @examples
#'# Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones = createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 = createColony(queen = pop[2], fathers = founderDrones[18:37])
#'
#' #Create drones that will be in DCA
#' colony1@drones = createDrones(colony1, nInd = 300)
#' colony2@drones = createDrones(colony2, nInd = 300)
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

#' @rdname pullDronesFromDCA
#' @title Pulls the drones from the DCA
#' @usage \method{pullDronesFromDCA}(DCA, nInd)
#' @description  Pulls a specified number of drones from the DCA and updates the DCA.
#'               Selected drones are removed from DCA.
#' @param DCA AlphaSimR population object created with \code{createDCA(...)} call
#' @param nInd Integer, the number of drones to pull from the DCA
#'
#' @examples
#' # Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones = createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 = createColony(queen = pop[2], fathers = founderDrones[18:37])
#'
#' #Create drones that will be in DCA
#' colony1@drones = createDrones(colony1, nInd = 300)
#' colony2@drones = createDrones(colony2, nInd = 300)
#'
#' DCA = createDCA(c(colony1, colony2))
#'
#' # Pull drones from DCA
#'fathers =  pullDronesFromDCA(DCA, nInd = 19)
#'
#' @return A list with two elements. The first element is the AlphaSimR population object of
#' selected drones. The second element is the updated DCA with the selected drones removed.
#'
#' @export
pullDronesFromDCA = function(DCA, nInd) {
  if (!"Pop" %in% class(DCA)) {
    stop("Argument DCA must be an object of the class Pop")
  }
  selectedDronesID = sample(DCA@id, size = nInd, replace = FALSE)
  sel = DCA@id %in% selectedDronesID
  selectedDrones = DCA[sel]
  updatedDCA = DCA[!sel]
  message(paste0("Selected ", nInd, " fathers from DCA"))
  return(list(selectedDrones = selectedDrones, DCA = updatedDCA))
}

#' @rdname pullDroneGroupsFromDCA
#' @title Pulls a drone package from DCA
#' @usage \method{pullDroneGroupsFromDCA}(DCA, n, nAvgFathers)
#' @description Pulls the packages of drones from DCA. Each package is then used to cros colonies.
#'              Number of drones in package is sampled from the Poisson distribution
#'              with the average = nAvgFathers. Selected drones are removed from DCA.
#'@seealso \code{\link[??????]{pullIndFromCaste}}
#'@param  DCA object of class pop. AlphaSimRBee Colony object from the \code{createColony(...)} call
#'@param n Integer. Number of the packages that needs to be created.
#'@param nAvgFathers Integer. The average number of drones that will be in the package.
#'
#'@examples
#'# Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones = createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 = createColony(queen = pop[2], fathers = founderDrones[18:37])
#'
#' #Create drones that will be in DCA
#' colony1@drones = createDrones(colony1, nInd = 300)
#' colony2@drones = createDrones(colony2, nInd = 300)
#'
#' DCA = createDCA(c(colony1, colony2))
#'
#' #pull drone packages from DCA
#'dronePack = pullDroneGroupsFromDCA(DCA, n = 7, nAvgFathers = 19)
#'
#'@return Two AlphaSim population objects of the colony and the group of pulled individuals.
#'@export
pullDroneGroupsFromDCA <- function(DCA, n, nAvgFathers) {
  if (!"Pop" %in% class(DCA)) {
    stop("Argument DCA must be an object of the class Pop")
  }
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

#' @rdname pullIndFromCaste
#' @title Randomly pulls a number of individuals from any caste group.
#' @usage \method{pullIndFromCaste}(colony, caste, nInd)
#' @description Pulls and separates a random number of individuals from any caste group.
#' Two list groups are created, the group of pulled individuals and the colony.
#'
#' @seealso \code{\link[??????]{pullIndFromCaste}}
#' @param colony Colony class. AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param caste Character. Replicating the caste class structure present in the hive (queen, drones, workers etc)
#' @param nInd Integer. Number of individuals to be pulled from the caste
#'
#' @examples inst/examples/examples_pullIndFromCaste.R
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
#' founderDrones = createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#'
#' #Create drones
#' colony1@drones = createDrones(colony1, nInd = 300)
#'
#' # Pull individuals from the caste
#' indDrone = pullIndFromCaste(colony1, caste = 'drones', nInd = 1)
#'
#'@return Two AlphaSim population objects of the colony and the group of pulled individuals.
#'@export
pullIndFromCaste = function(colony, caste, nInd) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be an object of the class Colony")
  }
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

#' @rdname crossVirginQueen
#' @title Crosses a virgin queen to a group drones.
#' @usage \method{crossVirginQueen}(virginQueen, fathers)
#' @description Crosses a virgin queen to a group of drones
#' @param virginQueen AlphaSimR population object
#' @param fathers AlphaSimR population class.
#'
#' @examples
#' # Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#'
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' founderDrones = createFounderDrones(pop[3:200], nDronesPerQueen = 17)
#' colony1 = createColony(queen = pop[1], fathers = founderDrones[1:17])
#' colony2 = createColony(queen = pop[2], fathers = founderDrones[18:37])
#'
#' #Create drones that will be in DCA
#' colony1@drones = createDrones(colony1, nInd = 300)
#' colony2@drones = createDrones(colony2, nInd = 300)
#'
#' DCA = createDCA(c(colony1, colony2))
#'
#' #pull drone packages from DCA
#' dronePack = pullDroneGroupsFromDCA(DCA, n = 7, nAvgFathers = 19)
#'
#'# Cross the virgin queen
#'
#' virginQueen = colony1@virgin_queens
#' queen = crossVirginQueen(virginQueen, fathers = dronePack[4])
#'
#' @return AlphaSim population object of a mated queen
#' @export
crossVirginQueen = function(virginQueen, fathers) {
  if (!"Pop" %in% class(virginQueen)) {
    stop("Argument virginQueen must be an object of the class Pop")
  }
  if (!"Pop" %in% class(fathers)) {
    stop("Argument fathers must be an object of the class Pop")
  }
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
