#Level 0 Auxilary Fuctions----

# Number of colonies----
#' @rdname nColonies
#' @method nColonies 
#' @title Returns number of colonies in the colonies object 
#' @usage \method{nColonies}(colonies)
#' @description Returns the number of colonies present in the colonies object
#' @param colonies AlphaSimRBee Colonies object from the \code{createColonies(...)} call 
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
#' # Create an apiary containing 10 colonies 
#' Apiary1 = createColonies(,n = 10)
#' 
#' #Check number of colonies present in the apiary 
#' nColonies(apiary2)
#' 
#' @return Integer. Number of colonies present in the colonies AlphaSimRBee object 
#' 
#' @export

nColonies <- function(colonies) {
  if (!"Colonies" %in% class(colonies)) {
    stop("colonies has to be a class Colonies")
  } 
  return(length(colonies@colonies))
}

# nWorkers----
#' @rdname nWorkers
#' @method nWorkers 
#' @title Number of workers present in the colony 
#' @usage \method{nWorkers}(colony)
#' @description Returns the number of workers present in the colony object
#' @param colony AlphaSimRBee population object of class "Colony" 
#'
#' @example 
#' #' #Create founder haplotypes
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
#' Check number of workers in the colony 
#' nWorkers(colony1)
#' 
#' @return Integer. Number of workers present in the colony AlphaSimRBee object 
#' 
#' @export
#'

nWorkers <- function(colony) {
  n = ifelse(!is.null(colony@workers), colony@workers@nInd, 0)
  return(n)
}


# nDrones----
#' @rdname nDrones
#' @method nDrones 
#' @title Number of workers present in the colony 
#' @usage \method{nDrones}(colony)
#' @description Returns the number of drones present in the colony object
#' @param colony AlphaSimRBee population object of class "Colony" 
#'
#' @example 
#' #' #Create founder haplotypes
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
#' colony1@drones = createDrones(colony1, nInd = 100)
#' 
#' Check number of drones in the colony 
#' nDrones(colony1)
#' 
#' @return Integer. Number of drones present in the colony AlphaSimRBee object 
#' 
#' @export

nDrones <- function(colony) {
  n = ifelse(!is.null(colony@drones), colony@drones@nInd, 0)
  return(n)
}


# nVirginQueens----
#' @rdname nVirginQueens
#' @method nVirginQueens 
#' @title Number of virgin queens present in the colony 
#' @usage \method{nVirginQueens}(colony)
#' @description Returns the number of virgin queens present in the colony object
#' @param colony AlphaSimRBee population object of class "Colony" 
#'
#' @example 
#' #' #Create founder haplotypes
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
#' colony1@virgin_queens = createVirginQueens(colony1, nInd = 5)
#' 
#' Check number of drones in the colony 
#' nVirginQueens(colony1)
#' 
#' @return Integer. Number of virgin queens present in the colony AlphaSimRBee object 
#' 
#' @export

nVirginQueens <- function(colony) {
  n = ifelse(!is.null(colony@virgin_queens), colony@virgin_queens@nInd, 0)
  return(n)
}

# nFathers----
#' @rdname nFathers
#' @method nFathers 
#' @title Number of fathers queens present in the colony 
#' @usage \method{nFathers}(colony)
#' @description Returns the number of fathers present in the colony object. 
#' If no queens are present in the colony, no fathers are present and function is returned as 0. 
#' @param colony AlphaSimRBee population object of class "Colony" 
#'
#' @example 
#' #' #Create founder haplotypes
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
#' 
#' Check number of fathers in the colony 
#' nFathers(colony1)
#' 
#' @return Integer. Number of fathers present in the colony AlphaSimRBee object 
#' 
#' @export

nFathers <- function(colony) {
  if (is.null(colony@queen)) {
    n = 0
  } else {
    n = ifelse(!is.null(colony@queen@misc$fathers), colony@queen@misc$fathers@nInd, 0)
  }
  return(n)
}

# isQueenMated----
isQueenMated <- function(x) {
  if ("Pop" %in% class(x)) {
    return(!is.null(x@misc$fathers))
  } else if ("Colony" %in% class(x)) {
    if (!is.null(x@queen)) {
      return(!is.null(x@queen@misc$fathers))
    } else {
      return(FALSE)
    }
  }
}

# Extract the queen's year of birth----
#' @rdname extractQueenYOB
#' @method extractQueenYOB
#' @title Extract the queen's year of birth
#' @usage \method{extractQueenYOB}(colony)
#' @description Extract the queen's year of birth \code{colony@queen@misc$yearOfBirth} slot
#' @param colony AlphaSimRBee population object of class "Colony"
#' 
#' @example 
#'#Create founder haplotypes
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
#' extractQueenYOB(colony)
#' 
#' @return Integer, the year of birth of the queen.
#' 
#' @export
#' 
extractQueenYOB <- function(colony) {
  return(colony@queen@misc$yearOfBirth)
}



# Compute the age of the queen----

#' @rdname computeQueenAge
#' @method computeQueenAge
#' @title Compute the queen's age in years
#' @usage \method{computeQueenAge}(colony, year)
#' @description Compute the age of the queen from the \code{colony@queen@misc$yearOfBirth} slot
#' @param colony AlphaSimRBee Colony object from the \code{createColony(...)} call
#' @param currentYear Integer, current year
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
#' setQueenAge(colony, year = 1)
#' extractQueenYOB(colony)
#' computerQueenAge(colony, currentYear = 5)
#' 
#' @return Integer, the age of the queen.
#' 
#' @export
#' 
computeQueenAge <- function(x, currentYear) {
  if ("Pop" %in% class(x)) {
    return(currentYear - x@misc$yearOfBirth)
  } else if ("Colony" %in% class(x)) {
    if (!is.null(x@queen)) {
      return(currentYear - x@queen@misc$yearOfBirth)
    }
  }
}


# Get colony IDs from the colonies----
#' @rdname getIDs
#' @method getIDs
#' @title Get the colonies IDs from the colonies
#' @usage \method{getIDs}(colonies)
#' @description Get the colony IDs from the colonies
#' @param colonies AlphaSimRBee Colonies object from the \code{createColonies(...)} call 
#'
#' @example 
#'  #Create founder haplotypes
#' founderPop = quickHaplo(nInd=200, nChr=1, segSites=10)
#' 
#' #Set simulation parameters
#' SP = SimParam$new(founderPop)
#' 
#' #Create population
#' pop = newPop(founderPop, simParam=SP)
#' 
#' #Create an apiary containing 10 colonies 
#' Apiary1 = createColonies(,n = 10)
#' 
#' #Get colony IDs from the colonies 
#' getIDs (Apiary1)
#' 

#' @return Character. Colony IDs
#' @export
#' 
getIDs <- function(colonies, ID) {
  return(sapply(colonies@colonies, FUN = function(x) x@id))
}



# Check whether the colony has split
hasSplit <- function(x) {
  if ("Colony" %in% class(x)) {
    return(x@split)
  } else if ("Colonies" %in% class(x)) {
    sapply(x@colonies, FUN = function(z) z@split)
  } else {
    stop("x has to be of class Colony or Colonies")
  }
}

# Check whether the colony has swarmed
hasSwarmed <- function(x) {
  if ("Colony" %in% class(x)) {
    return(x@swarm)
  } else if ("Colonies" %in% class(x)) {
    sapply(x@colonies, FUN = function(z) z@swarm)
  } else {
    stop("x has to be of class Colony or Colonies")
  }
}
# Check whether the colony has superseded
hasSuperseded <- function(x) {
  if ("Colony" %in% class(x)) {
    return(x@supersedure)
  } else if ("Colonies" %in% class(x)) {
    sapply(x@colonies, FUN = function(z) z@supersedure)
  } else {
    stop("x has to be of class Colony or Colonies")
  }
}

# Check the production
isProductive <- function(x) {
  if ("Colony" %in% class(x)) {
    return(x@production)
  } else if ("Colonies" %in% class(x)) {
    sapply(x@colonies, FUN = function(z) z@production)
  } else {
    stop("x has to be of class Colony or Colonies")
  }
}