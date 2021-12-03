# Level 0 Auxiliary Functions

#' @rdname nColonies
#' @title Returns number of colonies in the colonies object
#' @usage \method{nColonies}(colonies)
#' @description Returns the number of colonies present in the colonies object
#' @param colonies AlphaSimRBee Colonies object from the \code{createColonies(...)} call
#'
#' @examples
#' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' # Create an apiary containing 10 colonies
#' Apiary1 <- createColonies(,n = 10)
#'
#' #Check number of colonies present in the apiary
#' nColonies(apiary2)
#'
#' @return Integer. Number of colonies present in the colonies AlphaSimRBee object
#'
#' @export
nColonies <- function(colonies) {
  if (!"Colonies" %in% class(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  n <- length(colonies@colonies)
  return(n)
}

#' @rdname nWorkers
#' @title Number of workers present in the colony
#' @usage \method{nWorkers}(colony)
#' @description Returns the number of workers present in the colony object
#' @param colony AlphaSimRBee population object of class "Colony"
#'
#' @examples
#' #' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 <- createColony(queen = base[1], fathers = base[2:15])
#' colony1@workers <- createWorkers(colony1, nInd = 1000)
#'
#' Check number of workers in the colony
#' nWorkers(colony1)
#'
#' @return Integer. Number of workers present in the colony AlphaSimRBee object
#'
#' @export
#'
nWorkers <- function(colony) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  n <- ifelse(is.null(colony@workers), 0, colony@workers@nInd)
  return(n)
}

#' @rdname nDrones
#' @title Number of workers present in the colony
#' @usage \method{nDrones}(colony)
#' @description Returns the number of drones present in the colony object
#' @param colony AlphaSimRBee population object of class "Colony"
#'
#' @examples
#' #' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 <- createColony(queen = base[1], fathers = base[2:15])
#' colony1@drones <- createDrones(colony1, nInd = 100)
#'
#' Check number of drones in the colony
#' nDrones(colony1)
#'
#' @return Integer. Number of drones present in the colony AlphaSimRBee object
#'
#' @export
nDrones <- function(colony) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  n <- ifelse(is.null(colony@drones), 0, colony@drones@nInd)
  return(n)
}

#' @rdname nVirginQueens
#' @title Number of virgin queens present in the colony
#' @usage \method{nVirginQueens}(colony)
#' @description Returns the number of virgin queens present in the colony object
#' @param colony AlphaSimRBee population object of class "Colony"
#'
#' @examples
#' #' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 <- createColony(queen = base[1], fathers = base[2:15])
#' colony1@virgin_queens <- createVirginQueens(colony1, nInd = 5)
#'
#' Check number of drones in the colony
#' nVirginQueens(colony1)
#'
#' @return Integer. Number of virgin queens present in the colony AlphaSimRBee object
#'
#' @export
nVirginQueens <- function(colony) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  n <- ifelse(is.null(colony@virgin_queens), 0, colony@virgin_queens@nInd)
  return(n)
}

#' @rdname nFathers
#' @title Number of fathers queens present in the colony
#' @usage \method{nFathers}(colony)
#' @description Returns the number of fathers present in the colony object.
#' If no queens are present in the colony, no fathers are present and function is returned as 0.
#' @param colony AlphaSimRBee population object of class "Colony"
#'
#' @examples
#' #' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 <- createColony(queen = base[1], fathers = base[2:15])
#'
#' Check number of fathers in the colony
#' nFathers(colony1)
#'
#' @return Integer. Number of fathers present in the colony AlphaSimRBee object
#'
#' @export
nFathers <- function(colony) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  if (is.null(colony@queen)) {
    n <- 0
  } else {
    n <- ifelse(is.null(colony@queen@misc$fathers), 0, colony@queen@misc$fathers@nInd)
  }
  return(n)
}

#' @rdname isQueenMated
#' @title Is the queen mated?
#' @usage \method{isQueenMated}(x)
#' @description
#' @param x Undefined argument. Can be a "Pop" class or "Colony" class
#'
#' @examples
#'
#' @return Logical.
#'
#' @export
isQueenMated <- function(x) {
  if ("Pop" %in% class(x)) {
    ret <- !is.null(x@misc$fathers)
  } else if ("Colony" %in% class(x)) {
    if (!is.null(x@queen)) {
      ret <- !is.null(x@queen@misc$fathers)
    } else {
      ret <- FALSE
    }
  } else {
    stop("Argument x must be a Colony or Pop class object!")
  }
  return(ret)
}

#' @rdname extractQueenYOB
#' @title Extract the queen's year of birth
#' @usage \method{extractQueenYOB}(colony)
#' @description Extract the queen's year of birth \code{colony@queen@misc$yearOfBirth} slot
#' @param colony AlphaSimRBee population object of class "Colony"
#'
#' @examples
#' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 <- createColony(queen = base[1], fathers = base[2:15])
#' setQueenAge(colony, 1)
#' extractQueenYOB(colony)
#'
#' @return Integer, the year of birth of the queen.
#'
#' @export
extractQueenYOB <- function(colony) {
  if (!"Colony" %in% class(colony)) {
    stop("Argument colony must be a Colony class object!")
  }
  year <- colony@queen@misc$yearOfBirth
  return(year)
}

#' @rdname computeQueenAge
#' @title Compute the queen's age in years
#' @usage \method{computeQueenAge}(colony, year)
#' @description Compute the age of the queen from the \code{colony@queen@misc$yearOfBirth} slot
#' @param x Undefined argument. Can be a "Pop" class or "Colony" class
#' @param currentYear Integer, current year
#'
#' @examples
#' #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Creates colony
#' colony1 <- createColony(queen = base[1], fathers = base[2:15])
#' setQueenAge(colony, year = 1)
#' extractQueenYOB(colony)
#' computerQueenAge(colony, currentYear = 5)
#'
#' @return Integer, the age of the queen.
#'
#' @export
computeQueenAge <- function(x, currentYear) {
  if ("Pop" %in% class(x)) {
    ret <- currentYear - x@misc$yearOfBirth
  } else if ("Colony" %in% class(x)) {
    if (!is.null(x@queen)) {
      ret <- currentYear - x@queen@misc$yearOfBirth
    }
  } else {
    stop("Argument x must be a Colony or Pop class object!")
  }
  return(ret)
}

#' @rdname getId
#' @title Get the colonies IDs from the colonies
#' @usage \method{getId}(colonies)
#' @description Get the colony IDs from the colonies
#' @param colonies AlphaSimRBee Colonies object from the \code{createColonies(...)} call
#'
#' @examples
#'  #Create founder haplotypes
#' founderPop <- quickHaplo(nInd=200, nChr=1, segSites=10)
#'
#' #Set simulation parameters
#' SP <- SimParam$new(founderPop)
#'
#' #Create population
#' pop <- newPop(founderPop, simParam=SP)
#'
#' #Create an apiary containing 10 colonies
#' Apiary1 <- createColonies(,n = 10)
#'
#' #Get colony IDs from the colonies
#' getId (Apiary1)
#'
#' @return Character. Colony IDs
#' @export
getId <- function(x) {
  if ("Colony" %in% class(x)) {
    id <- x@id
  } else if ("Colonies" %in% class(x)) {
    id <- sapply(x@colonies, FUN = function(x) x@id)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(id)
}

#' @rdname hasSplit
#' @title Test to see if colony/colonies have split in the current period
#' @usage \method{hasSplit}(x)
#' @description
#' @param x Undefined argument. Can be a "Colony" class or "Colonies" class
#'
#' @examples
#'
#' @return
#'
#' @export
hasSplit <- function(x) {
  if ("Colony" %in% class(x)) {
    ret <- x@split
  } else if ("Colonies" %in% class(x)) {
    ret <- sapply(x@colonies, FUN = function(z) z@split)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname hasSwarmed
#' @title Test to see if colony/colonies have swarmed in the current period
#' @usage \method{hasSwarmed}(x)
#' @description
#' @param x Undefined argument. Can be a "Colony" class or "Colonies" class
#'
#' @examples
#' @return
#'
#' @export
hasSwarmed <- function(x) {
  if ("Colony" %in% class(x)) {
    ret <- x@swarm
  } else if ("Colonies" %in% class(x)) {
    ret <- sapply(x@colonies, FUN = function(z) z@swarm)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname hasSuperseded
#' @title Test to see if colony/colonies have superseded in the current period
#' @usage \method{hasSuperseded}(x)
#' @description
#' @param x Undefined argument. Can be a "Colony" class or "Colonies" class
#'
#' @examples
#'
#' @return
#'
#' @export
hasSuperseded <- function(x) {
  if ("Colony" %in% class(x)) {
    ret <- x@supersedure
  } else if ("Colonies" %in% class(x)) {
    ret <- sapply(x@colonies, FUN = function(z) z@supersedure)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname isProductive
#' @title Test to see if colony/colonies are currently productive
#' @usage \method{isProductive}(x)
#' @description
#'
#' @param x Undefined argument. Can be a "Colony" class or "Colonies" class
#'
#' @examples
#'
#' @return
#'
#' @export
isProductive <- function(x) {
  if ("Colony" %in% class(x)) {
    ret <- x@production
  } else if ("Colonies" %in% class(x)) {
    ret <- sapply(x@colonies, FUN = function(z) z@production)
  } else {
    stop("Argument x must be Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname simulateHoneyBeeGenomes
#' @title Simulate the Honey bee genome, including the csd locus
#' @usage \method{simulateHoneyBeeGenomes}(nInd, nChr, nSegSites, Ne, nBp. genLen,
#'                                         mutRate, histNe, histGen, split, csdChr,
#'                                         csdPos, nCsdHaplos, nThreads)
#'
#' @description
#'
#' @param nInd number of individuals to simulate
#' @param nChr number of chromosomes to simulate
#' @param nSegSites number of segregating sites to keep per chromosome
#' @param Ne effective population size
#' @param nBp  base pair length of chromosome
#' @param genLen genetic length of chromosome in Morgans
#' @param mutRate per base pair mutation rate
#' @param histNe effective population size in previous generations
#' @param histGen number of generations ago for effective population sizes given in histNe
#' @param split an optional historic population split in terms of generations ago
#' @param csdChr which number chromosome the csd locus is located on
#' @param csdPos vector. position of the csd locus on the chromosome
#' @param nCsdHaplos number of possible csd haplotypes
#' @param nThreads if OpenMP is available, this will allow for simulating chromosomes in parallel.
#'                 If the value is NULL, the number of threads is automatically detected
#'
#' @examples
#' tmp <- simulateHoneyBeeGenomes(nInd = 1000, ...)
#' founderGenomes <- tmp$founderGenomes
#' SP <- tmp$SP
#' csd <- tmp$csd
#' rm(tmp)
#'
#' @return
#'
#' @export
simulateHoneyBeeGenomes <- function(nInd = NULL,
                                   nChr = 16,
                                   nSegSites = 1000,
                                   Ne = 157598,  # Wallberg et al. (2014)
                                   nBp = 2.252e+8 / 16, # GenBank Amel_Hv3.1
                                   genLen = 34.5 / 16, # Hunt&Page (1995)
                                   mutRate = 9.0e-9, # Yang et al. (2015)
                                   histNe = Ne, # TODO revise and citation
                                   histGen = 1, # TODO revise and citation
                                   split = NULL, # TODO revise and citation
                                   csdChr = 3, # TODO citation
                                   csdPos = 0.865, # TODO citation
                                   nCsdHaplos = 100, # TODO revise & citation
                                   nThreads = NULL) {
  # No of possible haplotypes from n biallelic SNP is 2^n, so we need at least
  # n seg sites (if 2^n must be at least k, then log2(2^n) = log2(k) = n log2(2);
  # then n must be at least log2(k) / log2(2) = log2(k))
  if (!is.null(csdChr)) {
    nCsdLoci <- ceiling(log2(nCsdHaplos))
    if (nSegSites < nCsdLoci) {
      stop("You must have at the least ", ceiling(log2(nCsdHaplos)), " segregating sites to simulate ", nCsdHaplos, " csd haplotypes!")
    }
  }
  ret <- vector(mode = "list", length = 3)
  names(ret) <- c("founderGenomes", "SimParam", "csd")
  # TODO: we will need to use runMacs(manualCommand = ...) to accomodate the honeybee demography,
  #       because runMacs2 works only with simple splits, while honenybee demography is more
  #       "involved"
  tmp$founderGenomes <- runMacs2(nInd = nInd,
                                nChr = nChr,
                                segSites = nSegSites,
                                Ne = Ne,
                                bp = nBp,
                                genLen = genLen,
                                mutRate = mutRate,
                                histNe = histNe,
                                histGen = histGen,
                                split = split,
                                inbred = FALSE,
                                ploidy = 2L,
                                nThreads = nThreads)
  if (!is.null(csdChr)) {
    tmp$SP <- SimParam$new(founderPop = founderGenomes)
    genMap <- tmp$SP$genMap
    csdPosStart <- floor(nSegSites * csdPos)
    csdPosStop <- ceiling(csdPos + nCsdLoci - 1)
    if (csdPosStop > nSegSites) {
      stop("Too few segregagting sites to simulate so many csd haplotypes at the given position!")
    }
    genMap[[csdChr]][csdPosStart:csdPosStop] <- 0
    tmp$SP$switchGenMap(genMap = genMap)
    tmp$csd <- list(chr = 3, start = csdPosStart, stop = csdPosStop)
  }
  return(tmp)
}

#' @rdname getCsdHaplo
#' @title Get the Haplotypes of the csd locus
#' @usage \method{getCsdHaplo}(x, csd)
#'
#'@param x Undefined argument. Can be a "Colony" class or "Colonies" class
#'@param csd complementary sex determination locus- list with nodes chr, start and stop
#'
#' @description
#'
#' @examples
#'
#' @return
#'
#' @export
getCsdHaplo <- function(x, csd = NULL) {
  if (is.null(csd) | !is.list(csd)) {
    stop("csd must be given and has to be a list with nodes chr, start, and stop")
  }
  if ("Pop" %in% class(x)) {
    ret <- pullSegSiteHaplo(pop = pop, chr = csd$chr)[, csd$start:csd$stop]
  } else if ("Colony" %in% class(x)) {
    ret <- vector(mode = "list", length = 4)
    names(ret) <- c("queen", "virgin_queens", "workers", "drones")
    ret$queen         <- getCsdHaplo(x = x@queen,         csd = csd) # TODO: would be nice to call getCsdHaplo(x = getQueen(x),        csd = csd)
    ret$virgin_queens <- getCsdHaplo(x = x@virgin_queens, csd = csd) # TODO: would be nice to call getCsdHaplo(x = getVirginQueens(x), csd = csd)
    ret$workers       <- getCsdHaplo(x = x@workers,       csd = csd)
    ret$drones        <- getCsdHaplo(x = x@drones,        csd = csd)
  } else if ("Colonies" %in% class(x)) {
    ret <- lapply(X = x, FUN = getCsdHaplo, csd = csd)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCsdGeno
#' @title Get the Genotypes of the csd locus
#' @usage \method{getCsdGeno}(x, csd)
#'
#'@param x Undefined argument. Can be a "Colony" class or "Colonies" class
#'@param csd complementary sex determination locus- list with nodes chr, start and stop
#'
#' @description
#'
#' @examples
#'
#' @return
#'
#' @export
getCsdGeno <- function(x, csd = NULL) {
  if (is.null(csd) | !is.list(csd)) {
    stop("Argument csd must be given and has to be a list with nodes chr, start, and stop!")
  }
  if ("Pop" %in% class(x)) {
    ret <- pullSegSiteGeno(pop = pop, chr = csd$chr)[, csd$start:csd$stop]
  } else if ("Colony" %in% class(x)) {
    ret <- vector(mode = "list", length = 4)
    names(ret) <- c("queen", "virgin_queens", "workers", "drones")
    ret$queen         <- getCsdGeno(x = x@queen,         csd = csd) # TODO: would be nice to call getCsdGeno(x = getQueen(x),        csd = csd)
    ret$virgin_queens <- getCsdGeno(x = x@virgin_queens, csd = csd) # TODO: would be nice to call getCsdGeno(x = getVirginQueens(x), csd = csd)
    ret$workers       <- getCsdGeno(x = x@workers,       csd = csd)
    ret$drones        <- getCsdGeno(x = x@drones,        csd = csd)
  } else if ("Colonies" %in% class(x)) {
    ret <- lapply(X = x, FUN = getCsdGeno, csd = csd)
    names(ret) <- c("colonies")
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}
