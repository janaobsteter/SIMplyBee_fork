# Level 0 Auxiliary Functions

#' @rdname nColonies
#' @title Number of colonies
#'
#' @description Returns the number of colonies in a colonies object
#'
#' @param colonies Colonies
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[2], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[1], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[1], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' nColonies(apiary)
#' nColonies(createColonies(n = 10))
#'
#' @return integer
#'
#' @export
nColonies <- function(colonies) {
  if (!"Colonies" %in% class(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  n <- length(colonies@colonies)
  return(n)
}

#' @rdname nCaste
#' @title Number of individuals of a caste in a colony
#'
#' @description Returns the number of individuals of a caste in a colony
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 100)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 3)
#' colony2 <- addWorkers(colony2, nInd = 200)
#' nCaste(colony1, caste = "queen")
#' nCaste(colony1, caste = "fathers")
#' nCaste(colony1, caste = "virgin_queens")
#' nCaste(colony1, caste = "workers")
#' nCaste(colony1, caste = "drones")
#'
#' apiary <- c(colony1, colony2)
#' nCaste(apiary, caste = "queen")
#' nCaste(apiary, caste = "fathers")
#' nCaste(apiary, caste = "virgin_queens")
#' nCaste(apiary, caste = "workers")
#' nCaste(apiary, caste = "drones")
#'
#' @return integer, named by colony id when \code{x} is Colonies
#'
#' @export
nCaste <- function(x, caste) {
  if ("Colony" %in% class(x)) {
    if (caste == "fathers") {
      if (is.null(x@queen)) {
        ret <- 0
      } else {
        ret <- ifelse(is.null(x@queen@misc$fathers), 0, nInd(x@queen@misc$fathers))
      }
    } else {
      ret <- ifelse(is.null(slot(x, caste)), 0, nInd(slot(x, caste)))
    }
  } else if ("Colonies" %in% class(x)) {
    ret <- sapply(X = x@colonies, FUN = nCaste, caste = caste)
    names(ret) <- getId(x)
  } else {
    stop("Argument colony must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname nQueen
#' @title Number of queens in a colony
#'
#' @description Returns the number of queens in a colony (expect 0 or 1)
#'
#' @param x Colony or Colonies
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:8])
#' nQueen(colony1)
#' nQueen(colony2)
#' colony2 <- removeQueen(colony2)
#' nQueen(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nQueen(apiary)
#'
#' @return integer, named by colony id when \code{x} is Colonies
#'
#' @export
nQueen <- function(x) {
  ret <- nCaste(x, caste = "queen")
  return(ret)
}

#' @rdname nFathers
#' @title Number of fathers in a colony
#'
#' @description Returns the number of nFathers (drones the queen mated with) in a colony
#'
#' @param x Colony or Colonies
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:8])
#' nFathers(colony1)
#' nFathers(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nFathers(apiary)
#'
#' @return integer, named by colony id when \code{x} is Colonies
#'
#' @export
nFathers <- function(x) {
  ret <- nCaste(x, caste = "fathers")
  return(ret)
}

#' @rdname nVirginQueens
#' @title Number of virgin queens in a colony
#'
#' @description Returns the number of virgin queens in a colony
#'
#' @param x Colony or Colonies
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addVirginQueens(colony1, nInd = 3)
#' colony2 <- addVirginQueens(colony2, nInd = 5)
#' nVirginQueens(colony1)
#' nVirginQueens(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nVirginQueens(apiary)
#'
#' @return integer, named by colony id when \code{x} is Colonies
#'
#' @export
nVirginQueens <- function(x) {
  ret <- nCaste(x, caste = "virgin_queens")
  return(ret)
}

#' @rdname nWorkers
#' @title Number of workers in a colony
#'
#' @description Returns the number of workers in a colony
#'
#' @param x Colony or Colonies
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 100)
#' colony2 <- addWorkers(colony2, nInd = 200)
#' nWorkers(colony1)
#' nWorkers(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nWorkers(apiary)
#'
#' @return integer, named by colony id when \code{x} is Colonies
#'
#' @export
nWorkers <- function(x) {
  ret <- nCaste(x, caste = "workers")
  return(ret)
}

#' @rdname nDrones
#' @title Number of drones in a colony
#'
#' @description Returns the number of drones in a colony
#'
#' @param x Colony or Colonies
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addDrones(colony1, nInd = 100)
#' colony2 <- addDrones(colony2, nInd = 200)
#' nDrones(colony1)
#' nDrones(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nDrones(apiary)
#'
#' @return integer, named by colony id when \code{x} is Colonies
#'
#' @export
nDrones <- function(x) {
  ret <- nCaste(x, caste = "drones")
  return(ret)
}

#' @rdname isQueenMated
#' @title Is the queen mated?
#'
#' @description When queen is mated, we store drones she mated with (fathers of
#' future progeny) in the same object to have genetic information for generating
#' future progeny. This function tests if queen is mated, by testing for the
#' presence of fathers.
#'
#' @param x Pop or Colony, queen or colony that will be inspected
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' isQueenMated(getQueen(colony1))
#' isQueenMated(colony1)
#' isQueenMated(apiary)
#'
#' colony1 <- removeQueen(colony1)
#' isQueenMated(colony1)
#'
#' colony2 <- supersedeColony(colony2, fathers = drones[1:5])
#' isQueenMated(colony2)
#'
#' @return logical, named by colony id when \code{x} is Colonies
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
  } else if ("Colonies" %in% class(x)) {
    ret <- sapply(X = x@colonies, FUN = isQueenMated)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getQueensYOB
#' @title Access the queen's year of birth
#'
#' @description Access the queen's year of birth
#'
#' @param x Pop, Colony, or Colonies
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybees
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#'
#' getQueensYOB(getQueen(colony1))
#' getQueensYOB(colony1)
#' getQueensYOB(apiary)
#'
#' queen <- getQueen(colony1)
#' queen <- setQueensYOB(queen, year = 2021)
#' getQueensYOB(queen)
#'
#' colony1 <- setQueensYOB(colony1, year = 2021)
#' getQueensYOB(colony1)
#'
#' apiary <- setQueensYOB(apiary, year = 2021)
#' getQueensYOB(apiary)
#'
#' queen <- setQueensYearOfBirth(queen, year = 2022)
#' getQueensYearOfBirth(queen)
#'
#' colony1 <- setQueensYearOfBirth(colony1, year = 2022)
#' getQueensYearOfBirth(colony1)
#'
#' apiary <- setQueensYearOfBirth(apiary, year = 2022)
#' getQueensYearOfBirth(apiary)
#'
#' @return numeric, the year of birth of the queen when \code{x} is colony or
#' queens when \code{x} is Colonies, \code{NA} if queen not present, named by
#' colony id when \code{x} is Colonies
#'
#' @export
getQueensYearOfBirth <- getQueensYOB <- function(x) {
  if ("Pop" %in% class(x)) {
    ret <- ifelse(is.null(x@misc$yearOfBirth), NA, x@misc$yearOfBirth)
  } else if ("Colony" %in% class(x)) {
    ret <- ifelse(is.null(x@queen@misc$yearOfBirth), NA, x@queen@misc$yearOfBirth)
  } else if ("Colonies" %in% class(x)) {
    ret <- sapply(X = x@colonies,
                  FUN = function(z) {
                    ifelse(is.null(z@queen@misc$yearOfBirth), NA, z@queen@misc$yearOfBirth)
                  })
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname computeQueensAge
#' @title Compute the queen's age in years
#'
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
#' @return integer, namedTODO
#'
#' @export
computeQueensAge <- function(x, currentYear) {
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
#' @title Get the colony ID
#'
#' @description Get the colony ID. This is by definition the ID of the queen.
#'
#' @param x Colony or Colonies
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
#' #Create an apiary containing 10 colonies
#' Apiary1 <- createColonies(,n = 10)
#'
#' #Get colony IDs from the colonies
#' getId(Apiary1)
#'
#' @return character
#'
#' @export
getId <- function(x) {
  if ("Colony" %in% class(x)) {
    id <- x@id
  } else if ("Colonies" %in% class(x)) {
    # Could have called Colony method for every colony of x, but the code below will be faster
    id <- sapply(x@colonies, FUN = function(x) x@id)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(id)
}

#' @rdname hasSplit
#' @title Test if colony has split
#'
#' @description Test if colony has split. This will obviously impact colony strength.
#'
#' @param x Colony or Colonies
#'
#' @examples
#' TODO
#'
#' @return logical, named by colony id when \code{x} is Colonies
#'
#' @export
hasSplit <- function(x) {
  if ("Colony" %in% class(x)) {
    ret <- x@split
  } else if ("Colonies" %in% class(x)) {
    # Could have called Colony method for every colony of x, but the code below will be faster
    ret <- sapply(x@colonies, FUN = function(z) z@split)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname hasSwarmed
#' @title Test if colony has swarmed
#'
#' @description Test if colony has swarmed. This will obviously have major impact
#' on the colony and its downstream events.
#'
#' @param x Colony or Colonies
#'
#' @examples
#' TODO
#'
#' @return logical, named by colony id when \code{x} is Colonies
#'
#' @export
hasSwarmed <- function(x) {
  if ("Colony" %in% class(x)) {
    ret <- x@swarm
  } else if ("Colonies" %in% class(x)) {
    # Could have called Colony method for every colony of x, but the code below will be faster
    ret <- sapply(x@colonies, FUN = function(z) z@swarm)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname hasSuperseded
#' @title Test if colony has superseded
#'
#' @description Test if colony has superseded.
#'
#' @param x Colony or Colonies
#'
#' @examples
#' TODO
#'
#' @return logical, named by colony id when \code{x} is Colonies
#'
#' @export
hasSuperseded <- function(x) {
  if ("Colony" %in% class(x)) {
    ret <- x@supersedure
  } else if ("Colonies" %in% class(x)) {
    # Could have called Colony method for every colony of x, but the code below will be faster
    ret <- sapply(x@colonies, FUN = function(z) z@supersedure)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname isProductive
#' @title Test if colony is currently productive
#'
#' @description Test if colony is currently productive. This can be used to
#' programatically decided if colony production can be simulated.
#'
#' @param x Colony or Colonies
#'
#' @examples
#' TODO
#'
#' @return logical, named by colony id when \code{x} is Colonies
#'
#' @export
isProductive <- function(x) {
  if ("Colony" %in% class(x)) {
    ret <- x@production
  } else if ("Colonies" %in% class(x)) {
    # Could have called Colony method for every colony of x, but the code below will be faster
    ret <- sapply(x@colonies, FUN = function(z) z@production)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname simulateHoneyBeeGenomes
#' @title Simulate the Honey bee genome, including the csd locus WORK IN PROGRESS

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
#' @return TODO
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
#' @title Get the Haplotypes of the csd locus WORK in PROGRESS
#'
#' @description TODO
#'
#' @param x Undefined argument. Can be a "Colony" class or "Colonies" class
#' @param csd complementary sex determination locus- list with nodes chr, start and stop
#'
#' @examples
#' TODO
#'
#' @return TODO
#'
#' @export
getCsdHaplo <- function(x, csd = NULL) {
  if (is.null(csd) | !is.list(csd)) {
    stop("csd must be given and has to be a list with nodes chr, start, and stop")
  }
  if ("Pop" %in% class(x)) {
    ret <- pullSegSiteHaplo(pop = pop, chr = csd$chr)[, csd$start:csd$stop]
  } else if ("Colony" %in% class(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "virgin_queens", "workers", "drones")
    ret$queen         <- getCsdHaplo(x = getQueen(x),        csd = csd)
    ret$fathers       <- getCsdHaplo(x = getFathers(x),      csd = csd)
    ret$virgin_queens <- getCsdHaplo(x = getVirginQueens(x), csd = csd)
    ret$workers       <- getCsdHaplo(x = getWorkers(x),      csd = csd)
    ret$drones        <- getCsdHaplo(x = getDrones(x),       csd = csd)
  } else if ("Colonies" %in% class(x)) {
    ret <- lapply(X = x@colonies, FUN = getCsdHaplo, csd = csd)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCsdGeno
#' @title Get the Genotypes of the csd locus WORK in PROGRESS
#'
#' @description TODO
#'
#' @param x Undefined argument. Can be a "Colony" class or "Colonies" class
#' @param csd complementary sex determination locus- list with nodes chr, start and stop
#'
#'
#' @examples
#' TODO
#'
#' @return TODO
#'
#' @export
getCsdGeno <- function(x, csd = NULL) {
  if (is.null(csd) | !is.list(csd)) {
    stop("Argument csd must be given and has to be a list with nodes chr, start, and stop!")
  }
  if ("Pop" %in% class(x)) {
    ret <- pullSegSiteGeno(pop = pop, chr = csd$chr)[, csd$start:csd$stop]
  } else if ("Colony" %in% class(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "virgin_queens", "workers", "drones")
    ret$queen         <- getCsdGeno(x = getQueen(x),        csd = csd)
    ret$fathers       <- getCsdGeno(x = getFathers(x),      csd = csd)
    ret$virgin_queens <- getCsdGeno(x = getVirginQueens(x), csd = csd)
    ret$workers       <- getCsdGeno(x = getWorkers(x),      csd = csd)
    ret$drones        <- getCsdGeno(x = getDrones(x),       csd = csd)
  } else if ("Colonies" %in% class(x)) {
    ret <- lapply(X = x@colonies, FUN = getCsdGeno, csd = csd)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}
