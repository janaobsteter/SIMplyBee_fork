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

#' @rdname getQueensAge
#' @title Get (calculate) the queen's age
#'
#' @description Get (calculate) the queen's age
#'
#' @param x Pop, Colony, or Colonies
#' @param currentYear integer, current year
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
#' queen <- getQueen(colony1)
#' queen <- setQueensYOB(queen, year = 2021)
#' getQueensAge(queen, currentYear = 2022)
#'
#' colony1 <- setQueensYOB(colony1, year = 2021)
#' getQueensAge(colony1, currentYear = 2022)
#'
#' apiary <- setQueensYOB(apiary, year = 2021)
#' getQueensAge(apiary, currentYear = 2022)
#'
#' @return numeric, the age of the queen when \code{x} is colony or queens when
#' \code{x} is Colonies, \code{NA} if queen of year of birth not present, named
#' by colony id when \code{x} is Colonies
#'
#' @export
getQueensAge <- function(x, currentYear) {
  if ("Pop" %in% class(x)) {
    if (is.null(x@misc$yearOfBirth)) {
      ret <- NA
    } else {
      ret <- currentYear - x@misc$yearOfBirth
    }
  } else if ("Colony" %in% class(x)) {
    if (is.null(x@queen)) {
      ret <- NA
    } else {
      ret <- currentYear - x@queen@misc$yearOfBirth
    }
  } else if ("Colonies" %in% class(x)) {
    ret <- sapply(X = x@colonies, FUN = getQueensAge, currentYear = currentYear)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getId
#' @title Get the colony ID
#'
#' @description Get the colony ID. This is by definition the ID of the queen.
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
#' getId(getQueen(colony1))
#' getId(colony1)
#' getId(colony2)
#' getId(apiary)
#'
#' colony2 <- removeQueen(colony2)
#' getId(colony2)
#'
#' @return character, \code{NA} when queen not present
#'
#' @export
getId <- function(x) {
  if (is.null(x)) {
    id <- NA
  } else if ("Pop" %in% class(x)) {
    id <- x@id
  } else if ("Colony" %in% class(x)) {
    id <- ifelse(is.null(x@id), NA, x@id)
  } else if ("Colonies" %in% class(x)) {
    # Could have called Colony method for every colony of x, but the code below will be faster
    id <- sapply(x@colonies, FUN = function(z) ifelse(is.null(z@id), NA, z@id))
  } else {
    stop("Argument x must be a NULL, Pop, Colony, or Colonies class object!")
  }
  return(id)
}

#' @rdname getLocation
#' @title Get the colony location
#'
#' @description Get the colony location as (x, y) coordinates.
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
#' @return numeric with two values when \code{x} is Colony and a list of numeric
#' with two values when \code{x} is Colonies (list named after colonies);
#' \code{c(NA, NA)} when location not set
#'
#' @export
getLocation <- function(x) {
  if ("Colony" %in% class(x)) {
    if(is.null(x@location)) {
      ret <- c(NA, NA)
    } else {
      ret <- x@location
    }
  } else if ("Colonies" %in% class(x)) {
    ret <- lapply(x@colonies, FUN = getLocation)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
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
#' @title Get haplotypes from the csd locus WORK in PROGRESS
#'
#' @description Get haplotypes from the csd locus. The csd locus is the
#' complementary sex determining locus in honeybees. Heterozygous individuals
#' become workers or queens, while homozygous individuals become unviable
#' "drones". Hence knowledge of haplotypes at the locus is critical for honeybee
#' simulations. The csd locus spans a number of non-recombining loci as defined
#' in SimParamBee and this function gives haplotypes at these loci.
#'
#' @param x Pop, Colony, or Colonies
#' @param csd list with nodes chr, start, and stop
#'
#' @examples
#' TODO
#'
#' @return TODO
#'
#' @export
getCsdHaplo <- function(x, csd = NULL) {
  # TODO: make use of SimParamBee once we have it
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
#' @title Get genotypes from the csd locus WORK in PROGRESS
#'
#' @description Get genotypes from the csd locus. The csd locus is the
#' complementary sex determining locus in honeybees. Heterozygous individuals
#' become workers or queens, while homozygous individuals become unviable
#' "drones". Hence knowledge of haplotypes at the locus is critical for honeybee
#' simulations. The csd locus spans a number of non-recombining loci as defined
#' in SimParamBee and this function gives genotypes at these loci.
#'
#' @param x Pop, Colony, or Colonies
#' @param csd list with nodes chr, start, and stop
#'
#' @details
#'
#' @examples
#' TODO
#'
#' @return TODO
#'
#' @export
getCsdGeno <- function(x, csd = NULL) {
  # TODO: make use of SimParamBee once we have it
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

#' @rdname getIbdHaplo
#' @title Access IBD haplotype data
#'
#' @description Access IBD (identity by descent) haplotype data
#'
#' @param pop Pop
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{pullIbdHaplo}}
#'
#' @return Matrix of haplotypes
#'
#' @export
getIbdHaplo <- function(pop, chr = NULL, simParam = NULL) {
  ret <- pullIbdHaplo(pop = pop, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getQtlHaplo
#' @title Access QTL haplotype data
#'
#' @description Access QTL haplotype data
#'
#' @param pop Pop
#' @param trait numeric, indicates which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{pullQtlHaplo}}
#'
#' @return Matrix of haplotypes
#'
#' @export
getQtlHaplo <- function(pop, trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- pullQtlHaplo(pop = pop, trait = trait, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getQtlGeno
#' @title Access QTL genotype data
#'
#' @description Access QTL genotype data
#'
#' @param pop Pop
#' @param trait numeric, indicates which trait's QTL genotype to retrieve
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{pullQtlGeno}}
#'
#' @return Matrix of genotypes
#'
#' @export
getQtlGeno <- function(pop, trait = 1, chr = NULL, simParam = NULL) {
  ret <- pullQtlGeno(pop = pop, trait = trait, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getSegSiteHaplo
#' @title Access haplotype data for all segregating sites
#'
#' @description Access haplotype data for all segregating sites
#'
#' @param pop Pop
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{pullSegSiteHaplo}}
#'
#' @return Matrix of haplotypes
#'
#' @export
getSegSiteHaplo <- function(pop, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- pullSegSiteHaplo(pop = pop, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getSegSiteGeno
#' @title Access genotype data for all segregating sites
#'
#' @description Access genotype data for all segregating sites
#'
#' @param pop Pop
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{pullSegSiteHaplo}}
#'
#' @return Matrix of genotypes
#'
#' @export
getSegSiteGeno <- function(pop, chr = NULL, simParam = NULL) {
  ret <- pullSegSiteGeno(pop = pop, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getSnpHaplo
#' @title Access SNP array haplotype data
#'
#' @description Access SNP array haplotype data
#'
#' @param pop Pop
#' @param snpChip numeric, indicates which SNP array haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{pullSnpHaplo}}
#'
#' @return Matrix of haplotypes
#'
#' @export
getSnpHaplo <- function(pop, snpChip = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- pullSnpHaplo(pop = pop, snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getSnpGeno
#' @title Access SNP array genotype data
#'
#' @description Access SNP array genotype data
#'
#' @param pop Pop
#' @param snpChip numeric, indicates which SNP array genotype to retrieve
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{pullSnpHaplo}}
#'
#' @return Matrix of genotypes
#'
#' @export
#' @export
getSnpGeno <- function(pop, snpChip = 1, chr = NULL, simParam = NULL) {
  ret <- pullSnpGeno(pop = pop, snpChip = snpChip, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getCasteIbdHaplo
#' @title Access IBD haplotype data of individuals in a caste
#'
#' @description Access IBD (identity by descent) haplotype data of individuals in a caste.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{getIbdHaplo}} and \code{\link{pullIbdHaplo}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getCasteIbdHaplo(colony1, caste = "queen")
#' getQueensIbdHaplo(colony1)
#'
#' getCasteIbdHaplo(colony1, caste = "fathers")
#' getCasteIbdHaplo(colony1, caste = "fathers", nInd = 2)
#' getCasteIbdHaplo(colony1, caste = "fathers", nInd = 2)
#' getFathersIbdHaplo(colony1)
#' getFathersIbdHaplo(colony1, nInd = 2)
#'
#' getCasteIbdHaplo(colony1, caste = "virgin_queens")
#' getVirginQueensIbdHaplo(colony1)
#'
#' getCasteIbdHaplo(colony1, caste = "workers")
#' getWorkersIbdHaplo(colony1)
#'
#' getCasteIbdHaplo(colony1, caste = "drones")
#' getDronesIbdHaplo(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteIbdHaplo(apiary, caste = "queen")
#' getQueensIbdHaplo(apiary)
#'
#' getCasteIbdHaplo(apiary, caste = "fathers")
#' getCasteIbdHaplo(apiary, caste = "fathers", nInd = 2)
#' getCasteIbdHaplo(apiary, caste = "fathers", nInd = 2)
#' getFathersIbdHaplo(apiary)
#' getFathersIbdHaplo(apiary, nInd = 2)
#'
#' getCasteIbdHaplo(apiary, caste = "virgin_queens")
#' getVirginQueensIbdHaplo(apiary)
#'
#' getCasteIbdHaplo(apiary, caste = "workers")
#' getWorkersIbdHaplo(apiary)
#'
#' getCasteIbdHaplo(apiary, caste = "drones")
#' getDronesIbdHaplo(apiary)
#'
#' @return matrix with haplotypes when \code{x} is Colony and list of matrices
#' with haplotypes when \code{x} is Colonies, named by colony id when \code{x}
#' is Colonies
#'
#' @export
getCasteIbdHaplo <- function(x, caste, nInd = NULL,
                             chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    ret <- getIbdHaplo(pop = tmp, chr = chr, simParam = simParam)
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getCasteIbdHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                        chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of the queen
#' @export
getQueensIbdHaplo <- function(x, chr = NULL, simParam = NULL) {
  ret <- getCasteIbdHaplo(x, caste = "queen",
                          chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of fathers
#' @export
getFathersIbdHaplo <- function(x, nInd = NULL,
                               chr = NULL, simParam = NULL) {
  ret <- getCasteIbdHaplo(x, caste = "fathers", nInd = nInd,
                          chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of virgin queens
#' @export
getVirginQueensIbdHaplo <- function(x, nInd = NULL,
                                    chr = NULL, simParam = NULL) {
  ret <- getCasteIbdHaplo(x, caste = "virgin_queens", nInd = nInd,
                          chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of workers
#' @export
getWorkersIbdHaplo <- function(x, nInd = NULL,
                               chr = NULL, simParam = NULL) {
  ret <- getCasteIbdHaplo(x, caste = "workers", nInd = nInd,
                          chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of drones
#' @export
getDronesIbdHaplo <- function(x, nInd = NULL,
                              chr = NULL, simParam = NULL) {
  ret <- getCasteIbdHaplo(x, caste = "drones", nInd = nInd,
                          chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getColonyIbdHaplo
#' @title Access IBD haplotype data of individuals in colony
#'
#' @description Access IBD (identity by descent) haplotype data of individuals
#' in colony.
#'
#' @param x Colony or Colonies
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#' "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample; can be a list to access different
#' number of different caste - when this is the case \code{nInd} takes precedence over
#' \code{caste} (see examples)
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @details
#'
#' @seealso \code{\link{getCasteIbdHaplo}} and \code{\link{getIbdHaplo}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getColonyIbdHaplo(colony1)
#' getColonyIbdHaplo(colony1, caste = c("queen", "fathers"))
#' getColonyIbdHaplo(colony1, nInd = 1)
#' getColonyIbdHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' apiary <- c(colony1, colony2)
#' getColonyIbdHaplo(apiary)
#' getColonyIbdHaplo(apiary, caste = c("queen", "fathers"))
#' getColonyIbdHaplo(apiary, nInd = 1)
#' getColonyIbdHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @return list of matrices with haplotypes when \code{x} is Colony (list nodes
#' named by caste) and list of a list of matrices with haplotypes when \code{x}
#' is Colonies, outer list is named by colony id when \code{x} is Colonies
#'
#' @export
getColonyIbdHaplo <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"), nInd = NULL,
                              chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in 1:length(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    if ("queen" %in% caste) {
      ret$queen <- getQueensIbdHaplo(x = x,
                                     chr = chr, simParam = simParam)
    }
    if ("fathers" %in% caste) {
      ret$fathers <- getFathersIbdHaplo(x = x, nInd = nInd$fathers,
                                        chr = chr, simParam = simParam)
    }
    if ("virgin_queens" %in% caste) {
      ret$virgin_queens <- getVirginQueensIbdHaplo(x = x, nInd = nInd$virgin_queens,
                                                   chr = chr, simParam = simParam)
    }
    if ("workers" %in% caste) {
      ret$workers <- getWorkersIbdHaplo(x = x, nInd = nInd$workers,
                                        chr = chr, simParam = simParam)
    }
    if ("drones" %in% caste) {
      ret$drones <- getDronesIbdHaplo(x = x, nInd = nInd$drones,
                                      chr = chr, simParam = simParam)
    }
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getColonyIbdHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                         chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteQtlHaplo
#' @title Access QTL haplotype data of individuals in a caste
#'
#' @description Access QTL haplotype data of individuals in a caste.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#' @param trait numeric, indicates which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{getQtlHaplo}} and \code{\link{pullQtlHaplo}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getCasteQtlHaplo(colony1, caste = "queen")
#' getQueensQtlHaplo(colony1)
#'
#' getCasteQtlHaplo(colony1, caste = "fathers")
#' getCasteQtlHaplo(colony1, caste = "fathers", nInd = 2)
#' getCasteQtlHaplo(colony1, caste = "fathers", nInd = 2)
#' getFathersQtlHaplo(colony1)
#' getFathersQtlHaplo(colony1, nInd = 2)
#'
#' getCasteQtlHaplo(colony1, caste = "virgin_queens")
#' getVirginQueensQtlHaplo(colony1)
#'
#' getCasteQtlHaplo(colony1, caste = "workers")
#' getWorkersQtlHaplo(colony1)
#'
#' getCasteQtlHaplo(colony1, caste = "drones")
#' getDronesQtlHaplo(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteQtlHaplo(apiary, caste = "queen")
#' getQueensQtlHaplo(apiary)
#'
#' getCasteQtlHaplo(apiary, caste = "fathers")
#' getCasteQtlHaplo(apiary, caste = "fathers", nInd = 2)
#' getCasteQtlHaplo(apiary, caste = "fathers", nInd = 2)
#' getFathersQtlHaplo(apiary)
#' getFathersQtlHaplo(apiary, nInd = 2)
#'
#' getCasteQtlHaplo(apiary, caste = "virgin_queens")
#' getVirginQueensQtlHaplo(apiary)
#'
#' getCasteQtlHaplo(apiary, caste = "workers")
#' getWorkersQtlHaplo(apiary)
#'
#' getCasteQtlHaplo(apiary, caste = "drones")
#' getDronesQtlHaplo(apiary)
#'
#' @return matrix with haplotypes when \code{x} is Colony and list of matrices
#' with haplotypes when \code{x} is Colonies, named by colony id when \code{x}
#' is Colonies
#'
#' @export
getCasteQtlHaplo <- function(x, caste, nInd = NULL,
                             trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    ret <- getQtlHaplo(pop = tmp, haplo = haplo, trait = trait, chr = chr, simParam = simParam)
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getCasteQtlHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                        trait = trait, haplo = haplo, chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of the queen
#' @export
getQueensQtlHaplo <- function(x,
                              trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteQtlHaplo(x, caste = "queen",
                          trait = trait, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of fathers
#' @export
getFathersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteQtlHaplo(x, caste = "fathers", nInd = nInd,
                          trait = trait, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of virgin queens
#' @export
getVirginQueensQtlHaplo <- function(x, nInd = NULL,
                                    trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteQtlHaplo(x, caste = "virgin_queens", nInd = nInd,
                          trait = trait, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype of workers
#' @export
getWorkersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteQtlHaplo(x, caste = "workers", nInd = nInd,
                          trait = trait, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of drones
#' @export
getDronesQtlHaplo <- function(x, nInd = NULL,
                              trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteQtlHaplo(x, caste = "drones", nInd = nInd,
                          trait = trait, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getColonyQtlHaplo
#' @title Access QTL haplotype data of individuals in colony
#'
#' @description Access QTL haplotype data of individuals in colony.
#'
#' @param x Colony or Colonies
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#' "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample; can be a list to access different
#' number of different caste - when this is the case \code{nInd} takes precedence over
#' \code{caste} (see examples)
#' @param trait numeric, indicates which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @details
#'
#' @seealso \code{\link{getCasteQtlHaplo}} and \code{\link{getQtlHaplo}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getColonyQtlHaplo(colony1)
#' getColonyQtlHaplo(colony1, caste = c("queen", "fathers"))
#' getColonyQtlHaplo(colony1, nInd = 1)
#' getColonyQtlHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' apiary <- c(colony1, colony2)
#' getColonyQtlHaplo(apiary)
#' getColonyQtlHaplo(apiary, caste = c("queen", "fathers"))
#' getColonyQtlHaplo(apiary, nInd = 1)
#' getColonyQtlHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @return list of matrices with haplotypes when \code{x} is Colony (list nodes
#' named by caste) and list of a list of matrices with haplotypes when \code{x}
#' is Colonies, outer list is named by colony id when \code{x} is Colonies
#'
#' @export
getColonyQtlHaplo <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"), nInd = NULL,
                              trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in 1:length(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    if ("queen" %in% caste) {
      ret$queen <- getQueensQtlHaplo(x = x,
                                     trait = trait, haplo = haplo, chr = chr, simParam = simParam)
    }
    if ("fathers" %in% caste) {
      ret$fathers <- getFathersQtlHaplo(x = x, nInd = nInd$fathers,
                                        trait = trait, haplo = haplo, chr = chr, simParam = simParam)
    }
    if ("virgin_queens" %in% caste) {
      ret$virgin_queens <- getVirginQueensQtlHaplo(x = x, nInd = nInd$virgin_queens,
                                                   trait = trait, haplo = haplo, chr = chr, simParam = simParam)
    }
    if ("workers" %in% caste) {
      ret$workers <- getWorkersQtlHaplo(x = x, nInd = nInd$workers,
                                        trait = trait, haplo = haplo, chr = chr, simParam = simParam)
    }
    if ("drones" %in% caste) {
      ret$drones <- getDronesQtlHaplo(x = x, nInd = nInd$drones,
                                      trait = trait, haplo = haplo, chr = chr, simParam = simParam)
    }
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getColonyQtlHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                         trait = trait, haplo = haplo, chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteQtlGeno
#' @title Access QTL genotype data of individuals in a caste
#'
#' @description Access QTL genotype data of individuals in a caste.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which trait's QTL genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{getQtlGeno}} and \code{\link{pullQtlGeno}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getCasteQtlGeno(colony1, caste = "queen")
#' getQueensQtlGeno(colony1)
#'
#' getCasteQtlGeno(colony1, caste = "fathers")
#' getCasteQtlGeno(colony1, caste = "fathers", nInd = 2)
#' getCasteQtlGeno(colony1, caste = "fathers", nInd = 2)
#' getFathersQtlGeno(colony1)
#' getFathersQtlGeno(colony1, nInd = 2)
#'
#' getCasteQtlGeno(colony1, caste = "virgin_queens")
#' getVirginQueensQtlGeno(colony1)
#'
#' getCasteQtlGeno(colony1, caste = "workers")
#' getWorkersQtlGeno(colony1)
#'
#' getCasteQtlGeno(colony1, caste = "drones")
#' getDronesQtlGeno(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteQtlGeno(apiary, caste = "queen")
#' getQueensQtlGeno(apiary)
#'
#' getCasteQtlGeno(apiary, caste = "fathers")
#' getCasteQtlGeno(apiary, caste = "fathers", nInd = 2)
#' getCasteQtlGeno(apiary, caste = "fathers", nInd = 2)
#' getFathersQtlGeno(apiary)
#' getFathersQtlGeno(apiary, nInd = 2)
#'
#' getCasteQtlGeno(apiary, caste = "virgin_queens")
#' getVirginQueensQtlGeno(apiary)
#'
#' getCasteQtlGeno(apiary, caste = "workers")
#' getWorkersQtlGeno(apiary)
#'
#' getCasteQtlGeno(apiary, caste = "drones")
#' getDronesQtlGeno(apiary)
#'
#' @return matrix with genotypes when \code{x} is Colony and list of matrices
#' with genotypes when \code{x} is Colonies, named by colony id when \code{x}
#' is Colonies
#'
#' @export
getCasteQtlGeno <- function(x, caste, nInd = NULL,
                            trait = 1, chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    ret <- getQtlGeno(pop = tmp, trait = trait, chr = chr, simParam = simParam)
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getCasteQtlGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                       trait = trait, chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of the queen
#' @export
getQueensQtlGeno <- function(x,
                             trait = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteQtlGeno(x, caste = "queen",
                         trait = trait, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of fathers
#' @export
getFathersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteQtlGeno(x, caste = "fathers", nInd = nInd,
                         trait = trait, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of virgin queens
#' @export
getVirginQueensQtlGeno <- function(x, nInd = NULL,
                                   trait = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteQtlGeno(x, caste = "virgin_queens", nInd = nInd,
                         trait = trait, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of workers
#' @export
getWorkersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteQtlGeno(x, caste = "workers", nInd = nInd,
                         trait = trait, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of drones
#' @export
getDronesQtlGeno <- function(x, nInd = NULL,
                             trait = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteQtlGeno(x, caste = "drones", nInd = nInd,
                         trait = trait, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getColonyQtlGeno
#' @title Access QTL genotype data of individuals in colony
#'
#' @description Access QTL genotype data of individuals in colony.
#'
#' @param x Colony or Colonies
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#' "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample; can be a list to access different
#' number of different caste - when this is the case \code{nInd} takes precedence over
#' \code{caste} (see examples)
#' @param trait numeric, indicates which trait's QTL genotype to retrieve
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @details
#'
#' @seealso \code{\link{getCasteQtlGeno}} and \code{\link{getQtlGeno}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getColonyQtlGeno(colony1)
#' getColonyQtlGeno(colony1, caste = c("queen", "fathers"))
#' getColonyQtlGeno(colony1, nInd = 1)
#' getColonyQtlGeno(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' apiary <- c(colony1, colony2)
#' getColonyQtlGeno(apiary)
#' getColonyQtlGeno(apiary, caste = c("queen", "fathers"))
#' getColonyQtlGeno(apiary, nInd = 1)
#' getColonyQtlGeno(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @return list of matrices with haplotypes when \code{x} is Colony (list nodes
#' named by caste) and list of a list of matrices with haplotypes when \code{x}
#' is Colonies, outer list is named by colony id when \code{x} is Colonies
#'
#' @export
getColonyQtlGeno <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"), nInd = NULL,
                             trait = 1, chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in 1:length(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    if ("queen" %in% caste) {
      ret$queen <- getQueensQtlGeno(x = x,
                                    trait = trait, chr = chr, simParam = simParam)
    }
    if ("fathers" %in% caste) {
      ret$fathers <- getFathersQtlGeno(x = x, nInd = nInd$fathers,
                                       trait = trait, chr = chr, simParam = simParam)
    }
    if ("virgin_queens" %in% caste) {
      ret$virgin_queens <- getVirginQueensQtlGeno(x = x, nInd = nInd$virgin_queens,
                                                  trait = trait, chr = chr, simParam = simParam)
    }
    if ("workers" %in% caste) {
      ret$workers <- getWorkersQtlGeno(x = x, nInd = nInd$workers,
                                       trait = trait, chr = chr, simParam = simParam)
    }
    if ("drones" %in% caste) {
      ret$drones <- getDronesQtlGeno(x = x, nInd = nInd$drones,
                                     trait = trait, chr = chr, simParam = simParam)
    }
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getColonyQtlGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                        trait = trait, chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteSegSiteHaplo
#' @title Access haplotype data for all segregating sites of individuals in a
#' caste
#'
#' @description Access haplotype data for all segregating sites of individuals
#' in a caste.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{getSegSiteHaplo}} and \code{\link{pullSegSiteHaplo}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getCasteSegSiteHaplo(colony1, caste = "queen")
#' getQueensSegSiteHaplo(colony1)
#'
#' getCasteSegSiteHaplo(colony1, caste = "fathers")
#' getCasteSegSiteHaplo(colony1, caste = "fathers", nInd = 2)
#' getCasteSegSiteHaplo(colony1, caste = "fathers", nInd = 2)
#' getFathersSegSiteHaplo(colony1)
#' getFathersSegSiteHaplo(colony1, nInd = 2)
#'
#' getCasteSegSiteHaplo(colony1, caste = "virgin_queens")
#' getVirginQueensSegSiteHaplo(colony1)
#'
#' getCasteSegSiteHaplo(colony1, caste = "workers")
#' getWorkersSegSiteHaplo(colony1)
#'
#' getCasteSegSiteHaplo(colony1, caste = "drones")
#' getDronesSegSiteHaplo(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteSegSiteHaplo(apiary, caste = "queen")
#' getQueensSegSiteHaplo(apiary)
#'
#' getCasteSegSiteHaplo(apiary, caste = "fathers")
#' getCasteSegSiteHaplo(apiary, caste = "fathers", nInd = 2)
#' getCasteSegSiteHaplo(apiary, caste = "fathers", nInd = 2)
#' getFathersSegSiteHaplo(apiary)
#' getFathersSegSiteHaplo(apiary, nInd = 2)
#'
#' getCasteSegSiteHaplo(apiary, caste = "virgin_queens")
#' getVirginQueensSegSiteHaplo(apiary)
#'
#' getCasteSegSiteHaplo(apiary, caste = "workers")
#' getWorkersSegSiteHaplo(apiary)
#'
#' getCasteSegSiteHaplo(apiary, caste = "drones")
#' getDronesSegSiteHaplo(apiary)
#'
#' @return matrix with haplotypes when \code{x} is Colony and list of matrices
#' with haplotypes when \code{x} is Colonies, named by colony id when \code{x}
#' is Colonies
#'
#' @export
getCasteSegSiteHaplo <- function(x, caste, nInd = NULL,
                                 haplo = "all", chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    ret <- getSegSiteHaplo(pop = tmp, haplo = haplo, chr = chr, simParam = simParam)
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getCasteSegSiteHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                            haplo = haplo, chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of the queen
#' @export
getQueensSegSiteHaplo <- function(x,
                                  haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteHaplo(x, caste = "queen",
                              haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of fathers
#' @export
getFathersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteHaplo(x, caste = "fathers", nInd = nInd,
                              haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of virgin queens
#' @export
getVirginQueensSegSiteHaplo <- function(x, nInd = NULL,
                                        haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteHaplo(x, caste = "virgin_queens", nInd = nInd,
                              haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of workers
#' @export
getWorkersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteHaplo(x, caste = "workers", nInd = nInd,
                              haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of drones
#' @export
getDronesSegSiteHaplo <- function(x, nInd = NULL,
                                  haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteHaplo(x, caste = "drones", nInd = nInd,
                              haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getCasteSegSiteGeno
#' @title Access genotype data for all segregating sites of individuals in a
#' caste
#'
#' @description Access genotype data for all segregating sites of individuals in
#' a caste.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{getSegSiteGeno}} and \code{\link{pullSegSiteGeno}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getCasteSegSiteGeno(colony1, caste = "queen")
#' getQueensSegSiteGeno(colony1)
#'
#' getCasteSegSiteGeno(colony1, caste = "fathers")
#' getCasteSegSiteGeno(colony1, caste = "fathers", nInd = 2)
#' getCasteSegSiteGeno(colony1, caste = "fathers", nInd = 2)
#' getFathersSegSiteGeno(colony1)
#' getFathersSegSiteGeno(colony1, nInd = 2)
#'
#' getCasteSegSiteGeno(colony1, caste = "virgin_queens")
#' getVirginQueensSegSiteGeno(colony1)
#'
#' getCasteSegSiteGeno(colony1, caste = "workers")
#' getWorkersSegSiteGeno(colony1)
#'
#' getCasteSegSiteGeno(colony1, caste = "drones")
#' getDronesSegSiteGeno(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteSegSiteGeno(apiary, caste = "queen")
#' getQueensSegSiteGeno(apiary)
#'
#' getCasteSegSiteGeno(apiary, caste = "fathers")
#' getCasteSegSiteGeno(apiary, caste = "fathers", nInd = 2)
#' getCasteSegSiteGeno(apiary, caste = "fathers", nInd = 2)
#' getFathersSegSiteGeno(apiary)
#' getFathersSegSiteGeno(apiary, nInd = 2)
#'
#' getCasteSegSiteGeno(apiary, caste = "virgin_queens")
#' getVirginQueensSegSiteGeno(apiary)
#'
#' getCasteSegSiteGeno(apiary, caste = "workers")
#' getWorkersSegSiteGeno(apiary)
#'
#' getCasteSegSiteGeno(apiary, caste = "drones")
#' getDronesSegSiteGeno(apiary)
#'
#' @return matrix with genotypes when \code{x} is Colony and list of matrices
#' with genotypes when \code{x} is Colonies, named by colony id when \code{x}
#' is Colonies
#'
#' @export
getCasteSegSiteGeno <- function(x, caste, nInd = NULL,
                                chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    ret <- getSegSiteGeno(pop = tmp, chr = chr, simParam = simParam)
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getCasteSegSiteGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                           chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of the queen
#' @export
getQueensSegSiteGeno <- function(x,
                                 chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteGeno(x, caste = "queen",
                             chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of fathers
#' @export
getFathersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteGeno(x, caste = "fathers", nInd = nInd,
                             chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of virgin queens
#' @export
getVirginQueensSegSiteGeno <- function(x, nInd = NULL,
                                       chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteGeno(x, caste = "virgin_queens", nInd = nInd,
                             chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of workers
#' @export
getWorkersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteGeno(x, caste = "workers", nInd = nInd,
                             chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of drones
#' @export
getDronesSegSiteGeno <- function(x, nInd = NULL,
                                 chr = NULL, simParam = NULL) {
  ret <- getCasteSegSiteGeno(x, caste = "drones", nInd = nInd,
                             chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getCasteSnpHaplo
#' @title Access SNP array haplotype data of individuals in a caste
#'
#' @description Access SNP array haplotype data of individuals in a caste.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{getSnpHaplo}} and \code{\link{pullSnpHaplo}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getCasteSnpHaplo(colony1, caste = "queen")
#' getQueensSnpHaplo(colony1)
#'
#' getCasteSnpHaplo(colony1, caste = "fathers")
#' getCasteSnpHaplo(colony1, caste = "fathers", nInd = 2)
#' getCasteSnpHaplo(colony1, caste = "fathers", nInd = 2)
#' getFathersSnpHaplo(colony1)
#' getFathersSnpHaplo(colony1, nInd = 2)
#'
#' getCasteSnpHaplo(colony1, caste = "virgin_queens")
#' getVirginQueensSnpHaplo(colony1)
#'
#' getCasteSnpHaplo(colony1, caste = "workers")
#' getWorkersSnpHaplo(colony1)
#'
#' getCasteSnpHaplo(colony1, caste = "drones")
#' getDronesSnpHaplo(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteSnpHaplo(apiary, caste = "queen")
#' getQueensSnpHaplo(apiary)
#'
#' getCasteSnpHaplo(apiary, caste = "fathers")
#' getCasteSnpHaplo(apiary, caste = "fathers", nInd = 2)
#' getCasteSnpHaplo(apiary, caste = "fathers", nInd = 2)
#' getFathersSnpHaplo(apiary)
#' getFathersSnpHaplo(apiary, nInd = 2)
#'
#' getCasteSnpHaplo(apiary, caste = "virgin_queens")
#' getVirginQueensSnpHaplo(apiary)
#'
#' getCasteSnpHaplo(apiary, caste = "workers")
#' getWorkersSnpHaplo(apiary)
#'
#' getCasteSnpHaplo(apiary, caste = "drones")
#' getDronesSnpHaplo(apiary)
#'
#' @return matrix with haplotypes when \code{x} is Colony and list of matrices
#' with haplotypes when \code{x} is Colonies, named by colony id when \code{x}
#' is Colonies
#'
#' @export
getCasteSnpHaplo <- function(x, caste, nInd = NULL,
                             snpChip = 1, haplo = "all", chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    ret <- getSnpHaplo(pop = tmp, haplo = haplo, snpChip = snpChip, chr = chr, simParam = simParam)
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getCasteSnpHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                        snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of the queen
#' @export
getQueensSnpHaplo <- function(x,
                              snpChip = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSnpHaplo(x, caste = "queen",
                          snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of fathers
#' @export
getFathersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSnpHaplo(x, caste = "fathers", nInd = nInd,
                          snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of virgin queens
#' @export
getVirginQueensSnpHaplo <- function(x, nInd = NULL,
                                    snpChip = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSnpHaplo(x, caste = "virgin_queens", nInd = nInd,
                          snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype of workers
#' @export
getWorkersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSnpHaplo(x, caste = "workers", nInd = nInd,
                          snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of drones
#' @export
getDronesSnpHaplo <- function(x, nInd = NULL,
                              snpChip = 1, haplo = "all", chr = NULL, simParam = NULL) {
  ret <- getCasteSnpHaplo(x, caste = "drones", nInd = nInd,
                          snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParam)
  return(ret)
}

#' @rdname getCasteSnpGeno
#' @title Access SNP array genotype data of individuals in a caste
#'
#' @description Access SNP array genotype data of individuals in a caste.
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if NULL all individuals
#' are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if NULL, all chromosome are retrieved
#' @param simParam SimParam
#'
#' @seealso \code{\link{getSnpGeno}} and \code{\link{pullSnpGeno}}
#'
#' @examples
#' # AlphaSimR
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' # Honeybee
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getCasteSnpGeno(colony1, caste = "queen")
#' getQueensSnpGeno(colony1)
#'
#' getCasteSnpGeno(colony1, caste = "fathers")
#' getCasteSnpGeno(colony1, caste = "fathers", nInd = 2)
#' getCasteSnpGeno(colony1, caste = "fathers", nInd = 2)
#' getFathersSnpGeno(colony1)
#' getFathersSnpGeno(colony1, nInd = 2)
#'
#' getCasteSnpGeno(colony1, caste = "virgin_queens")
#' getVirginQueensSnpGeno(colony1)
#'
#' getCasteSnpGeno(colony1, caste = "workers")
#' getWorkersSnpGeno(colony1)
#'
#' getCasteSnpGeno(colony1, caste = "drones")
#' getDronesSnpGeno(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteSnpGeno(apiary, caste = "queen")
#' getQueensSnpGeno(apiary)
#'
#' getCasteSnpGeno(apiary, caste = "fathers")
#' getCasteSnpGeno(apiary, caste = "fathers", nInd = 2)
#' getCasteSnpGeno(apiary, caste = "fathers", nInd = 2)
#' getFathersSnpGeno(apiary)
#' getFathersSnpGeno(apiary, nInd = 2)
#'
#' getCasteSnpGeno(apiary, caste = "virgin_queens")
#' getVirginQueensSnpGeno(apiary)
#'
#' getCasteSnpGeno(apiary, caste = "workers")
#' getWorkersSnpGeno(apiary)
#'
#' getCasteSnpGeno(apiary, caste = "drones")
#' getDronesSnpGeno(apiary)
#'
#' @return matrix with genotypes when \code{x} is Colony and list of matrices
#' with genotypes when \code{x} is Colonies, named by colony id when \code{x}
#' is Colonies
#'
#' @export
getCasteSnpGeno <- function(x, caste, nInd = NULL,
                            snpChip = 1, chr = NULL, simParam = NULL) {
  if ("Colony" %in% class(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    ret <- getSnpGeno(pop = tmp, snpChip = snpChip, chr = chr, simParam = simParam)
  } else if ("Colonies" %in% class(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in 1:nCol) {
      ret[[colony]] <- getCasteSnpGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                       snpChip = snpChip, chr = chr, simParam = simParam)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of the queen
#' @export
getQueensSnpGeno <- function(x,
                             snpChip = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteSnpGeno(x, caste = "queen",
                         snpChip = snpChip, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of fathers
#' @export
getFathersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteSnpGeno(x, caste = "fathers", nInd = nInd,
                         snpChip = snpChip, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of virgin queens
#' @export
getVirginQueensSnpGeno <- function(x, nInd = NULL,
                                   snpChip = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteSnpGeno(x, caste = "virgin_queens", nInd = nInd,
                         snpChip = snpChip, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of workers
#' @export
getWorkersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteSnpGeno(x, caste = "workers", nInd = nInd,
                         snpChip = snpChip, chr = chr, simParam = simParam)
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of drones
#' @export
getDronesSnpGeno <- function(x, nInd = NULL,
                             snpChip = 1, chr = NULL, simParam = NULL) {
  ret <- getCasteSnpGeno(x, caste = "drones", nInd = nInd,
                         snpChip = snpChip, chr = chr, simParam = simParam)
  return(ret)
}

