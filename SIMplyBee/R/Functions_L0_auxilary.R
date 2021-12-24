# Level 0 Auxiliary Functions

#' @rdname nColonies
#' @title Number of colonies
#'
#' @description Level 0 function that returns the number of colonies in a
#'   colonies object.
#'
#' @param colonies \code{\link{Colonies-class}}
#'
#' @return integer
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
#' nColonies(apiary)
#' nColonies(createColonies(nCol = 10))
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
#' @title Level 0 function that returns the number of individuals of a caste in a
#'   colony
#'
#' @description Returns the number of individuals of a caste in a colony
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers",
#'   "drones", or "all"
#'
#' @seealso \code{\link{nQueens}}, \code{\link{nFathers}},
#'   \code{\link{nVirginQueens}}, \code{\link{nWorkers}}, and
#'   \code{\link{nDrones}}
#'
#' @return when \code{x} is \code{\link{Colony-class}} return is integer for
#'   \code{caste != "all"} or list for \code{caste == "all"} with nodes named
#'   by caste; when \code{x} is \code{\link{Colonies-class}} return is named
#'   integer for \code{caste != "all"} or named list of lists for
#'   \code{caste == "all"}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
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
#' nCaste(colony1, caste = "all")
#'
#' apiary <- c(colony1, colony2)
#' nCaste(apiary, caste = "queen")
#' nCaste(apiary, caste = "fathers")
#' nCaste(apiary, caste = "virgin_queens")
#' nCaste(apiary, caste = "workers")
#' nCaste(apiary, caste = "drones")
#' nCaste(apiary, caste = "all")
#'
#' @export
nCaste <- function(x, caste = "all") {
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "virgin_queens", "workers", "drones")
      for (caste in names(ret)) {
        ret[[caste]] <- nCaste(x = x, caste = caste)
      }
    } else {
      if (caste == "fathers") {
        if (isQueenPresent(x)) {
          ret <- ifelse(isQueenMated(x), nInd(x@queen@misc[[1]]$fathers), 0)
        } else {
          ret <- 0
        }
      } else {
        ret <- ifelse(is.null(slot(x, caste)), 0, nInd(slot(x, caste)))
      }
    }
  } else if (isColonies(x)) {
    fun <- ifelse(caste == "all", lapply, sapply)
    ret <- fun(X = x@colonies, FUN = nCaste, caste = caste)
    names(ret) <- getId(x)
  } else {
    stop("Argument colony must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname nQueens
#' @title Number of queens in a colony
#'
#' @description Returns the number of queens in a colony (expect 0 or 1)
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return integer, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:8])
#' nQueens(colony1)
#' nQueens(colony2)
#' colony2 <- removeQueen(colony2)
#' nQueens(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nQueens(apiary)
#'
#' @export
nQueens <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- nCaste(x, caste = "queen")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname nFathers
#' @title Number of fathers in a colony
#'
#' @description Returns the number of nFathers (drones the queen mated with) in a colony
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return integer, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:8])
#' nFathers(colony1)
#' nFathers(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nFathers(apiary)
#'
#' @export
nFathers <- function(x) {
  if (isPop(x)) {
    nInd <- nInd(x)
    ret <- rep(x = 0, times = nInd)
    for (ind in seq_len(nInd)) {
      if (isQueenMated(x[ind])) {
        ret[ind] <- nInd(x@misc[[ind]]$fathers)
      }
    }
  } else if (isColony(x) | isColonies(x)) {
    ret <- nCaste(x, caste = "fathers")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname nVirginQueens
#' @title Number of virgin queens in a colony
#'
#' @description Returns the number of virgin queens in a colony
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return integer, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
nVirginQueens <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- nCaste(x, caste = "virgin_queens")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname nWorkers
#' @title Number of workers in a colony
#'
#' @description Returns the number of workers in a colony
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return integer, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
nWorkers <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- nCaste(x, caste = "workers")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname nDrones
#' @title Number of drones in a colony
#'
#' @description Returns the number of drones in a colony
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return integer, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
nDrones <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- nCaste(x, caste = "drones")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname pHomBrood
#' @title Percentage of homozygous brood of a queen
#'
#' @description Level 0 function that returns the percentage of homozygous brood
#'   in a colony (these are non viable individuals and only their number is
#'   stored).
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#'
#' TODO: describe queen's and colony's pHomBrood
#'
#' @return numeric, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#'
#' # Virgin queen
#' pHomBrood(basePop[2])
#'
#' # Mated queen
#' pHomBrood(crossVirginQueen(pop = basePop[2], fathers = drones[1:5]))
#'
#' # Queen of the colony
#' pHomBrood(getQueen(colony1))
#'
#' # Colony
#' pHomBrood(colony1)
#'
#' # Colonies
#' apiary <- c(colony1, colony2)
#' pHomBrood(apiary)
#'
#' @export
pHomBrood <- function(x) {
  if (isPop(x)) {
    ret <- rep(x = NA, times = nInd(x))
    for (ind in seq_len(nInd(x))) {
      if (!is.null(x@misc[[ind]]$pHomBrood)) {
        ret[ind] <- x@misc[[ind]]$pHomBrood
      }
    }
  } else if (isColony(x)) {
    # TODO: report colony, not queen pHomBrood
    if (is.null(x@queen@misc[[1]]$pHomBrood)) {
      ret <- NA
    } else {
      ret <- x@queen@misc[[1]]$pHomBrood
    }
  } else if (isColonies(x)) {
    ret <- sapply(X = x@colonies, FUN = pHomBrood)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname isQueenPresent
#' @title Is the queen present
#'
#' @description Level 0 function that returns queen's presence status (is she
#'   present/alive or not).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- buildUpColony(colony1)
#' colony2 <- buildUpColony(colony2)
#' apiary <- c(colony1, colony2)
#' isQueenPresent(colony1)
#' isQueenPresent(colony2)
#' isQueenPresent(apiary)
#'
#' colony1r <- removeQueen(colony1)
#' isQueenPresent(colony1r)
#'
#' colony2s <- supersedeColony(colony2)
#' isQueenPresent(colony2s)
#'
#' tmp <- swarmColony(buildUpColony(colony1))
#' isQueenPresent(tmp$swarm)
#' isQueenPresent(tmp$remnant)
#'
#' @export
isQueenPresent <- function(x) {
  if (isColony(x)) {
    ret <- !is.null(x@queen)
  } else if (isColonies(x)) {
    ret <- sapply(X = x@colonies, FUN = isQueenPresent)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname areVirginQueensPresent
#' @title Are virgin queen(s) present
#'
#' @description Level 0 function that returns virgin queen(s) presence status.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
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
#' areVirginQueensPresent(colony1)
#' areVirginQueensPresent(colony2)
#' areVirginQueensPresent(apiary)
#'
#' colony1 <- addVirginQueens(colony1)
#' colony2 <- buildUpColony(colony2)
#' apiary <- c(colony1, colony2)
#'
#' areVirginQueensPresent(colony1)
#' areVirginQueensPresent(colony2)
#' areVirginQueensPresent(apiary)
#'
#' colony1r <- removeQueen(colony1)
#' areVirginQueensPresent(colony1r)
#'
#' colony2s <- supersedeColony(colony2)
#' areVirginQueensPresent(colony2s)
#'
#' tmp <- swarmColony(colony1)
#' areVirginQueensPresent(tmp$swarm)
#' areVirginQueensPresent(tmp$remnant)
#'
#' tmp <- splitColony(colony1)
#' areVirginQueensPresent(tmp$split)
#' areVirginQueensPresent(tmp$remnant)
#'
#' @export
areVirginQueensPresent <- function(x) {
  if (isColony(x)) {
    ret <- !is.null(x@virgin_queens)
  } else if (isColonies(x)) {
    ret <- sapply(X = x@colonies, FUN = areVirginQueensPresent)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname isQueenMated
#' @title Is the queen mated?
#'
#' @description Level 0 function that returns queen's mating status.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony3 <- createColony(virgin_queen = basePop[4])
#' apiary <- c(colony1, colony2, colony3)
#' isQueenMated(getQueen(colony1))
#' isQueenMated(colony1)
#' isQueenMated(apiary)
#'
#' isQueenMated(removeQueen(colony1))
#'
#' isQueenMated(supersedeColony(colony2))
#'
#' isQueenMated(c(getQueen(colony1), getQueen(colony2), getVirginQueens(colony3)))
#'
#' isQueenMated(crossVirginQueen(basePop[2], drones))
#'
#' @export
isQueenMated <- function(x) {
  if (isPop(x)) {
    if (nInd(x) > 0) {
      ret <- sapply(X = x@misc, FUN = function(z) !is.null(z$fathers))
    } else {
      ret <- FALSE
    }
  } else if (isColony(x)) {
    if (isQueenPresent(x)) {
      ret <- isQueenMated(x@queen)
    } else {
      ret <- FALSE
    }
  } else if (isColonies(x)) {
    ret <- sapply(X = x@colonies, FUN = isQueenMated)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getQueensYearOfBirth
#' @title Access the queen's year of birth
#'
#' @description Level 0 function that returns the queen's year of birth.
#'
#' @param x \code{\link{Pop-class}} (one or more than one queen),
#'   \code{\link{Colony-class}} (one colony), or
#'   \code{\link{Colonies-class}} (more colonies)
#'
#' @return numeric, the year of birth of the queen(s); named when theres is more
#'   than one queen; \code{NA} if queen not present
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
#' getQueensYearOfBirth(getQueen(colony1))
#' getQueensYearOfBirth(c(getQueen(colony1), getQueen(colony2)))
#' getQueensYearOfBirth(colony1)
#' getQueensYearOfBirth(apiary)
#'
#' queen1 <- getQueen(colony1)
#' queen1 <- setQueensYearOfBirth(queen1, year = 2022)
#' getQueensYearOfBirth(queen1)
#'
#' queen2 <- getQueen(colony2)
#' queens <- setQueensYearOfBirth(c(queen1, queen2), year = 2023)
#' getQueensYearOfBirth(queens)
#'
#' colony1 <- setQueensYearOfBirth(colony1, year = 2022)
#' getQueensYearOfBirth(colony1)
#'
#' apiary <- setQueensYearOfBirth(apiary, year = 2022)
#' getQueensYearOfBirth(apiary)
#'
#' @export
getQueensYearOfBirth <- function(x) {
  if (isPop(x)) {
    nInd <- nInd(x)
    ret <- rep(x = NA, times = nInd)
    for (ind in seq_len(nInd)) {
      if (!is.null(x@misc[[ind]]$yearOfBirth)) {
        ret[ind] <- x@misc[[ind]]$yearOfBirth
      }
    }
    if (nInd > 1) {
      names(ret) <- getId(x)
    }
  } else if (isColony(x)) {
    ret <- ifelse(is.null(x@queen@misc[[1]]$yearOfBirth), NA, x@queen@misc[[1]]$yearOfBirth)
  } else if (isColonies(x)) {
    ret <- sapply(X = x@colonies, FUN = getQueensYearOfBirth)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getQueensYearOfBirth Access the queen's year of birth
#' @export
getQueensYOB <- getQueensYearOfBirth

#' @rdname getQueensAge
#' @title Get (calculate) the queen's age
#'
#' @description Level 0 function that returns the queen's age.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#' @param currentYear integer, current year
#'
#' @return numeric, the age of the queen(s); named when theres is more
#'   than one queen; \code{NA} if queen not present
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
#' @export
getQueensAge <- function(x, currentYear) {
  if (isPop(x)) {
    nInd <- nInd(x)
    ret <- rep(x = NA, times = nInd)
    for (ind in seq_len(nInd)) {
      if (!is.null(x@misc[[ind]]$yearOfBirth)) {
        ret[ind] <- currentYear - x@misc[[ind]]$yearOfBirth
      }
    }
    if (nInd > 1) {
      names(ret) <- getId(x)
    }
  } else if (isColony(x)) {
    if (isQueenPresent(x)) {
      ret <- currentYear - x@queen@misc[[1]]$yearOfBirth
    } else {
      ret <- NA
    }
  } else if (isColonies(x)) {
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
#' @description Level 0 function that returns the colony ID. This is by
#'   definition the ID of the queen.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or \code{\link{Colonies-class}}
#'
#' @return character, \code{NA} when queen not present
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
#' getId(getQueen(colony1))
#' getId(colony1)
#' getId(colony2)
#' getId(apiary)
#'
#' colony2 <- removeQueen(colony2)
#' getId(colony2)
#'
#' @export
getId <- function(x) {
  if (is.null(x)) {
    id <- NA
  } else if (isPop(x)) {
    id <- x@id
  } else if (isColony(x)) {
    id <- ifelse(is.null(x@id), NA, x@id)
  } else if (isColonies(x)) {
    id <- sapply(x@colonies, FUN = getId)
  } else {
    stop("Argument x must be a NULL, Pop, Colony, or Colonies class object!")
  }
  return(id)
}

#' @rdname getLocation
#' @title Get the colony location
#'
#' @description Level 0 function that returns the colony location as (x, y)
#'   coordinates.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return numeric with two values when \code{x} is \code{\link{Colony-class}}
#'   and a list of numeric with two values when \code{x} is
#'   \code{\link{Colonies-class}} (list named after colonies); \code{c(NA, NA)}
#'   when location not set
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
#' colony2 <- setLocation(colony2, location = loc2)
#' getLocation(colony2)
#'
#' getLocation(c(colony1, colony2))
#'
#' # Assuming one location (as in bringing colonies to an apiary at a location!)
#' apiary <- setLocation(apiary, location = loc1)
#' getLocation(apiary)
#'
#' # Assuming different locations (so tmp is not an apiary in one location!)
#' tmp <- setLocation(c(colony1, colony2), location = list(loc1, loc2))
#' getLocation(tmp)
#'
#' @export
getLocation <- function(x) {
  if (isColony(x)) {
    if(is.null(x@location)) {
      ret <- as.numeric(c(NA, NA))
    } else {
      ret <- x@location
    }
  } else if (isColonies(x)) {
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
#' @description Level 0 function that returns colony split status. This will
#'   obviously impact colony strength.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony
#' hasSplit(colony)
#' colony <- buildUpColony(colony, nWorkers = 100)
#' colony
#' hasSplit(colony)
#' tmp <- splitColony(colony)
#' tmp
#' hasSplit(tmp$split)
#' hasSplit(tmp$remnant)
#'
#' colony1 <- createColony(queen = basePop[1], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[2], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' apiary <- buildUpColonies(apiary, nWorkers = 100)
#' tmp <- splitColonies(apiary)
#' tmp
#' hasSplit(tmp$splits)
#' hasSplit(tmp$remnants)
#'
#' @export
hasSplit <- function(x) {
  if (isColony(x)) {
    ret <- x@split
  } else if (isColonies(x)) {
    ret <- sapply(x@colonies, FUN = hasSplit)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname hasSwarmed
#' @title Test if colony has swarmed
#'
#' @description Level 0 function that returns colony swarmed status. This will
#'   obviously have major impact on the colony and its downstream events.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony
#' hasSwarmed(colony)
#' colony <- buildUpColony(colony, nWorkers = 100)
#' colony
#' hasSwarmed(colony)
#' tmp <- swarmColony(colony)
#' tmp
#' hasSwarmed(tmp$swarm)
#' hasSwarmed(tmp$remnant)
#'
#' colony1 <- createColony(queen = basePop[1], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[2], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' apiary <- buildUpColonies(apiary, nWorkers = 100)
#' tmp <- swarmColonies(apiary)
#' tmp
#' hasSwarmed(tmp$swarms)
#' hasSwarmed(tmp$remnants)
#'
#' @export
hasSwarmed <- function(x) {
  if (isColony(x)) {
    ret <- x@swarm
  } else if (isColonies(x)) {
    ret <- sapply(x@colonies, FUN = hasSwarmed)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname hasSuperseded
#' @title Test if colony has superseded
#'
#' @description Level 0 function that returns colony supersedure status.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony
#' hasSuperseded(colony)
#' colony <- buildUpColony(colony, nWorkers = 100)
#' colony
#' hasSuperseded(colony)
#' tmp <- supersedeColony(colony)
#' tmp
#' hasSuperseded(tmp)
#'
#' colony1 <- createColony(queen = basePop[1], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[2], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' apiary <- buildUpColonies(apiary, nWorkers = 100)
#' tmp <- supersedeColonies(apiary)
#' tmp
#' hasSuperseded(tmp)
#'
#' @export
hasSuperseded <- function(x) {
  if (isColony(x)) {
    ret <- x@supersedure
  } else if (isColonies(x)) {
    ret <- sapply(x@colonies, FUN = hasSuperseded)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname hasCollapsed
#' @title Test if colony has collapsed
#'
#' @description Level 0 function that returns colony collapse status.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony
#' hasCollapsed(colony)
#' colony <- buildUpColony(colony, nWorkers = 100)
#' colony
#' hasCollapsed(colony)
#' tmp <- collapseColony(colony)
#' tmp
#' hasCollapsed(tmp)
#'
#' colony1 <- createColony(queen = basePop[1], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[2], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' apiary <- buildUpColonies(apiary, nWorkers = 100)
#' tmp <- collapseColonies(apiary)
#' tmp
#' hasCollapsed(tmp)
#'
#' @export
hasCollapsed <- function(x) {
  if (isColony(x)) {
    ret <- x@collapse
  } else if (isColonies(x)) {
    ret <- sapply(x@colonies, FUN = hasCollapsed)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname isProductive
#' @title Test if colony is currently productive
#'
#' @description Level 0 function that returns colony production status. This can
#'   be used to decided if colony production can be simulated.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony
#' isProductive(colony)
#' colony <- buildUpColony(colony, nWorkers = 100)
#' colony
#' isProductive(colony)
#'
#' colony1 <- createColony(queen = basePop[1], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[2], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' apiary <- buildUpColonies(apiary, nWorkers = 100)
#' isProductive(apiary)
#'
#' @export
isProductive <- function(x) {
  if (isColony(x)) {
    ret <- x@production
  } else if (isColonies(x)) {
    ret <- sapply(x@colonies, FUN = isProductive)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname simulateHoneyBeeGenomes
#' @title Simulate the Honey bee genome
#'
#' @description Level 0 function that returns simulated honeybee genomes
#'
#' @param nInd number of individuals to simulate
#' @param nChr number of chromosomes to simulate
#' @param nSegSites number of segregating sites to keep per chromosome
#' @param Ne effective population size
#' @param nBp  base pair length of chromosome
#' @param genLen genetic length of chromosome in Morgans
#' @param mutRate per base pair mutation rate
#' @param histNe effective population size in previous generations
#' @param histGen number of generations ago for effective population sizes given
#'   in histNe
#' @param split an optional historic population split in terms of generations
#'   ago
#' @param nThreads if OpenMP is available, this will allow for simulating
#'   chromosomes in parallel. If the value is NULL, the number of threads is
#'   automatically detected
#'
#' @return \code{\link{MapPop-class}}
#'
#' @examples
#' founderGenomes <- simulateHoneyBeeGenomes(nInd = 10, nChr = 1,
#'                                           nSegSites = 2, Ne = 10)
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
                                    nThreads = NULL) {
  # TODO: we will need to use runMacs(manualCommand = ...) to accomodate the honeybee demography,
  #       because runMacs2 works only with simple splits, while honeybee demography is more
  #       "involved"; see also see also https://github.com/HighlanderLab/SIMplyBee/issues/45
  founderGenomes <- runMacs2(nInd = nInd,
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
  return(founderGenomes)
}

#' @rdname isCsdActive
#' @title Is csd locus activated
#'
#' @description Level 0 function that checks if the csd locus has been
#'   activated. See \code{\link{SimParamBee}} for more information about the csd
#'   locus.
#'
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
#' isCsdActive()
#'
#' SP <- SimParamBee$new(founderGenomes)
#' isCsdActive()
#'
#' @export
isCsdActive <- function(simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- !is.null(simParamBee$csdChr)
  return(ret)
}

#' @rdname getCsdAlleles
#' @title Get csd alleles
#'
#' @description Level 0 function that returns alleles from the csd locus. See
#'   \code{\link{SimParamBee}} for more information about the csd locus.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#' @param nInd numeric, for how many individuals; if \code{NULL} all individuals
#'   are taken; this can be useful as a test of sampling individuals
#' @param allele character, either "all" for both alleles or an integer for a
#'   single allele, use a value of 1 for female allele and a value of 2 for male
#'   allele
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Pop-class}}, list
#'   of matrices with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   (list nodes named by caste) and list of a list of matrices with haplotypes
#'   when \code{x} is \code{\link{Colonies-class}}, outer list is named by
#'   colony id when \code{x} is \code{\link{Colonies-class}}; \code{NULL} when
#'   \code{x} is \code{NULL}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' apiary <- c(colony1, colony2)
#'
#' getCsdAlleles(getQueen(colony1))
#' getCsdAlleles(getVirginQueens(colony1))
#' getCsdAlleles(getFathers(colony1))
#' getCsdAlleles(getWorkers(colony1))
#' getCsdAlleles(getDrones(colony1))
#'
#' getCsdAlleles(colony1)
#'
#' getCsdAlleles(getDrones(colony2))
#' getCsdAlleles(colony2)
#'
#' getCsdAlleles(apiary)
#'
#' getCsdAlleles(apiary, nInd = 2)
#'
#' @export
getCsdAlleles <- function(x, nInd = NULL, allele = "all", simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isCsdActive(simParamBee = simParamBee)) {
    stop("The csd locus has not been set!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullSegSiteHaplo(pop = x, haplo = allele, chr = simParamBee$csdChr,
                            simParam = simParamBee)[, simParamBee$csdPosStart:simParamBee$csdPosStop, drop = FALSE]
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "virgin_queens", "workers", "drones")
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCsdAlleles(x = tmp, allele = allele, simParamBee = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    ret <- lapply(X = x@colonies, FUN = getCsdAlleles, nInd = nInd,
                  allele = allele, simParamBee = simParamBee)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCsdGeno
#' @title Get genotypes from the csd locus
#'
#' @description Level 0 function that returns genotypes from the csd locus. See
#'   \code{\link{SimParamBee}} for more information about the csd locus.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#' @param nInd numeric, for how many individuals; if \code{NULL} all individuals
#'   are taken; this can be useful as a test of sampling individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details The returned genotypes are spanning multiple bi-allelic SNP of
#'   non-recombining haplotypes. In most cases you will want to use
#'   \code{\link{getCsdAlleles}}.
#'
#' @return matrix with genotypes when \code{x} is \code{\link{Pop-class}}, list
#'   of matrices with genotypes when \code{x} is \code{\link{Colony-class}}
#'   (list nodes named by caste) and list of a list of matrices with genotypes
#'   when \code{x} is \code{\link{Colonies-class}}, outer list is named by
#'   colony id when \code{x} is \code{\link{Colonies-class}}; \code{NULL} when
#'   \code{x} is \code{NULL}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
#' apiary <- c(colony1, colony2)
#'
#' getCsdGeno(getQueen(colony1))
#' getCsdGeno(getVirginQueens(colony1))
#' getCsdGeno(getFathers(colony1))
#' getCsdGeno(getWorkers(colony1))
#' getCsdGeno(getDrones(colony1))
#'
#' getCsdGeno(colony1)
#'
#' getCsdAlleles(getDrones(colony2))
#' getCsdAlleles(colony2)
#'
#' getCsdGeno(apiary)
#' getCsdGeno(apiary, nInd = 2)
#'
#' @export
getCsdGeno <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isCsdActive(simParamBee = simParamBee)) {
    stop("The csd locus has not been set!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullSegSiteGeno(pop = x, chr = simParamBee$csdChr,
                           simParam = simParamBee)[, simParamBee$csdPosStart:simParamBee$csdPosStop, drop = FALSE]
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "virgin_queens", "workers", "drones")
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCsdGeno(x = tmp, simParamBee = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    ret <- lapply(X = x@colonies, FUN = getCsdGeno, nInd = nInd,
                  simParamBee = simParamBee)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname isGenoHeterozygous
#' @title Test if a multilocus genotype is heterozygous
#'
#' @description Level 0 function that returns heterozygote status for a
#'   multilocus genotype.
#'
#' @param x integer or matrix, output from \code{\link{getCsdGeno}}
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony1 <- createColony(queen = basePop[2], fathers = drones)
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 10)
#'
#' (tmp <- getCsdGeno(getQueen(colony1)))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#'
#' (tmp <- getCsdGeno(getVirginQueens(colony1)))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#'
#' (tmp <- getCsdGeno(getWorkers(colony1)))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#'
# Not exporting this function, since its just a helper and quite specific for
#   our csd locus implementation
isGenoHeterozygous <- function(x) {
  if (!is.matrix(x)) {
    stop("Argument x must be a matrix class object!")
  }
  ret <- apply(X = x, MARGIN = 1, FUN = function(z) any(z == 1))
  return(ret)
}

#' @rdname isCsdHeterozygous
#' @title Test if individuals are heterozygous at the csd locus
#'
#' @description Level 0 function that returns if individuals of a population are
#'   heterozygous at the csd locus. See \code{\link{SimParamBee}} for more
#'   information about the csd locus.
#'
#' @param pop \code{\link{Pop-class}}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details We could expand \code{isCsdHeterozygous} to work also with
#'   \code{\link{Colony-class}} and \code{\link{Colonies-class}} if needed
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 5)
#' colony1 <- createColony(queen = basePop[2], fathers = drones)
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 10)
#'
#' isCsdHeterozygous(getQueen(colony1))
#'
#' isCsdHeterozygous(getWorkers(colony1))
#'
#' isCsdHeterozygous(getVirginQueens(colony1))
#'
#' @export
isCsdHeterozygous <- function(pop, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  geno <- getCsdGeno(x = pop, simParamBee = simParamBee)
  # Could inline isGenoHeterozygous() here, but isGenoHeterozygous is far easier
  # to test than isCsdHeterozygous()
  ret <- isGenoHeterozygous(x = geno)
  return(ret)
}

#' @rdname nCsdAlleles
#' @title Report the number of distinct csd alleles
#'
#' @description Level 0 function that returns the number of distinct csd alleles
#'   in input. See \code{\link{SimParamBee}} for more information about the csd
#'   locus.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details Queen will always have 2 csd alleles, since she has to be
#'   heterozygous to be viable. The same holds for individual virgin queens and
#'   workers, but note that looking at csd genotypes of virgin queens or workers
#'   we are looking at a sample of 1 csd allele from the queen and 1 csd allele
#'   from their fathers, noting that homozygous genotypes are excluded.
#'   Therefore, \code{nCsdAlleles()} from virgin queens and workers is a noisy
#'   realisation of \code{nCsdAlleles()} from queens and fathers. For this
#'   reason, we also report \code{nCsdAlleles()} from queens and fathers
#'   combined (see the \code{queenAndFathers} list node). This last measure is
#'   then the expected number of csd alleles in a colony as opposed to realised
#'   number of csd alleles in a sample of virgin queens and workers. Similarly
#'   as for virgin queens and workers, \code{nCsdAlleles()} from drones gives a
#'   noisy realisation of \code{nCsdAlleles()} from queens. The amount of noise
#'   will depend on the number of individuals, so in most cases there should be
#'   minimal amount of noise.
#'
#' @return integer representing the number of distinct csd alleles when \code{x}
#'   is \code{\link{Pop-class}} (or ), list of integer
#'   when \code{x} is \code{\link{Colony-class}} (list nodes named by caste) and
#'   list of a list of integer when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}; the integer rep
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 3)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' apiary <- c(colony1, colony2)
#'
#' nCsdAlleles(getQueen(colony1))
#' nCsdAlleles(getFathers(colony1))
#' nCsdAlleles(getVirginQueens(colony1))
#' nCsdAlleles(getWorkers(colony1))
#' nCsdAlleles(getDrones(colony1))
#'
#' nCsdAlleles(colony1)
#'
#' nCsdAlleles(apiary)
#'
#' @export
nCsdAlleles <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(x)) {
    ret <- NA
  } else if (isPop(x)) {
    haplo <- getCsdAlleles(x = x, simParamBee = simParamBee)
    haplo <- haplo[!duplicated(x = haplo), ]
    ret <- nrow(haplo)
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 6)
    names(ret) <- c("queen", "fathers", "queenAndFathers", "virgin_queens", "workers", "drones")
    ret$queen           <- nCsdAlleles(x = getQueen(x),                   simParamBee = simParamBee)
    ret$fathers         <- nCsdAlleles(x = getFathers(x),                 simParamBee = simParamBee)
    ret$queenAndFathers <- nCsdAlleles(x = c(getQueen(x), getFathers(x)), simParamBee = simParamBee)
    ret$virgin_queens   <- nCsdAlleles(x = getVirginQueens(x),            simParamBee = simParamBee)
    ret$workers         <- nCsdAlleles(x = getWorkers(x),                 simParamBee = simParamBee)
    ret$drones          <- nCsdAlleles(x = getDrones(x),                  simParamBee = simParamBee)
  } else if (isColonies(x)) {
    ret <- lapply(X = x@colonies, FUN = nCsdAlleles, simParamBee = simParamBee)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getIbdHaplo
#' @title Access IBD haplotypes
#'
#' @description Level 0 function that returns IBD (identity by descent)
#'   haplotypes.
#'
#' @param pop \code{\link{Pop-class}}
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#' are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullIbdHaplo}}
#'
#' @return Matrix of haplotypes
#'
#' @export
getIbdHaplo <- function(pop, chr = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullIbdHaplo(pop = pop, chr = chr, simParam = simParam)
  } else {
    stop("Argument pop must be a Pop class object!")
  }
  return(ret)
}

#' @rdname getQtlHaplo
#' @title Access QTL haplotypes
#'
#' @description Level 0 function that returns QTL haplotypes.
#'
#' @param pop \code{\link{Pop-class}}
#' @param trait numeric, indicates which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#' are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullQtlHaplo}}
#'
#' @return Matrix of haplotypes
#'
#' @export
getQtlHaplo <- function(pop, trait = 1, haplo = "all", chr = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullQtlHaplo(pop = pop, trait = trait, haplo = haplo, chr = chr, simParam = simParam)
  } else {
    stop("Argument pop must be a Pop class object!")
  }
  return(ret)
}

#' @rdname getQtlGeno
#' @title Access QTL genotypes
#'
#' @description Level 0 function that returns QTL genotypes.
#'
#' @param pop \code{\link{Pop-class}}
#' @param trait numeric, indicates which trait's QTL genotype to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#' are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullQtlGeno}}
#'
#' @return Matrix of genotypes
#'
#' @export
getQtlGeno <- function(pop, trait = 1, chr = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullQtlGeno(pop = pop, trait = trait, chr = chr, simParam = simParam)
  } else {
    stop("Argument pop must be a Pop class object!")
  }
  return(ret)
}

#' @rdname getSegSiteHaplo
#' @title Access haplotypes for all segregating sites
#'
#' @description Level 0 function that returns haplotypes for all segregating
#'   sites.
#'
#' @param pop \code{\link{Pop-class}}
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#' are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullSegSiteHaplo}}
#'
#' @return Matrix of haplotypes
#'
#' @export
getSegSiteHaplo <- function(pop, haplo = "all", chr = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullSegSiteHaplo(pop = pop, haplo = haplo, chr = chr, simParam = simParam)
  } else {
    stop("Argument pop must be a Pop class object!")
  }
  return(ret)
}

#' @rdname getSegSiteGeno
#' @title Access genotypes for all segregating sites
#'
#' @description Level 0 function that returns genotypes for all segregating
#'   sites.
#'
#' @param pop \code{\link{Pop-class}}
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#' are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullSegSiteHaplo}}
#'
#' @return Matrix of genotypes
#'
#' @export
getSegSiteGeno <- function(pop, chr = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullSegSiteGeno(pop = pop, chr = chr, simParam = simParam)
  } else {
    stop("Argument pop must be a Pop class object!")
  }
  return(ret)
}

#' @rdname getSnpHaplo
#' @title Access SNP array haplotypes
#'
#' @description Level 0 function that returns SNP array haplotypes.
#'
#' @param pop \code{\link{Pop-class}}
#' @param snpChip numeric, indicates which SNP array haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#' single set of haplotypes, use a value of 1 for female haplotypes and a value of
#' 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#' are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullSnpHaplo}}
#'
#' @return Matrix of haplotypes
#'
#' @export
getSnpHaplo <- function(pop, snpChip = 1, haplo = "all", chr = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullSnpHaplo(pop = pop, snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParam)
  } else {
    stop("Argument pop must be a Pop class object!")
  }
  return(ret)
}

#' @rdname getSnpGeno
#' @title Access SNP array genotypes
#'
#' @description Level 0 function that returns SNP array genotypes.
#'
#' @param pop \code{\link{Pop-class}}
#' @param snpChip numeric, indicates which SNP array genotype to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#' are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullSnpHaplo}}
#'
#' @return Matrix of genotypes
#'
#' @export
getSnpGeno <- function(pop, snpChip = 1, chr = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullSnpGeno(pop = pop, snpChip = snpChip, chr = chr, simParam = simParam)
  } else {
    stop("Argument pop must be a Pop class object!")
  }
  return(ret)
}

#' @rdname getCasteIbdHaplo
#' @title Access IBD haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns IBD (identity by descent)
#'   haplotypes of individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getIbdHaplo}} and \code{\link{pullIbdHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
getCasteIbdHaplo <- function(x, caste, nInd = NULL,
                             chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getIbdHaplo(pop = tmp, chr = chr, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteIbdHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                              chr = chr, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of the queen
#' @export
getQueensIbdHaplo <- function(x, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x, caste = "queen",
                            chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of fathers
#' @export
getFathersIbdHaplo <- function(x, nInd = NULL,
                               chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x, caste = "fathers", nInd = nInd,
                            chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of virgin queens
#' @export
getVirginQueensIbdHaplo <- function(x, nInd = NULL,
                                    chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x, caste = "virgin_queens", nInd = nInd,
                            chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of workers
#' @export
getWorkersIbdHaplo <- function(x, nInd = NULL,
                               chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x, caste = "workers", nInd = nInd,
                            chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of drones
#' @export
getDronesIbdHaplo <- function(x, nInd = NULL,
                              chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x, caste = "drones", nInd = nInd,
                            chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonyIbdHaplo
#' @title Access IBD haplotypes of individuals in colony
#'
#' @description Level 0 function that returns IBD (identity by descent)
#'   haplotypes of individuals in colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCasteIbdHaplo}} and \code{\link{getIbdHaplo}}
#'
#' @return list of matrices with haplotypes when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of matrices with haplotypes when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyIbdHaplo(colony1)
#' getColonyIbdHaplo(colony1, caste = c("queen", "fathers"))
#' getColonyIbdHaplo(colony1, nInd = 1)
#' getColonyIbdHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' getColonyIbdHaplo(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyIbdHaplo(apiary)
#' getColonyIbdHaplo(apiary, caste = c("queen", "fathers"))
#' getColonyIbdHaplo(apiary, nInd = 1)
#' getColonyIbdHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @export
getColonyIbdHaplo <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                              nInd = NULL, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getIbdHaplo(pop = tmp, chr = chr, simParam = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyIbdHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                         chr = chr, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteQtlHaplo
#' @title Access QTL haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns QTL haplotypes of individuals in a
#'   caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param trait numeric, indicates which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getQtlHaplo}} and \code{\link{pullQtlHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
getCasteQtlHaplo <- function(x, caste, nInd = NULL,
                             trait = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getQtlHaplo(pop = tmp, haplo = haplo, trait = trait, chr = chr, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteQtlHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                              trait = trait, haplo = haplo, chr = chr, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
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
                              trait = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "queen",
                            trait = trait, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of fathers
#' @export
getFathersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "fathers", nInd = nInd,
                            trait = trait, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of virgin queens
#' @export
getVirginQueensQtlHaplo <- function(x, nInd = NULL,
                                    trait = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "virgin_queens", nInd = nInd,
                            trait = trait, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype of workers
#' @export
getWorkersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)| isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "workers", nInd = nInd,
                            trait = trait, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of drones
#' @export
getDronesQtlHaplo <- function(x, nInd = NULL,
                              trait = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "drones", nInd = nInd,
                            trait = trait, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonyQtlHaplo
#' @title Access QTL haplotypes of individuals in colony
#'
#' @description Level 0 function that returns QTL haplotypes of individuals in
#'   colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param trait numeric, indicates which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCasteQtlHaplo}} and \code{\link{getQtlHaplo}}
#'
#' @return list of matrices with haplotypes when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of matrices with haplotypes when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyQtlHaplo(colony1)
#' getColonyQtlHaplo(colony1, caste = c("queen", "fathers"))
#' getColonyQtlHaplo(colony1, nInd = 1)
#' getColonyQtlHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' getColonyQtlHaplo(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyQtlHaplo(apiary)
#' getColonyQtlHaplo(apiary, caste = c("queen", "fathers"))
#' getColonyQtlHaplo(apiary, nInd = 1)
#' getColonyQtlHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @export
getColonyQtlHaplo <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                              nInd = NULL, trait = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getQtlHaplo(pop = tmp, trait = trait, haplo = haplo,
                                    chr = chr, simParam = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyQtlHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                         trait = trait, haplo = haplo, chr = chr, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteQtlGeno
#' @title Access QTL genotypes of individuals in a caste
#'
#' @description Level 0 function that returns QTL genotypes of individuals in a
#'   caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param trait numeric, indicates which trait's QTL genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getQtlGeno}} and \code{\link{pullQtlGeno}}
#'
#' @return matrix with genotypes when \code{x} is \code{\link{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
getCasteQtlGeno <- function(x, caste, nInd = NULL,
                            trait = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getQtlGeno(pop = tmp, trait = trait, chr = chr, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteQtlGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                             trait = trait, chr = chr, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
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
                             trait = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlGeno(x, caste = "queen",
                           trait = trait, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of fathers
#' @export
getFathersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlGeno(x, caste = "fathers", nInd = nInd,
                           trait = trait, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of virgin queens
#' @export
getVirginQueensQtlGeno <- function(x, nInd = NULL,
                                   trait = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlGeno(x, caste = "virgin_queens", nInd = nInd,
                           trait = trait, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of workers
#' @export
getWorkersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlGeno(x, caste = "workers", nInd = nInd,
                           trait = trait, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of drones
#' @export
getDronesQtlGeno <- function(x, nInd = NULL,
                             trait = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlGeno(x, caste = "drones", nInd = nInd,
                           trait = trait, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonyQtlGeno
#' @title Access QTL genotypes of individuals in colony
#'
#' @description Level 0 function that returns QTL genotypes of individuals in
#'   colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param trait numeric, indicates which trait's QTL genotype to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCasteQtlGeno}} and \code{\link{getQtlGeno}}
#'
#' @return list of matrices with genotypes when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of matrices with genotypes when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyQtlGeno(colony1)
#' getColonyQtlGeno(colony1, caste = c("queen", "fathers"))
#' getColonyQtlGeno(colony1, nInd = 1)
#' getColonyQtlGeno(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' getColonyQtlGeno(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyQtlGeno(apiary)
#' getColonyQtlGeno(apiary, caste = c("queen", "fathers"))
#' getColonyQtlGeno(apiary, nInd = 1)
#' getColonyQtlGeno(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @export
getColonyQtlGeno <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                             nInd = NULL, trait = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getQtlGeno(pop = tmp, trait = trait,
                                   chr = chr, simParam = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyQtlGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                        trait = trait, chr = chr, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteSegSiteHaplo
#' @title Access haplotypes for all segregating sites of individuals in a
#'   caste
#'
#' @description Level 0 function that returns haplotypes for all segregating
#'   sites of individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getSegSiteHaplo}} and \code{\link{pullSegSiteHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
getCasteSegSiteHaplo <- function(x, caste, nInd = NULL,
                                 haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getSegSiteHaplo(pop = tmp, haplo = haplo, chr = chr, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSegSiteHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                  haplo = haplo, chr = chr, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
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
                                  haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteHaplo(x, caste = "queen",
                                haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of fathers
#' @export
getFathersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteHaplo(x, caste = "fathers", nInd = nInd,
                                haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of virgin queens
#' @export
getVirginQueensSegSiteHaplo <- function(x, nInd = NULL,
                                        haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteHaplo(x, caste = "virgin_queens", nInd = nInd,
                                haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of workers
#' @export
getWorkersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteHaplo(x, caste = "workers", nInd = nInd,
                                haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of drones
#' @export
getDronesSegSiteHaplo <- function(x, nInd = NULL,
                                  haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteHaplo(x, caste = "drones", nInd = nInd,
                                haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonySegSiteHaplo
#' @title Access haplotypes for all segregating sites of individuals in
#'   colony
#'
#' @description Level 0 function that returns haplotypes for all segregating
#'   sites of individuals in colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCasteSegSiteHaplo}} and \code{\link{getSegSiteHaplo}}
#'
#' @return list of matrices with haplotypes when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of matrices with haplotypes when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonySegSiteHaplo(colony1)
#' getColonySegSiteHaplo(colony1, caste = c("queen", "fathers"))
#' getColonySegSiteHaplo(colony1, nInd = 1)
#' getColonySegSiteHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' getColonySegSiteHaplo(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonySegSiteHaplo(apiary)
#' getColonySegSiteHaplo(apiary, caste = c("queen", "fathers"))
#' getColonySegSiteHaplo(apiary, nInd = 1)
#' getColonySegSiteHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @export
getColonySegSiteHaplo <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                                  nInd = NULL, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getSegSiteHaplo(pop = tmp, haplo = haplo,
                                        chr = chr, simParam = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonySegSiteHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                             haplo = haplo, chr = chr, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteSegSiteGeno
#' @title Access genotypes for all segregating sites of individuals in a
#'   caste
#'
#' @description Level 0 function that returns genotypes for all segregating
#'   sites of individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getSegSiteGeno}} and \code{\link{pullSegSiteGeno}}
#'
#' @return matrix with genotypes when \code{x} is \code{\link{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
getCasteSegSiteGeno <- function(x, caste, nInd = NULL,
                                chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getSegSiteGeno(pop = tmp, chr = chr, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSegSiteGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                 chr = chr, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
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
                                 chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteGeno(x, caste = "queen",
                               chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of fathers
#' @export
getFathersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteGeno(x, caste = "fathers", nInd = nInd,
                               chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of virgin queens
#' @export
getVirginQueensSegSiteGeno <- function(x, nInd = NULL,
                                       chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteGeno(x, caste = "virgin_queens", nInd = nInd,
                               chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of workers
#' @export
getWorkersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteGeno(x, caste = "workers", nInd = nInd,
                               chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of drones
#' @export
getDronesSegSiteGeno <- function(x, nInd = NULL,
                                 chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteGeno(x, caste = "drones", nInd = nInd,
                               chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonySegSiteGeno
#' @title Access genotypes for all segregating sites of individuals in
#'   colony
#'
#' @description Level 0 function that returns genotypes for all segregating
#'   sites of individuals in colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCasteSegSiteHaplo}} and \code{\link{getSegSiteHaplo}}
#'
#' @return list of matrices with genotypes when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of matrices with genotypes when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonySegSiteGeno(colony1)
#' getColonySegSiteGeno(colony1, caste = c("queen", "fathers"))
#' getColonySegSiteGeno(colony1, nInd = 1)
#' getColonySegSiteGeno(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' getColonySegSiteGeno(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonySegSiteGeno(apiary)
#' getColonySegSiteGeno(apiary, caste = c("queen", "fathers"))
#' getColonySegSiteGeno(apiary, nInd = 1)
#' getColonySegSiteGeno(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @export
getColonySegSiteGeno <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                                 nInd = NULL, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getSegSiteGeno(pop = tmp, chr = chr, simParam = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonySegSiteGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                            chr = chr, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteSnpHaplo
#' @title Access SNP array haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns SNP array haplotypes of
#'   individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getSnpHaplo}} and \code{\link{pullSnpHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addSnpChip(nSnpPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
getCasteSnpHaplo <- function(x, caste, nInd = NULL,
                             snpChip = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getSnpHaplo(pop = tmp, haplo = haplo, snpChip = snpChip, chr = chr, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSnpHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                              snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
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
                              snpChip = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpHaplo(x, caste = "queen",
                            snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)

}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of fathers
#' @export
getFathersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpHaplo(x, caste = "fathers", nInd = nInd,
                            snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of virgin queens
#' @export
getVirginQueensSnpHaplo <- function(x, nInd = NULL,
                                    snpChip = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpHaplo(x, caste = "virgin_queens", nInd = nInd,
                            snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype of workers
#' @export
getWorkersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpHaplo(x, caste = "workers", nInd = nInd,
                            snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of drones
#' @export
getDronesSnpHaplo <- function(x, nInd = NULL,
                              snpChip = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpHaplo(x, caste = "drones", nInd = nInd,
                            snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonySnpHaplo
#' @title Access SNP array haplotypes of individuals in colony
#'
#' @description Level 0 function that returns SNP array haplotypes of
#'   individuals in colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param snpChip numeric, indicates which SNP array haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCasteSnpHaplo}} and \code{\link{getSnpHaplo}}
#'
#' @return list of matrices with haplotypes when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of matrices with haplotypes when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addSnpChip(nSnpPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonySnpHaplo(colony1)
#' getColonySnpHaplo(colony1, caste = c("queen", "fathers"))
#' getColonySnpHaplo(colony1, nInd = 1)
#' getColonySnpHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' getColonySnpHaplo(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonySnpHaplo(apiary)
#' getColonySnpHaplo(apiary, caste = c("queen", "fathers"))
#' getColonySnpHaplo(apiary, nInd = 1)
#' getColonySnpHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @export
getColonySnpHaplo <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                              nInd = NULL, snpChip = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getSnpHaplo(pop = tmp, snpChip = snpChip, haplo = haplo,
                                    chr = chr, simParam = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonySnpHaplo(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                         snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteSnpGeno
#' @title Access SNP array genotypes of individuals in a caste
#'
#' @description Level 0 function that returns SNP array genotypes of individuals
#'   in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getSnpGeno}} and \code{\link{pullSnpGeno}}
#'
#' @return matrix with genotypes when \code{x} is \code{\link{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addSnpChip(nSnpPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
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
#' @export
getCasteSnpGeno <- function(x, caste, nInd = NULL,
                            snpChip = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getSnpGeno(pop = tmp, snpChip = snpChip, chr = chr, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSnpGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                             snpChip = snpChip, chr = chr, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
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
                             snpChip = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpGeno(x, caste = "queen",
                           snpChip = snpChip, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of fathers
#' @export
getFathersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpGeno(x, caste = "fathers", nInd = nInd,
                           snpChip = snpChip, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of virgin queens
#' @export
getVirginQueensSnpGeno <- function(x, nInd = NULL,
                                   snpChip = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpGeno(x, caste = "virgin_queens", nInd = nInd,
                           snpChip = snpChip, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of workers
#' @export
getWorkersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpGeno(x, caste = "workers", nInd = nInd,
                           snpChip = snpChip, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of drones
#' @export
getDronesSnpGeno <- function(x, nInd = NULL,
                             snpChip = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpGeno(x, caste = "drones", nInd = nInd,
                           snpChip = snpChip, chr = chr, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonySnpGeno
#' @title Access SNP array genotypes of individuals in colony
#'
#' @description Level 0 function that returns SNP array genotypes of individuals
#'   in colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param snpChip numeric, indicates which SNP array genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCasteSnpGeno}} and \code{\link{getSnpGeno}}
#'
#' @return list of matrices with genotypes when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of matrices with genotypes when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addSnpChip(nSnpPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' getColonySnpGeno(colony1)
#' getColonySnpGeno(colony1, caste = c("queen", "fathers"))
#' getColonySnpGeno(colony1, nInd = 1)
#' getColonySnpGeno(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' getColonySnpGeno(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonySnpGeno(apiary)
#' getColonySnpGeno(apiary, caste = c("queen", "fathers"))
#' getColonySnpGeno(apiary, nInd = 1)
#' getColonySnpGeno(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @export
getColonySnpGeno <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                             nInd = NULL, snpChip = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getSnpGeno(pop = tmp, snpChip = snpChip,
                                   chr = chr, simParam = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonySnpGeno(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                                        snpChip = snpChip, chr = chr, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteGv
#' @title Access genetic values of individuals in a caste
#'
#' @description Level 0 function that returns genetic values of individuals in a
#'   caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#'
#' @seealso \code{\link{gv}}
#'
#' @return vector of genetic values when \code{x} is \code{\link{Colony-class}}
#'   and list of vectors of genetic values when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 10)
#'
#' getCasteGv(colony1, caste = "queen")
#' getQueensGv(colony1)
#'
#' getCasteGv(colony1, caste = "fathers")
#' getCasteGv(colony1, caste = "fathers", nInd = 2)
#' getCasteGv(colony1, caste = "fathers", nInd = 2)
#' getFathersGv(colony1)
#' getFathersGv(colony1, nInd = 2)
#'
#' getCasteGv(colony1, caste = "virgin_queens")
#' getVirginQueensGv(colony1)
#'
#' getCasteGv(colony1, caste = "workers")
#' getWorkersGv(colony1)
#'
#' getCasteGv(colony1, caste = "drones")
#' getDronesGv(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteGv(apiary, caste = "queen")
#' getQueensGv(apiary)
#'
#' getCasteGv(apiary, caste = "fathers")
#' getCasteGv(apiary, caste = "fathers", nInd = 2)
#' getCasteGv(apiary, caste = "fathers", nInd = 2)
#' getFathersGv(apiary)
#' getFathersGv(apiary, nInd = 2)
#'
#' getCasteGv(apiary, caste = "virgin_queens")
#' getVirginQueensGv(apiary)
#'
#' getCasteGv(apiary, caste = "workers")
#' getWorkersGv(apiary)
#'
#' getCasteGv(apiary, caste = "drones")
#' getDronesGv(apiary)
#'
#' @export
getCasteGv <- function(x, caste, nInd = NULL) {
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- gv(pop = tmp)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteGv(x = x@colonies[[colony]], caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteGv Access genetic value of the queen
#' @export
getQueensGv <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteGv(x, caste = "queen")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteGv Access genetic values of fathers
#' @export
getFathersGv <- function(x, nInd = NULL) {
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteGv(x, caste = "fathers", nInd = nInd)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteGv Access genetic values of virgin queens
#' @export
getVirginQueensGv <- function(x, nInd = NULL) {
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteGv(x, caste = "virgin_queens", nInd = nInd)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteGv Access genetic values of workers
#' @export
getWorkersGv <- function(x, nInd = NULL) {
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteGv(x, caste = "workers", nInd = nInd)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteGv Access genetic values of drones
#' @export
getDronesGv <- function(x, nInd = NULL) {
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteGv(x, caste = "drones", nInd = nInd)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonyGv
#' @title Access genetic values of individuals in colony
#'
#' @description Level 0 function that returns genetic values of individuals in
#'   colony.
#'
#' @param x \code{\link{Colony-class}} or Colonies
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#'
#' @seealso \code{\link{gv}}
#'
#' @return list of vector of genetic values when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of vectors of genetic values when \code{x} is \code{\link{Colonies-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyGv(colony1)
#' getColonyGv(colony1, caste = c("queen", "fathers"))
#' getColonyGv(colony1, nInd = 1)
#' getColonyGv(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' getColonyGv(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyGv(apiary)
#' getColonyGv(apiary, caste = c("queen", "fathers"))
#' getColonyGv(apiary, nInd = 1)
#' getColonyGv(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))
#'
#' @export
getColonyGv <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                        nInd = NULL) {
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- gv(pop = tmp)
      }
    }
    # TODO: should we add colony node here too or will that be done elsewhere?
    #       see also https://github.com/HighlanderLab/SIMplyBee/issues/28 (for gv)
    #       see also https://github.com/HighlanderLab/SIMplyBee/issues/29 (for bv and dd)
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyGv(x = x@colonies[[colony]], caste = caste, nInd = nInd)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteBv
#' @title Access breeding values of individuals in a caste
#'
#' @description Level 0 function that returns breeding values of individuals in
#'   a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{bv}}
#'
#' @return vector of breeding values when \code{x} is \code{\link{Colony-class}}
#'   and list of vectors of breeding values when \code{x} is
#'   \code{\link{Colonies-class}}, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' try(getCasteBv(colony1, caste = "queen")) # TODO
#' try(getQueensBv(colony1)) # TODO
#'
#' getCasteBv(colony1, caste = "fathers")
#' getCasteBv(colony1, caste = "fathers", nInd = 2)
#' getCasteBv(colony1, caste = "fathers", nInd = 2)
#' getFathersBv(colony1)
#' getFathersBv(colony1, nInd = 2)
#'
#' try(getCasteBv(colony1, caste = "virgin_queens")) # TODO
#' try(getVirginQueensBv(colony1)) # TODO
#'
#' getCasteBv(colony1, caste = "workers")
#' getWorkersBv(colony1)
#'
#' getCasteBv(colony1, caste = "drones")
#' getDronesBv(colony1)
#'
#' apiary <- c(colony1, colony2)
#' try(getCasteBv(apiary, caste = "queen")) # TODO
#' try(getQueensBv(apiary)) # TODO
#'
#' getCasteBv(apiary, caste = "fathers")
#' getCasteBv(apiary, caste = "fathers", nInd = 2)
#' getCasteBv(apiary, caste = "fathers", nInd = 2)
#' getFathersBv(apiary)
#' getFathersBv(apiary, nInd = 2)
#'
#' try(getCasteBv(apiary, caste = "virgin_queens")) # TODO
#' try(getVirginQueensBv(apiary)) # TODO
#'
#' getCasteBv(apiary, caste = "workers")
#' getWorkersBv(apiary)
#'
#' getCasteBv(apiary, caste = "drones")
#' getDronesBv(apiary)
#'
#' @export
getCasteBv <- function(x, caste, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- bv(pop = tmp, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteBv(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                        simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteBv Access breeding value of the queen
#' @export
getQueensBv <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteBv(x, caste = "queen",
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteBv Access breeding values of fathers
#' @export
getFathersBv <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteBv(x, caste = "fathers", nInd = nInd,
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteBv Access breeding values of virgin queens
#' @export
getVirginQueensBv <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteBv(x, caste = "virgin_queens", nInd = nInd,
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteBv Access breeding values of workers
#' @export
getWorkersBv <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteBv(x, caste = "workers", nInd = nInd,
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteBv Access breeding values of drones
#' @export
getDronesBv <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteBv(x, caste = "drones", nInd = nInd,
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonyBv
#' @title Access breeding values of individuals in colony
#'
#' @description Level 0 function that returns breeding values of individuals in
#'   colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{bv}}
#'
#' @return list of vector of breeding values when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of vectors of breeding values when \code{x} is
#'   \code{\link{Colonies-class}}, outer list is named by colony id when
#'   \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' try(getColonyBv(colony1)) # TODO
#' try(getColonyBv(colony1, caste = c("queen", "fathers"))) # TODO
#' try(getColonyBv(colony1, nInd = 1)) # TODO
#' try(getColonyBv(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))) # TODO
#'
#' try(getColonyBv(colony2)) # TODO
#'
#' apiary <- c(colony1, colony2)
#' try(getColonyBv(apiary)) # TODO
#' try(getColonyBv(apiary, caste = c("queen", "fathers"))) # TODO
#' try(getColonyBv(apiary, nInd = 1)) # TODO
#' try(getColonyBv(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))) # TODO
#'
#' @export
getColonyBv <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                        nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- bv(pop = tmp, simParam = simParamBee)
      }
    }
    # TODO: should we add colony node here too or will that be done elsewhere?
    #       we might need some theoretical development first to derive it first!
    #       see also https://github.com/HighlanderLab/SIMplyBee/issues/29
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyBv(x = x@colonies[[colony]], caste = caste,
                                   nInd = nInd, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCasteDd
#' @title Access dominance deviations of individuals in a caste
#'
#' @description Level 0 function that returns dominance deviations of
#'   individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virgin_queens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{dd}}
#'
#' @return vector of dominance deviations when \code{x} is
#'   \code{\link{Colony-class}} and list of vectors of dominance deviations when
#'   \code{x} is \code{\link{Colonies-class}}, named by colony id when \code{x}
#'   is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#'
#' try(getCasteDd(colony1, caste = "queen")) # TODO
#' try(getQueensDd(colony1)) # TODO
#'
#' getCasteDd(colony1, caste = "fathers")
#' getCasteDd(colony1, caste = "fathers", nInd = 2)
#' getCasteDd(colony1, caste = "fathers", nInd = 2)
#' getFathersDd(colony1)
#' getFathersDd(colony1, nInd = 2)
#'
#' try(getCasteDd(colony1, caste = "virgin_queens")) # TODO
#' try(getVirginQueensDd(colony1)) # TODO
#'
#' getCasteDd(colony1, caste = "workers")
#' getWorkersDd(colony1)
#'
#' getCasteDd(colony1, caste = "drones")
#' getDronesDd(colony1)
#'
#' apiary <- c(colony1, colony2)
#' try(getCasteDd(apiary, caste = "queen")) # TODO
#' try(getQueensDd(apiary)) # TODO
#'
#' getCasteDd(apiary, caste = "fathers")
#' getCasteDd(apiary, caste = "fathers", nInd = 2)
#' getCasteDd(apiary, caste = "fathers", nInd = 2)
#' getFathersDd(apiary)
#' getFathersDd(apiary, nInd = 2)
#'
#' try(getCasteDd(apiary, caste = "virgin_queens")) # TODO
#' try(getVirginQueensDd(apiary)) # TODO
#'
#' getCasteDd(apiary, caste = "workers")
#' getWorkersDd(apiary)
#'
#' getCasteDd(apiary, caste = "drones")
#' getDronesDd(apiary)
#'
#' @export
getCasteDd <- function(x, caste, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCaste(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- dd(pop = tmp, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteDd(x = x@colonies[[colony]], caste = caste, nInd = nInd,
                        simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteDd Access dominance deviation of the queen
#' @export
getQueensDd <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteDd(x, caste = "queen",
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteDd Access dominance deviations of fathers
#' @export
getFathersDd <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteDd(x, caste = "fathers", nInd = nInd,
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteDd Access dominance deviations of virgin queens
#' @export
getVirginQueensDd <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteDd(x, caste = "virgin_queens", nInd = nInd,
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteDd Access dominance deviations of workers
#' @export
getWorkersDd <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteDd(x, caste = "workers", nInd = nInd,
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteDd Access dominance deviations of drones
#' @export
getDronesDd <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteDd(x, caste = "drones", nInd = nInd,
                      simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname getColonyDd
#' @title Access dominance deviations of individuals in colony
#'
#' @description Level 0 function that returns dominance deviations of
#'   individuals in colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, a combination of "queen", "fathers", "virgin_queens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{dd}}
#'
#' @return list of vector of dominance deviations when \code{x} is
#'   \code{\link{Colony-class}} (list nodes named by caste) and list of a list
#'   of vectors of dominance deviations when \code{x} is
#'   \code{\link{Colonies-class}}, outer list is named by colony id when
#'   \code{x} is \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' try(getColonyDd(colony1)) # TODO
#' try(getColonyDd(colony1, caste = c("queen", "fathers"))) # TODO
#' try(getColonyDd(colony1, nInd = 1)) # TODO
#' try(getColonyDd(colony1, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))) # TODO
#'
#' apiary <- c(colony1, colony2)
#' try(getColonyDd(apiary)) # TODO
#' try(getColonyDd(apiary, caste = c("queen", "fathers"))) # TODO
#' try(getColonyDd(apiary, nInd = 1)) # TODO
#' try(getColonyDd(apiary, nInd = list("queen" = 1, "fathers" = 2, "virgin_queens" = 1))) # TODO
#'
#' @export
getColonyDd <- function(x, caste = c("queen", "fathers", "virgin_queens", "workers", "drones"),
                        nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    if (is.list(nInd)) {
      caste <- names(nInd)
    } else {
      if (length(nInd) > 1) {
        warning("Using only the first value of nInd!")
      }
      nIndOrig <- nInd
      nInd <- vector(mode = "list", length = length(caste))
      if (!is.null(nIndOrig)) {
        for (node in seq_along(caste)) {
          nInd[[node]] <- nIndOrig
        }
      }
      names(nInd) <- caste
    }
    ret <- vector(mode = "list", length = length(caste))
    names(ret) <- caste
    for (caste in names(ret)) {
      tmp <- getCaste(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- dd(pop = tmp, simParam = simParamBee)
      }
    }
    # TODO: should we add colony node here too or will that be done elsewhere?
    #       we might need some theoretical development first to derive it first!
    #       see also https://github.com/HighlanderLab/SIMplyBee/issues/29
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyDd(x = x@colonies[[colony]], caste = caste,
                                   nInd = nInd, simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}
