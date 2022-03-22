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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' apiary <- c(colony1, colony2)
#' nColonies(apiary)
#' nColonies(createColonies(nCol = 10))
#' @export
nColonies <- function(colonies) {
  if (!"Colonies" %in% class(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  n <- length(colonies@colonies)
  return(n)
}

#' @rdname nNULLColonies
#' @title Number of NULL colonies
#'
#' @description Level 0 function that returns the number of colonies in a
#'   colonies object that are in fact empty \code{NULL}.
#'
#' @param colonies \code{\link{Colonies-class}}
#'
#' @return integer
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' founderDrones <- createDrones(x = basePop[1:2], nInd = 10)
#' colony1 <- createColony(queen = basePop[3], fathers = founderDrones[1:10])
#' colony2 <- createColony(queen = basePop[4], fathers = founderDrones[11:20])
#' apiary <- c(colony1, colony2)
#' getId(apiary)
#'
#' nColonies(apiary)
#' nColonies(selectColonies(apiary, ID = c("3", "4")))
#' nColonies(selectColonies(apiary, ID = c("4", "5")))
#' nColonies(selectColonies(apiary, ID = c("5", "6")))
#'
#' nNULLColonies(apiary)
#' nNULLColonies(selectColonies(apiary, ID = c("3", "4")))
#' nNULLColonies(selectColonies(apiary, ID = c("4", "5")))
#' nNULLColonies(selectColonies(apiary, ID = c("5", "6")))
#' @export
nNULLColonies <- function(colonies) {
  if (!"Colonies" %in% class(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  n <- sum(sapply(X = colonies@colonies, FUN = is.null))
  return(n)
}

#' @rdname nCaste
#' @title Level 0 function that returns the number of individuals of a caste in a
#'   colony
#'
#' @description Returns the number of individuals of a caste in a colony
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virginQueens", "workers",
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 100)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 3)
#' colony2 <- addWorkers(colony2, nInd = 200)
#' nCaste(colony1, caste = "queen")
#' nCaste(colony1, caste = "fathers")
#' nCaste(colony1, caste = "virginQueens")
#' nCaste(colony1, caste = "workers")
#' nCaste(colony1, caste = "drones")
#' nCaste(colony1, caste = "all")
#'
#' apiary <- c(colony1, colony2)
#' nCaste(apiary, caste = "queen")
#' nCaste(apiary, caste = "fathers")
#' nCaste(apiary, caste = "virginQueens")
#' nCaste(apiary, caste = "workers")
#' nCaste(apiary, caste = "drones")
#' nCaste(apiary, caste = "all")
#' @export
nCaste <- function(x, caste = "all") {
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "virginQueens", "workers", "drones")
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:8])
#' nQueens(colony1)
#' nQueens(colony2)
#' colony2 <- removeQueen(colony2)
#' nQueens(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nQueens(apiary)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:8])
#' nFathers(colony1)
#' nFathers(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nFathers(apiary)
#' @export
nFathers <- function(x) {
  if (isPop(x)) {
    if (any(!(isVirginQueen(x) | isQueen(x)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addVirginQueens(colony1, nInd = 3)
#' colony2 <- addVirginQueens(colony2, nInd = 5)
#' nVirginQueens(colony1)
#' nVirginQueens(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nVirginQueens(apiary)
#' @export
nVirginQueens <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- nCaste(x, caste = "virginQueens")
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 100)
#' colony2 <- addWorkers(colony2, nInd = 200)
#' nWorkers(colony1)
#' nWorkers(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nWorkers(apiary)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addDrones(colony1, nInd = 100)
#' colony2 <- addDrones(colony2, nInd = 200)
#' nDrones(colony1)
#' nDrones(colony2)
#'
#' apiary <- c(colony1, colony2)
#' nDrones(apiary)
#' @export
nDrones <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- nCaste(x, caste = "drones")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname computeQueensPHomBrood
#' @title Theoretical percentage of homozygous brood of a queen
#'
#' @description Level 0 function that returns the theoretical or
#' expected percentage of homozygous brood per queen. The percentage
#' is computed based on the csd allele of the queen and the drones
#' the queen mated with (the fathers).
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#'
#' @return numeric, theoretical homozygosity named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#'
#' # Mated queen
#' computeQueensPHomBrood(colony1@queen)
#'
#' # Colony
#' computeQueensPHomBrood(colony1)
#'
#' # Colonies
#' apiary <- c(colony1, colony2)
#' computeQueensPHomBrood(apiary)
#' @export

computeQueensPHomBrood <- function(x) {
  if (isPop(x)) {
    ret <- rep(x = NA, times = nInd(x))
    for (ind in seq_len(nInd(x))) {
      if (is.null(x@misc[[ind]]$fathers)) {
        ret[ind] <- NA
      } else {
        queensCsd <- apply(getCsdAlleles(x), MARGIN = 1, FUN = function(x) paste0(x, collapse = ""))
        fathersCsd <- apply(getCsdAlleles(x@misc[[ind]]$fathers), MARGIN = 1, FUN = function(x) paste0(x, collapse = ""))
        ret[ind] <- sum(fathersCsd %in% queensCsd) / (length(queensCsd) * length(fathersCsd))
      }
    }
  } else if (isColony(x)) {
    ret <- computeQueensPHomBrood(x = x@queen)
  } else if (isColonies(x)) {
    ret <- sapply(X = x@colonies, FUN = computeQueensPHomBrood)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop or Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname pHomBrood
#' @title Theoretical percentage of homozygous brood of a queen
#'
#' @description Level 0 function that returns the theoretical percentage of
#' homozygous brood of a queen in a colony.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#' TODO: describe queen's and colony's pHomBrood
#'   https://github.com/HighlanderLab/SIMplyBee/issues/80
#'   https://github.com/HighlanderLab/SIMplyBee/issues/104
#'
#' @return numeric, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#'
#' # Virgin queen
#' try(pHomBrood(basePop[2]))
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
#' @export
pHomBrood <- function(x) {
  if (isPop(x)) {
    if (any(!isQueen(x))) {
      stop("Individuals in x must be queens!")
    }
    ret <- rep(x = NA, times = nInd(x))
    for (ind in seq_len(nInd(x))) {
      if (!is.null(x@misc[[ind]]$pHomBrood)) {
        ret[ind] <- x@misc[[ind]]$pHomBrood
      }
    }
  } else if (isColony(x)) {
    # TODO: report colony, not queen pHomBrood
    # https://github.com/HighlanderLab/SIMplyBee/issues/80
    # https://github.com/HighlanderLab/SIMplyBee/issues/104
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

#' @rdname nHomBrood
#' @title Total realised number of homozygous brood produced by a queen.
#'
#' @description Level 0 function that returns the total number of
#' homozygous brood of a queen in a colony (these are non viable
#' individuals, only their number is stored)
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#'
#' @return numeric, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#'
#' # Virgin queen
#' try(nHomBrood(basePop[2]))
#'
#' # Mated queen
#' nHomBrood(crossVirginQueen(pop = basePop[2], fathers = drones[1:5]))
#'
#' # Queen of the colony
#' nHomBrood(getQueen(colony1))
#'
#' # Colony
#' nHomBrood(colony1)
#'
#' # Colonies
#' apiary <- c(colony1, colony2)
#' nHomBrood(apiary)
#' @export
nHomBrood <- function(x) {
  if (isPop(x)) {
    if (any(!isQueen(x))) {
      stop("Individuals in x must be queens!")
    }
    ret <- rep(x = NA, times = nInd(x))
    for (ind in seq_len(nInd(x))) {
      if (!is.null(x@misc[[ind]]$nHomBrood)) {
        ret[ind] <- x@misc[[ind]]$nHomBrood
      }
    }
  } else if (isColony(x)) {
    if (is.null(x@queen@misc[[1]]$nHomBrood)) {
      ret <- NA
    } else {
      ret <- x@queen@misc[[1]]$nHomBrood
    }
  } else if (isColonies(x)) {
    ret <- sapply(X = x@colonies, FUN = nHomBrood)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname isQueen
#' @title Is individual a queen
#'
#' @description Level 0 function that tests if individuals are queens
#'
#' @param x \code{\link{Pop-class}}
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addWorkers(colony)
#' colony <- addDrones(colony)
#' colony <- addVirginQueens(colony)
#'
#' isQueen(getQueen(colony))
#' isQueen(getWorkers(colony, nInd = 2))
#' isQueen(getDrones(colony, nInd = 2))
#' isQueen(getVirginQueens(colony, nInd = 2))
#'
#' bees <- c(
#'   getQueen(colony),
#'   getWorkers(colony, nInd = 2),
#'   getDrones(colony, nInd = 2),
#'   getVirginQueens(colony, nInd = 2)
#' )
#' isQueen(bees)
#' @export
isQueen <- function(x) {
  if (isPop(x)) {
    ret <- sapply(
      X = x@misc,
      FUN = function(z) {
        ifelse(test = is.null(z$caste),
          yes = FALSE,
          no = z$caste == "Q"
        )
      }
    )
  } else {
    stop("Argument x must be a Pop class object!")
  }
  return(ret)
}

#' @rdname isDrone
#' @title Is individual a drone
#'
#' @description Level 0 function that tests if individuals are drones
#'
#' @param x \code{\link{Pop-class}}
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addWorkers(colony)
#' colony <- addDrones(colony)
#' colony <- addVirginQueens(colony)
#'
#' isDrone(getQueen(colony))
#' isDrone(getWorkers(colony, nInd = 2))
#' isDrone(getDrones(colony, nInd = 2))
#' isDrone(getVirginQueens(colony, nInd = 2))
#'
#' bees <- c(
#'   getQueen(colony),
#'   getWorkers(colony, nInd = 2),
#'   getDrones(colony, nInd = 2),
#'   getVirginQueens(colony, nInd = 2)
#' )
#' isDrone(bees)
#' @export
isDrone <- function(x) {
  if (isPop(x)) {
    ret <- sapply(
      X = x@misc,
      FUN = function(z) {
        ifelse(test = is.null(z$caste),
          yes = FALSE,
          no = z$caste == "D"
        )
      }
    )
  } else {
    stop("Argument x must be a Pop class object!")
  }
  return(ret)
}

#' @rdname isWorker
#' @title Is individual a worker
#'
#' @description Level 0 function that tests if individuals are workers
#'
#' @param x \code{\link{Pop-class}}
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addWorkers(colony)
#' colony <- addDrones(colony)
#' colony <- addVirginQueens(colony)
#'
#' isWorker(getQueen(colony))
#' isWorker(getWorkers(colony, nInd = 2))
#' isWorker(getDrones(colony, nInd = 2))
#' isWorker(getVirginQueens(colony, nInd = 2))
#'
#' bees <- c(
#'   getQueen(colony),
#'   getWorkers(colony, nInd = 2),
#'   getDrones(colony, nInd = 2),
#'   getVirginQueens(colony, nInd = 2)
#' )
#' isWorker(bees)
#' @export
isWorker <- function(x) {
  if (isPop(x)) {
    ret <- sapply(
      X = x@misc,
      FUN = function(z) {
        ifelse(test = is.null(z$caste),
          yes = FALSE,
          no = z$caste == "W"
        )
      }
    )
  } else {
    stop("Argument x must be a Pop class object!")
  }
  return(ret)
}

#' @rdname isVirginQueen
#' @title Is individual a virgin queen
#'
#' @description Level 0 function that tests if individuals are virgin queens
#'
#' @param x \code{\link{Pop-class}}
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addWorkers(colony)
#' colony <- addDrones(colony)
#' colony <- addVirginQueens(colony)
#'
#' isVirginQueen(getQueen(colony))
#' isVirginQueen(getWorkers(colony, nInd = 2))
#' isVirginQueen(getDrones(colony, nInd = 2))
#' isVirginQueen(getVirginQueens(colony, nInd = 2))
#'
#' bees <- c(
#'   getQueen(colony),
#'   getWorkers(colony, nInd = 2),
#'   getDrones(colony, nInd = 2),
#'   getVirginQueens(colony, nInd = 2)
#' )
#' isVirginQueen(bees)
#' @export
isVirginQueen <- function(x) {
  if (isPop(x)) {
    ret <- sapply(
      X = x@misc,
      FUN = function(z) {
        ifelse(test = is.null(z$caste),
          yes = FALSE,
          no = z$caste == "V"
        )
      }
    )
  } else {
    stop("Argument x must be a Pop class object!")
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' @export
areVirginQueensPresent <- function(x) {
  if (isColony(x)) {
    ret <- !is.null(x@virginQueens)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony3 <- createColony(virginQueen = basePop[4])
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
#' @export
isQueenMated <- function(x) {
  if (isPop(x)) {
    if (nInd(x) > 0) {
      if (!all(isQueen(x) | isVirginQueen(x))) {
        stop("Individuals in x must be virgin queens or queens!")
      }
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' @export
getQueensYearOfBirth <- function(x) {
  if (isPop(x)) {
    if (any(!(isVirginQueen(x) | isQueen(x)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' @export
getQueensAge <- function(x, currentYear) {
  if (isPop(x)) {
    if (any(!(isVirginQueen(x) | isQueen(x)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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

#' @rdname getCasteId
#'
#' @title Get IDs of individuals of a caste, or ID of all members of colony
#'
#' @description Level 0 function that returns the ID individuals of a caste. To
#'   get the individuals, use \code{\link{getCaste}}.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virginQueens", "workers",
#'   "drones", or "all"
#'
#' @seealso \code{\link{getCaste}}
#'
#' @return when \code{x} is \code{\link{Pop-class}} for \code{caste != "all"}
#'  or list for \code{caste == "all"} with ID nodes named by caste;
#'    when \code{x} is \code{\link{Colony-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"}
#'   or named list for \code{caste == "all"} indluding caste members IDs;
#'    when \code{x} is \code{\link{Colonies-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"} or named list of lists of
#'   \code{\link{Pop-class}} for \code{caste == "all"} indluding caste members IDs

#'
#' @seealso \code{\link{getCaste}} and \code{\link{caste}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#'
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 4)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addWorkers(colony2, nInd = 20)
#'
#' apiary = c(colony1, colony2)
#'
#' getCasteId(x = drones)
#' getCasteId(x = colony1)
#' getCasteId(x = colony1, caste = "workers")
#' getCasteId(x = apiary)
#' getCasteId(x = apiary, caste = "virginQueens")
#'
#' # Create a data.frame with id, colony, and caste information
#' (tmpC <- getCaste(colony1))
#' (tmpI <- getCasteId(colony1))
#' tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI))
#' head(tmp);tail(tmp)
#'
#' (tmpC <- getCaste(apiary))
#' (tmpI <- getCasteId(apiary))
#' (tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI)))
#' tmp$colony <- sapply(X = strsplit(x = rownames(tmp), split = ".",
#'                                   fixed = TRUE),
#'                      FUN = function(z) z[[1]])
#' head(tmp);tail(tmp)
#'
#' @export
getCasteId <- function(x, caste = "all") {
  if (isPop(x)) {
    ret <- x@id
  } else if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "virginQueens", "workers", "drones")
      for (caste in names(ret)) {
        tmp <- getCastePop(x = x, caste = caste)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp@id
        }
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- tmp@id
      }
    }
  } else if (isColonies(x)) {
    fun <- ifelse(caste == "all", lapply, sapply)
    ret <- fun(X = x@colonies, FUN = getCasteId, caste = caste)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCaste
#' @title Report caste of an individual
#'
#' @description Level 0 function that reports caste of an individual
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#'
#' @return When x is \code{\link{Pop-class}}, character of caste status; if you
#'   get \code{NA} note that this is not supposed to happen. When x is
#'   \code{\link{Colony-class}}, list with character vectors (list is named with
#'   caste). When x is \code{\link{Colonies-class}}, list of lists with
#'   character vectors (list is named with colony id).
#'
#' @seealso \code{\link{getCaste}} and \code{\link{getCasteId}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[1:5])
#' colony1 <- addWorkers(colony1)
#' colony2 <- addWorkers(colony2)
#' colony1 <- addDrones(colony1)
#' colony2 <- addDrones(colony2)
#' colony1 <- addVirginQueens(colony1)
#' apiary <- c(colony1, colony2)
#'
#' getCaste(getQueen(colony1))
#' getCaste(getFathers(colony1))
#' getCaste(getWorkers(colony1))
#' getCaste(getDrones(colony1))
#' getCaste(getVirginQueens(colony1))
#'
#' bees <- c(getQueen(colony1),
#'           getFathers(colony1, nInd = 2),
#'           getWorkers(colony1, nInd = 2),
#'           getDrones(colony1, nInd = 2),
#'           getVirginQueens(colony1, nInd = 2))
#' getCaste(bees)
#'
#' # Create a data.frame with id, colony, and caste information
#' (tmpC <- getCaste(colony1))
#' (tmpI <- getCasteId(colony1))
#' tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI))
#' head(tmp);tail(tmp)
#'
#' (tmpC <- getCaste(apiary))
#' (tmpI <- getCasteId(apiary))
#' (tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI)))
#' tmp$colony <- sapply(X = strsplit(x = rownames(tmp), split = ".",
#'                                   fixed = TRUE),
#'                      FUN = function(z) z[[1]])
#' head(tmp);tail(tmp)
#'
#' @export
getCaste <- function(x) {
  if (isPop(x)) {
    ret <- sapply(X = x@misc,
                  FUN = function(z) {
                    ifelse(test = is.null(z$caste),
                           yes = NA,
                           no = z$caste)
                  })
    ret <- factor(x = ret, levels = c("Q", "F", "W", "D", "V"),
                  labels = list("queen", "fathers", "workers", "drones", "virginQueens"))
    ret <- as.character(ret)
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "virginQueens", "workers", "drones")
    for (caste in names(ret)) {
      tmp <- getCastePop(x = x, caste = caste)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCaste(tmp)
      }
    }
  } else if (isColonies(x)) {
    ret <- lapply(X = x@colonies, FUN = caste)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' @export
getLocation <- function(x) {
  if (isColony(x)) {
    if (is.null(x@location)) {
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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

#' @rdname getEvents
#'
#' @title Report which colony events have occurred
#'
#' @description Level 0 function that returns a matrix of logicals reporting the
#'   status of the colony events. The events are: split, swarm, supersedure,
#'   collapse, and production. These events impact colony status, strength, and
#'   could also impact downstream phenotypes.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#'
#' @return matrix of logicals, named by colony id when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#'
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 4)
#' colony1 <- addDrones(colony1, nInd = 2)
#' getEvents(colony1)
#'
#' colony1 <- supersedeColony(colony1)
#' getEvents(colony1)
#'
#' colony2 <- addWorkers(colony2, nInd = 20)
#' apiary <- c(colony1, colony2)
#' getEvents(apiary)
#' @export
#'
getEvents <- function(x) {
  if (!isColony(x) & !isColonies(x)) {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  ret <- cbind(hasSplit(x), hasSwarmed(x), hasSuperseded(x), hasCollapsed(x), isProductive(x))
  colnames(ret) <- c("split", "swarmed", "superseded", "collapsed", "productive")
  if (isColonies(x)) {
    rownames(ret) <- getId(x)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' founderGenomes <- simulateHoneyBeeGenomes(
#'   nInd = 10, nChr = 1,
#'   nSegSites = 2, Ne = 10
#' )
#' @export
simulateHoneyBeeGenomes <- function(nMelN = 0L,
                                    nMelS = 0L,
                                    nCar = 0L,
                                    nLig = 0L,
                                    ploidy = 2L,
                                    nChr = 16L,
                                    nSegSites = 100L,
                                    Ne = 170000L, # Wallberg et al. (2014)
                                    nBp = 2.252e+8 / 16, # GenBank Amel_Hv3.1
                                    genLen = 3.199121, # Beye et al., 2006
                                    mutRate = 3.4e-9, # Yang et al. (2015)
                                    recRate = 2.3e-7, # Beye et al., 2006
                                    nThreads = NULL) {
  if (ploidy != 1) {
    nMelN <- nMelN * ploidy
    nMelS <- nMelS * ploidy
    nCar <- nCar * ploidy
    nLig <- nLig * ploidy
  }

  nInd <- (nMelN + nMelS + nCar + nLig) / 2
  mu <- 4 * Ne * mutRate
  rho <- 4 * Ne * recRate

  command <- paste0(
    nBp, " -t ", mu, " -r ", rho, " -I 4 ", nMelS, " ", nMelN, " ", nCar, " ", nLig,
    " -ej 0.01912 2 1 -en 0.01913 1 1.235 -ej 0.03676 4 3 -en 0.03677 3 1.118 -ej 0.44118 3 1 -en 0.44119 1 2.059"
  )


  founderGenomes <- runMacs(
    nInd = nInd,
    nChr = nChr,
    segSites = nSegSites,
    species = "GENERIC",
    nThreads = nThreads,
    manualCommand = command,
    manualGenLen = genLen
  )
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
#' @export
isCsdActive <- function(simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- !is.null(simParamBee$csdChr)
  return(ret)
}

#' @rdname reduceDroneHaplo
#' @title Reduce drone's double haplotypes to a single haplotype
#'
#' @description Level 0 function that returns one haplotype of drones, because
#'   we internally simulate them as diploid (doubled haploid). This is a utility
#'   function that you likely don't need to use.
#'
#' @param haplo \code{\link{matrix-class}}
#' @param pop \code{\link{Pop-class}}
#'
#' @details While this function is meant to work on male (drone) haplotypes, we
#'   handle cases where the \code{haplo} matrix contains male and female
#'   haplotypes, which is why you need to provide \code{pop}. We only reduce
#'   haplotypes for males though.
#'
#' @return matrix with one haplotype per drone instead of two
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#' drones <- createDrones(x = basePop[1], nInd = 2)
#'
#' (tmp <- getSegSiteHaplo(drones))
#' SIMplyBee:::reduceDroneHaplo(haplo = tmp, pop = drones)
#'
#' (tmp <- getSegSiteHaplo(c(basePop, drones)))
#' SIMplyBee:::reduceDroneHaplo(haplo = tmp, pop = c(basePop, drones))
#'
#' @export
reduceDroneHaplo <- function(haplo, pop) {
  if (!is.matrix(haplo)) {
    stop("Argument haplo must be a matrix class object!")
  }
  if (!isPop(pop)) {
    stop("Argument pop must be a matrix class object!")
  }
  idHap <- rownames(haplo)
  id <- sapply(X = strsplit(x = idHap, split = "_"), FUN = function(z) z[[1]])
  idSelF <- id %in% pop@id[pop@sex == "F"]
  idSelM <- id %in% pop@id[pop@sex == "M"]
  sel <- idSelF | (idSelM & grepl(x = idHap, pattern = "_1"))
  ret <- haplo[sel, ]
  return(ret)
}

#' @rdname reduceDroneGeno
#' @title Reduce drones' genotype to a single haplotype
#'
#' @description Level 0 function that reduces drone's genotype to a single
#'   haplotype, because we internally simulate them as diploid (doubled
#'   haploid). This is a utility function that you likely don't need to use.
#'
#' @param geno \code{\link{matrix-class}}
#' @param pop \code{\link{Pop-class}}
#'
#' @return matrix with genotype as one haplotype per drone instead of two
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#' drones <- createDrones(x = basePop[1], nInd = 2)
#'
#' (tmp <- getSegSiteGeno(drones))
#' SIMplyBee:::reduceDroneGeno(geno = tmp, pop = drones)
#'
#' (tmp <- getSegSiteGeno(c(basePop, drones)))
#' SIMplyBee:::reduceDroneGeno(geno = tmp, pop = c(basePop, drones))
#'
#' @export
reduceDroneGeno <- function(geno, pop) {
  if (!is.matrix(geno)) {
    stop("Argument geno must be a matrix class object!")
  }
  if (!isPop(pop)) {
    stop("Argument pop must be a matrix class object!")
  }
  id <- rownames(geno)
  sel <- id %in% pop@id[pop@sex == "M"]
  if (any(sel)) {
    geno[sel, ] <- geno[sel, ] / 2
  }
  return(geno)
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
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if \code{TRUE}, the function will return a set of
#'   csd alleles in either the entire population, colony, or colonies. Default
#'   is \code{FALSE}.
#' @param unique logical, return only the unique set of csd alleles. Default
#'   is \code{FALSE}.
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details  If both collapse and unique are \code{TRUE}, the function
#'   returns a unique set of csd alleles in the entire population, colony, or
#'   colonies.
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' apiary <- c(colony1, colony2)
#'
#' getCsdAlleles(getQueen(colony1))
#' getCsdAlleles(getFathers(colony1))
#' getCsdAlleles(getWorkers(colony1))
#' getCsdAlleles(getDrones(colony1))
#'
#' getCsdAlleles(colony1)
#' getCsdAlleles(colony1, unique = TRUE)
#' getCsdAlleles(colony1, collapse = TRUE)
#' getCsdAlleles(colony1, collapse = TRUE, unique = TRUE)
#'
#' getCsdAlleles(getDrones(colony2))
#' getCsdAlleles(colony2)
#'
#' getCsdAlleles(apiary)
#' getCsdAlleles(apiary, unique = TRUE)
#' getCsdAlleles(apiary, collapse = TRUE)
#' getCsdAlleles(apiary, collapse = TRUE, unique = TRUE)
#'
#' getCsdAlleles(apiary, nInd = 2)
#' @export
getCsdAlleles <- function(x, nInd = NULL, allele = "all", dronesHaploid = TRUE,
                          collapse = FALSE, unique = FALSE, simParamBee = NULL) {
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
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneHaplo(haplo = ret, pop = x)
    }
    if (unique) {
      ret <- ret[!duplicated(x = ret), , drop = FALSE]
    }
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "virginQueens", "workers", "drones")
    for (caste in names(ret)) {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCsdAlleles(x = tmp, allele = allele,
                                      dronesHaploid = dronesHaploid,
                                      unique = unique,
                                      simParamBee = simParamBee)
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
      if (unique) {
        ret <- ret[!duplicated(ret), , drop = FALSE]
      }
    }
  } else if (isColonies(x)) {
    ret <- lapply(X = x@colonies, FUN = getCsdAlleles, nInd = nInd,
                  allele = allele, dronesHaploid = dronesHaploid,
                  collapse = collapse, unique = unique,
                  simParamBee = simParamBee)
    if (collapse) {
      ret <- do.call("rbind", ret)
      if (unique) {
        ret <- ret[!duplicated(ret), , drop = FALSE]
      }
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname getCsdGeno
#' @title Get genotypes from the csd locus
#'
#' @description Level 0 function that returns genotypes from the csd locus. See
#'   \code{\link{SimParamBee}} for more information about the csd locus and how
#'   we have implemented it.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#' @param nInd numeric, for how many individuals; if \code{NULL} all individuals
#'   are taken; this can be useful as a test of sampling individuals
#' @param dronesHaploid logical, return haploid result for drones?
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details The returned genotypes are spanning multiple bi-allelic SNP of
#'   a non-recombining csd locus / haplotype. In most cases you will want to use
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
#' apiary <- c(colony1, colony2)
#'
#' getCsdGeno(getQueen(colony1))
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
#' @export
getCsdGeno <- function(x, nInd = NULL, dronesHaploid = TRUE,
                       simParamBee = NULL) {
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
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneGeno(geno = ret, pop = x)
    }
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "virginQueens", "workers", "drones")
    for (caste in names(ret)) {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCsdGeno(x = tmp, dronesHaploid = dronesHaploid,
                                   simParamBee = simParamBee)
      }
    }
  } else if (isColonies(x)) {
    ret <- lapply(X = x@colonies, FUN = getCsdGeno, nInd = nInd,
                  dronesHaploid = dronesHaploid, simParamBee = simParamBee)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony1 <- createColony(queen = basePop[2], fathers = drones)
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 10)
#'
#' isCsdHeterozygous(getQueen(colony1))
#'
#' isCsdHeterozygous(getWorkers(colony1))
#'
#' isCsdHeterozygous(getVirginQueens(colony1))
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
#' @param collapse logical, if \code{TRUE}, the function will return the number
#'   of distinct csd alleles in either the entire population, colony, or
#'   colonies. Default is \code{FALSE}.
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details Queen has 2 distinct csd alleles, since she has to be heterozygous
#'   to be viable. The same holds for individual virgin queens and workers, but
#'   note that looking at csd genotypes of virgin queens or workers we are
#'   looking at a sample of 1 csd allele from the queen and 1 csd allele from
#'   their fathers, noting that homozygous genotypes are excluded. Therefore,
#'   \code{nCsdAlleles()} from virgin queens and workers is a noisy realisation
#'   of \code{nCsdAlleles()} from queens and fathers. For this reason, we also
#'   report \code{nCsdAlleles()} from queens and fathers combined (see the
#'   \code{queenAndFathers} list node) when \code{x} is
#'   \code{\link{Colony-class}}. This last measure is then the expected number
#'   of csd alleles in a colony as opposed to realised number of csd alleles in
#'   a sample of virgin queens and workers. Similarly as for virgin queens and
#'   workers, \code{nCsdAlleles()} from drones gives a noisy realisation of
#'   \code{nCsdAlleles()} from queens. The amount of noise will depend on the
#'   number of individuals, so in most cases with reasonable number of
#'   individuals there should be minimal amount of noise.
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' nCsdAlleles(colony1, collapse = TRUE)
#'
#' nCsdAlleles(apiary)
#' nCsdAlleles(apiary, collapse = TRUE)
#' @export
nCsdAlleles <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(x)) {
    ret <- NA
  } else if (isPop(x)) {
    haplo <- getCsdAlleles(x = x, unique = TRUE, simParamBee = simParamBee)
    ret <- nrow(haplo)
  } else if (isColony(x)) {
    if (collapse) {
      haplo <- getCsdAlleles(x = x, collapse = TRUE, unique = TRUE, simParamBee = simParamBee)
      ret <- nrow(haplo)
    } else {
      ret <- vector(mode = "list", length = 6)
      names(ret) <- c("queen", "fathers", "queenAndFathers", "virgin_queens", "workers", "drones")
      ret$queen <- nCsdAlleles(x = getQueen(x), simParamBee = simParamBee)
      ret$fathers <- nCsdAlleles(x = getFathers(x), simParamBee = simParamBee)
      ret$virgin_queens <- nCsdAlleles(x = getVirginQueens(x), simParamBee = simParamBee)
      ret$workers <- nCsdAlleles(x = getWorkers(x), simParamBee = simParamBee)
      ret$drones <- nCsdAlleles(x = getDrones(x), simParamBee = simParamBee)
      # Can't combine queen (diploid) and fathers (haploid) using c(getQueen(x), getFathers(x)),
      #   so we will get their alleles and count them
      tmp <- rbind(
        getCsdAlleles(x = getQueen(x), simParamBee = simParamBee),
        getCsdAlleles(x = getFathers(x), simParamBee = simParamBee)
      )
      tmp <- tmp[!duplicated(tmp), , drop = FALSE]
      ret$queenAndFathers <- nrow(tmp)
    }
  } else if (isColonies(x)) {
    if (collapse) {
      haplo <- getCsdAlleles(x = x, collapse = TRUE, unique = TRUE, simParamBee = simParamBee)
      ret <- nrow(haplo)
    } else {
      ret <- lapply(X = x@colonies, FUN = nCsdAlleles, collapse = collapse, simParamBee = simParamBee)
      names(ret) <- getId(x)
    }
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' SP <- SimParam$new(founderGenomes)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
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
#' getCasteIbdHaplo(colony1, caste = "virginQueens")
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
#' getCasteIbdHaplo(apiary, caste = "virginQueens")
#' getVirginQueensIbdHaplo(apiary)
#'
#' getCasteIbdHaplo(apiary, caste = "workers")
#' getWorkersIbdHaplo(apiary)
#'
#' getCasteIbdHaplo(apiary, caste = "drones")
#' getDronesIbdHaplo(apiary)
#' @export
getCasteIbdHaplo <- function(x, caste, nInd = NULL, chr = NULL,
                             dronesHaploid = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getIbdHaplo(pop = tmp, chr = chr, simParam = simParamBee)
      if (dronesHaploid && any(tmp@sex == "M")) {
        ret <- reduceDroneHaplo(haplo = ret, pop = tmp)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteIbdHaplo(x = x[[colony]], caste = caste, nInd = nInd,
                              chr = chr, dronesHaploid = dronesHaploid,
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

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of the queen
#' @export
getQueensIbdHaplo <- function(x, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x,
      caste = "queen",
      chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of fathers
#' @export
getFathersIbdHaplo <- function(x, nInd = NULL, chr = NULL, dronesHaploid = TRUE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x, caste = "fathers", nInd = nInd, chr = chr,
                            dronesHaploid = dronesHaploid,
                            simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of virgin queens
#' @export
getVirginQueensIbdHaplo <- function(x, nInd = NULL, chr = NULL,
                                    simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x,
      caste = "virginQueens", nInd = nInd,
      chr = chr, simParamBee = simParamBee
    )
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
    ret <- getCasteIbdHaplo(x,
      caste = "workers", nInd = nInd,
      chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of drones
#' @export
getDronesIbdHaplo <- function(x, nInd = NULL, chr = NULL, dronesHaploid = TRUE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteIbdHaplo(x, caste = "drones", nInd = nInd, chr = chr,
                            dronesHaploid = dronesHaploid,
                            simParamBee = simParamBee)
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyIbdHaplo(colony1)
#' getColonyIbdHaplo(colony1, caste = c("queen", "fathers"))
#' getColonyIbdHaplo(colony1, nInd = 1)
#' getColonyIbdHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonyIbdHaplo(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyIbdHaplo(apiary)
#' getColonyIbdHaplo(apiary, caste = c("queen", "fathers"))
#' getColonyIbdHaplo(apiary, nInd = 1)
#' getColonyIbdHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonyIbdHaplo <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
                              nInd = NULL, chr = NULL, dronesHaploid = TRUE,
                              simParamBee = NULL) {
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getIbdHaplo(pop = tmp, chr = chr,
                                    simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret[[caste]] <- reduceDroneHaplo(haplo = ret[[caste]], pop = tmp)
        }
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyIbdHaplo(x = x[[colony]], caste = caste,
                                         nInd = nInd, chr = chr,
                                         dronesHaploid = dronesHaploid,
                                         simParamBee = simParamBee)
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param trait numeric, indicates which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
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
#' getCasteQtlHaplo(colony1, caste = "virginQueens")
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
#' getCasteQtlHaplo(apiary, caste = "virginQueens")
#' getVirginQueensQtlHaplo(apiary)
#'
#' getCasteQtlHaplo(apiary, caste = "workers")
#' getWorkersQtlHaplo(apiary)
#'
#' getCasteQtlHaplo(apiary, caste = "drones")
#' getDronesQtlHaplo(apiary)
#' @export
getCasteQtlHaplo <- function(x, caste, nInd = NULL,
                             trait = 1, haplo = "all", chr = NULL,
                             dronesHaploid = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getQtlHaplo(pop = tmp, haplo = haplo, trait = trait, chr = chr, simParam = simParamBee)
      if (dronesHaploid && any(tmp@sex == "M")) {
        ret <- reduceDroneHaplo(haplo = ret, pop = tmp)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteQtlHaplo(x = x[[colony]], caste = caste, nInd = nInd,
                              trait = trait, haplo = haplo, chr = chr,
                              dronesHaploid = dronesHaploid,
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

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of the queen
#' @export
getQueensQtlHaplo <- function(x,
                              trait = 1, haplo = "all", chr = NULL,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "queen",
                            trait = trait, haplo = haplo, chr = chr,
                            simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of fathers
#' @export
getFathersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL,
                               dronesHaploid = TRUE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "fathers", nInd = nInd,
                            trait = trait, haplo = haplo, chr = chr,
                            dronesHaploid = dronesHaploid,
                            simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of virgin queens
#' @export
getVirginQueensQtlHaplo <- function(x, nInd = NULL,
                                    trait = 1, haplo = "all", chr = NULL,
                                    simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "virginQueens", nInd = nInd,
                            trait = trait, haplo = haplo, chr = chr,
                            simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype of workers
#' @export
getWorkersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }

  if (isColony(x)| isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "workers", nInd = nInd,
                            trait = trait, haplo = haplo, chr = chr,
                            simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of drones
#' @export
getDronesQtlHaplo <- function(x, nInd = NULL,
                              trait = 1, haplo = "all", chr = NULL,
                              dronesHaploid = TRUE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlHaplo(x, caste = "drones", nInd = nInd,
                            trait = trait, haplo = haplo, chr = chr,
                            dronesHaploid = dronesHaploid,
                            simParamBee = simParamBee)
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
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
#' @param dronesHaploid logical, return haploid result for drones?
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyQtlHaplo(colony1)
#' getColonyQtlHaplo(colony1, caste = c("queen", "fathers"))
#' getColonyQtlHaplo(colony1, nInd = 1)
#' getColonyQtlHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonyQtlHaplo(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyQtlHaplo(apiary)
#' getColonyQtlHaplo(apiary, caste = c("queen", "fathers"))
#' getColonyQtlHaplo(apiary, nInd = 1)
#' getColonyQtlHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonyQtlHaplo <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
                              nInd = NULL, trait = 1, haplo = "all", chr = NULL,
                              dronesHaploid = TRUE,
                              simParamBee = NULL) {
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {

        ret[[caste]] <- getQtlHaplo(pop = tmp, trait = trait, haplo = haplo,
                                    chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret[[caste]] <- reduceDroneHaplo(haplo = ret[[caste]], pop = tmp)
        }
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyQtlHaplo(x = x[[colony]], caste = caste, nInd = nInd,
                                         trait = trait, haplo = haplo, chr = chr,
                                         dronesHaploid = dronesHaploid,
                                         simParamBee = simParamBee)
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param trait numeric, indicates which trait's QTL genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
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
#' getCasteQtlGeno(colony1, caste = "virginQueens")
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
#' getCasteQtlGeno(apiary, caste = "virginQueens")
#' getVirginQueensQtlGeno(apiary)
#'
#' getCasteQtlGeno(apiary, caste = "workers")
#' getWorkersQtlGeno(apiary)
#'
#' getCasteQtlGeno(apiary, caste = "drones")
#' getDronesQtlGeno(apiary)
#' @export
getCasteQtlGeno <- function(x, caste, nInd = NULL,
                            trait = 1, chr = NULL, dronesHaploid = TRUE,
                            simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getQtlGeno(pop = tmp, trait = trait, chr = chr, simParam = simParamBee)
      if (dronesHaploid && any(tmp@sex == "M")) {
        ret <- reduceDroneGeno(geno = ret, pop = tmp)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteQtlGeno(x = x[[colony]], caste = caste, nInd = nInd,
                             trait = trait, chr = chr,
                             dronesHaploid = dronesHaploid,
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

#' @describeIn getCasteQtlGeno Access QTL genotype data of the queen
#' @export
getQueensQtlGeno <- function(x,
                             trait = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlGeno(x,
      caste = "queen",
      trait = trait, chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of fathers
#' @export
getFathersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL, dronesHaploid = TRUE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlGeno(x, caste = "fathers", nInd = nInd,
                           trait = trait, chr = chr,
                           dronesHaploid = dronesHaploid,
                           simParamBee = simParamBee)
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
    ret <- getCasteQtlGeno(x,
      caste = "virginQueens", nInd = nInd,
      trait = trait, chr = chr, simParamBee = simParamBee
    )
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
    ret <- getCasteQtlGeno(x,
      caste = "workers", nInd = nInd,
      trait = trait, chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of drones
#' @export
getDronesQtlGeno <- function(x, nInd = NULL,
                             trait = 1, chr = NULL, dronesHaploid = TRUE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteQtlGeno(x, caste = "drones", nInd = nInd,
                           trait = trait, chr = chr,
                           dronesHaploid = dronesHaploid,
                           simParamBee = simParamBee)
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param trait numeric, indicates which trait's QTL genotype to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyQtlGeno(colony1)
#' getColonyQtlGeno(colony1, caste = c("queen", "fathers"))
#' getColonyQtlGeno(colony1, nInd = 1)
#' getColonyQtlGeno(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonyQtlGeno(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyQtlGeno(apiary)
#' getColonyQtlGeno(apiary, caste = c("queen", "fathers"))
#' getColonyQtlGeno(apiary, nInd = 1)
#' getColonyQtlGeno(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonyQtlGeno <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
                             nInd = NULL, trait = 1, chr = NULL,
                             dronesHaploid = TRUE, simParamBee = NULL) {
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getQtlGeno(pop = tmp, trait = trait,
                                   chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret[[caste]] <- reduceDroneGeno(geno = ret[[caste]], pop = tmp)
        }
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonyQtlGeno(x = x[[colony]], caste = caste, nInd = nInd,
                                        trait = trait, chr = chr,
                                        dronesHaploid = dronesHaploid,
                                        simParamBee = simParamBee)
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
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
#' getCasteSegSiteHaplo(colony1, caste = "virginQueens")
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
#' getCasteSegSiteHaplo(apiary, caste = "virginQueens")
#' getVirginQueensSegSiteHaplo(apiary)
#'
#' getCasteSegSiteHaplo(apiary, caste = "workers")
#' getWorkersSegSiteHaplo(apiary)
#'
#' getCasteSegSiteHaplo(apiary, caste = "drones")
#' getDronesSegSiteHaplo(apiary)
#' @export
getCasteSegSiteHaplo <- function(x, caste, nInd = NULL,
                                 haplo = "all", chr = NULL,
                                 dronesHaploid = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getSegSiteHaplo(pop = tmp, haplo = haplo, chr = chr, simParam = simParamBee)
      if (dronesHaploid && any(tmp@sex == "M")) {
        ret <- reduceDroneHaplo(haplo = ret, pop = tmp)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSegSiteHaplo(x = x[[colony]], caste = caste, nInd = nInd,
                                  haplo = haplo, chr = chr,
                                  dronesHaploid = dronesHaploid,
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

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of the queen
#' @export
getQueensSegSiteHaplo <- function(x,
                                  haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteHaplo(x,
      caste = "queen",
      haplo = haplo, chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of fathers
#' @export
getFathersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL,
                                   dronesHaploid = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteHaplo(x, caste = "fathers", nInd = nInd,
                                haplo = haplo, chr = chr,
                                dronesHaploid = dronesHaploid,
                                simParamBee = simParamBee)
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
    ret <- getCasteSegSiteHaplo(x,
      caste = "virginQueens", nInd = nInd,
      haplo = haplo, chr = chr, simParamBee = simParamBee
    )
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
    ret <- getCasteSegSiteHaplo(x,
      caste = "workers", nInd = nInd,
      haplo = haplo, chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of drones
#' @export
getDronesSegSiteHaplo <- function(x, nInd = NULL,
                                  haplo = "all", chr = NULL,
                                  dronesHaploid = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteHaplo(x, caste = "drones", nInd = nInd,
                                haplo = haplo, chr = chr,
                                dronesHaploid = dronesHaploid,
                                simParamBee = simParamBee)
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
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
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonySegSiteHaplo(colony1)
#' getColonySegSiteHaplo(colony1, caste = c("queen", "fathers"))
#' getColonySegSiteHaplo(colony1, nInd = 1)
#' getColonySegSiteHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonySegSiteHaplo(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonySegSiteHaplo(apiary)
#' getColonySegSiteHaplo(apiary, caste = c("queen", "fathers"))
#' getColonySegSiteHaplo(apiary, nInd = 1)
#' getColonySegSiteHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonySegSiteHaplo <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
                                  nInd = NULL, haplo = "all", chr = NULL,
                                  dronesHaploid = TRUE, simParamBee = NULL) {
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getSegSiteHaplo(pop = tmp, haplo = haplo,
                                        chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret[[caste]] <- reduceDroneHaplo(haplo = ret[[caste]], pop = tmp)
        }
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonySegSiteHaplo(x = x[[colony]], caste = caste, nInd = nInd,
                                             haplo = haplo, chr = chr,
                                             dronesHaploid = dronesHaploid,
                                             simParamBee = simParamBee)
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
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
#' getCasteSegSiteGeno(colony1, caste = "virginQueens")
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
#' getCasteSegSiteGeno(apiary, caste = "virginQueens")
#' getVirginQueensSegSiteGeno(apiary)
#'
#' getCasteSegSiteGeno(apiary, caste = "workers")
#' getWorkersSegSiteGeno(apiary)
#'
#' getCasteSegSiteGeno(apiary, caste = "drones")
#' getDronesSegSiteGeno(apiary)
#' @export
getCasteSegSiteGeno <- function(x, caste, nInd = NULL,
                                chr = NULL, dronesHaploid = TRUE,
                                simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getSegSiteGeno(pop = tmp, chr = chr, simParam = simParamBee)
      if (dronesHaploid && any(tmp@sex == "M")) {
        ret <- reduceDroneGeno(geno = ret, pop = tmp)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSegSiteGeno(x = x[[colony]], caste = caste, nInd = nInd,
                                 chr = chr, dronesHaploid = dronesHaploid,
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

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of the queen
#' @export
getQueensSegSiteGeno <- function(x,
                                 chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteGeno(x,
      caste = "queen",
      chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of fathers
#' @export
getFathersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, dronesHaploid = TRUE,
                                  simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteGeno(x, caste = "fathers", nInd = nInd,
                               chr = chr, dronesHaploid = dronesHaploid,
                               simParamBee = simParamBee)
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
    ret <- getCasteSegSiteGeno(x,
      caste = "virginQueens", nInd = nInd,
      chr = chr, simParamBee = simParamBee
    )
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
    ret <- getCasteSegSiteGeno(x,
      caste = "workers", nInd = nInd,
      chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of drones
#' @export
getDronesSegSiteGeno <- function(x, nInd = NULL,
                                 chr = NULL, dronesHaploid = TRUE,
                                 simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSegSiteGeno(x, caste = "drones", nInd = nInd,
                               chr = chr, dronesHaploid = dronesHaploid,
                               simParamBee = simParamBee)
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonySegSiteGeno(colony1)
#' getColonySegSiteGeno(colony1, caste = c("queen", "fathers"))
#' getColonySegSiteGeno(colony1, nInd = 1)
#' getColonySegSiteGeno(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonySegSiteGeno(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonySegSiteGeno(apiary)
#' getColonySegSiteGeno(apiary, caste = c("queen", "fathers"))
#' getColonySegSiteGeno(apiary, nInd = 1)
#' getColonySegSiteGeno(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonySegSiteGeno <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
                                 nInd = NULL, chr = NULL,
                                 dronesHaploid = TRUE, simParamBee = NULL) {
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getSegSiteGeno(pop = tmp, chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret[[caste]] <- reduceDroneGeno(geno = ret[[caste]], pop = tmp)
        }
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonySegSiteGeno(x = x[[colony]], caste = caste,
                                            nInd = nInd, chr = chr,
                                            dronesHaploid = dronesHaploid,
                                            simParamBee = simParamBee)
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
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
#' getCasteSnpHaplo(colony1, caste = "virginQueens")
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
#' getCasteSnpHaplo(apiary, caste = "virginQueens")
#' getVirginQueensSnpHaplo(apiary)
#'
#' getCasteSnpHaplo(apiary, caste = "workers")
#' getWorkersSnpHaplo(apiary)
#'
#' getCasteSnpHaplo(apiary, caste = "drones")
#' getDronesSnpHaplo(apiary)
#' @export
getCasteSnpHaplo <- function(x, caste, nInd = NULL,
                             snpChip = 1, haplo = "all", chr = NULL,
                             dronesHaploid = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getSnpHaplo(pop = tmp, haplo = haplo, snpChip = snpChip, chr = chr, simParam = simParamBee)
      if (dronesHaploid && any(tmp@sex == "M")) {
        ret <- reduceDroneHaplo(haplo = ret, pop = tmp)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSnpHaplo(x = x[[colony]], caste = caste, nInd = nInd,
                              snpChip = snpChip, haplo = haplo, chr = chr,
                              dronesHaploid = dronesHaploid,
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

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of the queen
#' @export
getQueensSnpHaplo <- function(x,
                              snpChip = 1, haplo = "all", chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpHaplo(x,
      caste = "queen",
      snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of fathers
#' @export
getFathersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL,
                               dronesHaploid = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpHaplo(x, caste = "fathers", nInd = nInd,
                            snpChip = snpChip, haplo = haplo, chr = chr,
                            dronesHaploid = dronesHaploid,
                            simParamBee = simParamBee)
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
    ret <- getCasteSnpHaplo(x,
      caste = "virginQueens", nInd = nInd,
      snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee
    )
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
    ret <- getCasteSnpHaplo(x,
      caste = "workers", nInd = nInd,
      snpChip = snpChip, haplo = haplo, chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of drones
#' @export
getDronesSnpHaplo <- function(x, nInd = NULL,
                              snpChip = 1, haplo = "all", chr = NULL,
                              dronesHaploid = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpHaplo(x, caste = "drones", nInd = nInd,
                            snpChip = snpChip, haplo = haplo, chr = chr,
                            dronesHaploid = dronesHaploid,
                            simParamBee = simParamBee)
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
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
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonySnpHaplo(colony1)
#' getColonySnpHaplo(colony1, caste = c("queen", "fathers"))
#' getColonySnpHaplo(colony1, nInd = 1)
#' getColonySnpHaplo(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonySnpHaplo(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonySnpHaplo(apiary)
#' getColonySnpHaplo(apiary, caste = c("queen", "fathers"))
#' getColonySnpHaplo(apiary, nInd = 1)
#' getColonySnpHaplo(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonySnpHaplo <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
                              nInd = NULL, snpChip = 1, haplo = "all", chr = NULL,
                              dronesHaploid = TRUE, simParamBee = NULL) {
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getSnpHaplo(pop = tmp, snpChip = snpChip, haplo = haplo,
                                    chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret[[caste]] <- reduceDroneHaplo(haplo = ret[[caste]], pop = tmp)
        }
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonySnpHaplo(x = x[[colony]], caste = caste,
                                         nInd = nInd, snpChip = snpChip,
                                         haplo = haplo, chr = chr,
                                         dronesHaploid = dronesHaploid,
                                         simParamBee = simParamBee)
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
#'   "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
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
#' getCasteSnpGeno(colony1, caste = "virginQueens")
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
#' getCasteSnpGeno(apiary, caste = "virginQueens")
#' getVirginQueensSnpGeno(apiary)
#'
#' getCasteSnpGeno(apiary, caste = "workers")
#' getWorkersSnpGeno(apiary)
#'
#' getCasteSnpGeno(apiary, caste = "drones")
#' getDronesSnpGeno(apiary)
#' @export
getCasteSnpGeno <- function(x, caste, nInd = NULL,
                            snpChip = 1, chr = NULL, dronesHaploid = TRUE,
                            simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- getSnpGeno(pop = tmp, snpChip = snpChip, chr = chr, simParam = simParamBee)
      if (dronesHaploid && any(tmp@sex == "M")) {
        ret <- reduceDroneGeno(geno = ret, pop = tmp)
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSnpGeno(x = x[[colony]], caste = caste, nInd = nInd,
                             snpChip = snpChip, chr = chr,
                             dronesHaploid = dronesHaploid,
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

#' @describeIn getCasteSnpGeno Access SNP array genotype data of the queen
#' @export
getQueensSnpGeno <- function(x,
                             snpChip = 1, chr = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpGeno(x,
      caste = "queen",
      snpChip = snpChip, chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of fathers
#' @export
getFathersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL, dronesHaploid = TRUE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpGeno(x, caste = "fathers", nInd = nInd,
                           snpChip = snpChip, chr = chr,
                           dronesHaploid = dronesHaploid,
                           simParamBee = simParamBee)
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
    ret <- getCasteSnpGeno(x,
      caste = "virginQueens", nInd = nInd,
      snpChip = snpChip, chr = chr, simParamBee = simParamBee
    )
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
    ret <- getCasteSnpGeno(x,
      caste = "workers", nInd = nInd,
      snpChip = snpChip, chr = chr, simParamBee = simParamBee
    )
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of drones
#' @export
getDronesSnpGeno <- function(x, nInd = NULL,
                             snpChip = 1, chr = NULL, dronesHaploid = TRUE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isColonies(x)) {
    ret <- getCasteSnpGeno(x, caste = "drones", nInd = nInd,
                           snpChip = snpChip, chr = chr,
                           dronesHaploid = dronesHaploid,
                           simParamBee = simParamBee)
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
#'   "workers", or "drones"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample; can be a list to
#'   access different number of different caste - when this is the case
#'   \code{nInd} takes precedence over \code{caste} (see examples)
#' @param snpChip numeric, indicates which SNP array genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
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
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
#'
#' getColonySnpGeno(colony1)
#' getColonySnpGeno(colony1, caste = c("queen", "fathers"))
#' getColonySnpGeno(colony1, nInd = 1)
#' getColonySnpGeno(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonySnpGeno(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonySnpGeno(apiary)
#' getColonySnpGeno(apiary, caste = c("queen", "fathers"))
#' getColonySnpGeno(apiary, nInd = 1)
#' getColonySnpGeno(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonySnpGeno <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
                             nInd = NULL, snpChip = 1, chr = NULL,
                             dronesHaploid = TRUE, simParamBee = NULL) {
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getSnpGeno(pop = tmp, snpChip = snpChip,
                                   chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret[[caste]] <- reduceDroneGeno(geno = ret[[caste]], pop = tmp)
        }
      }
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- getColonySnpGeno(x = x[[colony]], caste = caste, nInd = nInd,
                                        snpChip = snpChip, chr = chr,
                                        dronesHaploid = dronesHaploid,
                                        simParamBee = simParamBee)
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname calcBeeGRMIbs
#' @title Calculate Genomic Relatedness Matrix (GRM) for honey bees from
#'   Identical By State genomic data
#'
#' @description Level 0 function that returns Genomic Relatedness Matrix (GRM)
#'   for honey bees from Identical By State genomic data (bi-allelic SNP
#'   represented as allele dosages) following the method of Druet and Legarra
#'   (2020)
#'
#' @param x \code{\link{matrix}} of genotypes represented as allele dosage coded
#'   as 0, 1, or 2 in females (queens or workers) and as 0 or 1 in males
#'   (fathers or drones); individuals are in rows and sites are in columns; no
#'   missing values are allowed (this is not checked - you will get NAs!)
#' @param sex character vector denoting sex for individuals with genotypes in
#'   \code{x} - \code{"F"} for female and \code{"M"} for male
#'
#' @return matrix of genomic relatedness coefficients
#'
#' @references Druet and Legarra (2020) Theoretical and empirical comparisons of
#'   expected and realized relationships for the X-chromosome. Genetics
#'   Selection Evolution, 52:50 \url{https://doi.org/10.1186/s12711-020-00570-6}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addWorkers(colony, nInd = 5)
#' colony <- addDrones(colony, nInd = 5)
#' colony <- addVirginQueens(colony, nInd = 2)
#'
#' genoQ <- getQueensSegSiteGeno(colony)
#' genoF <- getFathersSegSiteGeno(colony)
#' genoW <- getWorkersSegSiteGeno(colony)
#' genoD <- getDronesSegSiteGeno(colony)
#' genoV <- getVirginQueensSegSiteGeno(colony)
#' genoMeanW <- apply(X = genoW, MARGIN = 2, FUN = mean)
#' genoMeanD <- apply(X = genoD, MARGIN = 2, FUN = mean)
#'
#' geno <- rbind(genoQ, genoF, genoW, genoD, genoV, genoMeanW, genoMeanD)
#' n <- length(rownames(geno))
#' rownames(geno)[c(n-1, n)] <- c("mw", "md")
#'
#' sex <- c(getQueen(colony)@sex,
#'          getFathers(colony)@sex,
#'          getWorkers(colony)@sex,
#'          getDrones(colony)@sex,
#'          getVirginQueens(colony)@sex,
#'          "F",
#'          "M")
#'
#' GRM <- calcBeeGRMIbs(x = geno, sex = sex)
#'
#' # library("Matrix"); image(as(GRM, "Matrix"))
#'
#' x <- diag(GRM)
#' hist(x)
#' summary(x)
#'
#' x <- GRM[lower.tri(x = GRM, diag = FALSE)]
#' hist(x)
#' summary(x)
#'
#' q <- rownames(genoQ)
#' f <- rownames(genoF)
#' w <- rownames(genoW)
#' d <- rownames(genoD)
#' v <- rownames(genoV)
#' mw <- "mw"
#' md <- "md"
#'
#' # Queen vs others
#' GRM[q, f]
#' GRM[q, w]
#' GRM[q, d]
#' GRM[q, v]
#' GRM[q, mw]
#' GRM[q, md]
#'
#' # Fathers vs others
#' GRM[f, f]
#' GRM[f, w]
#' GRM[f, d]
#' GRM[f, v]
#' GRM[f, mw]
#' GRM[f, md]
#'
#' # Workers vs others
#' GRM[w, w]
#' GRM[w, d]
#' GRM[w, v]
#' GRM[w, mw]
#' GRM[w, md]
#'
#' @export
calcBeeGRMIbs <- function(x, sex) {
  if (!is.matrix(x)) {
    stop("Argument x must be a matrix class object!")
  }
  if (!is.character(sex)) {
    stop("Argument sex must be a character class object!")
  }
  if (any(!sex %in% c("F", "M"))) {
    print(table(sex, useNA = "ifany"))
    stop("Entries in sex should be either F (for females) or M (for males)!")
  }
  if (nrow(x) != length(sex)) {
    stop("Dimensions of x (number of rows) and sex (length) must match!")
  }
  alleleSum <- apply(X = x, FUN = sum, MARGIN = 2)
  ploidy <- (sex == "F") + 1
  alleleFreq <- alleleSum / sum(ploidy)
  for (site in 1:ncol(x)) {
    x[, site] <- x[, site] - ploidy * alleleFreq[site]
  }
  G <- tcrossprod(x) / (2 * sum(alleleFreq * (1 - alleleFreq)))
  return(G)
}

#' @rdname calcBeeGRMIbd
#' @title Calculate Genomic Relatedness Matrix (GRM) for honey bees from
#'   Identical By Descent genomic data
#'
#' @description Level 0 function that returns Genomic Relatedness Matrix (GRM)
#'   for honey bees from Identical By Descent genomic data (tracked alleles
#'   since the founders) - see references on the background theory.
#'
#' @param x \code{\link{matrix}} of haplotypes/genomes with allele indicators
#'   for the founders coded as 1, 2, ... Haplotypes/genome are in rows and sites
#'   are in columns; no missing values are allowed (this is not checked!). Row
#'   names are essential (formated as ind_genome as returned by AlphaSimR IBD
#'   functions) to infer the individual and their ploidy (see examples)!
#'
#' @return a list with a matrix of gametic relatedness coefficients (genome) and
#'   a matrix of individual relatedness coefficients (indiv)
#'
#' @references
#' Grossman and Eisen (1989) Inbreeding, coancestry, and covariance between
#'   relatives for X-chromosomal loci. The Journal of Heredity,
#'   \url{https://doi.org/10.1093/oxfordjournals.jhered.a110812}
#'
#' Fernando and Grossman (1989) Covariance between relatives for X-chromosomal
#'   loci in a population in disequilibrium. Theoretical and Applied Genetics,
#'   \url{https://doi.org/10.1007/bf00305821}
#'
#' Fernando and Grossman (1990) Genetic evaluation with autosomal
#'   and X-chromosomal inheritance. Theoretical and Applied Genetics,
#'   \url{https://doi.org/10.1007/bf00224018}
#'
#' Van Arendonk, Tier, and Kinghorn (1994) Use of multiple genetic markers in
#'   prediction of breeding values. Genetics,
#'   \url{https://doi.org/10.1093/genetics/137.1.319}
#'
#' Hill and Weir (2011) Variation in actual relationship as a consequence of
#'   Mendelian sampling and linkage. Genetics Research,
#'   \url{https://doi.org/10.1017/s0016672310000480}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony <- addWorkers(colony, nInd = 5)
#' colony <- addDrones(colony, nInd = 5)
#' colony <- addVirginQueens(colony, nInd = 2)
#'
#' haploQ <- getQueensIbdHaplo(colony)
#' haploF <- getFathersIbdHaplo(colony)
#' haploW <- getWorkersIbdHaplo(colony)
#' haploD <- getDronesIbdHaplo(colony)
#' haploV <- getVirginQueensIbdHaplo(colony)
#' SP$pedigree
#'
#' haplo <- rbind(haploQ, haploF, haploW, haploD, haploV)
#'
#' GRMs <- calcBeeGRMIbd(x = haplo)
#'
#' # library("Matrix"); image(as(GRMs$genome, "Matrix"))
#' # library("Matrix"); image(as(GRMs$indiv, "Matrix"))
#'
#' GRMs
#'
#' x <- diag(GRMs$genome)
#' hist(x)
#' summary(x)
#'
#' x <- GRMs$genome[lower.tri(x = GRMs$genome, diag = FALSE)]
#' hist(x)
#' summary(x)
#'
#' x <- diag(GRMs$indiv)
#' hist(x)
#' summary(x)
#'
#' x <- GRMs$indiv[lower.tri(x = GRMs$indiv, diag = FALSE)]
#' hist(x)
#' summary(x)
#'
#' qI <- getQueen(colony)@id
#' fI <- sort(getFathers(colony)@id)
#' wI <- sort(getWorkers(colony)@id)
#' dI <- sort(getDrones(colony)@id)
#' vI <- sort(getVirginQueens(colony)@id)
#'
#' qG <- c(t(outer(X = qI, Y = 1:2, FUN = paste, sep = "_")))
#' fG <- paste(fI, 1, sep = "_")
#' wG <- c(t(outer(X = wI, Y = 1:2, FUN = paste, sep = "_")))
#' dG <- paste(dI, 1, sep = "_")
#' vG <- c(t(outer(X = vI, Y = 1:2, FUN = paste, sep = "_")))
#'
#' # Queen vs others
#' GRMs$genome[qG, qG]
#' GRMs$indiv[qI, qI]
#'
#' GRMs$genome[fG, qG]
#' GRMs$indiv[fI, qI]
#'
#' GRMs$genome[wG, qG]
#' GRMs$indiv[wI, qI]
#'
#' GRMs$genome[dG, qG]
#' GRMs$indiv[dI, qI]
#'
#' GRMs$genome[vG, qG]
#' GRMs$indiv[vI, qI]
#'
#' # Fathers vs others
#' GRMs$genome[fG, fG]
#' GRMs$indiv[fI, fI]
#'
#' GRMs$genome[wG, fG]
#' GRMs$indiv[wI, fI]
#'
#' GRMs$genome[dG, fG]
#' GRMs$indiv[dI, fI]
#'
#' GRMs$genome[vG, fG]
#' GRMs$indiv[vI, fI]
#'
#' # Workers vs others
#' GRMs$genome[wG, wG]
#' GRMs$indiv[wI, wI]
#'
#' GRMs$genome[dG, wG]
#' GRMs$indiv[dI, wI]
#'
#' GRMs$genome[vG, wG]
#' GRMs$indiv[vI, wI]
#'
#' @export
calcBeeGRMIbd <- function(x) {
  if (!is.matrix(x)) {
    stop("Argument x must be a matrix class object!")
  }
  nHap <- nrow(x)
  nSit <- ncol(x)
  hapId <- rownames(x)
  id <- sapply(X = strsplit(x = hapId, split = "_"), FUN = function(z) z[[1]])
  idUnique <- unique(id)
  nInd <- length(idUnique)
  ploidy <- table(id)[idUnique]

  # IBD matching
  G <- matrix(data = numeric(), nrow = nHap, ncol = nHap,
              dimnames = list(hapId, hapId))
  x <- t(x) # orient for faster access (R is column major); unsure it speeds-up!
  for (hap1 in 1:nHap) {
    tmp <- x[, hap1]
    for (hap2 in 1:hap1) {
      G[hap2, hap1] <- sum(tmp == x[, hap2])
    }
    G[hap1, 1:(hap1 - 1)] <- G[1:(hap1 - 1), hap1]
  }
  G <- G / nSit

  # Using the Van Arendonk et al. (1994) "trick" of A=1/2KGK^T, but adapted to
  #   haplo-diploids
  K <- matrix(data = 0.0, nrow = nInd, ncol = nHap,
              dimnames = list(idUnique, hapId))
  lastCol <- 0
  ones <- c(1, 1)
  for (ind in 1:nInd) {
    if (ploidy[ind] == 2) {
      cols <- lastCol + c(1, 2)
      lastCol <- cols[2]
      K[ind, cols] <- ones
    } else {
      lastCol <- lastCol + 1
      K[ind, lastCol] <- 1
    }
  }
  return(list(genome = G, indiv = 0.5 * K %*% G %*% t(K)))
}

#' @rdname getCasteGv
#' @title Access genetic values of individuals in a caste
#'
#' @description Level 0 function that returns genetic values of individuals in a
#'   caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
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
#' getCasteGv(colony1, caste = "virginQueens")
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
#' getCasteGv(apiary, caste = "virginQueens")
#' getVirginQueensGv(apiary)
#'
#' getCasteGv(apiary, caste = "workers")
#' getWorkersGv(apiary)
#'
#' getCasteGv(apiary, caste = "drones")
#' getDronesGv(apiary)
#' @export
getCasteGv <- function(x, caste, nInd = NULL) {
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- gv(pop = tmp)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteGv(x = x[[colony]], caste = caste, nInd = nInd)
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
    ret <- getCasteGv(x, caste = "virginQueens", nInd = nInd)
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyGv(colony1)
#' getColonyGv(colony1, caste = c("queen", "fathers"))
#' getColonyGv(colony1, nInd = 1)
#' getColonyGv(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonyGv(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyGv(apiary)
#' getColonyGv(apiary, caste = c("queen", "fathers"))
#' getColonyGv(apiary, nInd = 1)
#' getColonyGv(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonyGv <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
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
      ret[[colony]] <- getColonyGv(x = x[[colony]], caste = caste, nInd = nInd)
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
#'
#' getCasteBv(colony1, caste = "queen")
#' getQueensBv(colony1)
#'
#' getCasteBv(colony1, caste = "fathers")
#' getCasteBv(colony1, caste = "fathers", nInd = 2)
#' getCasteBv(colony1, caste = "fathers", nInd = 2)
#' getFathersBv(colony1)
#' getFathersBv(colony1, nInd = 2)
#'
#' getCasteBv(colony1, caste = "virginQueens")
#' getVirginQueensBv(colony1)
#'
#' getCasteBv(colony1, caste = "workers")
#' getWorkersBv(colony1)
#'
#' getCasteBv(colony1, caste = "drones")
#' getDronesBv(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteBv(apiary, caste = "queen")
#' getQueensBv(apiary)
#'
#' getCasteBv(apiary, caste = "fathers")
#' getCasteBv(apiary, caste = "fathers", nInd = 2)
#' getCasteBv(apiary, caste = "fathers", nInd = 2)
#' getFathersBv(apiary)
#' getFathersBv(apiary, nInd = 2)
#'
#' getCasteBv(apiary, caste = "virginQueens")
#' getVirginQueensBv(apiary)
#'
#' getCasteBv(apiary, caste = "workers")
#' getWorkersBv(apiary)
#'
#' getCasteBv(apiary, caste = "drones")
#' getDronesBv(apiary)
#' @export
getCasteBv <- function(x, caste, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- bv(pop = tmp, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteBv(
        x = x[[colony]], caste = caste, nInd = nInd,
        simParamBee = simParamBee
      )
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
    ret <- getCasteBv(x,
      caste = "queen",
      simParamBee = simParamBee
    )
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
    ret <- getCasteBv(x,
      caste = "fathers", nInd = nInd,
      simParamBee = simParamBee
    )
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
    ret <- getCasteBv(x,
      caste = "virginQueens", nInd = nInd,
      simParamBee = simParamBee
    )
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
    ret <- getCasteBv(x,
      caste = "workers", nInd = nInd,
      simParamBee = simParamBee
    )
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
    ret <- getCasteBv(x,
      caste = "drones", nInd = nInd,
      simParamBee = simParamBee
    )
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyBv(colony1)
#' getColonyBv(colony1, caste = c("queen", "fathers"))
#' getColonyBv(colony1, nInd = 1)
#' getColonyBv(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' getColonyBv(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getColonyBv(apiary)
#' getColonyBv(apiary, caste = c("queen", "fathers"))
#' getColonyBv(apiary, nInd = 1)
#' getColonyBv(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonyBv <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
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
      ret[[colony]] <- getColonyBv(
        x = x[[colony]], caste = caste,
        nInd = nInd, simParamBee = simParamBee
      )
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
#' @param caste character, "queen", "fathers", "virginQueens", "workers", or
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addDrones(colony2, nInd = 4)
#' colony1 <- addVirginQueens(colony1, nInd = 2)
#'
#' getCasteDd(colony1, caste = "queen")
#' getQueensDd(colony1)
#'
#' getCasteDd(colony1, caste = "fathers")
#' getCasteDd(colony1, caste = "fathers", nInd = 2)
#' getCasteDd(colony1, caste = "fathers", nInd = 2)
#' getFathersDd(colony1)
#' getFathersDd(colony1, nInd = 2)
#'
#' getCasteDd(colony1, caste = "virginQueens")
#' getVirginQueensDd(colony1)
#'
#' getCasteDd(colony1, caste = "workers")
#' getWorkersDd(colony1)
#'
#' getCasteDd(colony1, caste = "drones")
#' getDronesDd(colony1)
#'
#' apiary <- c(colony1, colony2)
#' getCasteDd(apiary, caste = "queen")
#' getQueensDd(apiary)
#'
#' getCasteDd(apiary, caste = "fathers")
#' getCasteDd(apiary, caste = "fathers", nInd = 2)
#' getCasteDd(apiary, caste = "fathers", nInd = 2)
#' getFathersDd(apiary)
#' getFathersDd(apiary, nInd = 2)
#'
#' getCasteDd(apiary, caste = "virginQueens")
#' getVirginQueensDd(apiary)
#'
#' getCasteDd(apiary, caste = "workers")
#' getWorkersDd(apiary)
#'
#' getCasteDd(apiary, caste = "drones")
#' getDronesDd(apiary)
#' @export
getCasteDd <- function(x, caste, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
    if (is.null(tmp)) {
      ret <- NULL
    } else {
      ret <- dd(pop = tmp, simParam = simParamBee)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteDd(
        x = x[[colony]], caste = caste, nInd = nInd,
        simParamBee = simParamBee
      )
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
    ret <- getCasteDd(x,
      caste = "queen",
      simParamBee = simParamBee
    )
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
    ret <- getCasteDd(x,
      caste = "fathers", nInd = nInd,
      simParamBee = simParamBee
    )
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
    ret <- getCasteDd(x,
      caste = "virginQueens", nInd = nInd,
      simParamBee = simParamBee
    )
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
    ret <- getCasteDd(x,
      caste = "workers", nInd = nInd,
      simParamBee = simParamBee
    )
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
    ret <- getCasteDd(x,
      caste = "drones", nInd = nInd,
      simParamBee = simParamBee
    )
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
#' @param caste character, a combination of "queen", "fathers", "virginQueens",
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
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- asVirginQueen(newPop(founderGenomes))
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 2)
#'
#' getColonyDd(colony1)
#' getColonyDd(colony1, caste = c("queen", "fathers"))
#' getColonyDd(colony1, nInd = 1)
#' getColonyDd(colony1, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#'
#' apiary <- c(colony1, colony2)
#' getColonyDd(apiary)
#' getColonyDd(apiary, caste = c("queen", "fathers"))
#' getColonyDd(apiary, nInd = 1)
#' getColonyDd(apiary, nInd = list("queen" = 1, "fathers" = 2, "virginQueens" = 1))
#' @export
getColonyDd <- function(x, caste = c("queen", "fathers", "virginQueens", "workers", "drones"),
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd[[caste]])
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
      ret[[colony]] <- getColonyDd(
        x = x[[colony]], caste = caste,
        nInd = nInd, simParamBee = simParamBee
      )
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}
