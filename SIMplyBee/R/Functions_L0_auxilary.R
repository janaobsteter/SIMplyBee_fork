# ---- Level 0 Auxiliary Functions ----

# n* ----

#' @rdname nColonies
#' @title Number of colonies in a MultiColony object
#'
#' @description Level 0 function that returns the number of colonies in a
#'   MultiColony object.
#'
#' @param multicolony \code{\link{MultiColony-class}}
#'
#' @seealso \code{\link{nNULLColonies}} and \code{\link{nEmptyColonies}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#' apiary <- createMultiColony(basePop[2:3], n = 2)
#' nColonies(apiary)
#' nColonies(createMultiColony(n = 10))
#' @export
nColonies <- function(multicolony) {
  if (!"MultiColony" %in% class(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  n <- length(multicolony@colonies)
  return(n)
}

#' @rdname nNULLColonies
#' @title Number of NULL colonies in a MultiColony object
#'
#' @description Level 0 function that returns the number of colonies in a
#'   MultiColony object that are in fact \code{NULL}.
#'
#' @param multicolony \code{\link{MultiColony-class}}
#'
#' @return integer
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' emptyApiary <- createMultiColony(n = 3)
#' emptyApiary1 <- c(createColony(), createColony())
#' nonEmptyApiary <- createMultiColony(basePop[2:3], n = 2)
#'
#' nColonies(nonEmptyApiary)
#' nColonies(emptyApiary)
#'
#' isEmpty(emptyApiary)
#' isEmpty(emptyApiary1)
#' isEmpty(nonEmptyApiary)
#' isNULLColonies(emptyApiary)
#' isNULLColonies(emptyApiary1)
#' isNULLColonies(nonEmptyApiary)
#'
#' nEmptyColonies(emptyApiary)
#' nEmptyColonies(emptyApiary1)
#' nEmptyColonies(nonEmptyApiary)
#' nNULLColonies(emptyApiary)
#' nNULLColonies(emptyApiary1)
#' nNULLColonies(nonEmptyApiary)
#'
#' @export
nColonies <- function(multicolony) {
  if (!"MultiColony" %in% class(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  n <- length(multicolony@colonies)
  return(n)
}

#' @describeIn nColonies Number of \code{NULL} colonies in a MultiColony object
#' @export
nNULLColonies <- function(multicolony) {
  if (!"MultiColony" %in% class(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  if (nColonies(multicolony) == 0) {
    ret <- 0
  } else {
    ret <- sum(isNULLColonies(multicolony))
  }
  return(ret)
}

#' @describeIn nColonies Number of empty colonies in a MultiColony object
#' @export
nEmptyColonies <- function(multicolony) {
  if (!"MultiColony" %in% class(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  if (nColonies(multicolony) == 0) {
    ret <- 0
  } else {
    ret <- sum(isEmpty(multicolony))
  }
  return(ret)
}

#' @rdname nCaste
#' @title Level 0 function that returns the number of individuals of a caste in a
#'   colony
#'
#' @description Returns the number of individuals of a caste in a colony
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#'
#' @seealso \code{\link{nQueens}}, \code{\link{nFathers}},
#'   \code{\link{nVirginQueens}}, \code{\link{nWorkers}}, and
#'   \code{\link{nDrones}}
#'
#' @return when \code{x} is \code{\link{Colony-class}} return is integer for
#'   \code{caste != "all"} or list for \code{caste == "all"} with nodes named
#'   by caste; when \code{x} is \code{\link{MultiColony-class}} return is named
#'   integer for \code{caste != "all"} or named list of lists for
#'   \code{caste == "all"}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 100, nDrones = 10)
#' colony <- addVirginQueens(x = colony, nInd = 3)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10)
#' apiary <- addVirginQueens(x = apiary, nInd = 3)
#'
#' # Check caste members
#' nCaste(colony, caste = "queen")
#' nCaste(colony, caste = "fathers")
#' nCaste(colony, caste = "virginQueens")
#' nCaste(colony, caste = "workers")
#' nCaste(colony, caste = "drones")
#' nCaste(colony, caste = "all")
#'
#' nCaste(apiary, caste = "queen")
#' nCaste(apiary, caste = "fathers")
#' nCaste(apiary, caste = "virginQueens")
#' nCaste(apiary, caste = "workers")
#' nCaste(apiary, caste = "drones")
#' nCaste(apiary, caste = "all")
#'
#' # Check number of queens
#' nQueens(colony)
#' nQueens(apiary)
#' apiary <- removeQueen(apiary)
#' nQueens(apiary)
#'
#' # Check number of fathers
#' nFathers(colony)
#' nFathers(apiary)
#'
#' # Check number of workers
#' nWorkers(colony)
#' nWorkers(apiary)
#'
#' # Check number of drones
#' nDrones(colony)
#' nDrones(apiary)
#'
#' # Check number of virgin queens
#' nVirginQueens(colony)
#' nVirginQueens(apiary)
#'
#' @export
nCaste <- function(x, caste = "all") {
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        ret[[caste]] <- nCaste(x = x, caste = caste)
      }
    } else {
      if (caste == "fathers") {
        ret <- ifelse(!is.null(slot(x, "queen")), nInd(x@queen@misc[[1]]$fathers), 0)
      } else if (caste == "drones") {
        ret <- ifelse(!is.null(slot(x, caste)), sum(isDrone(x@drones)), 0)
      } else {
        ret <- ifelse(!is.null(slot(x, caste)), nInd(slot(x, caste)), 0)
      }
    }
  } else if (isMultiColony(x)) {
      fun <- ifelse(caste == "all", lapply, sapply)
      ret <- fun(x@colonies, FUN = function(z) ifelse(isEmpty(z), 0, nCaste(x = z, caste = caste)))
      names(ret) <- getId(x)
  } else {
    stop("Argument colony must be a Colony or MultiColony class object!")
  }
  return(ret)
}


#' @describeIn nCaste Number of queens in a colony
#' @export
nQueens <- function(x) {
  ret <- nCaste(x, caste = "queen")
  return(ret)
}

#' @describeIn nCaste Number of fathers in a colony
#' @export
nFathers <- function(x) {
  if (isPop(x)) {
    if (any(!(isQueen(x)))) {
      stop("Individuals in x must be queens!")
    }
    nInd <- nInd(x)
    ret <- rep(x = 0, times = nInd)
    for (ind in seq_len(nInd)) {
      if (isQueen(x[ind])) {
        ret[ind] <- nInd(x@misc[[ind]]$fathers)
      }
    }
  } else {
    ret <- nCaste(x, caste = "fathers")
  }
  return(ret)
}

#' @describeIn nCaste Number of workers in a colony
#' @export
nWorkers <- function(x) {
  ret <- nCaste(x, caste = "workers")
  return(ret)
}

#' @describeIn nCaste Number of drones in a colony
#' @export
nDrones <- function(x) {
  ret <- nCaste(x, caste = "drones")
  return(ret)
}

#' @describeIn nCaste Number of virgin queens in a colony
#' @export
nVirginQueens <- function(x) {
  ret <- nCaste(x, caste = "virginQueens")
  return(ret)
}

# pHomBrood ----

#' @rdname calcQueensPHomBrood
#' @title The expected proportion and a realised number of csd homozygous brood
#'
#' @description Level 0 functions that calculate or report the proportion of csd
#'   homozygous brood of a queen or a colony. The csd locus determines viability
#'   of fertilised eggs (brood) - homozygous brood is removed by workers. These
#'   functions 1) calculate the expected proportion of homozygous brood from the
#'   csd allele of the queen and fathers, 2) report the expected proportion of
#'   homozygous brood, or 3) report a realised number of homozygous brood due to
#'   inheritance process. See \code{vignette(package = "SIMplyBee")} for more
#'   details.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{MultiColony-class}}
#'
#' @seealso Demo in the introductory vignette
#'   \code{vignette("Honeybee_biology", package="SIMplyBee")}
#'
#' @return numeric, expected csd homozygosity named by colony id when \code{x}
#'   is \code{\link{MultiColony-class}}
#'
#' @examples
#' # This is a bit long example - the key is at the end!
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
#' colony <- addVirginQueens(x = colony, nInd = 1)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10)
#'
#' # Virgin queen
#' try(calcQueensPHomBrood(basePop[5]))
#'
#' # Queens of colony
#' calcQueensPHomBrood(colony)
#'
#' # Queens of apiary
#' calcQueensPHomBrood(apiary)
#'
#' # Inbreed virgin queen with her brothers to generate csd homozygous brood
#' colony2 <- createColony(x = getVirginQueens(colony))
#' colony2 <- cross(x = colony2, drones = pullDrones(x = colony, nInd = nFathersPoisson())[[1]])
#'
#' # Calculate the expected csd homozygosity
#' calcQueensPHomBrood(getQueen(colony2))
#' pHomBrood(colony2)
#'
#' # Evaluate a realised csd homozygosity
#' nHomBrood(addWorkers(colony2, nInd = 100))
#' nHomBrood(addWorkers(colony2, nInd = 100))
#' # nHomBrood will vary between function calls due to inheritance process
#' @export
calcQueensPHomBrood <- function(x) {
  if (isPop(x)) {
    ret <- rep(x = NA, times = nInd(x))
    for (ind in seq_len(nInd(x))) {
      if (!isQueen(x)) {
        stop("calcQueensPHomBrood can only be used with queens!")
      } else {
        queensCsd <- apply(
          X = getCsdAlleles(x[ind]), MARGIN = 1,
          FUN = function(x) paste0(x, collapse = "")
        )
        fathersCsd <- apply(
          X = getCsdAlleles(x@misc[[ind]]$fathers), MARGIN = 1,
          FUN = function(x) paste0(x, collapse = "")
        )
        nComb <- length(queensCsd) * length(fathersCsd)
        ret[ind] <- sum(fathersCsd %in% queensCsd) / nComb
      }
    }
  } else if (isColony(x)) {
    ret <- calcQueensPHomBrood(x = x@queen)
  } else if (isMultiColony(x)) {
    ret <- sapply(X = x@colonies, FUN = calcQueensPHomBrood)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn calcQueensPHomBrood Expected percentage of csd homozygous brood
#'   of a queen / colony
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
    # TODO: report queen's and colony's pHomBrood / nHomBrood
    #       https://github.com/HighlanderLab/SIMplyBee/issues/80
    #       https://github.com/HighlanderLab/SIMplyBee/issues/104
    if (is.null(x@queen@misc[[1]]$pHomBrood)) {
      ret <- NA
    } else {
      ret <- x@queen@misc[[1]]$pHomBrood
    }
  } else if (isMultiColony(x)) {
    ret <- sapply(X = x@colonies, FUN = pHomBrood)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn calcQueensPHomBrood Realised number of csd homozygous brood
#'   produced by a queen
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
  } else if (isMultiColony(x)) {
    ret <- sapply(X = x@colonies, FUN = nHomBrood)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

# is* ----

#' @rdname isCaste
#' @title Is individual a member of a specific caste
#'
#' @description Level 0 function that tests if individuals are members of a
#'   specific caste
#'
#' @param x \code{\link{Pop-class}}
#' @param caste character, one of "queen", "fathers", "workers", "drones", or
#'   "virginQueens"; only single value is used
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{isQueen}}, \code{\link{isFather}},
#'   \code{\link{isVirginQueen}}, \code{\link{isWorker}}, and
#'   \code{\link{isDrone}}
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
#' colony <- addVirginQueens(x = colony, nInd = 4)
#'
#' isCaste(getQueen(colony), caste = "queen")
#' isCaste(getFathers(colony, nInd = 2), caste = "fathers")
#' isCaste(getWorkers(colony, nInd = 2), caste = "workers") # random sample!
#' isCaste(getDrones(colony, nInd = 2), caste = "drones")
#' isCaste(getVirginQueens(colony, nInd = 2), caste = "virginQueens")
#'
#' bees <- c(
#'   getQueen(colony),
#'   getFathers(colony, nInd = 2),
#'   getWorkers(colony, nInd = 2),
#'   getDrones(colony, nInd = 2),
#'   getVirginQueens(colony, nInd = 2)
#' )
#' isCaste(bees, caste = "queen")
#' isCaste(bees, caste = "fathers")
#' isCaste(bees, caste = "workers")
#' isCaste(bees, caste = "drones")
#' isCaste(bees, caste = "virginQueens")
#'
#' isQueen(getQueen(colony))
#' isQueen(getFathers(colony, nInd = 2))
#'
#' isFather(getQueen(colony))
#' isFather(getFathers(colony, nInd = 2))
#'
#' isWorker(getQueen(colony))
#' isWorker(getFathers(colony, nInd = 2))
#' isWorker(getWorkers(colony, nInd = 2))
#'
#' isDrone(getQueen(colony))
#' isDrone(getFathers(colony, nInd = 2))
#' isDrone(getDrones(colony, nInd = 2))
#'
#' isVirginQueen(getQueen(colony))
#' isVirginQueen(getFathers(colony, nInd = 2))
#' isVirginQueen(getVirginQueens(colony, nInd = 2))
#'
#' @export
isCaste <- function(x, caste, simParamBee = NULL) {
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) {
    ret <- simParamBee$caste[x@id] == caste[1]
  } else if (is.null(x)) {
    ret <- NULL
  } else {
    stop("Argument x must be a Pop class object or NULL!")
  }
  return(ret)
}

#' @describeIn isCaste Is individual a queen
#' @export
isQueen <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- isCaste(x = x, caste = "queen", simParamBee = simParamBee)
  return(ret)
}

#' @describeIn isCaste Is individual a father
#' @export
isFather <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- isCaste(x = x, caste = "fathers", simParamBee = simParamBee)
  return(ret)
}

#' @describeIn isCaste Is individual a worker

#' @export
isWorker <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- isCaste(x = x, caste = "workers", simParamBee = simParamBee)
  return(ret)
}


#' @describeIn isCaste Is individual a drone
#' @export
isDrone <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- isCaste(x = x, caste = "drones", simParamBee = simParamBee)
  return(ret)
}


#' @describeIn isCaste Is individual a virgin queen
#' @export
isVirginQueen <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- isCaste(x = x, caste = "virginQueens", simParamBee = simParamBee)
  return(ret)
}

#' @rdname isQueenPresent
#' @title Is the queen present
#'
#' @description Level 0 function that returns queen's presence status (is she
#'   present/alive or not).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10)
#'
#' isQueenPresent(colony)
#' isQueenPresent(apiary)
#'
#' colony <- removeQueen(colony)
#' isQueenPresent(colony)
#' @export
isQueenPresent <- function(x) {
  if (isColony(x) | isMultiColony(x)) {
    if (length(nQueens(x)) > 0) {
      ret <- nQueens(x) > 0
    } else {
      ret <- FALSE
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}


#' @rdname isFathersPresent
#' @title Are fathers present (=queen mated)
#'
#' @description Level 0 function that returns fathers presence status (are they
#'   present or not, which means the queen is mated).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' isFathersPresent(colony)
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' isFathersPresent(apiary)
#'
#' colony <- cross(colony, drones = droneGroups[[1]])
#' isFathersPresent(removeDrones(colony))
#'
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' isFathersPresent(removeDrones(apiary))
#' @export
isFathersPresent <- function(x) {
  if (isColony(x) | isMultiColony(x)) {
    if (length(nFathers(x)) > 0) {
      ret <- nFathers(x) > 0
    } else {
      ret <- FALSE
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn isFathersPresent Are fathers present
#' @export
areFathersPresent <- isFathersPresent

#' @rdname isWorkersPresent
#' @title Are workers present
#'
#' @description Level 0 function that returns workers presence status (are they
#'   present or not).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10)
#'
#' isWorkersPresent(colony)
#' isWorkersPresent(removeWorkers(colony))
#' isWorkersPresent(apiary)
#' isWorkersPresent(removeWorkers(apiary))
#' @export
isWorkersPresent <- function(x) {
  if (isColony(x) | isMultiColony(x)) {
    if (length(nWorkers(x)) > 0) {
      ret <- nWorkers(x) > 0
    } else {
      ret <- FALSE
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn isWorkersPresent Are workers present
#' @export
areWorkersPresent <- isWorkersPresent

#' @rdname isDronesPresent
#' @title Are drones present
#'
#' @description Level 0 function that returns drones presence status (are they
#'   present or not).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10)
#'
#' isDronesPresent(colony)
#' isDronesPresent(removeDrones(colony))
#' isDronesPresent(apiary)
#' isDronesPresent(removeDrones(apiary))
#' @export
isDronesPresent <- function(x) {
  if (isColony(x) | isMultiColony(x)) {
    if (length(nDrones(x)) > 0) {
      ret <- nDrones(x) > 0
    } else {
      ret <- FALSE
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn isWorkersPresent Are drones present
#' @export
areDronesPresent <- isDronesPresent

#' @rdname isVirginQueensPresent
#' @title Are virgin queen(s) present
#'
#' @description Level 0 function that returns virgin queen(s) presence status.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- addVirginQueens(x = colony, nInd = 4)
#' isVirginQueensPresent(colony)
#' isVirginQueensPresent(pullVirginQueens(colony)$remnant)
#' isVirginQueensPresent(removeQueen(colony))
#' # TODO: Should removeQueen() initiate creation of virginQueens #339
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/339
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10)
#' isVirginQueensPresent(apiary)
#'
#' tmp <- swarm(x = apiary)
#' isVirginQueensPresent(tmp$swarm)
#' isVirginQueensPresent(tmp$remnant)
#' @export
isVirginQueensPresent <- function(x) {
  if (isColony(x) | isMultiColony(x)) {
    if (length(nVirginQueens(x)) > 0) {
      ret <- nVirginQueens(x) > 0
    } else {
      ret <- FALSE
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @rdname isEmpty
#' @title Check whether a population, colony or a multicolony
#'   object has no individuals within
#'
#' @description Check whether a population, colony or a multicolony
#'   object has no individuals within.
#'
#' @param x \code{\link{Pop-class}} or \code{\link{Colony-class}} or
#'   \code{\link{MultiColony-class}}
#'
#' @return boolean when \code{x} is \code{\link{Pop-class}} or
#'   \code{\link{Colony-class}}, and named vector of boolean when
#'   \code{x} is \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' isEmpty(new(Class = "Pop"))
#' isEmpty(basePop[0])
#' isEmpty(basePop)
#'
#' emptyColony <- createColony()
#' nonEmptyColony <- createColony(basePop[1])
#' isEmpty(emptyColony)
#' isEmpty(nonEmptyColony)
#'
#' emptyApiary <- createMultiColony(n = 3)
#' emptyApiary1 <- c(createColony(), createColony())
#' emptyApiary2 <- createMultiColony()
#' nonEmptyApiary <- createMultiColony(basePop[2:5], n = 4)
#'
#' isEmpty(emptyApiary)
#' isEmpty(emptyApiary1)
#' isEmpty(nonEmptyApiary)
#' isNULLColonies(emptyApiary)
#' isNULLColonies(emptyApiary1)
#' isNULLColonies(nonEmptyApiary)
#'
#' nEmptyColonies(emptyApiary)
#' nEmptyColonies(emptyApiary1)
#' nEmptyColonies(nonEmptyApiary)
#' nNULLColonies(emptyApiary)
#' nNULLColonies(emptyApiary1)
#' nNULLColonies(nonEmptyApiary)
#'
#' @export
isEmpty <- function(x) {
  if (is.null(x)) {
    ret <- TRUE
  } else if (isPop(x)) {
    if (length(x@nInd) == 0  || x@nInd == 0) {
      ret <- TRUE
    } else {
      ret <- FALSE
    }
  } else if (isColony(x)) {
    if (isEmpty(x@queen) & isEmpty(x@virginQueens) & isEmpty(x@workers) & isEmpty(x@drones)) {
      ret <- TRUE
    } else {
      ret <- FALSE
    }
  } else if (isMultiColony(x)) {
    if (nColonies(x) > 0) {
      ret <- sapply(X = x@colonies, FUN = isEmpty, simplify = TRUE)
      names(ret) <- getId(x)
    } else {
      ret <- TRUE
    }
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @rdname isNULLColonies
#' @title Check which of the colonies in a multicolony are NULL
#'
#' @description Check which of the colonies in a multicolony are NULL
#'
#' @param multicolony  \code{\link{MultiColony-class}}
#'
#' @return Named vector of boolean
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' emptyApiary <- createMultiColony(n = 3)
#' emptyApiary1 <- c(createColony(), createColony())
#' nonEmptyApiary <- createMultiColony(basePop[2:5], n = 4)
#'
#' isEmpty(emptyApiary)
#' isEmpty(emptyApiary1)
#' isEmpty(nonEmptyApiary)
#' isNULLColonies(emptyApiary)
#' isNULLColonies(emptyApiary1)
#' isNULLColonies(nonEmptyApiary)
#'
#' nEmptyColonies(emptyApiary)
#' nEmptyColonies(emptyApiary1)
#' nEmptyColonies(nonEmptyApiary)
#' nNULLColonies(emptyApiary)
#' nNULLColonies(emptyApiary1)
#' nNULLColonies(nonEmptyApiary)
#'
#' @export
isNULLColonies <- function(multicolony) {
  if (isMultiColony(multicolony)) {
    ret <- sapply(X = multicolony@colonies, FUN = is.null, simplify = TRUE)
    names(ret) <- getId(multicolony)
  } else {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  return(ret)
}

# get (general) ----

#' @describeIn isVirginQueensPresent Are virgin queen(s) present
#' @export
areVirginQueensPresent <- isVirginQueensPresent

#' @rdname getQueenYearOfBirth
#' @title Access the queen's year of birth
#'
#' @description Level 0 function that returns the queen's year of birth.
#'
#' @param x \code{\link{Pop-class}} (one or more than one queen),
#'   \code{\link{Colony-class}} (one colony), or
#'   \code{\link{MultiColony-class}} (more colonies)
#'
#' @return numeric, the year of birth of the queen(s); named when theres is more
#'   than one queen; \code{NA} if queen not present
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' queen <- getQueen(colony)
#' queen <- setQueensYearOfBirth(queen, year = 2022)
#' getQueenYearOfBirth(queen)
#'
#' getQueenYearOfBirth(getQueen(colony))
#' colony <- setQueensYearOfBirth(colony, year = 2030)
#' getQueenYearOfBirth(colony)
#'
#' apiary <- setQueensYearOfBirth(apiary, year = 2022)
#' getQueenYearOfBirth(apiary)
#' @export
getQueenYearOfBirth <- function(x) {
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
  } else if (isMultiColony(x)) {
    ret <- sapply(X = x@colonies, FUN = getQueenYearOfBirth)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @rdname getQueenAge
#' @title Get (calculate) the queen's age
#'
#' @description Level 0 function that returns the queen's age.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{MultiColony-class}}
#' @param currentYear integer, current year
#'
#' @return numeric, the age of the queen(s); named when theres is more
#'   than one queen; \code{NA} if queen not present
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' queen <- getQueen(colony)
#' queen <- setQueensYearOfBirth(queen, year = 2020)
#' getQueenAge(queen, currentYear = 2022)
#'
#' colony <- setQueensYearOfBirth(colony, year = 2021)
#' getQueenAge(colony, currentYear = 2022)
#'
#' apiary <- setQueensYearOfBirth(apiary, year = 2018)
#' getQueenAge(apiary, currentYear = 2022)
#' @export
getQueenAge <- function(x, currentYear) {
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
  } else if (isMultiColony(x)) {
    ret <- sapply(X = x@colonies, FUN = getQueenAge, currentYear = currentYear)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @rdname getId
#' @title Get the colony ID
#'
#' @description Level 0 function that returns the colony ID. This is by
#'   definition the ID of the queen.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or \code{\link{MultiColony-class}}
#'
#' @return character, \code{NA} when queen not present
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' getId(getQueen(colony)) # Pop class
#' getId(colony) # Colony Class
#' getId(apiary) # MultiColony Class
#'
#' colony2 <- removeQueen(colony)
#' getId(colony2)
#' @export
getId <- function(x) {
  if (is.null(x)) {
    id <- NA
  } else if (isPop(x)) {
    id <- x@id
  } else if (isColony(x)) {
    id <- ifelse(is.null(x@id), NA, x@id)
  } else if (isMultiColony(x)) {
    id <- sapply(x@colonies, FUN = getId)
  } else {
    stop("Argument x must be a NULL, Pop, Colony, or MultiColony class object!")
  }
  return(id)
}

#' @rdname getCasteId
#' @title Get IDs of individuals of a caste, or ID of all members of colony
#'
#' @description Level 0 function that returns the ID individuals of a caste. To
#'   get the individuals, use \code{\link{getCastePop}}. To get individuals'
#'   caste, use \code{\link{getCaste}}.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param collapse logical, if all IDs should be returned as a single vector
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCaste}}
#'
#' @return when \code{x} is \code{\link{Pop-class}} for \code{caste != "all"}
#'  or list for \code{caste == "all"} with ID nodes named by caste;
#'    when \code{x} is \code{\link{Colony-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"}
#'   or named list for \code{caste == "all"} indluding caste members IDs;
#'    when \code{x} is \code{\link{MultiColony-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"} or named list of lists of
#'   \code{\link{Pop-class}} for \code{caste == "all"} indluding caste members IDs
#'
#' @seealso \code{\link{getCastePop}} and \code{\link{getCaste}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 20, nDrones = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 10, nDrones = 2)
#' apiary <- addVirginQueens(apiary, nInd = 4)
#'
#' getCasteId(x = drones)
#' getCasteId(x = colony)
#' getCasteId(x = apiary, caste = "workers")
#' getCasteId(x = apiary)
#' getCasteId(x = apiary, caste = "virginQueens")
#' # Get all IDs as a single vector
#' getCasteId(x = colony, caste = "all", collapse = TRUE)
#' getCasteId(x = apiary, caste = "workers", collapse = TRUE)
#' getCasteId(x = apiary, caste = "drones", collapse = TRUE)
#' getCasteId(x = apiary, caste = "all", collapse = TRUE)
#'
#' # Create a data.frame with id, colony, and caste information
#' (tmpC <- getCaste(apiary[[1]]))
#' (tmpI <- getCasteId(apiary[[1]]))
#' tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI))
#' head(tmp)
#' tail(tmp)
#'
#' (tmpC <- getCaste(apiary))
#' (tmpI <- getCasteId(apiary))
#' (tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI)))
#' tmp$colony <- sapply(
#'   X = strsplit(
#'     x = rownames(tmp), split = ".",
#'     fixed = TRUE
#'   ),
#'   FUN = function(z) z[[1]]
#' )
#' head(tmp)
#' tail(tmp)
#' @export
getCasteId <- function(x, caste = "all", collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (isPop(x)) {
    ret <- x@id
  } else if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCastePop(x = x, caste = caste)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp@id
        }
      }
      if (collapse) {
        ret <- as.vector(unlist(ret))
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- tmp@id
      }
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(X = x@colonies, FUN = getCasteId, caste = caste)
    if (collapse) {
      ret <- as.vector(unlist(ret))
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @rdname getCasteSex
#' @title Get sex of individuals of a caste, or sex of all members of colony
#'
#' @description Level 0 function that returns the sex individuals of a caste. To
#'   get the individuals, use \code{\link{getCastePop}}. To get individuals'
#'   caste, use \code{\link{getCaste}}.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getCaste}}
#'
#' @return when \code{x} is \code{\link{Pop-class}} for \code{caste != "all"}
#'  or list for \code{caste == "all"} with sex nodes named by caste;
#'    when \code{x} is \code{\link{Colony-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"}
#'   or named list for \code{caste == "all"} indluding caste members sexes;
#'    when \code{x} is \code{\link{MultiColony-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"} or named list of lists of
#'   \code{\link{Pop-class}} for \code{caste == "all"} indluding caste members sexes
#'
#' @seealso \code{\link{getCastePop}} and \code{\link{getCaste}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 20, nDrones = 5)
#' colony <- addVirginQueens(colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 10, nDrones = 2)
#' apiary <- addVirginQueens(apiary, nInd = 4)
#'
#' getCasteSex(x = drones)
#' getCasteSex(x = colony)
#' getCasteSex(x = apiary, caste = "workers")
#' getCasteSex(x = apiary)
#' getCasteSex(x = apiary, caste = "virginQueens")
#'
#' # Create a data.frame with sex, colony, and caste information
#' (tmpC <- getCaste(apiary[[1]]))
#' (tmpS <- getCasteSex(apiary[[1]]))
#' (tmpI <- getCasteId(apiary[[1]]))
#' tmp <- data.frame(caste = unlist(tmpC), sex = unlist(tmpS), id = unlist(tmpI))
#' head(tmp)
#' tail(tmp)
#'
#' (tmpC <- getCaste(apiary))
#' (tmpS <- getCasteSex(apiary))
#' (tmpI <- getCasteId(apiary))
#' tmp <- data.frame(caste = unlist(tmpC), sex = unlist(tmpS), id = unlist(tmpI))
#' tmp$colony <- sapply(
#'   X = strsplit(
#'     x = rownames(tmp), split = ".",
#'     fixed = TRUE
#'   ),
#'   FUN = function(z) z[[1]]
#' )
#' head(tmp)
#' tail(tmp)
#' @export
getCasteSex <- function(x, caste = "all", simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (isPop(x)) {
    ret <- x@sex
  } else if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCastePop(x = x, caste = caste)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp@sex
        }
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- tmp@sex
      }
    }
  } else if (isMultiColony(x)) {
    fun <- ifelse(caste == "all", lapply, sapply)
    ret <- fun(X = x@colonies, FUN = getCasteSex, caste = caste)
    names(ret) <- getCaste(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @rdname getCaste
#' @title Report caste of an individual
#'
#' @description Level 0 function that reports caste of an individual
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{MultiColony-class}}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters

#'
#' @return When x is \code{\link{Pop-class}}, character of caste status; if you
#'   get \code{NA} note that this is not supposed to happen. When x is
#'   \code{\link{Colony-class}}, list with character vectors (list is named with
#'   caste). When x is \code{\link{MultiColony-class}}, list of lists with
#'   character vectors (list is named with colony id).
#'
#' @seealso \code{\link{getCastePop}} and \code{\link{getCasteId}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 20, nDrones = 5)
#' colony <- addVirginQueens(colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 10, nDrones = 2)
#' apiary <- addVirginQueens(apiary, nInd = 4)
#'
#' getCaste(getQueen(colony))
#' getCaste(getFathers(colony))
#' getCaste(getWorkers(colony))
#' getCaste(getDrones(colony))
#' getCaste(getVirginQueens(colony))
#'
#' bees <- c(
#'   getQueen(colony),
#'   getFathers(colony, nInd = 2),
#'   getWorkers(colony, nInd = 2),
#'   getDrones(colony, nInd = 2),
#'   getVirginQueens(colony, nInd = 2)
#' )
#' getCaste(bees)
#'
#' getCaste(colony)
#' getCaste(apiary)
#'
#' # Create a data.frame with id, colony, and caste information
#' (tmpC <- getCaste(apiary[[1]]))
#' (tmpI <- getCasteId(apiary[[1]]))
#' tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI))
#' head(tmp)
#' tail(tmp)
#'
#' (tmpC <- getCaste(apiary))
#' (tmpI <- getCasteId(apiary))
#' (tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI)))
#' tmp$colony <- sapply(
#'   X = strsplit(
#'     x = rownames(tmp), split = ".",
#'     fixed = TRUE
#'   ),
#'   FUN = function(z) z[[1]]
#' )
#' head(tmp)
#' tail(tmp)
#' @export
getCaste <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) {
    ret <- simParamBee$caste[x@id]
    ret <- as.character(ret)
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
    for (caste in names(ret)) {
      tmp <- getCastePop(x = x, caste = caste)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCaste(tmp)
      }
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(X = x@colonies, FUN = getCaste)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @rdname getLocation
#' @title Get the colony location
#'
#' @description Level 0 function that returns the colony location as (x, y)
#'   coordinates.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return numeric with two values when \code{x} is \code{\link{Colony-class}}
#'   and a list of numeric with two values when \code{x} is
#'   \code{\link{MultiColony-class}} (list named after colonies); \code{c(NA, NA)}
#'   when location not set
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' getLocation(colony)
#' getLocation(apiary[[1]])
#' getLocation(apiary)
#'
#' loc <- c(123, 456)
#' colony <- setLocation(colony, location = loc)
#' getLocation(colony)
#'
#' loc1 <- c(512, 722)
#' colony1 <- setLocation(apiary[[1]], location = loc1)
#' getLocation(colony1)
#'
#' loc2 <- c(189, 357)
#' colony2 <- setLocation(apiary[[2]], location = loc2)
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
      ret <- NULL
    } else {
      ret <- x@location
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(x@colonies, FUN = getLocation)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

# Events ----

#' @rdname hasSplit
#' @title Test if colony has split
#'
#' @description Level 0 function that returns colony split status. This will
#'   obviously impact colony strength.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#'
#' hasSplit(colony)
#' tmp <- split(colony)
#' hasSplit(tmp$split)
#' hasSplit(tmp$remnant)
#'
#' hasSplit(apiary)
#' tmp2 <- split(apiary)
#' hasSplit(tmp2$split)
#' hasSplit(tmp2$remnant)
#' @export
hasSplit <- function(x) {
  if (isColony(x)) {
    ret <- x@split
  } else if (isMultiColony(x)) {
    ret <- sapply(x@colonies, FUN = hasSplit)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @rdname getEvents
#' @title Report which colony events have occurred
#'
#' @description Level 0 function that returns a matrix of logicals reporting the
#'   status of the colony events. The events are: split, swarm, supersedure,
#'   collapse, and production. These events impact colony status, strength, and
#'   could also impact downstream phenotypes.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return matrix of logicals, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(apiary, nInd = 4)
#'
#' getEvents(colony)
#' getEvents(apiary)
#'
#' tmp <- swarm(colony)
#' getEvents(tmp$swarm)
#' getEvents(tmp$remnant)
#'
#' apiary <- supersede(apiary)
#' getEvents(apiary)
#' @export
getEvents <- function(x) {
  if (!isColony(x) & !isMultiColony(x)) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- cbind(hasSplit(x), hasSwarmed(x), hasSuperseded(x), hasCollapsed(x), isProductive(x))
  colnames(ret) <- c("split", "swarmed", "superseded", "collapsed", "productive")
  if (isMultiColony(x)) {
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
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#'
#' hasSwarmed(colony)
#' tmp <- swarm(colony)
#' hasSwarmed(tmp$swarm)
#' hasSwarmed(tmp$remnant)
#'
#' hasSwarmed(apiary)
#' tmp2 <- swarm(apiary)
#' hasSwarmed(tmp2$swarm)
#' hasSwarmed(tmp2$remnant)
#' @export
hasSwarmed <- function(x) {
  if (isColony(x)) {
    ret <- x@swarm
  } else if (isMultiColony(x)) {
    ret <- sapply(x@colonies, FUN = hasSwarmed)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @rdname hasSuperseded
#' @title Test if colony has superseded
#'
#' @description Level 0 function that returns colony supersedure status.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#'
#' hasSuperseded(colony)
#' colony <- supersede(colony)
#' hasSuperseded(colony)
#'
#' hasSuperseded(apiary)
#' apiary <- supersede(apiary)
#' hasSuperseded(apiary)
#' @export
hasSuperseded <- function(x) {
  if (isColony(x)) {
    ret <- x@supersedure
  } else if (isMultiColony(x)) {
    ret <- sapply(x@colonies, FUN = hasSuperseded)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @rdname hasCollapsed
#' @title Test if colony has collapsed
#'
#' @description Level 0 function that returns colony collapse status.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(colony, nInd = 5)
#'
#' hasCollapsed(colony)
#' colony <- collapse(colony)
#' hasCollapsed(colony)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#'
#' hasCollapsed(apiary)
#' apiary <- collapse(apiary)
#' hasCollapsed(apiary)
#' @export
hasCollapsed <- function(x) {
  if (isColony(x)) {
    ret <- x@collapse
  } else if (isMultiColony(x)) {
    ret <- sapply(x@colonies, FUN = hasCollapsed)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @rdname isProductive
#' @title Test if colony is currently productive
#'
#' @description Level 0 function that returns colony production status. This can
#'   be used to decided if colony production can be simulated.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#'
#' isProductive(colony)
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' isProductive(colony)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' isProductive(apiary)
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' isProductive(apiary)
#' @export
isProductive <- function(x) {
  if (isColony(x)) {
    ret <- x@production
  } else if (isMultiColony(x)) {
    ret <- sapply(x@colonies, FUN = isProductive)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be Colony or MultiColony class object!")
  }
  return(ret)
}

# Genome ----

#' @rdname simulateHoneyBeeGenomes
#' @title Simulate the Honey bee genome
#'
#' @description Level 0 function that returns simulated honeybee genomes
#'
#' @param nMelN integer, number of Apis mellifera mellifera North individuals to simulate
#' @param nMelS integer, number of Apis mellifera mellifera South individuals to simulate
#' @param nCar integer, number of Apis mellifera carnica individuals to simulate
#' @param nLig integer, number of Apis mellifera ligustica individuals to simulate
#' @param Ne integer, effective size of the simulated population. Currently set to
#'   170,000, according to Wallberg et al., 2014. Would discourage you to change it
#'   since it is linked to the parameters of the demographic model we use for the simulation.
#'   However, there might be some edge cases when using a different Ne is necessary,
#'   but proceed with caution.
#' @param ploidy integer, the ploidy of the individuals
#' @param nChr integer, number of chromosomes to simulate
#' @param nSegSites integer, number of segregating sites to keep per chromosome
#' @param nBp integer, base pair length of chromosome
#' @param genLen numeric, genetic length of chromosome in Morgans
#' @param mutRate numeric, per base pair mutation rate
#' @param recRate numeric, per base pair recombination rate
#' @param nThreads integer, if OpenMP is available, this will allow for simulating
#'   chromosomes in parallel. If \code{NULL}, the number of threads is
#'   automatically detected
#'
#' @return \code{\link{MapPop-class}}
#'
#' @references
#' Wallberg, A., Bunikis, I., Pettersson, O.V. et al.
#'   A hybrid de novo genome assembly of the honeybee, Apis mellifera,
#'   with chromosome-length scaffolds. 2019, BMC Genomics 20:275.
#'   https://doi.org/10.1186/s12864-019-5642-0
#'
#' Beye M, Gattermeier I, Hasselmann M, et al. Exceptionally high levels
#'   of recombination across the honey bee genome.
#'   2006, Genome Res 16(11):1339-1344. doi:10.1101/gr.5680406
#'
#' Wallberg, A., Han, F., Wellhagen, G. et al. A worldwide survey of
#'   genome sequence variation provides insight into the evolutionary
#'   history of the honeybee Apis mellifera.
#'   2014, Nat Genet 46:1081–1088. https://doi.org/10.1038/ng.3077
#'
#' Yang S, Wang L, Huang J, Zhang X, Yuan Y, Chen JQ, Hurst LD, Tian D.
#'   Parent-progeny sequencing indicates higher mutation rates in heterozygotes.
#'   2015, Nature 523(7561):463-7. doi: 10.1038/nature14649.
#'
#' @examples
#' # founderGenomes <- simulateHoneyBeeGenomes(
#' #   nInd = 10, nChr = 1,
#' #   nSegSites = 2, Ne = 10
#' # )
#' @export
simulateHoneyBeeGenomes <- function(nMelN = 0L,
                                    nMelS = 0L,
                                    nCar = 0L,
                                    nLig = 0L,
                                    Ne = 170000L,
                                    ploidy = 2L,
                                    nChr = 16L,
                                    nSegSites = 100L,
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

  # For now, the user cannot change this since all the model was specified with these numbers
  genInt <- 1

  nInd <- (nMelN + nMelS + nCar + nLig) / 2
  mu <- 4 * Ne * mutRate
  rho <- 4 * Ne * recRate

  # M lineage split
  timeM_y <- 13000
  timeM_g <- timeM_y / genInt
  timeNeM <- timeM_g / (4 * Ne)
  NeM <- 210000
  NeChangeM <- NeM / Ne

  # C lineage split
  timeC_y <- 25000
  timeC_g <- timeC_y / genInt
  timeNeC <- timeC_g / (Ne * 4)
  NeC <- 190000
  NeChangeC <- NeC / Ne

  # M - C lineage split
  timeMC_y <- 300000
  timeMC_g <- timeMC_y / genInt
  timeNeMC <- timeMC_g / (Ne * 4)
  NeMC <- 350000
  NeChangeMC <- NeMC / Ne

  command <- paste0(
    nBp, " -t ", mu, " -r ", rho, " -I 4 ", nMelS, " ", nMelN, " ", nCar, " ", nLig,
    " -ej ", timeNeM, " 2 1 -en ", timeNeM + 0.00001, " 1 ", NeChangeM,
    " -ej ", timeNeC, " 4 3 -en ", timeNeC + 0.00001, " 3 ", NeChangeC,
    " -ej ", timeNeMC, " 3 1 -en ", timeNeMC + 0.00001, " 1 ", NeChangeMC
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
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 2)
#'
#' (tmp <- getSegSiteHaplo(drones))
#' SIMplyBee:::reduceDroneHaplo(haplo = tmp, pop = drones)
#'
#' (tmp <- getSegSiteHaplo(c(basePop, drones)))
#' SIMplyBee:::reduceDroneHaplo(haplo = tmp, pop = c(basePop, drones))
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
  ret <- haplo[sel, , drop = FALSE]
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
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 2)
#'
#' (tmp <- getSegSiteGeno(drones))
#' SIMplyBee:::reduceDroneGeno(geno = tmp, pop = drones)
#'
#' (tmp <- getSegSiteGeno(c(basePop, drones)))
#' SIMplyBee:::reduceDroneGeno(geno = tmp, pop = c(basePop, drones))
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
    geno[sel, ] <- geno[sel, , drop = FALSE] / 2
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
#'   \code{\link{MultiColony-class}}
#' @param nInd numeric, for how many individuals; if \code{NULL} all individuals
#'   are taken; this can be useful as a test of sampling individuals
#' @param allele character, either "all" for both alleles or an integer for a
#'   single allele, use a value of 1 for female allele and a value of 2 for male
#'   allele
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if \code{TRUE}, the function will return a set of
#'   csd alleles across the entire population, colony, or multicolony (not
#'   separately for each caste when \code{x} is a colony or each caste of
#'   each colony when \code{x} is a multicolony. This is a way to get one single
#'   object as an output across castes or colonies. Note this has nothing to do
#'   with the colony collapse. It's like \code{paste(..., collapse = TRUE)}.
#'   Default is \code{FALSE}. See examples about this behaviour.
#' @param unique logical, return only the unique set of csd alleles. This argument
#'   interacts with \code{collapse}. Default is \code{FALSE}. See examples about
#'   this behaviour.
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details If both collapse and unique are \code{TRUE}, the function returns a
#'   unique set of csd alleles in the entire population, colony, or multicolony
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Pop-class}}, list
#'   of matrices with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   (list nodes named by caste) and list of a list of matrices with haplotypes
#'   when \code{x} is \code{\link{MultiColony-class}}, outer list is named by
#'   colony id when \code{x} is \code{\link{MultiColony-class}}; \code{NULL} when
#'   \code{x} is \code{NULL}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#'
#' # Use getCsdAlleles on a Population
#' getCsdAlleles(getQueen(colony))
#' getCsdAlleles(getFathers(colony))
#' getCsdAlleles(getWorkers(colony))
#' getCsdAlleles(getDrones(colony))
#'
#' # Use getCsdAlleles on a Colony
#' getCsdAlleles(colony)
#' getCsdAlleles(colony, unique = TRUE)
#' getCsdAlleles(colony, collapse = TRUE)
#' getCsdAlleles(colony, collapse = TRUE, unique = TRUE)
#'
#' # Use getCsdAlleles on a MultiColony
#' getCsdAlleles(apiary)
#' getCsdAlleles(apiary, unique = TRUE)
#' getCsdAlleles(apiary, collapse = TRUE)
#' getCsdAlleles(apiary, collapse = TRUE, unique = TRUE)
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
    ret <- pullMarkerHaplo(x, markers = paste(simParamBee$csdChr,
                                              simParamBee$csdPosStart:simParamBee$csdPosStop,
                                              sep="_"))
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneHaplo(haplo = ret, pop = x)
    }
    if (unique) {
      ret <- ret[!duplicated(x = ret), , drop = FALSE]
    }
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
    for (caste in names(ret)) {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCsdAlleles(
          x = tmp, allele = allele,
          dronesHaploid = dronesHaploid,
          unique = unique,
          simParamBee = simParamBee
        )
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
      if (unique) {
        ret <- ret[!duplicated(ret), , drop = FALSE]
      }
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(
      X = x@colonies, FUN = getCsdAlleles, nInd = nInd,
      allele = allele, dronesHaploid = dronesHaploid,
      collapse = collapse, unique = unique,
      simParamBee = simParamBee
    )
    if (collapse) {
      ret <- do.call("rbind", ret)
      if (unique) {
        ret <- ret[!duplicated(ret), , drop = FALSE]
      }
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
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
#'   \code{\link{MultiColony-class}}
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
#'   when \code{x} is \code{\link{MultiColony-class}}, outer list is named by
#'   colony id when \code{x} is \code{\link{MultiColony-class}}; \code{NULL} when
#'   \code{x} is \code{NULL}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 4)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' # Use getCsdGeno on a Population
#' getCsdGeno(getQueen(colony))
#' getCsdGeno(getFathers(colony))
#' getCsdGeno(getWorkers(colony))
#'
#' # Using dronesHaploid = TRUE returns drones as haploids instead of double haploids
#' getCsdGeno(getDrones(colony), nInd = 3, dronesHaploid = TRUE)
#' # Using dronesHaploid = FALSE returns drones as double haploids
#' getCsdGeno(getDrones(colony), nInd = 3, dronesHaploid = FALSE)
#'
#' # Use getCsdGeno on a Colony
#' getCsdGeno(colony)
#'
#' # Compare the use of the dronesHaploid parameter
#' getCsdGeno(colony, nInd = 4, dronesHaploid = TRUE)
#' getCsdGeno(colony, nInd = 4, dronesHaploid = FALSE)
#'
#' # Use getCsdGeno on a MultiColony
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
    ret <- pullMarkerGeno(x, markers = paste(simParamBee$csdChr,
                                             simParamBee$csdPosStart:simParamBee$csdPosStop,
                                             sep="_"))
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneGeno(geno = ret, pop = x)
    }
  } else if (isColony(x)) {
    ret <- vector(mode = "list", length = 5)
    names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
    for (caste in names(ret)) {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCsdGeno(
          x = tmp, dronesHaploid = dronesHaploid,
          simParamBee = simParamBee
        )
      }
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(
      X = x@colonies, FUN = getCsdGeno, nInd = nInd,
      dronesHaploid = dronesHaploid, simParamBee = simParamBee
    )
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
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
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 4)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' # Caste members taken from Colony class
#' (tmp <- getCsdGeno(getQueen(colony)))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#'
#' (tmp <- getCsdGeno(getVirginQueens(colony)))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#'
#' (tmp <- getCsdGeno(getWorkers(colony)))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#'
#' # Caste members taken from MultiColony class
#' (tmp <- getCsdGeno(getQueen(apiary[[1]])))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#'
#' (tmp <- getCsdGeno(getVirginQueens(apiary[[1]])))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#'
#' (tmp <- getCsdGeno(getWorkers(apiary[[1]])))
#' SIMplyBee:::isGenoHeterozygous(tmp)
#' # Not exporting this function, since its just a helper and quite specific for
#' #   our csd locus implementation
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
#'   \code{\link{Colony-class}} and \code{\link{MultiColony-class}} if needed
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 4)
#'
#' # Use isCsdHeterozygous on a Population
#' isCsdHeterozygous(getQueen(colony))
#' isCsdHeterozygous(getWorkers(colony))
#' isCsdHeterozygous(getVirginQueens(colony))
#' isCsdHeterozygous(getDrones(colony))
#' @export
isCsdHeterozygous <- function(pop, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  geno <- getCsdGeno(x = pop, simParamBee = simParamBee, dronesHaploid = FALSE)
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
#'   \code{\link{MultiColony-class}}
#' @param collapse logical, if \code{TRUE}, the function will return the number
#'   of distinct csd alleles in either the entire population, colony, or
#'   multicolony. Note this has nothing to do with the colony collapse. It's
#'   like \code{paste(..., collapse = TRUE)}. Default is \code{FALSE}. See
#'   examples about this behaviour.Default is \code{FALSE}.
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
#'   list of a list of integer when \code{x} is \code{\link{MultiColony-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}; the integer rep
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 4)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' nCsdAlleles(getQueen(colony))
#' nCsdAlleles(getFathers(colony))
#' nCsdAlleles(getVirginQueens(colony))
#' nCsdAlleles(getWorkers(colony))
#' nCsdAlleles(getDrones(colony))
#'
#' nCsdAlleles(colony)
#' nCsdAlleles(colony, collapse = TRUE)
#'
#' nCsdAlleles(apiary)
#' nCsdAlleles(apiary, collapse = TRUE)
#' @export
nCsdAlleles <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    haplo <- getCsdAlleles(x = x, unique = TRUE, simParamBee = simParamBee)
    ret <- nrow(haplo)
  } else if (isColony(x)) {
    if (collapse) {
      haplo <- getCsdAlleles(x = x, collapse = TRUE, unique = TRUE, simParamBee = simParamBee)
      ret <- nrow(haplo)
    } else {
      ret <- vector(mode = "list", length = 6)
      names(ret) <- c("queen", "fathers", "queenAndFathers", "workers", "drones", "virginQueens")
      ret$queen <- nCsdAlleles(x = getQueen(x), simParamBee = simParamBee)
      ret$fathers <- nCsdAlleles(x = getFathers(x), simParamBee = simParamBee)
      ret$workers <- nCsdAlleles(x = getWorkers(x), simParamBee = simParamBee)
      ret$drones <- nCsdAlleles(x = getDrones(x), simParamBee = simParamBee)
      ret$virginQueens <- nCsdAlleles(x = getVirginQueens(x), simParamBee = simParamBee)
      # Can't combine queen (diploid) and fathers (haploid) using c(getQueen(x), getFathers(x)),
      #   so we will get their alleles and count them
      tmp <- rbind(
        getCsdAlleles(x = getQueen(x), simParamBee = simParamBee),
        getCsdAlleles(x = getFathers(x), simParamBee = simParamBee)
      )
      tmp <- tmp[!duplicated(tmp), , drop = FALSE]
      ret$queenAndFathers <- nrow(tmp)
    }
  } else if (isMultiColony(x)) {
    if (collapse) {
      haplo <- getCsdAlleles(x = x, collapse = TRUE, unique = TRUE, simParamBee = simParamBee)
      ret <- nrow(haplo)
    } else {
      ret <- lapply(X = x@colonies, FUN = nCsdAlleles, collapse = collapse, simParamBee = simParamBee)
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

# get* aliases for AlphaSimR pull* ----

#' @rdname getIbdHaplo
#' @title Access IBD haplotypes
#'
#' @description Level 0 function that returns IBD (identity by descent)
#'   haplotypes.
#'
#' @param pop \code{\link{Pop-class}}
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param snpChip integer, indicating which SNP array loci are to be retrieved,
#'   if \code{NULL}, all sites are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#' @details This is an alias for the AlphaSimR function to distinguish between getting
#'   and pulling
#' @seealso \code{\link{pullIbdHaplo}}
#' @return Matrix of haplotypes
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' # Use getIbdHaplo on a Population
#' getIbdHaplo(getWorkers(colony))
#' getIbdHaplo(getQueen(colony))
#' getIbdHaplo(getDrones(colony))
#' @export
getIbdHaplo <- function(pop, chr = NULL, snpChip = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullIbdHaplo(pop = pop, chr = chr, snpChip = snpChip, simParam = simParam)
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
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullQtlHaplo}}
#' @details This is an alias for the AlphaSimR function to distinguish between getting
#'   and pulling
#' @return Matrix of haplotypes
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' # Use getQtlHaplo on a Population
#' getQtlHaplo(getWorkers(colony))
#' getQtlHaplo(getQueen(colony))
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
#'   are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#' @details This is an alias for the AlphaSimR function to distinguish between getting
#'   and pulling
#' @seealso \code{\link{pullQtlGeno}}
#'
#' @return Matrix of genotypes
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' # Use getQtlGeno on a Population
#' getQtlGeno(getWorkers(colony))
#' getQtlGeno(getQueen(colony))
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
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullSegSiteHaplo}}
#' @details This is an alias for the AlphaSimR function to distinguish between getting
#'   and pulling
#' @return Matrix of haplotypes
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' # Use getSegSiteHaplo on a Population
#' getSegSiteHaplo(getWorkers(colony))
#' getSegSiteHaplo(getQueen(colony))
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
#'   are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullSegSiteHaplo}}
#' @details This is an alias for the AlphaSimR function to distinguish between getting
#'   and pulling
#' @return Matrix of genotypes
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' # Use getSegSiteGeno on a Population
#' getSegSiteGeno(getWorkers(colony))
#' getSegSiteGeno(getQueen(colony))
#' getSegSiteGeno(getDrones(colony))
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
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullSnpHaplo}}
#' @details This is an alias for the AlphaSimR function to distinguish between getting
#'   and pulling
#' @return Matrix of haplotypes
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addSnpChip(nSnpPerChr = 5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' # Use getSnpHaplo on a Population
#' getSnpHaplo(getWorkers(colony))
#' getSnpHaplo(getQueen(colony))
#' getSnpHaplo(getDrones(colony))
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
#'   are retrieved
#' @param simParam \code{\link{SimParam}}, global simulation parameters
#'
#' @seealso \code{\link{pullSnpHaplo}}
#' @details This is an alias for the AlphaSimR function to distinguish between getting
#'   and pulling
#' @return Matrix of genotypes
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addSnpChip(nSnpPerChr = 5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony  class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' # Use getSnpGeno on a Population
#' getSnpGeno(getWorkers(colony))
#' getSnpGeno(getQueen(colony))
#' getSnpGeno(getDrones(colony))
#' @export
getSnpGeno <- function(pop, snpChip = 1, chr = NULL, simParam = NULL) {
  if (isPop(pop)) {
    ret <- pullSnpGeno(pop = pop, snpChip = snpChip, chr = chr, simParam = simParam)
  } else {
    stop("Argument pop must be a Pop class object!")
  }
  return(ret)
}

# get*Ibd* ----

#' @rdname getCasteIbdHaplo
#' @title Access IBD haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns IBD (identity by descent)
#'   haplotypes of individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param snpChip integer, indicating which SNP array loci are to be retrieved,
#'   if \code{NULL}, all sites are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with haplotypes of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getIbdHaplo}} and \code{\link{pullIbdHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteIbdHaplo(x = colony, caste = "queen")
#' getQueenIbdHaplo(colony)
#'
#' getCasteIbdHaplo(colony, caste = "fathers")
#' getCasteIbdHaplo(colony, caste = "fathers", nInd = 2)
#' getCasteIbdHaplo(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersIbdHaplo(colony)
#' getFathersIbdHaplo(colony, nInd = 2)
#'
#' getCasteIbdHaplo(colony, caste = "virginQueens")
#' getVirginQueensIbdHaplo(colony)
#'
#' getCasteIbdHaplo(colony, caste = "workers")
#' getWorkersIbdHaplo(colony)
#'
#' getCasteIbdHaplo(colony, caste = "drones")
#' getDronesIbdHaplo(colony)
#'
#' # Get haplotypes for all individuals
#' getCasteIbdHaplo(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getCasteIbdHaplo(colony, caste = "all", collapse = TRUE)
#'
#' # MultiColony
#' getCasteIbdHaplo(x = apiary, caste = "queen")
#' getQueenIbdHaplo(apiary)
#' # Or collapse all the haplotypes into a single matrix
#' getQueenIbdHaplo(apiary, collapse = TRUE)
#'
#' getCasteIbdHaplo(apiary, caste = "fathers")
#' getCasteIbdHaplo(apiary, caste = "fathers", nInd = 2)
#' getCasteIbdHaplo(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getCasteIbdHaplo(apiary, caste = "all")
#' getCasteIbdHaplo(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteIbdHaplo <- function(x, caste, nInd = NULL, chr = NULL, snpChip = NULL,
                             dronesHaploid = TRUE, collapse = FALSE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteIbdHaplo(x = x, caste = caste, nInd = nInd, chr = chr,
                                snpChip = snpChip, dronesHaploid = dronesHaploid,
                                collapse = collapse, simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getIbdHaplo(pop = tmp, chr = chr, snpChip = snpChip, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneHaplo(haplo = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteIbdHaplo(
        x = x[[colony]],
        caste = caste,
        nInd = nInd,
        chr = chr,
        snpChip = snpChip,
        dronesHaploid = dronesHaploid,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of the queen
#' @export
getQueenIbdHaplo <- function(x, chr = NULL, snpChip = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteIbdHaplo(x,
                          caste = "queen",
                          chr = chr,
                          snpChip = snpChip,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of fathers
#' @export
getFathersIbdHaplo <- function(x, nInd = NULL, chr = NULL, snpChip = NULL,
                               dronesHaploid = TRUE,
                               collapse = FALSE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteIbdHaplo(x,
                          caste = "fathers", nInd = nInd, chr = chr,
                          snpChip = snpChip,
                          dronesHaploid = dronesHaploid,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of virgin queens
#' @export
getVirginQueensIbdHaplo <- function(x, nInd = NULL, chr = NULL, snpChip = NULL,
                                    collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteIbdHaplo(x,
                          caste = "virginQueens",
                          nInd = nInd,
                          chr = chr,
                          snpChip = snpChip,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of workers
#' @export
getWorkersIbdHaplo <- function(x, nInd = NULL,
                               chr = NULL, snpChip = NULL,
                               collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteIbdHaplo(x,
                          caste = "workers",
                          nInd = nInd,
                          chr = chr,
                          snpChip = snpChip,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteIbdHaplo Access IBD haplotype data of drones
#' @export
getDronesIbdHaplo <- function(x, nInd = NULL, chr = NULL, snpChip = NULL,
                              dronesHaploid = TRUE,
                              collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteIbdHaplo(x,
                          caste = "drones",
                          nInd = nInd, chr = chr,
                          snpChip = snpChip,
                          dronesHaploid = dronesHaploid,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

# get*Qtl* ----

#' @rdname getCasteQtlHaplo
#' @title Access QTL haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns QTL haplotypes of individuals in a
#'   caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param trait numeric, indicates which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with haplotypes of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getQtlHaplo}} and \code{\link{pullQtlHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteQtlHaplo(colony, caste = "queen")
#' getQueenQtlHaplo(colony)
#'
#' getCasteQtlHaplo(colony, caste = "fathers")
#' getCasteQtlHaplo(colony, caste = "fathers", nInd = 2)
#' getCasteQtlHaplo(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersQtlHaplo(colony)
#' getFathersQtlHaplo(colony, nInd = 2)
#'
#' getCasteQtlHaplo(colony, caste = "virginQueens")
#' getVirginQueensQtlHaplo(colony)
#'
#' getCasteQtlHaplo(colony, caste = "workers")
#' getWorkersQtlHaplo(colony)
#'
#' getCasteQtlHaplo(colony, caste = "drones")
#' getDronesQtlHaplo(colony)
#'
#' # Get haplotypes for all individuals
#' getCasteQtlHaplo(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getCasteQtlHaplo(colony, caste = "all", collapse = TRUE)
#'
#' getCasteQtlHaplo(apiary, caste = "queen")
#' getQueenQtlHaplo(apiary)
#'
#' getCasteQtlHaplo(apiary, caste = "fathers")
#' getCasteQtlHaplo(apiary, caste = "fathers", nInd = 2)
#' getCasteQtlHaplo(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getCasteQtlHaplo(apiary, caste = "all")
#' getCasteQtlHaplo(apiary, caste = "all", collapse = TRUE)
#'
#' @export
getCasteQtlHaplo <- function(x, caste, nInd = NULL,
                             trait = 1, haplo = "all", chr = NULL,
                             dronesHaploid = TRUE, collapse = FALSE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteQtlHaplo(x = x, caste = caste, nInd = nInd,
                                trait = trait, haplo = haplo,
                                chr = chr, dronesHaploid = dronesHaploid,
                                collapse = collapse,
                                simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getQtlHaplo(pop = tmp, haplo = haplo, trait = trait, chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneHaplo(haplo = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteQtlHaplo(
        x = x[[colony]], caste = caste, nInd = nInd,
        trait = trait, haplo = haplo, chr = chr,
        dronesHaploid = dronesHaploid,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of the queen
#' @export
getQueenQtlHaplo <- function(x,
                             trait = 1, haplo = "all", chr = NULL,
                             collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlHaplo(x,
                          caste = "queen",
                          trait = trait, haplo = haplo, chr = chr,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of fathers
#' @export
getFathersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL,
                               dronesHaploid = TRUE,
                               collapse = FALSE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlHaplo(x,
                          caste = "fathers", nInd = nInd,
                          trait = trait, haplo = haplo, chr = chr,
                          dronesHaploid = dronesHaploid,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of virgin queens
#' @export
getVirginQueensQtlHaplo <- function(x, nInd = NULL,
                                    trait = 1, haplo = "all", chr = NULL,
                                    collapse = FALSE,
                                    simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlHaplo(x,
                          caste = "virginQueens", nInd = nInd,
                          trait = trait, haplo = haplo, chr = chr,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype of workers
#' @export
getWorkersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL,
                               collapse = FALSE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlHaplo(x,
                          caste = "workers", nInd = nInd,
                          trait = trait, haplo = haplo, chr = chr,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteQtlHaplo Access QTL haplotype data of drones
#' @export
getDronesQtlHaplo <- function(x, nInd = NULL,
                              trait = 1, haplo = "all", chr = NULL,
                              dronesHaploid = TRUE,
                              collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlHaplo(x,
                          caste = "drones", nInd = nInd,
                          trait = trait, haplo = haplo, chr = chr,
                          dronesHaploid = dronesHaploid,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname getCasteQtlGeno
#' @title Access QTL genotypes of individuals in a caste
#'
#' @description Level 0 function that returns QTL genotypes of individuals in a
#'   caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param trait numeric, indicates which trait's QTL genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with genotypes of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getQtlGeno}} and \code{\link{pullQtlGeno}}
#'
#' @return matrix with genotypes when \code{x} is \code{\link{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteQtlGeno(colony, caste = "queen")
#' getQueenQtlGeno(colony)
#'
#' getCasteQtlGeno(colony, caste = "fathers")
#' getCasteQtlGeno(colony, caste = "fathers", nInd = 2)
#' getCasteQtlGeno(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersQtlGeno(colony)
#' getFathersQtlGeno(colony, nInd = 2)
#'
#' getCasteQtlGeno(colony, caste = "virginQueens")
#' getVirginQueensQtlGeno(colony)
#'
#' getCasteQtlGeno(colony, caste = "workers")
#' getWorkersQtlGeno(colony)
#'
#' getCasteQtlGeno(colony, caste = "drones")
#' getDronesQtlGeno(colony)
#'
#' # Get genotypes for all individuals
#' getCasteQtlGeno(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getCasteQtlGeno(colony, caste = "all", collapse = TRUE)
#'
#' getCasteQtlGeno(apiary, caste = "queen")
#' getQueenQtlGeno(apiary)
#'
#' getCasteQtlGeno(apiary, caste = "fathers")
#' getCasteQtlGeno(apiary, caste = "fathers", nInd = 2)
#' getCasteQtlGeno(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the genotypes of all individuals either by colony or in a single matrix
#' getCasteQtlGeno(apiary, caste = "all")
#' getCasteQtlGeno(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteQtlGeno <- function(x, caste, nInd = NULL,
                            trait = 1, chr = NULL, dronesHaploid = TRUE,
                            collapse = FALSE,
                            simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteQtlGeno(x = x, caste = caste, nInd = nInd,
                               trait = trait, chr = chr,
                               dronesHaploid = dronesHaploid,
                               collapse = collapse,
                               simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getQtlGeno(pop = tmp, trait = trait, chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneGeno(geno = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteQtlGeno(
        x = x[[colony]], caste = caste, nInd = nInd,
        trait = trait, chr = chr,
        dronesHaploid = dronesHaploid,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of the queen
#' @export
getQueenQtlGeno <- function(x,
                            trait = 1, chr = NULL,
                            collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlGeno(x,
                         caste = "queen",
                         trait = trait, chr = chr,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of fathers
#' @export
getFathersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL, dronesHaploid = TRUE,
                              collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlGeno(x,
                         caste = "fathers", nInd = nInd,
                         trait = trait, chr = chr,
                         dronesHaploid = dronesHaploid,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of virgin queens
#' @export
getVirginQueensQtlGeno <- function(x, nInd = NULL,
                                   trait = 1, chr = NULL,
                                   collapse = FALSE,
                                   simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlGeno(x,
                         caste = "virginQueens", nInd = nInd,
                         trait = trait, chr = chr,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of workers
#' @export
getWorkersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL,
                              collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlGeno(x,
                         caste = "workers", nInd = nInd,
                         trait = trait, chr = chr,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteQtlGeno Access QTL genotype data of drones
#' @export
getDronesQtlGeno <- function(x, nInd = NULL,
                             trait = 1, chr = NULL, dronesHaploid = TRUE,
                             collapse = FALSE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteQtlGeno(x,
                         caste = "drones", nInd = nInd,
                         trait = trait, chr = chr,
                         dronesHaploid = dronesHaploid,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

# get*SegSite* ----

#' @rdname getCasteSegSiteHaplo
#' @title Access haplotypes for all segregating sites of individuals in a
#'   caste
#'
#' @description Level 0 function that returns haplotypes for all segregating
#'   sites of individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with haplotypes of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getSegSiteHaplo}} and \code{\link{pullSegSiteHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteSegSiteHaplo(colony, caste = "queen")
#' getQueenSegSiteHaplo(colony)
#'
#' getCasteSegSiteHaplo(colony, caste = "fathers")
#' getCasteSegSiteHaplo(colony, caste = "fathers", nInd = 2)
#' getCasteSegSiteHaplo(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersSegSiteHaplo(colony)
#' getFathersSegSiteHaplo(colony, nInd = 2)
#'
#' getCasteSegSiteHaplo(colony, caste = "virginQueens")
#' getVirginQueensSegSiteHaplo(colony)
#'
#' getCasteSegSiteHaplo(colony, caste = "workers")
#' getWorkersSegSiteHaplo(colony)
#'
#' getCasteSegSiteHaplo(colony, caste = "drones")
#' getDronesSegSiteHaplo(colony)
#'
#' # Get haplotypes for all individuals
#' getCasteSegSiteHaplo(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getCasteSegSiteHaplo(colony, caste = "all", collapse = TRUE)
#'
#' getCasteSegSiteHaplo(apiary, caste = "queen")
#' getQueenSegSiteHaplo(apiary)
#'
#' getCasteSegSiteHaplo(apiary, caste = "fathers")
#' getCasteSegSiteHaplo(apiary, caste = "fathers", nInd = 2)
#' getCasteSegSiteHaplo(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getCasteSegSiteHaplo(apiary, caste = "all")
#' getCasteSegSiteHaplo(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteSegSiteHaplo <- function(x, caste, nInd = NULL,
                                 haplo = "all", chr = NULL,
                                 dronesHaploid = TRUE, collapse = FALSE,
                                 simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteSegSiteHaplo(x = x, caste = caste, nInd = nInd,
                                    haplo = haplo, chr = chr,
                                    dronesHaploid = dronesHaploid,
                                    collapse = collapse,
                                    simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getSegSiteHaplo(pop = tmp, haplo = haplo, chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneHaplo(haplo = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSegSiteHaplo(
        x = x[[colony]], caste = caste, nInd = nInd,
        haplo = haplo, chr = chr,
        dronesHaploid = dronesHaploid,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of the queen
#' @export
getQueenSegSiteHaplo <- function(x,
                                 haplo = "all", chr = NULL,
                                 collapse = FALSE,
                                 simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteHaplo(x,
                              caste = "queen",
                              haplo = haplo, chr = chr,
                              collapse = collapse,
                              simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of fathers
#' @export
getFathersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL,
                                   dronesHaploid = TRUE,
                                   collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteHaplo(x,
                              caste = "fathers", nInd = nInd,
                              haplo = haplo, chr = chr,
                              dronesHaploid = dronesHaploid,
                              collapse = collapse,
                              simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of virgin queens
#' @export
getVirginQueensSegSiteHaplo <- function(x, nInd = NULL,
                                        haplo = "all", chr = NULL,
                                        collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteHaplo(x,
                              caste = "virginQueens", nInd = nInd,
                              haplo = haplo, chr = chr,
                              collapse = collapse,
                              simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of workers
#' @export
getWorkersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL,
                                   collapse = FALSE,
                                   simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteHaplo(x,
                              caste = "workers", nInd = nInd,
                              haplo = haplo, chr = chr,
                              collapse = collapse,
                              simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSegSiteHaplo Access haplotype data for all segregating sites of drones
#' @export
getDronesSegSiteHaplo <- function(x, nInd = NULL,
                                  haplo = "all", chr = NULL,
                                  dronesHaploid = TRUE,
                                  collapse = FALSE,
                                  simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteHaplo(x,
                              caste = "drones", nInd = nInd,
                              haplo = haplo, chr = chr,
                              dronesHaploid = dronesHaploid,
                              collapse = collapse,
                              simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname getCasteSegSiteGeno
#' @title Access genotypes for all segregating sites of individuals in a
#'   caste
#'
#' @description Level 0 function that returns genotypes for all segregating
#'   sites of individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with genotypes of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getSegSiteGeno}} and \code{\link{pullSegSiteGeno}}
#'
#' @return matrix with genotypes when \code{x} is \code{\link{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteSegSiteGeno(colony, caste = "queen")
#' getQueenSegSiteGeno(colony)
#'
#' getCasteSegSiteGeno(colony, caste = "fathers")
#' getCasteSegSiteGeno(colony, caste = "fathers", nInd = 2)
#' getCasteSegSiteGeno(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersSegSiteGeno(colony)
#' getFathersSegSiteGeno(colony, nInd = 2)
#'
#' getCasteSegSiteGeno(colony, caste = "virginQueens")
#' getVirginQueensSegSiteGeno(colony)
#'
#' getCasteSegSiteGeno(colony, caste = "workers")
#' getWorkersSegSiteGeno(colony)
#'
#' getCasteSegSiteGeno(colony, caste = "drones")
#' getDronesSegSiteGeno(colony)
#'
#' # Get genotypes for all individuals
#' getCasteSegSiteGeno(colony, caste = "all")
#' # Get all genotypes in a single matrix
#' getCasteSegSiteGeno(colony, caste = "all", collapse = TRUE)
#'
#' getCasteSegSiteGeno(apiary, caste = "queen")
#' getQueenSegSiteGeno(apiary)
#'
#' getCasteSegSiteGeno(apiary, caste = "fathers")
#' getCasteSegSiteGeno(apiary, caste = "fathers", nInd = 2)
#' getCasteSegSiteGeno(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the genotypes of all individuals either by colony or in a single matrix
#' getCasteSegSiteGeno(apiary, caste = "all")
#' getCasteSegSiteGeno(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteSegSiteGeno <- function(x, caste, nInd = NULL,
                                chr = NULL, dronesHaploid = TRUE,
                                collapse = FALSE,
                                simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteSegSiteGeno(x = x, caste = caste, nInd = nInd,
                                   chr = chr, dronesHaploid = dronesHaploid,
                                   collapse = collapse,
                                   simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getSegSiteGeno(pop = tmp, chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneGeno(geno = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSegSiteGeno(
        x = x[[colony]], caste = caste, nInd = nInd,
        chr = chr, dronesHaploid = dronesHaploid,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of the queen
#' @export
getQueenSegSiteGeno <- function(x,
                                chr = NULL, collapse = FALSE,
                                simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteGeno(x,
                             caste = "queen",
                             collapse = collapse,
                             chr = chr, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of fathers
#' @export
getFathersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, dronesHaploid = TRUE,
                                  collapse = FALSE,
                                  simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteGeno(x,
                             caste = "fathers", nInd = nInd,
                             chr = chr, dronesHaploid = dronesHaploid,
                             collapse = collapse,
                             simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of virgin queens
#' @export
getVirginQueensSegSiteGeno <- function(x, nInd = NULL,
                                       chr = NULL, collapse = FALSE,
                                       simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteGeno(x,
                             caste = "virginQueens", nInd = nInd,
                             chr = chr, collapse = collapse,
                             simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of workers
#' @export
getWorkersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, collapse = FALSE,
                                  simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteGeno(x,
                             caste = "workers", nInd = nInd,
                             chr = chr, collapse = collapse,
                             simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSegSiteGeno Access genotype data for all segregating sites of drones
#' @export
getDronesSegSiteGeno <- function(x, nInd = NULL,
                                 chr = NULL, dronesHaploid = TRUE,
                                 collapse = FALSE,
                                 simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSegSiteGeno(x,
                             caste = "drones", nInd = nInd,
                             chr = chr, dronesHaploid = dronesHaploid,
                             collapse = collapse,
                             simParamBee = simParamBee
  )
  return(ret)
}

# get*Snp* ----

#' @rdname getCasteSnpHaplo
#' @title Access SNP array haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns SNP array haplotypes of
#'   individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with haplotypes of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getSnpHaplo}} and \code{\link{pullSnpHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addSnpChip(nSnpPerChr = 5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteSnpHaplo(colony, caste = "queen")
#' getQueenSnpHaplo(colony)
#'
#' getCasteSnpHaplo(colony, caste = "fathers")
#' getCasteSnpHaplo(colony, caste = "fathers", nInd = 2)
#' getCasteSnpHaplo(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersSnpHaplo(colony)
#' getFathersSnpHaplo(colony, nInd = 2)
#'
#' getCasteSnpHaplo(colony, caste = "virginQueens")
#' getVirginQueensSnpHaplo(colony)
#'
#' getCasteSnpHaplo(colony, caste = "workers")
#' getWorkersSnpHaplo(colony)
#'
#' getCasteSnpHaplo(colony, caste = "drones")
#' getDronesSnpHaplo(colony)
#'
#' # Get haplotypes for all individuals
#' getCasteSnpHaplo(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getCasteSnpHaplo(colony, caste = "all", collapse = TRUE)
#'
#' getCasteSnpHaplo(apiary, caste = "queen")
#' getQueenSnpHaplo(apiary)
#'
#' getCasteSnpHaplo(apiary, caste = "fathers")
#' getCasteSnpHaplo(apiary, caste = "fathers", nInd = 2)
#' getCasteSnpHaplo(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getCasteSnpHaplo(apiary, caste = "all")
#' getCasteSnpHaplo(apiary, caste = "all", collapse = TRUE)
#'
#' @export
getCasteSnpHaplo <- function(x, caste, nInd = NULL,
                             snpChip = 1, haplo = "all", chr = NULL,
                             dronesHaploid = TRUE, collapse = FALSE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteSnpHaplo(x = x, caste = caste, nInd = nInd,
                                snpChip = snpChip, haplo = haplo,
                                chr = chr, dronesHaploid = dronesHaploid,
                                collapse = collapse,
                                simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getSnpHaplo(pop = tmp, haplo = haplo, snpChip = snpChip, chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneHaplo(haplo = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSnpHaplo(
        x = x[[colony]], caste = caste, nInd = nInd,
        snpChip = snpChip, haplo = haplo, chr = chr,
        dronesHaploid = dronesHaploid,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of the queen
#' @export
getQueenSnpHaplo <- function(x,
                             snpChip = 1, haplo = "all", chr = NULL,
                             collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpHaplo(x,
                          caste = "queen",
                          snpChip = snpChip, haplo = haplo, chr = chr,
                          collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of fathers
#' @export
getFathersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL,
                               dronesHaploid = TRUE, collapse = FALSE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpHaplo(x,
                          caste = "fathers", nInd = nInd,
                          snpChip = snpChip, haplo = haplo, chr = chr,
                          dronesHaploid = dronesHaploid,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of virgin queens
#' @export
getVirginQueensSnpHaplo <- function(x, nInd = NULL,
                                    snpChip = 1, haplo = "all", chr = NULL,
                                    collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpHaplo(x,
                          caste = "virginQueens", nInd = nInd,
                          snpChip = snpChip, haplo = haplo, chr = chr,
                          collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype of workers
#' @export
getWorkersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL,
                               collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpHaplo(x,
                          caste = "workers", nInd = nInd,
                          snpChip = snpChip, haplo = haplo, chr = chr,
                          collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSnpHaplo Access SNP array haplotype data of drones
#' @export
getDronesSnpHaplo <- function(x, nInd = NULL,
                              snpChip = 1, haplo = "all", chr = NULL,
                              dronesHaploid = TRUE, collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpHaplo(x,
                          caste = "drones", nInd = nInd,
                          snpChip = snpChip, haplo = haplo, chr = chr,
                          dronesHaploid = dronesHaploid,
                          collapse = collapse,
                          simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname getCasteSnpGeno
#' @title Access SNP array genotypes of individuals in a caste
#'
#' @description Level 0 function that returns SNP array genotypes of individuals
#'   in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with genotypes of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getSnpGeno}} and \code{\link{pullSnpGeno}}
#'
#' @return matrix with genotypes when \code{x} is \code{\link{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addSnpChip(nSnpPerChr = 5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteSnpGeno(colony, caste = "queen")
#' getQueenSnpGeno(colony)
#'
#' getCasteSnpGeno(colony, caste = "fathers")
#' getCasteSnpGeno(colony, caste = "fathers", nInd = 2)
#' getCasteSnpGeno(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersSnpGeno(colony)
#' getFathersSnpGeno(colony, nInd = 2)
#'
#' getCasteSnpGeno(colony, caste = "virginQueens")
#' getVirginQueensSnpGeno(colony)
#'
#' getCasteSnpGeno(colony, caste = "workers")
#' getWorkersSnpGeno(colony)
#'
#' getCasteSnpGeno(colony, caste = "drones")
#' getDronesSnpGeno(colony)
#'
#' # Get genotypes for all individuals
#' getCasteSnpGeno(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getCasteSnpGeno(colony, caste = "all", collapse = TRUE)
#'
#' getCasteSnpGeno(apiary, caste = "queen")
#' getQueenSnpGeno(apiary)
#'
#' getCasteSnpGeno(apiary, caste = "fathers")
#' getCasteSnpGeno(apiary, caste = "fathers", nInd = 2)
#' getCasteSnpGeno(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getCasteSnpGeno(apiary, caste = "all")
#' getCasteSnpGeno(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteSnpGeno <- function(x, caste, nInd = NULL,
                            snpChip = 1, chr = NULL, dronesHaploid = TRUE,
                            collapse = FALSE,
                            simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteSnpGeno(x = x, caste = caste, nInd = nInd,
                               snpChip = snpChip, chr = chr,
                               dronesHaploid = dronesHaploid, collapse = collapse,
                               simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getSnpGeno(pop = tmp, snpChip = snpChip, chr = chr, simParam = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneGeno(geno = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteSnpGeno(
        x = x[[colony]], caste = caste, nInd = nInd,
        snpChip = snpChip, chr = chr,
        dronesHaploid = dronesHaploid,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of the queen
#' @export
getQueenSnpGeno <- function(x,
                            snpChip = 1, chr = NULL,
                            collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpGeno(x,
                         caste = "queen",
                         snpChip = snpChip, chr = chr,
                         collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of fathers
#' @export
getFathersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL, dronesHaploid = TRUE,
                              collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpGeno(x,
                         caste = "fathers", nInd = nInd,
                         snpChip = snpChip, chr = chr,
                         dronesHaploid = dronesHaploid,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of virgin queens
#' @export
getVirginQueensSnpGeno <- function(x, nInd = NULL,
                                   snpChip = 1, chr = NULL,
                                   collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpGeno(x,
                         caste = "virginQueens", nInd = nInd,
                         snpChip = snpChip, chr = chr,
                         collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of workers
#' @export
getWorkersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL,
                              collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpGeno(x,
                         caste = "workers", nInd = nInd,
                         snpChip = snpChip, chr = chr,
                         collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteSnpGeno Access SNP array genotype data of drones
#' @export
getDronesSnpGeno <- function(x, nInd = NULL,
                             snpChip = 1, chr = NULL, dronesHaploid = TRUE,
                             collapse = FALSE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteSnpGeno(x,
                         caste = "drones", nInd = nInd,
                         snpChip = snpChip, chr = chr,
                         dronesHaploid = dronesHaploid,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

# getPooledGeno and calcGRM* ----

#' @rdname getPooledGeno
#' @title Get a pooled genotype from true genotypes
#'
#' @description Level 0 function that returns a pooled genotype from true
#'   genotypes to mimic genotyping of a pool of colony members.
#'
#' @param x matrix, true genotypes with individuals in rows and sites in columns
#' @param type character, "mean" for average genotype or "count" for the counts
#'   of reference and alternative alleles
#' @param sex character, vector of "F" and "M" to denote the sex of individuals
#'   in \code{x}
#'
#' @return a numeric vector with average allele dosage when \code{type = "mean"}
#'   and a two-row matrix with the counts of reference (1st row) and
#'   alternative (2nd row) alleles
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#'
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#' apiary <- createMultiColony(basePop[2:3], n = 2)
#' apiary <- cross(x = apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' genoQ <- getQueenSegSiteGeno(apiary[[1]])
#' genoF <- getFathersSegSiteGeno(apiary[[1]])
#' genoW <- getWorkersSegSiteGeno(apiary[[1]])
#' genoD <- getDronesSegSiteGeno(apiary[[1]])
#' genoV <- getVirginQueensSegSiteGeno(apiary[[1]])
#'
#' # Pool of drones
#' sexD <- getCasteSex(apiary[[1]], caste = "drones")
#' getPooledGeno(x = genoD, type = "count", sex = sexD)[, 1:10]
#' (poolD <- getPooledGeno(x = genoD, type = "mean", sex = sexD))[, 1:10]
#' # ... compare to queen's genotype
#' genoQ[, 1:10]
#' plot(
#'   y = poolD, x = genoQ, ylim = c(0, 2), xlim = c(0, 2),
#'   ylab = "Average allele dosage in drones",
#'   xlab = "Allele dosage in the queen"
#' )
#'
#' # As an exercise you could repeat the above with different numbers of drones!
#'
#' # Pool of workers
#' getPooledGeno(x = genoW, type = "count")[, 1:10]
#' (poolW <- getPooledGeno(x = genoW, type = "mean"))[, 1:10]
#' # ... compare to fathers' and queen's avearage genotype
#' sexF <- getCasteSex(apiary[[1]], caste = "fathers")
#' sexQ <- rep(x = "F", times = nrow(genoF))
#' sexFQ <- c(sexF, sexQ)
#' genoFQ <- rbind(genoF, genoQ[rep(x = 1, times = nrow(genoF)), ])
#' (poolFQ <- getPooledGeno(x = genoFQ, type = "mean", sex = sexFQ))[, 1:10]
#' plot(
#'   y = poolW, x = poolFQ, ylim = c(0, 2), xlim = c(0, 2),
#'   ylab = "Average allele dosage in workers",
#'   xlab = "Average allele dosage in the queen and fathers"
#' )
#'
#' # As an exercise you could repeat the above with different numbers of workers!
#'
#' @export
getPooledGeno <- function(x, type = NULL, sex = NULL) {
  if (!is.matrix(x)) {
    stop("Argument x must be a matrix class object!")
  }
  n <- nrow(x)
  if (is.null(sex)) {
    warning("Argument sex is NULL. Assuming that all individuals are female/diploid!")
    sex <- rep(x = "F", times = n)
  }
  nPloids <- sum((sex == "F")) * 2 + sum((sex == "M"))
  if (any(!(sex %in% c("F", "M")))) {
    stop("Argument sex must contain only F and M!")
  }
  ret <- apply(X = x, MARGIN = 2, FUN = sum)
  if (type == "mean") {
    ret <- ret / nPloids * 2
    ret <- matrix(ret, nrow = 1, dimnames = list(NULL, names(ret)))
    # / nPloids gives allele frequency and * 2 gives diploid dosage
  } else if (type == "count") {
    ret <- rbind(nPloids - ret, ret)
    rownames(ret) <- c("0", "1")
  } else {
    stop("Argument type must be mean or count!")
  }
  return(ret)
}

#' @rdname calcBeeGRMIbs
#' @title Calculate Genomic Relatedness Matrix (GRM) for honeybees from
#'   Identical By State genomic data
#'
#' @description Level 0 function that returns Genomic Relatedness Matrix (GRM)
#'   for honeybees from Identical By State genomic data (bi-allelic SNP
#'   represented as allele dosages) following the method for the sex X
#'   chromosome (Druet and Legarra, 2020)
#'
#' @param x \code{\link{matrix}} of genotypes represented as allele dosage coded
#'   as 0, 1, or 2 in females (queens or workers) and as 0 or 1 in males
#'   (fathers or drones); individuals are in rows and sites are in columns; no
#'   missing values are allowed (this is not checked - you will get NAs!)
#' @param sex character vector denoting sex for individuals with genotypes in
#'   \code{x} - \code{"F"} for female and \code{"M"} for male
#' @param alleleFreq numeric, vector of allele frequencies for the sites in
#'   \code{x}; if \code{NULL}, then \code{\link{calcBeeAlleleFreq}} is used
#'
#' @return matrix of genomic relatedness coefficients
#'
#' @references Druet and Legarra (2020) Theoretical and empirical comparisons of
#'   expected and realized relationships for the X-chromosome. Genetics
#'   Selection Evolution, 52:50 \url{https://doi.org/10.1186/s12711-020-00570-6}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#'
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#' apiary <- createMultiColony(basePop[2:3], n = 2)
#' apiary <- cross(x = apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' genoQ <- getQueenSegSiteGeno(apiary[[1]])
#' genoF <- getFathersSegSiteGeno(apiary[[1]])
#' genoW <- getWorkersSegSiteGeno(apiary[[1]])
#' genoD <- getDronesSegSiteGeno(apiary[[1]])
#' genoV <- getVirginQueensSegSiteGeno(apiary[[1]])
#' genoMeanW <- apply(X = genoW, MARGIN = 2, FUN = mean)
#' genoMeanD <- apply(X = genoD, MARGIN = 2, FUN = mean)
#'
#' geno <- rbind(genoQ, genoF, genoW, genoD, genoV, genoMeanW, genoMeanD)
#' n <- length(rownames(geno))
#' rownames(geno)[c(n - 1, n)] <- c("mw", "md")
#'
#' sex <- getCasteSex(x = apiary[[1]])
#' sex <- c(
#'   sex$queen, sex$fathers, sex$workers, sex$drones, sex$virginQueens,
#'   "F", "M"
#' )
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
#' # Calculating allele frequencies ourselves (say, to "shift" base population)
#' aF <- calcBeeAlleleFreq(x = geno, sex = sex)
#' hist(aF)
#' GRM2 <- calcBeeGRMIbs(x = geno, sex = sex, alleleFreq = aF)
#' stopifnot(identical(GRM2, GRM))
#' @export
calcBeeGRMIbs <- function(x, sex, alleleFreq = NULL) {
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
  nSite <- ncol(x)
  ploidy <- (sex == "F") + 1
  if (is.null(alleleFreq)) {
    alleleFreq <- calcBeeAlleleFreq(x = x, sex = sex)
  } else {
    if (length(alleleFreq) != nSite) {
      stop(paste0("Argument alleleFreq must be of length: ", nSite, "!"))
    }
  }
  for (site in 1:nSite) {
    x[, site] <- x[, site] - ploidy * alleleFreq[site]
  }
  G <- tcrossprod(x) / (2 * sum(alleleFreq * (1 - alleleFreq)))
  return(G)
}

#' @describeIn calcBeeGRMIbs Calculate allele frequencies from honeybee genotypes
#' @export
calcBeeAlleleFreq <- function(x, sex) {
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
  return(alleleFreq)
}

#' @rdname calcBeeGRMIbd
#' @title Calculate Genomic Relatedness Matrix (GRM) for honeybees from
#'   Identical By Descent genomic data
#'
#' @description Level 0 function that returns Genomic Relatedness Matrix (GRM)
#'   for honeybees from Identical By Descent genomic data (tracked alleles
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
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#' apiary <- createMultiColony(basePop[2:3], n = 2)
#' apiary <- cross(x = apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' haploQ <- getQueenIbdHaplo(apiary[[1]])
#' haploF <- getFathersIbdHaplo(apiary[[1]])
#' haploW <- getWorkersIbdHaplo(apiary[[1]])
#' haploD <- getDronesIbdHaplo(apiary[[1]])
#' haploV <- getVirginQueensIbdHaplo(apiary[[1]])
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
#' qI <- getQueen(apiary[[1]])@id
#' fI <- sort(getFathers(apiary[[1]])@id)
#' wI <- sort(getWorkers(apiary[[1]])@id)
#' dI <- sort(getDrones(apiary[[1]])@id)
#' vI <- sort(getVirginQueens(apiary[[1]])@id)
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
  G <- matrix(
    data = numeric(), nrow = nHap, ncol = nHap,
    dimnames = list(hapId, hapId)
  )
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
  K <- matrix(
    data = 0.0, nrow = nInd, ncol = nHap,
    dimnames = list(idUnique, hapId)
  )
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

# get*Pheno ----

#' @rdname getCastePheno
#' @title Access phenotype values of individuals in a caste
#'
#' @description Level 0 function that returns phenotype values of individuals in a
#'   caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with phenotypes of all the individuals
#'
#' @seealso \code{\link{pheno}}
#'
#' @return vector of genetic values when \code{x} is \code{\link{Colony-class}}
#'   and list of vectors of genetic values when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10, var = 1)
#' SP$setVarE(varE = 1)
#' # TODO: how should we handle the creation of phenotypes when residual variance
#' #       is set (then we get phenotypes automatically and we should not call
#' #       setPheno() below - this overwrites previous phenotypes), but when the
#' #       residual variance is not set, we have to call setPheno()
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/235
#'
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCastePheno(colony, caste = "queen")
#' getQueenPheno(colony)
#'
#' getCastePheno(colony, caste = "fathers")
#' getCastePheno(colony, caste = "fathers", nInd = 2)
#' getCastePheno(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersPheno(colony)
#' getFathersPheno(colony, nInd = 2)
#'
#' getCastePheno(colony, caste = "virginQueens")
#' getVirginQueensPheno(colony)
#'
#' getCastePheno(colony, caste = "workers")
#' getWorkersPheno(colony)
#'
#' getCastePheno(colony, caste = "drones")
#' getDronesPheno(colony)
#'
#' # Get phenotypes for all individuals
#' getCastePheno(colony, caste = "all")
#' # Get all phenotypes in a single matrix
#' getCastePheno(colony, caste = "all", collapse = TRUE)
#'
#' getCastePheno(apiary, caste = "queen")
#' getQueenPheno(apiary)
#'
#' getCastePheno(apiary, caste = "fathers")
#' getCastePheno(apiary, caste = "fathers", nInd = 2)
#' getCastePheno(apiary, caste = "fathers", nInd = 2) # random sample!
#' getFathersPheno(apiary)
#' getFathersPheno(apiary, nInd = 2)
#'
#' getCastePheno(apiary, caste = "virginQueens")
#' getVirginQueensPheno(apiary)
#'
#' getCastePheno(apiary, caste = "workers")
#' getWorkersPheno(apiary)
#'
#' getCastePheno(apiary, caste = "drones")
#' getDronesPheno(apiary)
#'
#' # Get the phenotypes of all individuals either by colony or in a single matrix
#' getCastePheno(apiary, caste = "all")
#' getCastePheno(apiary, caste = "all", collapse = TRUE)
#' @export
getCastePheno <- function(x, caste, nInd = NULL, collapse = FALSE) {
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCastePheno(x = x, caste = caste, nInd = nInd,
                             collapse = collapse)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- pheno(pop = tmp)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCastePheno(x = x[[colony]], caste = caste, nInd = nInd, collapse = collapse)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCastePheno Access phenotype value of the queen
#' @export
getQueenPheno <- function(x, collapse = FALSE) {
  ret <- getCastePheno(x, caste = "queen", collapse = collapse)
  return(ret)
}

#' @describeIn getCastePheno Access phenotype values of fathers
#' @export
getFathersPheno <- function(x, nInd = NULL, collapse = FALSE) {
  ret <- getCastePheno(x, caste = "fathers", nInd = nInd, collapse = collapse)
  return(ret)
}

#' @describeIn getCastePheno Access phenotype values of virgin queens
#' @export
getVirginQueensPheno <- function(x, nInd = NULL, collapse = FALSE) {
  ret <- getCastePheno(x, caste = "virginQueens", nInd = nInd, collapse = collapse)
  return(ret)
}

#' @describeIn getCastePheno Access phenotype values of workers
#' @export
getWorkersPheno <- function(x, nInd = NULL, collapse = FALSE) {
  ret <- getCastePheno(x, caste = "workers", nInd = nInd, collapse = collapse)
  return(ret)
}

#' @describeIn getCastePheno Access phenotype values of drones
#' @export
getDronesPheno <- function(x, nInd = NULL, collapse = FALSE) {
  ret <- getCastePheno(x, caste = "drones", nInd = nInd, collapse = collapse)
  return(ret)
}

#' @rdname calcColonyValue
#' @title Calculate colony value(s)
#'
#' @description Level 0 function that calculate value(s) of a colony.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param FUN function, that calculates colony value from values of
#'   colony members
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... other arguments of \code{FUN}
#'
#' @seealso \code{\link{mapCasteToColonyValue}} as an example of \code{FUN}
#'
#' @return a matrix with one value or a row of values when \code{x} is
#'   \code{\link{Colony-class}} and a row-named matrix when \code{x} is
#'   \code{\link{MultiColony-class}}, where names are colony IDs
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#'
#' # Define two traits that collectively affect colony honey yield:
#' # 1) queen's effect on colony honey yield, say via pheromone secretion phenotype
#' # 2) workers' effect on colony honey yield, say via foraging ability phenotype
#' # The traits will have a negative genetic correlation of -0.5 and heritability
#' # of 0.25 (on an individual level)
#' mean <- c(20, 0)
#' nWorkers <- 10
#' varA <- c(1, 1 / nWorkers)
#' corA <- matrix(data = c(
#'   1.0, -0.5,
#'   -0.5, 1.0
#' ), nrow = 2, byrow = TRUE)
#' varE <- c(3, 3 / nWorkers)
#' varA / (varA + varE)
#' SP$addTraitADE(nQtlPerChr = 100,
#'                mean = mean,
#'                var = varA, corA = corA,
#'                meanDD = 0.1, varDD = 0.2, corD = corA,
#'                relAA = 0.1, corAA = corA)
#' SP$setVarE(varE = varE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create and cross Colony and MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(colony, nWorkers = nWorkers, nDrones = 3)
#' apiary <- createMultiColony(basePop[3:5], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(apiary, nWorkers = nWorkers, nDrones = 3)
#'
#' # Colony value - shorthand version
#' # (using the default mapCasteToColony*() functions - you can provide yours instead!)
#' # ... phenotype value
#' calcColonyPheno(colony)
#' calcColonyPheno(apiary)
#' # ... genetic value
#' calcColonyGv(colony)
#' calcColonyGv(apiary)
#' # ... breeding value
#' # TODO: Uncomment getQueenBv() with nTrait>1 once AlphaSimR bug is solved
#' #   https://github.com/gaynorr/AlphaSimR/issues/83
#' #   https://github.com/HighlanderLab/SIMplyBee/issues/399
#' # calcColonyBv(colony)
#' # calcColonyBv(apiary)
#' # ... dominance value
#' # TODO: Uncomment getQueenBv() with nTrait>1 once AlphaSimR bug is solved
#' #   https://github.com/gaynorr/AlphaSimR/issues/83
#' #   https://github.com/HighlanderLab/SIMplyBee/issues/399
#' # calcColonyDd(colony)
#' # calcColonyDd(apiary)
#' # ... epistasis value
#' # TODO: Uncomment getQueenBv() with nTrait>1 once AlphaSimR bug is solved
#' #   https://github.com/gaynorr/AlphaSimR/issues/83
#' #   https://github.com/HighlanderLab/SIMplyBee/issues/399
#' # calcColonyAa(colony)
#' # calcColonyAa(apiary)
#'
#' # Colony value - long version
#' # (using the default mapCasteToColony*() function - you can provide yours instead!)
#' calcColonyValue(colony, FUN = mapCasteToColonyPheno)
#' calcColonyValue(apiary, FUN = mapCasteToColonyPheno)
#'
#' # Colony value - long version - using a function stored in SimParamBee (SP)
#' # (using the default mapCasteToColony*() function - you can provide yours instead!)
#' SP$colonyValueFUN <- mapCasteToColonyPheno
#' calcColonyValue(colony)
#' calcColonyValue(apiary)
#'
#' @export
# TODO: Calculate inheritance, selection and production criteria in the Colony #23
#       https://github.com/HighlanderLab/SIMplyBee/issues/23
# TODO: Do we need to do anything to add GxE to colony values? #353
#       https://github.com/HighlanderLab/SIMplyBee/issues/353
# TODO: Develop theory for colony genetic values under non-linearity/non-additivity #403
#       https://github.com/HighlanderLab/SIMplyBee/issues/403
calcColonyValue <- function(x, FUN = NULL, simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(FUN)) {
    FUN <- simParamBee$colonyValueFUN
  }
  if (is.null(FUN)) {
    stop("You must provide FUN or set it in the SimParamBee object!")
  }
  if (isColony(x)) {
    ret <- FUN(colony = x, ...)
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    # We could create a matrix output container here, BUT we don't know the output
    # dimension of FUN() so we create list and row bind the list nodes later
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- FUN(colony = x[[colony]], ...)
    }
    ret <- do.call("rbind", ret)
    rownames(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony phenotype value from caste individuals' phenotype values
#' @export
calcColonyPheno <- function(x, FUN = mapCasteToColonyPheno, simParamBee = NULL, ...) {
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

# get*Gv ----

#' @rdname getCasteGv
#' @title Access genetic values of individuals in a caste
#'
#' @description Level 0 function that returns genetic values of individuals
#'   in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with genetic values of all the individuals
#'
#' @seealso \code{\link{gv}}
#'
#' @return vector of phenotype values when \code{x} is \code{\link{Colony-class}}
#'   and list of vectors of genetic values when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10, var = 1)
#' SP$addSnpChip(5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteGv(colony, caste = "queen")
#' getQueenGv(colony)
#'
#' getCasteGv(colony, caste = "fathers")
#' getCasteGv(colony, caste = "fathers", nInd = 2)
#' getCasteGv(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersGv(colony)
#' getFathersGv(colony, nInd = 2)
#'
#' getCasteGv(colony, caste = "virginQueens")
#' getVirginQueensGv(colony)
#'
#' getCasteGv(colony, caste = "workers")
#' getWorkersGv(colony)
#'
#' getCasteGv(colony, caste = "drones")
#' getDronesGv(colony)
#'
#' # Get genetic values for all individuals
#' getCasteGv(colony, caste = "all")
#' # Get all genetic values in a single matrix
#' getCasteGv(colony, caste = "all", collapse = TRUE)
#'
#' getCasteGv(apiary, caste = "queen")
#' getQueenGv(apiary)
#'
#' getCasteGv(apiary, caste = "fathers")
#' getCasteGv(apiary, caste = "fathers", nInd = 2)
#' getCasteGv(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the genetic values of all individuals either by colony or in a single matrix
#' getCasteGv(apiary, caste = "all")
#' getCasteGv(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteGv <- function(x, caste, nInd = NULL, collapse = FALSE) {
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteGv(x = x, caste = caste, nInd = nInd,
                          collapse = collapse)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- gv(pop = tmp)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteGv(x = x[[colony]], caste = caste, nInd = nInd, collapse = collapse)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteGv Access genetic value of the queen
#' @export
getQueenGv <- function(x, collapse = FALSE) {
  ret <- getCasteGv(x, caste = "queen", collapse = collapse)
  return(ret)
}

#' @describeIn getCasteGv Access genetic values of fathers
#' @export
getFathersGv <- function(x, nInd = NULL, collapse = FALSE) {
  ret <- getCasteGv(x, caste = "fathers", nInd = nInd, collapse = collapse)
  return(ret)
}

#' @describeIn getCasteGv Access genetic values of virgin queens
#' @export
getVirginQueensGv <- function(x, nInd = NULL, collapse = FALSE) {
  ret <- getCasteGv(x, caste = "virginQueens", nInd = nInd, collapse = collapse)
  return(ret)
}

#' @describeIn getCasteGv Access genetic values of workers
#' @export
getWorkersGv <- function(x, nInd = NULL, collapse = FALSE) {
  ret <- getCasteGv(x, caste = "workers", nInd = nInd, collapse = collapse)
  return(ret)
}

#' @describeIn getCasteGv Access genetic values of drones
#' @export
getDronesGv <- function(x, nInd = NULL, collapse = FALSE) {
  ret <- getCasteGv(x, caste = "drones", nInd = nInd, collapse = collapse)
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony genetic value from caste individuals' genetic values
#' @export
calcColonyGv <- function(x, FUN = mapCasteToColonyGv, simParamBee = NULL, ...) {
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

# get*Bv ----

#' @rdname getCasteBv
#' @title Access breeding values of individuals in a caste
#'
#' @description Level 0 function that returns breeding values of individuals in
#'   a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with breeding valued of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{bv}}
#'
#' @return vector of breeding values when \code{x} is \code{\link{Colony-class}}
#'   and list of vectors of breeding values when \code{x} is
#'   \code{\link{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitA(nQtlPerChr = 10, var = 1)
#' SP$addSnpChip(5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteBv(colony, caste = "queen")
#' getQueenBv(colony)
#'
#' getCasteBv(colony, caste = "fathers")
#' getCasteBv(colony, caste = "fathers", nInd = 2)
#' getCasteBv(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersBv(colony)
#' getFathersBv(colony, nInd = 2)
#'
#' getCasteBv(colony, caste = "virginQueens")
#' getVirginQueensBv(colony)
#'
#' getCasteBv(colony, caste = "workers")
#' getWorkersBv(colony)
#'
#' getCasteBv(colony, caste = "drones")
#' getDronesBv(colony)
#'
#' # Get breeding values for all individuals
#' getCasteBv(colony, caste = "all")
#' # Get all breeding values in a single matrix
#' getCasteBv(colony, caste = "all", collapse = TRUE)
#'
#' getCasteBv(apiary, caste = "queen")
#' getQueenBv(apiary)
#'
#' getCasteBv(apiary, caste = "fathers")
#' getCasteBv(apiary, caste = "fathers", nInd = 2)
#' getCasteBv(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the breeding values of all individuals either by colony or in a single matrix
#' getCasteBv(apiary, caste = "all")
#' getCasteBv(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteBv <- function(x, caste, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteBv(x = x, caste = caste, nInd = nInd,
                          collapse = collapse, simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- bv(pop = tmp, simParam = simParamBee)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteBv(
        x = x[[colony]], caste = caste, nInd = nInd,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteBv Access breeding value of the queen
#' @export
getQueenBv <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteBv(x,
                    caste = "queen",
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteBv Access breeding values of fathers
#' @export
getFathersBv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteBv(x,
                    caste = "fathers", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteBv Access breeding values of virgin queens
#' @export
getVirginQueensBv <- function(x, nInd = NULL,collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteBv(x,
                    caste = "virginQueens", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteBv Access breeding values of workers
#' @export
getWorkersBv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteBv(x,
                    caste = "workers", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteBv Access breeding values of drones
#' @export
getDronesBv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteBv(x,
                    caste = "drones", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony breeding value from caste individuals' breeding values
#' @export
calcColonyBv <- function(x, FUN = mapCasteToColonyBv, simParamBee = NULL, ...) {
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

# get*Dd ----

#' @rdname getCasteDd
#' @title Access dominance values of individuals in a caste
#'
#' @description Level 0 function that returns dominance values of
#'   individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with dominance values of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{dd}}
#'
#' @return vector of dominance values when \code{x} is
#'   \code{\link{Colony-class}} and list of vectors of dominance values when
#'   \code{x} is \code{\link{MultiColony-class}}, named by colony id when \code{x}
#'   is \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteDd(colony, caste = "queen")
#' getQueenDd(colony)
#'
#' getCasteDd(colony, caste = "fathers")
#' getCasteDd(colony, caste = "fathers", nInd = 2)
#' getCasteDd(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersDd(colony)
#' getFathersDd(colony, nInd = 2)
#'
#' getCasteDd(colony, caste = "virginQueens")
#' getVirginQueensDd(colony)
#'
#' getCasteDd(colony, caste = "workers")
#' getWorkersDd(colony)
#'
#' getCasteDd(colony, caste = "drones")
#' getDronesDd(colony)
#'
#' # Get dominance valued for all individuals
#' getCasteDd(colony, caste = "all")
#' # Get all dominance values in a single matrix
#' getCasteDd(colony, caste = "all", collapse = TRUE)
#'
#' getCasteDd(apiary, caste = "queen")
#' getQueenDd(apiary)
#'
#' getCasteDd(apiary, caste = "fathers")
#' getCasteDd(apiary, caste = "fathers", nInd = 2)
#' getCasteDd(apiary, caste = "fathers", nInd = 2) # random sample!
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
#'
#' # Get the dominance values of all individuals either by colony or in a single matrix
#' getCasteDd(apiary, caste = "all")
#' getCasteDd(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteDd <- function(x, caste, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteDd(x = x, caste = caste, nInd = nInd,
                          collapse = collapse, simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- dd(pop = tmp, simParam = simParamBee)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteDd(
        x = x[[colony]], caste = caste, nInd = nInd,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteDd Access dominance value of the queen
#' @export
getQueenDd <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteDd(x,
                    caste = "queen",
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteDd Access dominance values of fathers
#' @export
getFathersDd <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteDd(x,
                    caste = "fathers", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteDd Access dominance values of virgin queens
#' @export
getVirginQueensDd <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteDd(x,
                    caste = "virginQueens", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteDd Access dominance values of workers
#' @export
getWorkersDd <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteDd(x,
                    caste = "workers", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteDd Access dominance values of drones
#' @export
getDronesDd <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteDd(x,
                    caste = "drones", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony dominance value from caste individuals' dominance values
#' @export
calcColonyDd <- function(x, FUN = mapCasteToColonyDd, simParamBee = NULL, ...) {
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

# get*Aa ----

#' @rdname getCasteAa
#' @title Access epistasis values of individuals in a caste
#'
#' @description Level 0 function that returns epistasis values of
#'   individuals in a caste.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with epistatic values of all the individuals
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{dd}}
#'
#' @return vector of epistasis values when \code{x} is
#'   \code{\link{Colony-class}} and list of vectors of epistasis values when
#'   \code{x} is \code{\link{MultiColony-class}}, named by colony id when \code{x}
#'   is \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$addTraitADE(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1, relAA = 0.5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#' colony <- addVirginQueens(x = colony, nInd = 5)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
#' apiary <- addVirginQueens(x = apiary, nInd = 5)
#'
#' getCasteAa(colony, caste = "queen")
#' getQueenAa(colony)
#'
#' getCasteAa(colony, caste = "fathers")
#' getCasteAa(colony, caste = "fathers", nInd = 2)
#' getCasteAa(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersAa(colony)
#' getFathersAa(colony, nInd = 2)
#'
#' getCasteAa(colony, caste = "virginQueens")
#' getVirginQueensAa(colony)
#'
#' getCasteAa(colony, caste = "workers")
#' getWorkersAa(colony)
#'
#' getCasteAa(colony, caste = "drones")
#' getDronesAa(colony)
#'
#' # Get epistatic values for all individuals
#' getCasteAa(colony, caste = "all")
#' # Get all epistatic values in a single matrix
#' getCasteAa(colony, caste = "all", collapse = TRUE)
#'
#' getCasteAa(apiary, caste = "queen")
#' getQueenAa(apiary)
#'
#' getCasteAa(apiary, caste = "fathers")
#' getCasteAa(apiary, caste = "fathers", nInd = 2)
#' getCasteAa(apiary, caste = "fathers", nInd = 2) # random sample!
#' getFathersAa(apiary)
#' getFathersAa(apiary, nInd = 2)
#'
#' getCasteAa(apiary, caste = "virginQueens")
#' getVirginQueensAa(apiary)
#'
#' getCasteAa(apiary, caste = "workers")
#' getWorkersAa(apiary)
#'
#' getCasteAa(apiary, caste = "drones")
#' getDronesAa(apiary)
#'
#' # Get the epistatic values of all individuals either by colony or in a single matrix
#' getCasteAa(apiary, caste = "all")
#' getCasteAa(apiary, caste = "all", collapse = TRUE)
#' @export
getCasteAa <- function(x, caste, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCasteAa(x = x, caste = caste, nInd = nInd,
                          collapse = collapse, simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- aa(pop = tmp, simParam = simParamBee)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCasteAa(
        x = x[[colony]], caste = caste, nInd = nInd,
        collapse = collapse,
        simParamBee = simParamBee
      )
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- do.call("rbind", ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCasteAa Access epistasis value of the queen
#' @export
getQueenAa <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteAa(x,
                    caste = "queen",
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteAa Access epistasis values of fathers
#' @export
getFathersAa <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteAa(x,
                    caste = "fathers", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteAa Access epistasis values of virgin queens
#' @export
getVirginQueensAa <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteAa(x,
                    caste = "virginQueens", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteAa Access epistasis values of workers
#' @export
getWorkersAa <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteAa(x,
                    caste = "workers", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCasteAa Access epistasis values of drones
#' @export
getDronesAa <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getCasteAa(x,
                    caste = "drones", nInd = nInd,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony epistasis value from caste individuals' epistasis value
#' @export
calcColonyAa <- function(x, FUN = mapCasteToColonyAa, simParamBee = NULL, ...) {
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

# Edit genome and controlled mating ----
#' @title Edit the csd locus
#'
#' @description Edits the csd locus in an entire population of individuals to
#'   ensure heterozygosity. The user can provide a list of csd alleles for each
#'   individual or, alternatively, the function samples a heterozygous genotype
#'   for each individual from all possible csd alleles. The gv slot is
#'   recalculated to reflect the any changes due to editing, but other slots
#'   remain the same.
#'
#' @param pop \code{\link{Pop-class}}
#' @param alleles \code{NULL} or list;
#'   If \code{NULL}, then the function samples a heterozygous csd genotype for
#'   each virgin queen from all possible csd alleles.
#'   If not \code{NULL}, the user provides a list of length \code{nInd} with each
#'   node holding a matrix or a data.frame, each having two rows and n columns.
#'   Each row must hold one csd haplotype (allele) that will be assigned to a
#'   virgin queen. The n columns span the length of the csd locus as specified
#'   in \code{\link{SimParamBee}}. The two csd alleles must be different to
#'   ensure heterozygosity at the csd locus.
#' @param simParamBee global simulation parameters.
#'
#' @return Returns an object of \code{\link{Pop-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 100, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
#' basePop <- createVirginQueens(founderGenomes, editCsd = FALSE)
#' nrow(getCsdAlleles(basePop, unique = TRUE))
#' all(isCsdHeterozygous(basePop))
#'
#' basePopEdited <- SIMplyBee:::editCsdLocus(basePop)
#' nrow(getCsdAlleles(basePopEdited, unique = TRUE))
#' all(isCsdHeterozygous(basePopEdited))
editCsdLocus <- function(pop, alleles = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  csdSites <- simParamBee$csdPosStart:simParamBee$csdPosStop
  if (is.null(alleles)) {
    # Create a matrix of all possible csd alleles
    alleles <- expand.grid(as.data.frame(matrix(rep(0:1, length(csdSites)), nrow = 2, byrow = FALSE)))
    # Sample two different alleles (without replacement) for each individual
    nAlleles <- simParamBee$nCsdAlleles
    alleles <- sapply(seq_len(pop@nInd), FUN = function(x) list(alleles[sample(nAlleles, size = 2, replace = F), ]))
  }

  if (pop@nInd != length(alleles)) {
    stop("The length of the allele list must match the number of individuals in the pop argument")
  }
  if (any(sapply(alleles, FUN = function(x) all(x[1, ] == x[2, ])))) {
    stop("You must provide two different alleles for each individual!")
  }

  # Pull out csd haplotype matrix
  csdH = pullMarkerHaplo(pop, markers = paste(simParamBee$csdChr, csdSites, sep="_"))
  # Prepare the haplotype matrix
  alleles <- as.matrix(do.call(rbind, alleles))
  rownames(alleles) <- rownames(csdH)
  colnames(alleles) <- colnames(csdH)

  pop <- setMarkerHaplo(pop, haplo=alleles)
  return(pop)
}

#' @rdname createRandomCrossPlan
#' @title Create a cross plan by randomly sampling drones for each queen
#'   from a drone population
#'
#' @description Level 0 function that creates a cross plan by randomly sampling
#'   a desired number of drones from a DCA and assigning them to either
#'   virgin queen or colony ID
#'
#' @param IDs numeric, IDs of either the virgin queens OR the colonies (can't have both
#'   in the same cross plan!)
#' @param drones \code{\link{Pop-class}}, drone population available for mating (DCA)
#' @param nDrones integer or function, number of drones to be mated with each virgin
#'   queen
#'
#' @return named list with names being virgin queen or colony input IDs with each
#'   list element holding the IDs of selected drones
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 15, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(basePop[1], n = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#'
#' # Create an apiary of colonies and cross it
#' apiary <- createMultiColony(x = basePop[2:5])
#' apiary <- cross(apiary, drones = droneGroups[1:4])
#' apiary <- buildUp(apiary, nWorkers = 100, nDrones = 100)
#'
#' # Create a DCA from the drones from the apiary
#' DCA <- createDCA(apiary)
#' DCA # Inspect the DCA
#'
#' # Create virgin queens, a virgin colony, and a virgin apiary
#' virginQueen1 <- basePop[6]
#' virginQueen2 <- basePop[7]
#' colony1 <- createColony(basePop[7])
#' apiary1 <- createMultiColony(basePop[8:10])
#'
#' # Create a combined cross plan for mating the virgin queens (with virgin queen IDs)
#' crossPlanVirginQueens <- createRandomCrossPlan(IDs = c(virginQueen1@id, virginQueen2@id),
#'                                                drones = DCA,
#'                                                nDrones = nFathersPoisson)
#'
#' # Cross the virgin queens according to the cross plan
#' virginQueen1 <- cross(virginQueen1, drones = DCA, crossPlan = crossPlanVirginQueens)
#' virginQueen2 <- cross(virginQueen2, drones = DCA, crossPlan = crossPlanVirginQueens)
#'
#' # Create a cross plan for mating the virgin colonies and the virgin apiary (with colony IDs)
#' crossPlanColonies <- createRandomCrossPlan(IDs = getId(c(colony1, apiary1)),
#'                                            drones = DCA,
#'                                            nDrones = nFathersPoisson)
#'
#' # Cross the colonies according to the cross plan
#' colony1 <- cross(colony1, drones = DCA, crossPlan = crossPlanColonies)
#' apiary1 <- cross(apiary1, drones = DCA, crossPlan = crossPlanColonies)
#' nFathers(colony1)
#' nFathers(apiary1)
#'
#' # You can mate virgin queens and colonies in the same way on the mating stations's DCA
#' # Create a mating station from colony1
#' matingStationDCA <- createMatingStationDCA(colony1, nDPQs = 20, nDronePerDPQ = 10)
#'
#' # Create another virgin apiary
#' apiary2 <- createMultiColony(basePop[11:13])
#'
#' # Create a cross plan with colonyIDs for crossing the apiary on the mating station
#' crossPlanApiary <- createRandomCrossPlan(IDs = getId(apiary2),
#'                                          drones = matingStationDCA,
#'                                          nDrones = nFathersPoisson)
#'
#' # Cross the apiary
#' apiary2 <- cross(apiary2, drones = matingStationDCA, crossPlan = crossPlanApiary)
#' nFathers(apiary2)
#'
#' @export
createRandomCrossPlan <- function(IDs, drones, nDrones) {
  if (!isPop(drones)) {
    stop("Argument drones must be a Pop class!")
  }
  drones <- drones[isDrone(drones)]
  if (is.function(nDrones)) {
    nDrones <- nDrones(n = length(IDs))
    fathersMatch <- rep.int(IDs, times = nDrones)
  } else {
    fathersMatch <- rep(IDs, each = nDrones)
  }
  fatherIDs <- sample(drones@id, size = length(fathersMatch), replace = FALSE)
  crossPlan <- base::split(fatherIDs, fathersMatch)
  return(crossPlan)
}
