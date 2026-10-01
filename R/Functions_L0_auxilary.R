
# ---- Level 0 Auxiliary Functions ----

# n* ----
#' @rdname nColonies
#' @title Number of colonies in a MultiColony object
#'
#' @description Level 0 function that returns the number of colonies in a
#'   MultiColony object.
#'
#' @param multicolony \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @seealso \code{\link[SIMplyBee]{nNULLColonies}} and \code{\link[SIMplyBee]{nEmptyColonies}}
#' @return integer
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}.
#'   For \code{nFathers()}, also a \code{\link[AlphaSimR]{Pop-class}}
#'   containing queens.
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{nQueens}}, \code{\link[SIMplyBee]{nFathers}},
#'   \code{\link[SIMplyBee]{nVirginQueens}}, \code{\link[SIMplyBee]{nWorkers}}, and
#'   \code{\link[SIMplyBee]{nDrones}}
#'
#' @return when \code{x} is \code{\link[SIMplyBee]{Colony-class}} return is integer for
#'   \code{caste != "all"} or list for \code{caste == "all"} with nodes named
#'   by caste; when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}} return is named
#'   integer for \code{caste != "all"} or named list of lists for
#'   \code{caste == "all"}. For \code{nFathers()} with a queen population,
#'   returns a numeric vector with one father count per queen.
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
nCaste <- function(x, caste = "all", simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        ret[[caste]] <- nCaste(x = x, caste = caste, simParamBee = simParamBee)
      }
    } else {
      if (caste == "fathers") {
        ret <- ifelse(!is.null(slot(x, "queen")),
                      nInd(x@queen@misc$fathers[[1]]),
                      0)
      } else if (caste == "drones") {
        ret <- ifelse(!is.null(slot(x, caste)), sum(isDrone(x = x@drones, simParamBee = simParamBee)), 0)
      } else {
        ret <- ifelse(!is.null(slot(x, caste)), nInd(slot(x, caste)), 0)
      }
    }
  } else if (isMultiColony(x)) {
    fun <- ifelse(caste == "all", lapply, sapply)
    ret <- fun(x@colonies, FUN = function(z) ifelse(isEmpty(z), 0, nCaste(x = z, caste = caste, simParamBee = simParamBee)))
    names(ret) <- getId(x)
  } else {
    stop("Argument colony must be a Colony or MultiColony class object!")
  }
  return(ret)
}


#' @describeIn nCaste Number of queens in a colony
#' @export
nQueens <- function(x, simParamBee = NULL) {
  ret <- nCaste(x, caste = "queen", simParamBee = simParamBee)
  return(ret)
}

#' @describeIn nCaste Number of fathers in a colony
#' @export
nFathers <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) {
    if (any(!(isQueen(x, simParamBee = simParamBee)))) {
      stop("Individuals in x must be queens!")
    }
    nInd <- nInd(x)
    ret <- rep(x = 0, times = nInd)
    for (ind in seq_len(nInd)) {
      if (isQueen(x[ind], simParamBee = simParamBee)) {
        ret[ind] <- nInd(x@misc$fathers[[ind]])
      }
    }
  } else if (isColony(x) || isMultiColony(x)) {
    ret <- nCaste(x, caste = "fathers", simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn nCaste Number of workers in a colony
#' @export
nWorkers <- function(x, simParamBee = NULL) {
  ret <- nCaste(x, caste = "workers", simParamBee = simParamBee)
  return(ret)
}

#' @describeIn nCaste Number of drones in a colony
#' @export
nDrones <- function(x, simParamBee = NULL) {
  ret <- nCaste(x, caste = "drones", simParamBee = simParamBee)
  return(ret)
}

#' @describeIn nCaste Number of virgin queens in a colony
#' @export
nVirginQueens <- function(x, simParamBee = NULL) {
  ret <- nCaste(x, caste = "virginQueens", simParamBee = simParamBee)
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
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso Demo in the introductory vignette
#'   \code{vignette("Honeybee_biology", package="SIMplyBee")}
#'
#' @return Numeric brood proportions for \code{calcQueensPHomBrood()} and
#'   \code{pHomBrood()}, or counts for \code{nHomBrood()}, named by colony ID
#'   when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}. An empty
#'   \code{\link[AlphaSimR]{Pop-class}} returns \code{numeric(0)}. Missing
#'   brood proportions and counts are represented by \code{NA_real_}.
#'   All three functions warn and return \code{NA_real_} for a colony without
#'   a queen.
#'
#' @examples
#' # This is a bit long example - the key is at the end!
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
calcQueensPHomBrood <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) {
    ret <- rep(x = NA_real_, times = nInd(x))
    for (ind in seq_len(nInd(x))) {
      if (!(all(isQueen(x, simParamBee = simParamBee)))) {
        stop("calcQueensPHomBrood can only be used with queens!")
      } else {
        queensCsd <- apply(
          X = getCsdAlleles(x[ind], simParamBee = simParamBee), MARGIN = 1,
          FUN = function(x) paste0(x, collapse = "")
        )
        fathersCsd <- apply(
          X = getCsdAlleles(x@misc$fathers[[ind]], simParamBee = simParamBee), MARGIN = 1,
          FUN = function(x) paste0(x, collapse = "")
        )
        nComb <- length(queensCsd) * length(fathersCsd)
        ret[ind] <- sum(fathersCsd %in% queensCsd) / nComb
      }
    }
  } else if (isColony(x)) {
    if (is.null(x@queen) || nInd(x@queen) == 0L) {
      warning("Colony has no queen - returning NA!")
      ret <- NA_real_
    } else {
      ret <- calcQueensPHomBrood(x = x@queen, simParamBee = simParamBee)
    }
  } else if (isMultiColony(x)) {
    ret <- sapply(X = x@colonies, FUN = calcQueensPHomBrood, simParamBee = simParamBee)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn calcQueensPHomBrood Expected proportion of csd homozygous brood
#'   of a queen / colony
#' @export
pHomBrood <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) {
    if (any(!isQueen(x, simParamBee = simParamBee))) {
      stop("Individuals in x must be queens!")
    }
    ret <- rep(x = NA_real_, times = nInd(x))
    for (ind in seq_len(nInd(x))) {
      if (!is.null(x@misc$pHomBrood[[ind]])) {
        ret[ind] <- x@misc$pHomBrood[[ind]]
      }
    }
  } else if (isColony(x)) {
    if (is.null(x@queen) || nInd(x@queen) == 0L) {
      warning("Colony has no queen - returning NA!")
      ret <- NA_real_
    } else {
      if (is.null(x@queen@misc$pHomBrood[[1]])) {
        ret <- NA_real_
      } else {
        ret <- x@queen@misc$pHomBrood[[1]]
      }
    }

  } else if (isMultiColony(x)) {
    ret <- sapply(X = x@colonies, FUN = pHomBrood, simParamBee = simParamBee)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn calcQueensPHomBrood Realised number of csd homozygous brood
#'   produced by a queen
#' @export
nHomBrood <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) {
    if (any(!isQueen(x, simParamBee = simParamBee))) {
      stop("Individuals in x must be queens!")
    }
    ret <- rep(x = NA_real_, times = nInd(x))
    for (ind in seq_len(nInd(x))) {
      if (!is.null(x@misc$nHomBrood[[ind]])) {
        ret[ind] <- x@misc$nHomBrood[[ind]]
      }
    }
  } else if (isColony(x)) {
    if (is.null(x@queen) || nInd(x@queen) == 0L) {
      warning("Colony has no queen - returning NA!")
      ret <- NA_real_
    } else if (is.null(x@queen@misc$nHomBrood[[1]])) {
      ret <- NA_real_
    } else {
      ret <- x@queen@misc$nHomBrood[[1]]
    }
  } else if (isMultiColony(x)) {
    ret <- sapply(X = x@colonies, FUN = nHomBrood, simParamBee = simParamBee)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
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
#' @param x \code{\link[AlphaSimR]{Pop-class}}
#' @param caste character, one of "queen", "fathers", "workers", "drones", or
#'   "virginQueens"; only single value is used
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{isQueen}}, \code{\link[SIMplyBee]{isFather}},
#'   \code{\link[SIMplyBee]{isVirginQueen}},
#'   \code{\link[SIMplyBee]{isWorker}}, and
#'   \code{\link[SIMplyBee]{isDrone}}
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
isVirginQueens <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- isCaste(x = x, caste = "virginQueens", simParamBee = simParamBee)
  return(ret)
}

#' @describeIn isCaste Is individual a virgin queen
#' @export
isVirginQueen <- isVirginQueens

#' @rdname isQueenPresent
#' @title Is the queen present
#'
#' @description Level 0 function that returns queen's presence status (is she
#'   present/alive or not).
#'
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
isQueenPresent <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isMultiColony(x)) {
    if (length(nQueens(x, simParamBee = simParamBee)) > 0) {
      ret <- nQueens(x, simParamBee = simParamBee) > 0
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
isFathersPresent <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isMultiColony(x)) {
    if (length(nFathers(x, simParamBee = simParamBee)) > 0) {
      ret <- nFathers(x, simParamBee = simParamBee) > 0
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
isWorkersPresent <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isMultiColony(x)) {
    if (length(nWorkers(x, simParamBee = simParamBee)) > 0) {
      ret <- nWorkers(x, simParamBee = simParamBee) > 0
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
isDronesPresent <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isMultiColony(x)) {
    if (length(nDrones(x, simParamBee = simParamBee)) > 0) {
      ret <- nDrones(x, simParamBee = simParamBee) > 0
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @return logical, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- addVirginQueens(x = colony, nInd = 4)
#' isVirginQueenPresent(colony)
#' isVirginQueenPresent(pullVirginQueens(colony)$remnant)
#' isVirginQueenPresent(removeQueen(colony))
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10)
#' isVirginQueenPresent(apiary)
#'
#' tmp <- swarm(x = apiary)
#' isVirginQueenPresent(tmp$swarm)
#' isVirginQueenPresent(tmp$remnant)
#' @export
isVirginQueensPresent <- function(x, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x) | isMultiColony(x)) {
    if (length(nVirginQueens(x, simParamBee = simParamBee)) > 0) {
      ret <- nVirginQueens(x, simParamBee = simParamBee) > 0
    } else {
      ret <- FALSE
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn isVirginQueensPresent Are virgin queen(s) present
#' @export
isVirginQueenPresent <- isVirginQueensPresent

#' @describeIn isVirginQueensPresent Are virgin queen(s) present
#' @export
areVirginQueensPresent <- isVirginQueensPresent

#' @rdname isEmpty
#' @title Check whether a population, colony or a multicolony
#'   object has no individuals within
#'
#' @description Check whether a population, colony or a multicolony
#'   object has no individuals within.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}} or \code{\link[SIMplyBee]{Colony-class}} or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return boolean when \code{x} is \code{\link[AlphaSimR]{Pop-class}} or
#'   \code{\link[SIMplyBee]{Colony-class}}, and named vector of boolean when
#'   \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' @param multicolony  \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return Named vector of boolean
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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

#' @rdname getId
#' @title Get the population/queen or colony IDs
#'
#' @description Level 0 function that returns individual IDs from a population
#'   or colony IDs from colonies.
#'
#' @param x \code{NULL}, \code{\link[AlphaSimR]{Pop-class}},
#'   \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return Character individual IDs for a \code{Pop}, or integer colony IDs
#'   for a \code{Colony} or nonempty \code{MultiColony}. An empty \code{Pop}
#'   returns \code{character(0)}; \code{NULL} input returns
#'   \code{NA_character_}. An empty \code{MultiColony} returns an empty list.
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
    id <- NA_character_
  } else if (isPop(x)) {
    id <- x@id
  } else if (isColony(x)) {
    id <- ifelse(is.null(x@id), NA_character_, x@id)
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
#'   get the individuals, use \code{\link[SIMplyBee]{getCastePop}}. To get individuals'
#'   caste, use \code{\link[SIMplyBee]{getCaste}}.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param collapse logical, if all IDs should be returned as a single vector
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{getCaste}}
#'
#' @return when \code{x} is \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"}
#'  or list for \code{caste == "all"} with ID nodes named by caste;
#'    when \code{x} is \code{\link[SIMplyBee]{Colony-class}} return is a named list of
#'   \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"}
#'   or named list for \code{caste == "all"} indluding caste members IDs;
#'    when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}} return is a named list of
#'   \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"} or named list of lists of
#'   \code{\link[AlphaSimR]{Pop-class}} for \code{caste == "all"} indluding caste members IDs
#'
#' @seealso \code{\link[SIMplyBee]{getCastePop}} and \code{\link[SIMplyBee]{getCaste}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
        tmp <- getCastePop(x = x, caste = caste, simParamBee = simParamBee)
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
      tmp <- getCastePop(x = x, caste = caste, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- tmp@id
      }
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(X = x@colonies, FUN = getCasteId, caste = caste, simParamBee = simParamBee)
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
#'   get the individuals, use \code{\link[SIMplyBee]{getCastePop}}. To get individuals'
#'   caste, use \code{\link[SIMplyBee]{getCaste}}.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param collapse logical, if \code{TRUE}, the function will return a single
#'   vector with sex information
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @return when \code{x} is \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"}
#'  or list for \code{caste == "all"} with sex nodes named by caste;
#'    when \code{x} is \code{\link[SIMplyBee]{Colony-class}} return is a named list of
#'   \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"}
#'   or named list for \code{caste == "all"} indluding caste members sexes;
#'    when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}} return is a named list of
#'   \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"} or named list of lists of
#'   \code{\link[AlphaSimR]{Pop-class}} for \code{caste == "all"} indluding caste members sexes
#'
#' @seealso \code{\link[SIMplyBee]{getCastePop}} and \code{\link[SIMplyBee]{getCaste}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' # Collapse information into a single vector
#' getCasteSex(colony, caste = "all", collapse = TRUE)
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
getCasteSex <- function(x, caste = "all", collapse = FALSE, simParamBee = NULL) {
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
        tmp <- getCastePop(x = x, caste = caste, simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp@sex
        }
      }
      if (collapse) {
        ret <- do.call("c", ret)
      }
    } else {
      tmp <- getCastePop(x = x, caste = caste, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- tmp@sex
      }
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(X = x@colonies, FUN = getCasteSex, caste = caste,
                  collapse = collapse, simParamBee = simParamBee)
    if (collapse) {
      ret <- do.call("c", ret)
    } else {
      names(ret) <- getCaste(x = x, simParamBee = simParamBee)
    }
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
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#' @param collapse logical, if \code{TRUE}, the function will return a single
#'   vector with caste information
#' @return When x is \code{\link[AlphaSimR]{Pop-class}}, character of caste status; if you
#'   get \code{NA} note that this is not supposed to happen. When x is
#'   \code{\link[SIMplyBee]{Colony-class}}, list with character vectors (list is named with
#'   caste). When x is \code{\link[SIMplyBee]{MultiColony-class}}, list of lists with
#'   character vectors (list is named with colony id).
#'
#' @seealso \code{\link[SIMplyBee]{getCastePop}} and \code{\link[SIMplyBee]{getCasteId}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' # Collapse information into a single vector
#' getCaste(colony, collapse = TRUE)
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
getCaste <- function(x, collapse = FALSE, simParamBee = NULL) {
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
      tmp <- getCastePop(x = x, caste = caste, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[caste] <- list(NULL)
      } else {
        ret[[caste]] <- getCaste(x = tmp, simParamBee = simParamBee)
      }
    }
    if (collapse) {
      ret <- do.call("c", ret)
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(X = x@colonies, FUN = getCaste, collapse = collapse, simParamBee = simParamBee)
    if (collapse) {
      ret <- do.call("c", ret)
    } else {
      names(ret) <- getId(x)
    }
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param collapse logical, if the return value should be a single matrix
#'   with locations of all the colonies; only applicable when input
#'   is a \code{\link[SIMplyBee]{MultiColony-class}} object
#'
#' @return numeric with two values when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   and a list of numeric with two values when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}} (list named after colonies); \code{c(NA, NA)}
#'   when location not set
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' getLocation(apiary, collapse = TRUE)
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
getLocation <- function(x, collapse = FALSE) {
  if (isColony(x)) {
    if (is.null(x@location)) {
      ret <- NULL
    } else {
      ret <- x@location
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(x@colonies, FUN = getLocation)
    names(ret) <- getId(x)
    if (collapse) {
      ret <- do.call(rbind, ret)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @rdname rcircle
#' @title Sample random points within a circle
#'
#' @description Level 0 function that samples random points (x, y) within a
#'   circle via rejection sampling.
#'
#' @param n integer, number of samples points
#' @param radius numeric, radius of the sampled circle
#' @param uniform logical, should sampling be uniform or according to a
#'   bi-variate spherical (uncorrelated) Gaussian distribution (see examples)
#' @param normScale numeric, if \code{uniform = FALSE}, a factor to scale radius
#'   to standard deviation of the Gaussian density in x and in y (see examples)
#'
#' @return matrix with two columns for the x and y coordinates of the points.
#'
#' @references
#' \insertRef{nubdotdev2021Best}{SIMplyBee}
#' \insertRef{wolfram2023Disk}{SIMplyBee}
#'
#' @examples
#' x <- rcircle(n = 500)
#' lim <- range(x)
#' plot(x, xlim = lim, ylim = lim, main = "Uniform")
#'
#' x <- rcircle(n = 500, uniform = FALSE)
#' lim <- range(x)
#' plot(x, xlim = lim, ylim = lim, main = "Gaussian")
#' @export
rcircle <- function(n = 1, radius = 1, uniform = TRUE, normScale = 1 / 3) {
  if (n < 1) {
    stop("Argument n must larger than 0!")
  }
  if (length(radius) > 1) {
    warning("When you provide multiple radius values, you are sampling from different circles!")
  }
  ret <- matrix(data = 0, nrow = n, ncol = 2)
  for (sample in 1:n) {
    keepGoing <- TRUE
    while (keepGoing) {
      if (uniform) {
        x <- runif(n = 1, min = -radius, max = radius)
        y <- runif(n = 1, min = -radius, max = radius)
      } else {
        x <- rnorm(n = 1, mean = 0, sd = radius * normScale)
        y <- rnorm(n = 1, mean = 0, sd = radius * normScale)
      }
      if ((x*x + y*y) <= radius) {
        keepGoing <- FALSE
      }
    }
    ret[sample, ] <- c(x, y)
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return matrix of logicals, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' apiary <- createMultiColony(basePop[3:4])
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @return logical, named by colony id when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' @return \code{\link[AlphaSimR]{MapPop-class}}
#'
#' @references
#' \insertRef{beye2006Exceptionally}{SIMplyBee}
#' \insertRef{yang2015Parent}{SIMplyBee}
#' \insertRef{wallberg2014Worldwide}{SIMplyBee}
#' \insertRef{wallberg2019Hybrid}{SIMplyBee}
#'
#' @seealso Due to the computational time and resources required to run this function,
#'   we do not include an example here, but we demonstrate
#'   its use in the Honeybee biology vignette.
#'
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
#'   activated. See \code{\link[SIMplyBee]{SimParamBee}} for more information about the csd
#'   locus.
#'
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
#' \dontshow{SP$nThreads = 1L}
#' isCsdActive()
#'
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#'   we internally simulate them as diploid (doubled haploid). This is an
#'   internal utility function that you likely don't need to use.
#'
#' @param haplo \code{\link{matrix-class}}
#' @param pop \code{\link[AlphaSimR]{Pop-class}}
#'
#' @details While this function is meant to work on male (drone) haplotypes, we
#'   handle cases where the \code{haplo} matrix contains male and female
#'   haplotypes, which is why you need to provide \code{pop}. We only reduce
#'   haplotypes for males though.
#'
#' @return matrix with one haplotype per drone instead of two - the order of
#'   individuals stays the same, but there will be less rows!
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 5)
#' SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 2)
#'
#' (tmp <- getSegSiteHaplo(drones, dronesHaploid = FALSE))
#' reduceDroneHaplo(haplo = tmp, pop = drones)
#'
#' (tmp <- getSegSiteHaplo(c(basePop, drones), dronesHaploid = FALSE))
#' reduceDroneHaplo(haplo = tmp, pop = c(basePop, drones))
#' @export
reduceDroneHaplo <- function(haplo, pop) {
  if (!is.matrix(haplo)) {
    stop("Argument haplo must be a matrix class object!")
  }
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
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
#'   haploid). This is an internal utility function that you likely don't need
#'   to use.
#'
#' @param geno \code{\link{matrix-class}}
#' @param pop \code{\link[AlphaSimR]{Pop-class}}
#'
#' @return matrix with genotype as one haplotype per drone instead of two - the
#'   order of individuals and the number of rows stays the same!
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 5)
#' SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 2)
#'
#' (tmp <- getSegSiteGeno(drones))
#' reduceDroneGeno(geno = tmp, pop = drones)
#'
#' (tmp <- getSegSiteGeno(c(basePop, drones)))
#' reduceDroneGeno(geno = tmp, pop = c(basePop, drones))
#' @export
reduceDroneGeno <- function(geno, pop) {
  if (!is.matrix(geno)) {
    stop("Argument geno must be a matrix class object!")
  }
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
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
#'   \code{\link[SIMplyBee]{SimParamBee}} for more information about the csd locus.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
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
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @details If both collapse and unique are \code{TRUE}, the function returns a
#'   unique set of csd alleles in the entire population, colony, or multicolony
#'
#' @return matrix with haplotypes when \code{x} is \code{\link[AlphaSimR]{Pop-class}}, list
#'   of matrices with haplotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   (list nodes named by caste) and list of a list of matrices with haplotypes
#'   when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}, outer list is named by
#'   colony id when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}; \code{NULL} when
#'   \code{x} is \code{NULL}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
#' \dontshow{SP$nThreads = 1L}
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
#' getCsdAlleles(getWorkers(colony))
#'
#' # Use getCsdAlleles on a Colony
#' getCsdAlleles(colony)
#' getCsdAlleles(colony, caste = "queen")
#' getQueenCsdAlleles(colony)
#' getCsdAlleles(colony, caste = "workers")
#' getWorkersCsdAlleles(colony)
#' # Same aliases exist for all the castes!
#'
#' getCsdAlleles(colony, unique = TRUE)
#' getCsdAlleles(colony, collapse = TRUE)
#' getCsdAlleles(colony, collapse = TRUE, unique = TRUE)
#'
#' # Use getCsdAlleles on a MultiColony
#' getCsdAlleles(apiary)
#' getCsdAlleles(apiary, unique = TRUE)
#' getCsdAlleles(apiary, collapse = TRUE, unique = TRUE)
#' getCsdAlleles(apiary, nInd = 2)
#' @export
getCsdAlleles <- function(x, caste = NULL, nInd = NULL, allele = "all", dronesHaploid = TRUE,
                          collapse = FALSE, unique = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (!isCsdActive(simParamBee = simParamBee)) {
    stop("The csd locus has not been set!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullMarkerHaplo(x, markers = paste(simParamBee$csdChr,
                                              simParamBee$csdPosStart:simParamBee$csdPosStop,
                                              sep="_"), simParam = simParamBee)
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneHaplo(haplo = ret, pop = x)
    }
    if (unique) {
      ret <- ret[!duplicated(x = ret), , drop = FALSE]
    }
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCsdAlleles(
          x = getCastePop(x, caste, simParamBee = simParamBee), allele = allele,
          dronesHaploid = dronesHaploid,
          unique = unique,
          simParamBee = simParamBee
        )
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
        if (unique) {
          ret <- ret[!duplicated(ret), , drop = FALSE]
        }
      }
    } else {
      ret <- getCsdAlleles(
        x = getCastePop(x, caste, simParamBee = simParamBee), allele = allele,
        dronesHaploid = dronesHaploid,
        unique = unique,
        simParamBee = simParamBee
      )
    }
  } else if (isMultiColony(x)) {
    ret <- lapply(
      X = x@colonies, FUN = getCsdAlleles, nInd = nInd,
      caste = caste, allele = allele, dronesHaploid = dronesHaploid,
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

#' @describeIn getCsdAlleles Access csd alleles of the queen
#' @export
getQueenCsdAlleles <- function(x, allele = "all", unique = FALSE, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       caste = "queen",
                       allele = allele,
                       unique = unique,
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCsdAlleles Access csd alleles of the fathers
#' @export
getFathersCsdAlleles <- function(x, nInd = NULL, allele = "all", dronesHaploid = TRUE,
                                 unique = FALSE, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       caste = "fathers",
                       nInd = nInd,
                       allele = allele,
                       dronesHaploid = dronesHaploid,
                       unique = unique,
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCsdAlleles Access csd alleles of the virgin queens
#' @export
getVirginQueensCsdAlleles <- function(x, nInd = NULL, allele = "all",
                                      unique = FALSE, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       caste = "virginQueens",
                       nInd = nInd,
                       allele = allele,
                       unique = unique,
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCsdAlleles Access csd alleles of the workers
#' @export
getWorkersCsdAlleles <- function(x, nInd = NULL, allele = "all",
                                 unique = FALSE, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       caste = "workers",
                       nInd = nInd,
                       allele = allele,
                       unique = unique,
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCsdAlleles Access csd alleles of the drones
#' @export
getDronesCsdAlleles <- function(x, nInd = NULL, allele = "all", dronesHaploid = TRUE,
                                unique = FALSE, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       caste = "drones",
                       nInd = nInd,
                       allele = allele,
                       dronesHaploid = dronesHaploid,
                       unique = unique,
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname getCsdGeno
#' @title Get genotypes from the csd locus
#'
#' @description Level 0 function that returns genotypes from the csd locus. See
#'   \code{\link[SIMplyBee]{SimParamBee}} for more information about the csd locus and how
#'   we have implemented it.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, for how many individuals; if \code{NULL} all individuals
#'   are taken; this can be useful as a test of sampling individuals
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with haplotypes of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @details The returned genotypes are spanning multiple bi-allelic SNP of
#'   a non-recombining csd locus / haplotype. In most cases you will want to use
#'   \code{\link[SIMplyBee]{getCsdAlleles}}.
#'
#' @return matrix with genotypes when \code{x} is \code{\link[AlphaSimR]{Pop-class}}, list
#'   of matrices with genotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   (list nodes named by caste) and list of a list of matrices with genotypes
#'   when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}, outer list is named by
#'   colony id when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}; \code{NULL} when
#'   \code{x} is \code{NULL}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' getCsdGeno(getWorkers(colony))
#'
#' # Using dronesHaploid = TRUE returns drones as haploids instead of double haploids
#' getCsdGeno(getDrones(colony), nInd = 3, dronesHaploid = TRUE)
#' # Using dronesHaploid = FALSE returns drones as double haploids
#' getCsdGeno(getDrones(colony), nInd = 3, dronesHaploid = FALSE)
#'
#' # Use getCsdGeno on a Colony
#' getCsdGeno(colony)
#' getCsdGeno(colony, caste = "queen")
#' getQueenCsdGeno(colony)
#' getCsdGeno(colony, caste = "workers")
#' getWorkersCsdGeno(colony)
#' # Same aliases exist for all the castes!
#'
#' # Use getCsdGeno on a MultiColony - same behaviour as for the Colony!
#' getCsdGeno(apiary)
#' getCsdGeno(apiary, nInd = 2)
#' @export
getCsdGeno <- function(x, caste = NULL, nInd = NULL, dronesHaploid = TRUE,
                       collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (!isCsdActive(simParamBee = simParamBee)) {
    stop("The csd locus has not been set!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullMarkerGeno(x, markers = paste(simParamBee$csdChr,
                                             simParamBee$csdPosStart:simParamBee$csdPosStop,
                                             sep="_"), simParam = simParamBee)
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneGeno(geno = ret, pop = x)
    }
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- getCsdGeno(
            x = tmp, dronesHaploid = dronesHaploid,
            simParamBee = simParamBee
          )
        }
      }
      if (collapse) {
        ret <- do.call("rbind", ret)
      }
    } else {
      ret <- getCsdGeno(
        x = getCastePop(x, caste, simParamBee = simParamBee), nInd = nInd,
        dronesHaploid = dronesHaploid, simParamBee = simParamBee
      )
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getCsdGeno(
        x = x[[colony]], caste = caste, nInd = nInd,
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCsdGeno Access csd genotypes of the queen
#' @export
getQueenCsdGeno <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       caste = "queen",
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCsdGeno Access csd genotypes of the fathers
#' @export
getFathersCsdGeno <- function(x, nInd = NULL, dronesHaploid = TRUE, collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       nInd = nInd,
                       caste = "fathers",
                       dronesHaploid = dronesHaploid,
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCsdGeno Access csd genotypes of the virgin queens
#' @export
getVirginQueensCsdGeno <- function(x, nInd = NULL, collapse = FALSE,
                                   simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       nInd = nInd,
                       caste = "virginQueens",
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCsdGeno Access csd genotypes of the virgin queens
#' @export
getWorkersCsdGeno <- function(x, nInd = NULL, collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       nInd = nInd,
                       caste = "workers",
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCsdGeno Access csd genotypes of the virgin queens
#' @export
getDronesCsdGeno <- function(x, nInd = NULL, dronesHaploid = TRUE,
                             collapse = FALSE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!(isColony(x) | isMultiColony(x))) {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  ret <- getCsdAlleles(x,
                       nInd = nInd,
                       caste = "drones",
                       dronesHaploid = dronesHaploid,
                       collapse = collapse,
                       simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname isGenoHeterozygous
#' @title Test if a multilocus genotype is heterozygous
#'
#' @description Level 0 function that returns heterozygote status for a
#'   multilocus genotype.
#'
#' @param x integer or matrix, output from \code{\link[SIMplyBee]{getCsdGeno}}
#'
#' @return logical
#' # Not exporting this function, since its just a helper
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
#'   heterozygous at the csd locus. See \code{\link[SIMplyBee]{SimParamBee}} for more
#'   information about the csd locus. The function also return \code{TRUE} for drones to
#'   mark their viability, although they are haploid.
#'
#' @param pop \code{\link[AlphaSimR]{Pop-class}}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @details We could expand \code{isCsdHeterozygous} to work also with
#'   \code{\link[SIMplyBee]{Colony-class}} and \code{\link[SIMplyBee]{MultiColony-class}} if needed
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' @export
isCsdHeterozygous <- function(pop, simParamBee = NULL) {
  if(is(pop,"MapPop")){
    genMap = pop@genMap
  }else{
    if(is.null(simParamBee)){
      simParamBee <- get("SP",envir=.GlobalEnv)
    }
    genMap = simParamBee$genMap
  }

  if (is.null(simParamBee$csdChr)) {
    stop("Csd locus not set.")
  }

  # Map markers to genetic map
  markers = paste(simParamBee$csdChr,
                  simParamBee$csdPosStart:simParamBee$csdPosStop,
                  sep="_")
  lociMap = mapLoci(markers, genMap)

  ret <- as.logical(as.vector(isHeterozygous(pop@geno, lociMap$lociPerChr, lociMap$lociLoc, simParamBee$nThreads)))
  names(ret) <- pop@id
  caste <- getCaste(pop, simParamBee = simParamBee)
  ret[caste == "drones"] <- TRUE
  return(ret)
}

#' @rdname nCsdAlleles
#' @title Report the number of distinct csd alleles
#'
#' @description Level 0 function that returns the number of distinct csd alleles
#'   in input. See \code{\link[SIMplyBee]{SimParamBee}} for more information about the csd
#'   locus.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param collapse logical, if \code{TRUE}, the function will return the number
#'   of distinct csd alleles in either the entire population, colony, or
#'   multicolony. Note this has nothing to do with the colony collapse. It's
#'   like \code{paste(..., collapse = TRUE)}. Default is \code{FALSE}. See
#'   examples about this behaviour.Default is \code{FALSE}.
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
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
#'   \code{\link[SIMplyBee]{Colony-class}}. This last measure is then the expected number
#'   of csd alleles in a colony as opposed to realised number of csd alleles in
#'   a sample of virgin queens and workers. Similarly as for virgin queens and
#'   workers, \code{nCsdAlleles()} from drones gives a noisy realisation of
#'   \code{nCsdAlleles()} from queens. The amount of noise will depend on the
#'   number of individuals, so in most cases with reasonable number of
#'   individuals there should be minimal amount of noise.
#'
#' @return integer representing the number of distinct csd alleles when \code{x}
#'   is \code{\link[AlphaSimR]{Pop-class}} (or ), list of integer
#'   when \code{x} is \code{\link[SIMplyBee]{Colony-class}} (list nodes named by caste) and
#'   list of a list of integer when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}},
#'   outer list is named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}; the integer rep
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' nCsdAlleles(getWorkers(colony))
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
      ret$queen <- nCsdAlleles(x = getQueen(x, simParamBee = simParamBee), simParamBee = simParamBee)
      ret$fathers <- nCsdAlleles(x = getFathers(x, simParamBee = simParamBee), simParamBee = simParamBee)
      ret$workers <- nCsdAlleles(x = getWorkers(x, simParamBee = simParamBee), simParamBee = simParamBee)
      ret$drones <- nCsdAlleles(x = getDrones(x, simParamBee = simParamBee), simParamBee = simParamBee)
      ret$virginQueens <- nCsdAlleles(x = getVirginQueens(x, simParamBee = simParamBee), simParamBee = simParamBee)
      # Can't combine queen (diploid) and fathers (haploid) using c(getQueen(x), getFathers(x)),
      #   so we will get their alleles and count them
      tmp <- rbind(
        getCsdAlleles(x = getQueen(x, simParamBee = simParamBee), simParamBee = simParamBee),
        getCsdAlleles(x = getFathers(x, simParamBee = simParamBee), simParamBee = simParamBee)
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

# get*Ibd* ----

#' @rdname getIbdHaplo
#' @title Access IBD haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns IBD (identity by descent)
#'   haplotypes of individuals in a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param snpChip integer, indicating which SNP array loci are to be retrieved,
#'   if \code{NULL}, all sites are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with haplotypes of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[AlphaSimR]{pullIbdHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 5)
#' SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 4)
#' \dontshow{SP$nThreads = 1L}
#' SP$setTrackRec(isTrackRec = TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 200)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 3, nDrones = 2)
#' colony <- addVirginQueens(x = colony, nInd = 2)
#'
#' apiary <- createMultiColony(basePop[3:4])
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 3, nDrones = 2)
#' apiary <- addVirginQueens(x = apiary, nInd = 2)
#'
#' # Input is a population
#' getIbdHaplo(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getIbdHaplo(queens)
#'
#' # Input is a colony
#' getIbdHaplo(x = colony, caste = "queen")
#' getQueenIbdHaplo(colony)
#'
#' getIbdHaplo(colony, caste = "workers", nInd = 3)
#' getIbdHaplo(colony, caste = "virginQueens")
#' getIbdHaplo(colony, caste = "drones")
#' getWorkersIbdHaplo(colony)
#' # Same aliases exist for all castes!
#'
#' # Get haplotypes for all individuals
#' getIbdHaplo(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getIbdHaplo(colony, caste = "all", collapse = TRUE)
#'
#' # Input is a MultiColony
#' getIbdHaplo(x = apiary, caste = "queen")
#' getQueenIbdHaplo(apiary)
#' # Or collapse all the haplotypes into a single matrix
#' getQueenIbdHaplo(apiary, collapse = TRUE)
#'
#'
#' getIbdHaplo(x = apiary, caste = "workers")
#' getIbdHaplo(x = apiary, caste = "drones")
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getIbdHaplo(apiary, caste = "all")
#' getIbdHaplo(apiary, caste = "all", collapse = TRUE)
#' @export
getIbdHaplo <- function(x, caste = NULL, nInd = NULL, chr = NULL, snpChip = NULL,
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
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullIbdHaplo(pop = x, chr = chr, snpChip = snpChip, simParam = simParamBee)
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneHaplo(haplo = ret, pop = x)
    }
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getIbdHaplo(x = x, caste = caste, nInd = nInd, chr = chr,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getIbdHaplo(x = tmp, chr = chr, snpChip = snpChip, simParamBee = simParamBee)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getIbdHaplo(
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
    stop("Argument x must be a Pop, Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getIbdHaplo Access IBD haplotype data of the queen
#' @export
getQueenIbdHaplo <- function(x, chr = NULL, snpChip = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getIbdHaplo(x,
                     caste = "queen",
                     chr = chr,
                     snpChip = snpChip,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getIbdHaplo Access IBD haplotype data of fathers
#' @export
getFathersIbdHaplo <- function(x, nInd = NULL, chr = NULL, snpChip = NULL,
                               dronesHaploid = TRUE,
                               collapse = FALSE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getIbdHaplo(x,
                     caste = "fathers", nInd = nInd, chr = chr,
                     snpChip = snpChip,
                     dronesHaploid = dronesHaploid,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getIbdHaplo Access IBD haplotype data of virgin queens
#' @export
getVirginQueensIbdHaplo <- function(x, nInd = NULL, chr = NULL, snpChip = NULL,
                                    collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getIbdHaplo(x,
                     caste = "virginQueens",
                     nInd = nInd,
                     chr = chr,
                     snpChip = snpChip,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getIbdHaplo Access IBD haplotype data of workers
#' @export
getWorkersIbdHaplo <- function(x, nInd = NULL,
                               chr = NULL, snpChip = NULL,
                               collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getIbdHaplo(x,
                     caste = "workers",
                     nInd = nInd,
                     chr = chr,
                     snpChip = snpChip,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getIbdHaplo Access IBD haplotype data of drones
#' @export
getDronesIbdHaplo <- function(x, nInd = NULL, chr = NULL, snpChip = NULL,
                              dronesHaploid = TRUE,
                              collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getIbdHaplo(x,
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

#' @rdname getQtlHaplo
#' @title Access QTL haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns QTL haplotypes of individuals in a
#'   caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param trait numeric (trait position) or character (trait name), indicates
#'   which trait's QTL haplotypes to retrieve
#' @param haplo character, either "all" for all haplotypes or an integer for a
#'   single set of haplotypes, use a value of 1 for female haplotypes and a
#'   value of 2 for male haplotypes
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with haplotypes of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{getQtlGeno}} and \code{\link[AlphaSimR]{pullQtlHaplo}} as well as
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 200)
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
#' # Input is a population
#' getQtlHaplo(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getQtlHaplo(queens)
#'
#' # Input is a Colony
#' getQtlHaplo(colony, caste = "queen")
#' getQueenQtlHaplo(colony)
#'
#' getQtlHaplo(colony, caste = "workers", nInd = 3)
#' getWorkersQtlHaplo(colony)
#' # Same aliases exist for all the castes!
#'
#' # Get haplotypes for all individuals
#' getQtlHaplo(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getQtlHaplo(colony, caste = "all", collapse = TRUE)
#'
#' # Input is a MultiColony - same behaviour as for the Colony
#' getQtlHaplo(apiary, caste = "queen")
#' getQueenQtlHaplo(apiary)
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getQtlHaplo(apiary, caste = "all")
#' getQtlHaplo(apiary, caste = "all", collapse = TRUE)
#'
#' @export
getQtlHaplo <- function(x, caste = NULL, nInd = NULL,
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
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullQtlHaplo(pop = x, trait = trait, haplo = haplo, chr = chr, simParam = simParamBee)
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneHaplo(haplo = ret, pop = x)
    }
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getQtlHaplo(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getQtlHaplo(x = tmp, haplo = haplo, trait = trait, chr = chr, simParamBee = simParamBee)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getQtlHaplo(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getQtlHaplo Access QTL haplotype data of the queen
#' @export
getQueenQtlHaplo <- function(x,
                             trait = 1, haplo = "all", chr = NULL,
                             collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlHaplo(x,
                     caste = "queen",
                     trait = trait, haplo = haplo, chr = chr,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getQtlHaplo Access QTL haplotype data of fathers
#' @export
getFathersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL,
                               dronesHaploid = TRUE,
                               collapse = FALSE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlHaplo(x,
                     caste = "fathers", nInd = nInd,
                     trait = trait, haplo = haplo, chr = chr,
                     dronesHaploid = dronesHaploid,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getQtlHaplo Access QTL haplotype data of virgin queens
#' @export
getVirginQueensQtlHaplo <- function(x, nInd = NULL,
                                    trait = 1, haplo = "all", chr = NULL,
                                    collapse = FALSE,
                                    simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlHaplo(x,
                     caste = "virginQueens", nInd = nInd,
                     trait = trait, haplo = haplo, chr = chr,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getQtlHaplo Access QTL haplotype of workers
#' @export
getWorkersQtlHaplo <- function(x, nInd = NULL,
                               trait = 1, haplo = "all", chr = NULL,
                               collapse = FALSE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlHaplo(x,
                     caste = "workers", nInd = nInd,
                     trait = trait, haplo = haplo, chr = chr,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getQtlHaplo Access QTL haplotype data of drones
#' @export
getDronesQtlHaplo <- function(x, nInd = NULL,
                              trait = 1, haplo = "all", chr = NULL,
                              dronesHaploid = TRUE,
                              collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlHaplo(x,
                     caste = "drones", nInd = nInd,
                     trait = trait, haplo = haplo, chr = chr,
                     dronesHaploid = dronesHaploid,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname getQtlGeno
#' @title Access QTL genotypes of individuals in a caste
#'
#' @description Level 0 function that returns QTL genotypes of individuals in a
#'   caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param trait numeric (trait position) or character (trait name), indicates
#'   which trait's QTL genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with genotypes of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{getQtlHaplo}},
#'   \code{\link[AlphaSimR]{pullQtlGeno}}, and
#'   \code{\link[SIMplyBee]{getPooledGeno}}, as well as
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return matrix with genotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' SP$addTraitA(nQtlPerChr = 10)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 200)
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
#' # Input is a population
#' getQtlGeno(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getQtlGeno(queens)
#'
#' # Input is a colony
#' getQtlGeno(colony, caste = "queen")
#' getQueenQtlGeno(colony)
#'
#' getQtlGeno(colony, caste = "workers", nInd = 3)
#' getWorkersQtlGeno(colony)
#' # Same aliases exist for all the castes!
#'
#' # Get genotypes for all individuals
#' getQtlGeno(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getQtlGeno(colony, caste = "all", collapse = TRUE)
#'
#' # Input is a MultiColony - same behaviour as for the Colony!
#' getQtlGeno(apiary, caste = "queen")
#' getQueenQtlGeno(apiary)
#'
#' # Get the genotypes of all individuals either by colony or in a single matrix
#' getQtlGeno(apiary, caste = "all")
#' getQtlGeno(apiary, caste = "all", collapse = TRUE)
#' @export
getQtlGeno <- function(x, caste = NULL, nInd = NULL,
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
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullQtlGeno(pop = x, trait = trait, chr = chr, simParam = simParamBee)
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getQtlGeno(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getQtlGeno(x = tmp, trait = trait, chr = chr, simParamBee = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneGeno(geno = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getQtlGeno(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getQtlGeno Access QTL genotype data of the queen
#' @export
getQueenQtlGeno <- function(x,
                            trait = 1, chr = NULL,
                            collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlGeno(x,
                    caste = "queen",
                    trait = trait, chr = chr,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getQtlGeno Access QTL genotype data of fathers
#' @export
getFathersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL, dronesHaploid = TRUE,
                              collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlGeno(x,
                    caste = "fathers", nInd = nInd,
                    trait = trait, chr = chr,
                    dronesHaploid = dronesHaploid,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getQtlGeno Access QTL genotype data of virgin queens
#' @export
getVirginQueensQtlGeno <- function(x, nInd = NULL,
                                   trait = 1, chr = NULL,
                                   collapse = FALSE,
                                   simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlGeno(x,
                    caste = "virginQueens", nInd = nInd,
                    trait = trait, chr = chr,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getQtlGeno Access QTL genotype data of workers
#' @export
getWorkersQtlGeno <- function(x, nInd = NULL,
                              trait = 1, chr = NULL,
                              collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlGeno(x,
                    caste = "workers", nInd = nInd,
                    trait = trait, chr = chr,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getQtlGeno Access QTL genotype data of drones
#' @export
getDronesQtlGeno <- function(x, nInd = NULL,
                             trait = 1, chr = NULL, dronesHaploid = TRUE,
                             collapse = FALSE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getQtlGeno(x,
                    caste = "drones", nInd = nInd,
                    trait = trait, chr = chr,
                    dronesHaploid = dronesHaploid,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

# get*SegSite* ----

#' @rdname getSegSiteHaplo
#' @title Access haplotypes for all segregating sites of individuals in a
#'   caste
#'
#' @description Level 0 function that returns haplotypes for all segregating
#'   sites of individuals in a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
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
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{getSegSiteGeno}} and \code{\link[AlphaSimR]{pullSegSiteHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' # Input is a population
#' getSegSiteHaplo(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getSegSiteHaplo(queens)
#'
#' # Input is a colony
#' getSegSiteHaplo(colony, caste = "queen")
#' getQueenSegSiteHaplo(colony)
#'
#' getSegSiteHaplo(colony, caste = "workers", nInd = 3)
#' getWorkersSegSiteHaplo(colony)
#' #Same aliases exist for all the castes!
#'
#' # Get haplotypes for all individuals
#' getSegSiteHaplo(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getSegSiteHaplo(colony, caste = "all", collapse = TRUE)
#'
#' #Input is a MultiColony - same behaviour as for the Colony!
#' getSegSiteHaplo(apiary, caste = "queen")
#' getQueenSegSiteHaplo(apiary)
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getSegSiteHaplo(apiary, caste = "all")
#' getSegSiteHaplo(apiary, caste = "all", collapse = TRUE)
#' @export
getSegSiteHaplo <- function(x, caste = NULL, nInd = NULL,
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
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullSegSiteHaplo(pop = x, haplo = haplo, chr = chr, simParam = simParamBee)
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneHaplo(haplo = ret, pop = x)
    }
  } else  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getSegSiteHaplo(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getSegSiteHaplo(x = tmp, haplo = haplo, chr = chr, simParamBee = simParamBee)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getSegSiteHaplo(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getSegSiteHaplo Access haplotype data for all segregating sites of the queen
#' @export
getQueenSegSiteHaplo <- function(x,
                                 haplo = "all", chr = NULL,
                                 collapse = FALSE,
                                 simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteHaplo(x,
                         caste = "queen",
                         haplo = haplo, chr = chr,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSegSiteHaplo Access haplotype data for all segregating sites of fathers
#' @export
getFathersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL,
                                   dronesHaploid = TRUE,
                                   collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteHaplo(x,
                         caste = "fathers", nInd = nInd,
                         haplo = haplo, chr = chr,
                         dronesHaploid = dronesHaploid,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSegSiteHaplo Access haplotype data for all segregating sites of virgin queens
#' @export
getVirginQueensSegSiteHaplo <- function(x, nInd = NULL,
                                        haplo = "all", chr = NULL,
                                        collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteHaplo(x,
                         caste = "virginQueens", nInd = nInd,
                         haplo = haplo, chr = chr,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSegSiteHaplo Access haplotype data for all segregating sites of workers
#' @export
getWorkersSegSiteHaplo <- function(x, nInd = NULL,
                                   haplo = "all", chr = NULL,
                                   collapse = FALSE,
                                   simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteHaplo(x,
                         caste = "workers", nInd = nInd,
                         haplo = haplo, chr = chr,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSegSiteHaplo Access haplotype data for all segregating sites of drones
#' @export
getDronesSegSiteHaplo <- function(x, nInd = NULL,
                                  haplo = "all", chr = NULL,
                                  dronesHaploid = TRUE,
                                  collapse = FALSE,
                                  simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteHaplo(x,
                         caste = "drones", nInd = nInd,
                         haplo = haplo, chr = chr,
                         dronesHaploid = dronesHaploid,
                         collapse = collapse,
                         simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname getSegSiteGeno
#' @title Access genotypes for all segregating sites of individuals in a
#'   caste
#'
#' @description Level 0 function that returns genotypes for all segregating
#'   sites of individuals in a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with genotypes of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{getSegSiteHaplo}},
#'   \code{\link[AlphaSimR]{pullSegSiteGeno}}, and
#'   \code{\link[SIMplyBee]{getPooledGeno}}
#'
#' @return matrix with genotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' # Input is a population
#' getSegSiteGeno(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getSegSiteGeno(queens)
#'
#' # Input is a colony
#' getSegSiteGeno(colony, caste = "queen")
#' getQueenSegSiteGeno(colony)
#'
#' getSegSiteGeno(colony, caste = "workers", nInd = 3)
#' getWorkersSegSiteGeno(colony)
#' # same aliases exist for all the castes!
#'
#' # Get genotypes for all individuals
#' getSegSiteGeno(colony, caste = "all")
#' # Get all genotypes in a single matrix
#' getSegSiteGeno(colony, caste = "all", collapse = TRUE)
#'
#' # Input is a MultiColony - same behaviour as for the Colony
#' getSegSiteGeno(apiary, caste = "queen")
#' getQueenSegSiteGeno(apiary)
#'
#' # Get the genotypes of all individuals either by colony or in a single matrix
#' getSegSiteGeno(apiary, caste = "all")
#' getSegSiteGeno(apiary, caste = "all", collapse = TRUE)
#' @export
getSegSiteGeno <- function(x, caste = NULL, nInd = NULL,
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
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullSegSiteGeno(pop = x, chr = chr, simParam = simParamBee)
  } else  if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getSegSiteGeno(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getSegSiteGeno(x = tmp, chr = chr, simParamBee = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneGeno(geno = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getSegSiteGeno(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getSegSiteGeno Access genotype data for all segregating sites of the queen
#' @export
getQueenSegSiteGeno <- function(x,
                                chr = NULL, collapse = FALSE,
                                simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteGeno(x,
                        caste = "queen",
                        collapse = collapse,
                        chr = chr, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSegSiteGeno Access genotype data for all segregating sites of fathers
#' @export
getFathersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, dronesHaploid = TRUE,
                                  collapse = FALSE,
                                  simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteGeno(x,
                        caste = "fathers", nInd = nInd,
                        chr = chr, dronesHaploid = dronesHaploid,
                        collapse = collapse,
                        simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSegSiteGeno Access genotype data for all segregating sites of virgin queens
#' @export
getVirginQueensSegSiteGeno <- function(x, nInd = NULL,
                                       chr = NULL, collapse = FALSE,
                                       simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteGeno(x,
                        caste = "virginQueens", nInd = nInd,
                        chr = chr, collapse = collapse,
                        simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSegSiteGeno Access genotype data for all segregating sites of workers
#' @export
getWorkersSegSiteGeno <- function(x, nInd = NULL,
                                  chr = NULL, collapse = FALSE,
                                  simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteGeno(x,
                        caste = "workers", nInd = nInd,
                        chr = chr, collapse = collapse,
                        simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSegSiteGeno Access genotype data for all segregating sites of drones
#' @export
getDronesSegSiteGeno <- function(x, nInd = NULL,
                                 chr = NULL, dronesHaploid = TRUE,
                                 collapse = FALSE,
                                 simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSegSiteGeno(x,
                        caste = "drones", nInd = nInd,
                        chr = chr, dronesHaploid = dronesHaploid,
                        collapse = collapse,
                        simParamBee = simParamBee
  )
  return(ret)
}

# get*Snp* ----

#' @rdname getSnpHaplo
#' @title Access SNP array haplotypes of individuals in a caste
#'
#' @description Level 0 function that returns SNP array haplotypes of
#'   individuals in a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
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
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{getSnpGeno}} and \code{\link[AlphaSimR]{pullSnpHaplo}}
#'
#' @return matrix with haplotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   and list of matrices with haplotypes when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' # Input is a population
#' getSnpHaplo(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getSnpHaplo(queens)
#'
#' # Input is a colony
#' getSnpHaplo(colony, caste = "queen")
#' getQueenSnpHaplo(colony)
#'
#' getSnpHaplo(colony, caste = "workers", nInd = 3)
#' getWorkersSnpHaplo(colony)
#' # Same aliases exist for all the castes!
#'
#' # Get haplotypes for all individuals
#' getSnpHaplo(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getSnpHaplo(colony, caste = "all", collapse = TRUE)
#'
#' # Input is a MultiColony - same behaviour as for the Colony!
#' getSnpHaplo(apiary, caste = "queen")
#' getQueenSnpHaplo(apiary)
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getSnpHaplo(apiary, caste = "all")
#' getSnpHaplo(apiary, caste = "all", collapse = TRUE)
#'
#' @export
getSnpHaplo <- function(x, caste = NULL, nInd = NULL,
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
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullSnpHaplo(pop = x, snpChip = snpChip, haplo = haplo, chr = chr, simParam = simParamBee)
    if (dronesHaploid && any(x@sex == "M")) {
      ret <- reduceDroneHaplo(haplo = ret, pop = x)
    }
  } else  if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getSnpHaplo(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getSnpHaplo(x = tmp, haplo = haplo, snpChip = snpChip, chr = chr, simParamBee = simParamBee)
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getSnpHaplo(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getSnpHaplo Access SNP array haplotype data of the queen
#' @export
getQueenSnpHaplo <- function(x,
                             snpChip = 1, haplo = "all", chr = NULL,
                             collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpHaplo(x,
                     caste = "queen",
                     snpChip = snpChip, haplo = haplo, chr = chr,
                     collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSnpHaplo Access SNP array haplotype data of fathers
#' @export
getFathersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL,
                               dronesHaploid = TRUE, collapse = FALSE,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpHaplo(x,
                     caste = "fathers", nInd = nInd,
                     snpChip = snpChip, haplo = haplo, chr = chr,
                     dronesHaploid = dronesHaploid,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSnpHaplo Access SNP array haplotype data of virgin queens
#' @export
getVirginQueensSnpHaplo <- function(x, nInd = NULL,
                                    snpChip = 1, haplo = "all", chr = NULL,
                                    collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpHaplo(x,
                     caste = "virginQueens", nInd = nInd,
                     snpChip = snpChip, haplo = haplo, chr = chr,
                     collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSnpHaplo Access SNP array haplotype of workers
#' @export
getWorkersSnpHaplo <- function(x, nInd = NULL,
                               snpChip = 1, haplo = "all", chr = NULL,
                               collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpHaplo(x,
                     caste = "workers", nInd = nInd,
                     snpChip = snpChip, haplo = haplo, chr = chr,
                     collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSnpHaplo Access SNP array haplotype data of drones
#' @export
getDronesSnpHaplo <- function(x, nInd = NULL,
                              snpChip = 1, haplo = "all", chr = NULL,
                              dronesHaploid = TRUE, collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpHaplo(x,
                     caste = "drones", nInd = nInd,
                     snpChip = snpChip, haplo = haplo, chr = chr,
                     dronesHaploid = dronesHaploid,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @rdname getSnpGeno
#' @title Access SNP array genotypes of individuals in a caste
#'
#' @description Level 0 function that returns SNP array genotypes of individuals
#'   in a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param snpChip numeric, indicates which SNP array genotypes to retrieve
#' @param chr numeric, chromosomes to retrieve, if \code{NULL}, all chromosome
#'   are retrieved
#' @param dronesHaploid logical, return haploid result for drones?
#' @param collapse logical, if the return value should be a single matrix
#'   with genotypes of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{getSnpHaplo}},
#'  \code{\link[AlphaSimR]{pullSnpGeno}}, and
#'  \code{\link[SIMplyBee]{getPooledGeno}}
#'
#' @return matrix with genotypes when \code{x} is \code{\link[SIMplyBee]{Colony-class}} and
#'   list of matrices with genotypes when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' # Input is a population
#' getSnpGeno(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getSnpGeno(queens)
#'
#' # Input is a colony
#' getSnpGeno(colony, caste = "queen")
#' getQueenSnpGeno(colony)
#'
#' getSnpGeno(colony, caste = "workers", nInd = 3)
#' getWorkersSnpGeno(colony)
#' # Same aliases exist for all the castes!
#'
#' # Get genotypes for all individuals
#' getSnpGeno(colony, caste = "all")
#' # Get all haplotypes in a single matrix
#' getSnpGeno(colony, caste = "all", collapse = TRUE)
#'
#' # Input is a MultiColony - same behaviour as for the Colony!
#' getSnpGeno(apiary, caste = "queen")
#' getQueenSnpGeno(apiary)
#'
#' # Get the haplotypes of all individuals either by colony or in a single matrix
#' getSnpGeno(apiary, caste = "all")
#' getSnpGeno(apiary, caste = "all", collapse = TRUE)
#' @export
getSnpGeno <- function(x, caste = NULL, nInd = NULL,
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
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pullSnpGeno(pop = x, snpChip = snpChip, chr = chr, simParam = simParamBee)
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getSnpGeno(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- getSnpGeno(x = tmp, snpChip = snpChip, chr = chr, simParamBee = simParamBee)
        if (dronesHaploid && any(tmp@sex == "M")) {
          ret <- reduceDroneGeno(geno = ret, pop = tmp)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getSnpGeno(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getSnpGeno Access SNP array genotype data of the queen
#' @export
getQueenSnpGeno <- function(x,
                            snpChip = 1, chr = NULL,
                            collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpGeno(x,
                    caste = "queen",
                    snpChip = snpChip, chr = chr,
                    collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSnpGeno Access SNP array genotype data of fathers
#' @export
getFathersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL, dronesHaploid = TRUE,
                              collapse = FALSE,
                              simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpGeno(x,
                    caste = "fathers", nInd = nInd,
                    snpChip = snpChip, chr = chr,
                    dronesHaploid = dronesHaploid,
                    collapse = collapse,
                    simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSnpGeno Access SNP array genotype data of virgin queens
#' @export
getVirginQueensSnpGeno <- function(x, nInd = NULL,
                                   snpChip = 1, chr = NULL,
                                   collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpGeno(x,
                    caste = "virginQueens", nInd = nInd,
                    snpChip = snpChip, chr = chr,
                    collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSnpGeno Access SNP array genotype data of workers
#' @export
getWorkersSnpGeno <- function(x, nInd = NULL,
                              snpChip = 1, chr = NULL,
                              collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpGeno(x,
                    caste = "workers", nInd = nInd,
                    snpChip = snpChip, chr = chr,
                    collapse = collapse, simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getSnpGeno Access SNP array genotype data of drones
#' @export
getDronesSnpGeno <- function(x, nInd = NULL,
                             snpChip = 1, chr = NULL, dronesHaploid = TRUE,
                             collapse = FALSE,
                             simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getSnpGeno(x,
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
#' @param type character, \code{"mean"} for average genotype (default) or
#'   \code{"count"} for the counts of reference and alternative alleles
#' @param sex character, vector of "F" and "M" to denote the sex of individuals
#'   in \code{x}
#'
#' @return a numeric vector with average allele dosage when \code{type = "mean"}
#'   or a two-row matrix with the counts of reference (1st row) and
#'   alternative (2nd row) alleles when \code{type = "count"}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' # How to get pooled genotypes of workers across multiple colonies?
#' tmp <- getSegSiteGeno(x = apiary, caste = "workers")
#' (tmp2 = lapply(X = tmp, FUN = getPooledGeno)) # as a list of one row matrices
#' t(sapply(X = tmp, FUN = getPooledGeno)) # as one matrix - option A
#' do.call(what = rbind, args = tmp2) # as one matrix - option B
#' @export
getPooledGeno <- function(x, type = "mean", sex = NULL) {
  if (!is.matrix(x)) {
    stop("Argument x must be a matrix class object!")
  }
  if (is.null(type) | !(type %in% c("mean", "count"))) {
    stop("Argument type must be specified as either mean or count!")
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
#'   \code{x}; if \code{NULL}, then \code{\link[SIMplyBee]{calcBeeAlleleFreq}}
#'   is used
#' @param returnComponents logical, return GRM as well as the components used
#'   to compute it (useful for GWAS by GBLUP)
#'
#' @return if \code{returnComponents = FALSE} (default) return a matrix of
#'   genomic relatedness coefficients; if \code{returnComponents = TRUE} return
#'   a list with the GRM, centred genotype matrix, allele frequencies, and
#'   scaling factor used to scale the crossproduct of centred genotype matrix
#'   to get the GRM.
#'
#' @references
#' \insertRef{druet2020Theoretical}{SIMplyBee}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 1, nDrones = nFathersPoisson)
#' colony <- createColony(basePop[2])
#' colony <- cross(x = colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' geno <- getSegSiteGeno(colony, collapse = TRUE)
#' sex <- getCasteSex(x = colony, collapse = TRUE)
#'
#' GRM <- calcBeeGRMIbs(x = geno, sex = sex)
#' # You can visualise this matrix with the function image() from the package 'Matrix'
#'
#' # Look at the diagonal at the relationship matrix
#' x <- diag(GRM)
#' hist(x)
#' summary(x)
#'
#' # Look at the off-diagonal at the relationship matrix
#' x <- GRM[lower.tri(x = GRM, diag = FALSE)]
#' hist(x)
#' summary(x)
#'
#' # Compare relationship between castes
#' ids <- getCasteId(colony)
#' idQueen <- ids$queen
#' idWorkers <- ids$workers
#' idDrones <- ids$drones
#'
#' # Queen vs others
#' GRM[idQueen, idWorkers]
#' GRM[idQueen, idDrones]
#'
#' # Workers vs worker
#' GRM[idWorkers, idWorkers]
#'
#' # Workers vs drones
#' GRM[idWorkers, idDrones]
#'
#' # Calculating allele frequencies ourselves (say, to "shift" base population)
#' aF <- calcBeeAlleleFreq(x = geno, sex = sex)
#' hist(aF)
#' GRM2 <- calcBeeGRMIbs(x = geno, sex = sex, alleleFreq = aF)
#' stopifnot(identical(GRM2, GRM))
#'
#' # You can also create relationships with pooled genomes
#' pooledGenoW <- getPooledGeno(getWorkersSegSiteGeno(colony),
#'                              type = "mean",
#'                              sex = getCasteSex(colony, caste="workers"))
#' queenGeno <- getQueenSegSiteGeno(colony)
#' # Compute relationship between pooled workers genotype and the queen
#' calcBeeGRMIbs(x = rbind(queenGeno, pooledGenoW), sex = c("F","F"))
#' # You can now compare how this compare to relationships between the queen
#' # individual workers!
#'
#' # Return components
#' calcBeeGRMIbs(x = rbind(queenGeno, pooledGenoW), sex = c("F","F"),
#'               returnComponents = TRUE)
#' @export
calcBeeGRMIbs <- function(x, sex, alleleFreq = NULL, returnComponents = FALSE) {
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
    # TODO: if speed ever becomes an issue, and RAM is available, we could do
    #       alleleFreq = matrix(data = ploidy * alleleFreq, nrow = 1, ncol = ncol(x))
    #       x = x - rep(x = 1, times = nInd(pop)) %*% alleleFreq
    #       This would overwrite x only once, at expense of doubling RAM
    x[, site] <- x[, site] - ploidy * alleleFreq[site]
  }
  scale <- 2 * sum(alleleFreq * (1 - alleleFreq))
  G <- tcrossprod(x) / scale
  if (returnComponents) {
    return(list(G = G, x = x, alleleFreq = alleleFreq, scale = scale))
  } else {
    return(G)
  }
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
#' \insertRef{fernando1989Covariance}{SIMplyBee}
#' \insertRef{fernando1990Genetic}{SIMplyBee}
#' \insertRef{grossman1989Inbreeding}{SIMplyBee}
#' \insertRef{hill2011Variation}{SIMplyBee}
#' \insertRef{vanarendonk1994Use}{SIMplyBee}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 1, nDrones = nFathersPoisson)
#' colony <- createColony(basePop[2])
#' colony <- cross(x = colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
#'
#' haploQ <- getQueenIbdHaplo(colony)
#' haploW <- getWorkersIbdHaplo(colony)
#' haploD <- getDronesIbdHaplo(colony)
#' SP$pedigree
#'
#' haplo <- rbind(haploQ, haploW, haploD)
#'
#' GRMs <- calcBeeGRMIbd(x = haplo)
#' # You can visualise this matrix with the image() functions from the "Matrix" package
#'
#' # Inspect the diagonal of the relationship matrix between individuals
#' x <- diag(GRMs$indiv)
#' hist(x)
#' summary(x)
#'
#'  # Inspect the off-diagonal of the relationship matrix between individuals
#' x <- GRMs$indiv[lower.tri(x = GRMs$indiv, diag = FALSE)]
#' hist(x)
#' summary(x)
#'
#' ids <- getCasteId(colony)
#' qI <- ids$queen
#' wI <- sort(ids$workers)
#' dI <- sort(ids$drones)
#'
#' qG <- c(t(outer(X = qI, Y = 1:2, FUN = paste, sep = "_")))
#' wG <- c(t(outer(X = wI, Y = 1:2, FUN = paste, sep = "_")))
#' dG <- paste(dI, 1, sep = "_")
#'
#' # Queen vs workers
#' GRMs$genome[wG, qG]
#' GRMs$indiv[wI, qI]
#'
#' # Queen vs drones
#' GRMs$genome[dG, qG]
#' GRMs$indiv[dI, qI]
#'
#' # Workers vs workers
#' GRMs$genome[wG, wG]
#' GRMs$indiv[wI, wI]
#'
#' # Workers vs drones
#' GRMs$genome[dG, wG]
#' GRMs$indiv[dI, wI]
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

#' @rdname getPheno
#' @title Access phenotype values of individuals in a caste
#'
#' @description Level 0 function that returns phenotype values of individuals in a
#'   caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with phenotypes of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[AlphaSimR]{pheno}} and
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return vector of genetic values when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   and list of vectors of genetic values when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' SP$addTraitA(nQtlPerChr = 10, var = 1)
#' SP$setVarE(varE = 1)
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
#' # Input is a population
#' getPheno(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getPheno(queens)
#'
#' # Input is a colony
#' getPheno(colony, caste = "queen")
#' getQueenPheno(colony)
#'
#' getPheno(colony, caste = "fathers")
#' getPheno(colony, caste = "fathers", nInd = 2)
#' getPheno(colony, caste = "fathers", nInd = 2) # random sample!
#' getFathersPheno(colony)
#' getFathersPheno(colony, nInd = 2)
#'
#' getPheno(colony, caste = "workers")
#' getWorkersPheno(colony)
#' # Same aliases exist for all the castes!!!
#'
#' # Get phenotypes for all individuals
#' getPheno(colony, caste = "all")
#' # Get all phenotypes in a single matrix
#' getPheno(colony, caste = "all", collapse = TRUE)
#'
#' # Input is a MultiColony - same behaviour as for the Colony!
#' getPheno(apiary, caste = "queen")
#' getQueenPheno(apiary)
#'
#' # Get the phenotypes of all individuals either by colony or in a single matrix
#' getPheno(apiary, caste = "all")
#' getPheno(apiary, caste = "all", collapse = TRUE)
#' @export
getPheno <- function(x, caste = NULL, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- pheno(pop = x)
    rownames(ret) <- x@id
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getPheno(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, use="order", simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- pheno(pop = tmp)
        rownames(ret) <- tmp@id
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getPheno(x = x[[colony]], caste = caste, nInd = nInd,
                      collapse = collapse, simParamBee = simParamBee)
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getPheno Access phenotype value of the queen
#' @export
getQueenPheno <- function(x, collapse = FALSE, simParamBee = NULL) {
  ret <- getPheno(x, caste = "queen", collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getPheno Access phenotype values of fathers
#' @export
getFathersPheno <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  ret <- getPheno(x, caste = "fathers", nInd = nInd, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getPheno Access phenotype values of virgin queens
#' @export
getVirginQueensPheno <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  ret <- getPheno(x, caste = "virginQueens", nInd = nInd, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getPheno Access phenotype values of workers
#' @export
getWorkersPheno <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  ret <- getPheno(x, caste = "workers", nInd = nInd, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getPheno Access phenotype values of drones
#' @export
getDronesPheno <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  ret <- getPheno(x, caste = "drones", nInd = nInd, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @rdname calcColonyValue
#' @title Calculate colony value(s)
#'
#' @description Level 0 function that calculate value(s) of a colony.
#'
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param FUN function, that calculates colony value from values of
#'   colony members
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#' @param ... other arguments of \code{FUN}
#'
#' @seealso \code{\link[SIMplyBee]{mapCasteToColonyValue}} as an example of \code{FUN},
#'   \code{\link[SIMplyBee]{selectColonies}} for example for to select colonies based
#'   on these values, and
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return a matrix with one value or a row of values when \code{x} is
#'   \code{\link[SIMplyBee]{Colony-class}} and a row-named matrix when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, where names are colony IDs
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#'
#' # Define two traits that collectively affect colony honey yield:
#' # 1) queen's effect on colony honey yield, say via pheromone secretion phenotype
#' # 2) workers' effect on colony honey yield, say via foraging ability phenotype
#' # The traits will have a negative genetic correlation of -0.5 and heritability
#' # of 0.25 (on an individual level)
#' nWorkers <- 10
#' mean <- c(10, 10 / nWorkers)
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
#' drones <- createDrones(x = basePop[1], nInd = 200)
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
#' # Phenotype value
#' calcColonyPheno(colony)
#' calcColonyPheno(apiary)
#' # Genetic value
#' calcColonyGv(colony)
#' calcColonyGv(apiary)
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
    ret <- FUN(x = x, ...)
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    # We could create a matrix output container here, BUT we don't know the output
    # dimension of FUN() so we create list and row bind the list nodes later
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- FUN(x = x[[colony]], ...)
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
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

#' @rdname calcInheritanceCriterion
#' @title Calculate the inheritance criterion
#'
#' @description Level 0 function that calculates the inheritance criterion as the
#'   sum of the queen (maternal) and workers (direct) effect from the queen,
#'   as defined by Du et al. (2021). This can be seen as the expected value
#'   of drones from the queen or half the expected value of virgin queens from
#'   the queen.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}} or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param queenTrait numeric (column position) or character (column name), trait
#'   that represents queen's effect on the colony value; if \code{NULL}
#'   then this effect is 0
#' @param workersTrait numeric (column position) or character (column name), trait
#'   that represents workers' effect on the colony value; if \code{NULL}
#'   then this effect is 0
#' @param use character, the measure to use for the calculation, being
#'   either "gv" (genetic value), "ebv" (estimated breeding value),
#'   or "pheno" (phenotypic value)
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#' @return integer when \code{x} is
#'   \code{\link[SIMplyBee]{Colony-class}} and a named list when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, where names are colony IDs
#'
#' @seealso \code{\link[SIMplyBee]{calcSelectionCriterion}} and
#'   \code{\link[SIMplyBee]{calcPerformanceCriterion}} and  as well as
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @references
#' \insertRef{du2021Short}{SIMplyBee}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' meanA <- c(10, 10 / SP$nWorkers)
#' varA <- c(1, 1 / SP$nWorkers)
#' corA <- matrix(data = c( 1.0, -0.5,
#'                         -0.5,  1.0), nrow = 2, byrow = TRUE)
#' SP$addTraitA(nQtlPerChr = 100, mean = meanA, var = varA, corA = corA,
#' name = c("queenTrait", "workersTrait"))
#' varE <- c(3, 3 / SP$nWorkers)
#' corE <- matrix(data = c(1.0, 0.3,
#'                         0.3, 1.0), nrow = 2, byrow = TRUE)
#' SP$setVarE(varE = varE, corE = corE)
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
#' calcInheritanceCriterion(colony, queenTrait = 1, workersTrait = 2)
#' calcInheritanceCriterion(apiary, queenTrait = 1, workersTrait = 2)
#'
#' apiary[[2]] <- removeQueen(apiary[[2]])
#' calcInheritanceCriterion(apiary, queenTrait = 1, workersTrait = 2)
#'
#' @export
calcInheritanceCriterion <- function(x, queenTrait = 1, workersTrait = 2, use = "gv", simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!use %in% c("gv", "ebv", "pheno")) {
    stop("Argument use must be 'gv', 'ebv', or 'pheno'!")
  }
  if (isPop(x)) {
    if(!all(isQueen(x, simParamBee = simParamBee))) {
      stop("x must be queens!")
    }
    if (is.null(queenTrait)) {
      queenEffect <- 0
    } else {
      queenEffect <- slot(x, use)[, queenTrait]
    }
    if (!is.null(workersTrait)) {
      workerEffect <- 0
    } else {
      workerEffect <- slot(x, use)[, workersTrait]
    }
    ret <- queenEffect + workerEffect
  } else if (isColony(x)) {
    if(!isQueenPresent(x, simParamBee = simParamBee)) {
      stop("No queen in the Colony!")
    }
    ret <- calcInheritanceCriterion(getQueen(x, simParamBee = simParamBee),
                                    queenTrait = queenTrait,
                                    workersTrait = workersTrait,
                                    use = use, simParamBee = simParamBee)
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      if (isQueenPresent(x[[colony]], simParamBee = simParamBee)) {
        ret[[colony]] <- calcInheritanceCriterion(x[[colony]],
                                                  queenTrait = queenTrait,
                                                  workersTrait = workersTrait,
                                                  use = use, simParamBee = simParamBee)
      } else {
        ret[colony] <- list(NULL)
      }
    }
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @rdname calcPerformanceCriterion
#' @title Calculate the performance criterion
#'
#' @description Level 0 function that calculates the performance criterion as the
#'   sum of the queen (maternal) effect from the queen and the workers (direct)
#'   effect from her workers, as defined by Du et al. (2021). This can be seen
#'   as the expected value of the colony.
#'
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param queenTrait numeric (column position) or character (column name), trait
#'   that represents queen's effect on the colony value; if \code{NULL}
#'   then this effect is 0
#' @param workersTrait numeric (column position) or character (column name), trait
#'   that represents workers' effect on the colony value; if \code{NULL}
#'   then this effect is 0
#' @param workersTraitFUN function, that will be applied to the workers effect
#'   values of workers, default is sum (see examples), but note that the correct
#'   function will depend on how you will setup simulation!
#' @param use character, the measure to use for the calculation, being
#'   either "gv" (genetic value),"ebv" (estimated breeding value),
#'   or "pheno" (phenotypic value)
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{calcSelectionCriterion}} and
#'   \code{\link[SIMplyBee]{calcInheritanceCriterion}} and  as well as
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return integer when \code{x} is
#'   \code{\link[SIMplyBee]{Colony-class}} and a named list when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, where names are colony IDs
#'
#' @references
#' \insertRef{du2021Short}{SIMplyBee}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' meanA <- c(10, 10 / SP$nWorkers)
#' varA <- c(1, 1 / SP$nWorkers)
#' corA <- matrix(data = c( 1.0, -0.5,
#'                         -0.5,  1.0), nrow = 2, byrow = TRUE)
#' SP$addTraitA(nQtlPerChr = 100, mean = meanA, var = varA, corA = corA,
#' name = c("queenTrait", "workersTrait"))
#' varE <- c(3, 3 / SP$nWorkers)
#' corE <- matrix(data = c(1.0, 0.3,
#'                         0.3, 1.0), nrow = 2, byrow = TRUE)
#' SP$setVarE(varE = varE, corE = corE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(colony)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(apiary)
#'
#' calcPerformanceCriterion(colony, queenTrait = 1, workersTrait = 2, workersTraitFUN = sum)
#' calcPerformanceCriterion(apiary, queenTrait = 1, workersTrait = 2, workersTraitFUN = sum)
#'
#' apiary[[2]] <- removeQueen(apiary[[2]])
#' calcPerformanceCriterion(apiary, queenTrait = 1,
#'                          workersTrait = 2, workersTraitFUN = sum)
#'
#' @export
calcPerformanceCriterion <- function(x, queenTrait = 1, workersTrait = 2,
                                     workersTraitFUN = sum, use = "gv",
                                     simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!use %in% c("gv", "ebv", "pheno")) {
    stop("Argument use must be 'gv', 'ebv', or 'pheno'!")
  }
  if (isColony(x)) {
    if(!isQueenPresent(x, simParamBee = simParamBee)) {
      stop("No queen in the Colony!")
    }
    if (!isWorkersPresent(x, simParamBee = simParamBee)) {
      stop("No workers in the Colony!")
    }
    if (is.null(queenTrait)) {
      queenEffect <- 0
    } else {
      queenEffect <- slot(x@queen, use)[, queenTrait]
    }
    if (!is.null(workersTrait)) {
      workerEffect <- 0
    } else {
      workerEffect <-workersTraitFUN(slot(x@workers, use)[, workersTrait])
    }
    ret <- queenEffect + workerEffect
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      if (isQueenPresent(x[[colony]], simParamBee = simParamBee)) {
        ret[[colony]] <- calcPerformanceCriterion(x[[colony]],
                                                  queenTrait = queenTrait,
                                                  workersTrait = workersTrait,
                                                  workersTraitFUN = workersTraitFUN,
                                                  use = use,
                                                  simParamBee = simParamBee)
      } else {
        ret[colony] <- list(NULL)
      }
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @rdname calcSelectionCriterion
#' @title Calculate the selection criterion
#'
#' @description Level 0 function that calculates the selection criterion as the
#'   sum of workers (direct) and queen (maternal) effects of workers,
#'   as defined by Du et al. (2021). This can be seen as the expected value
#'   of virgin queens from the queen (as well as workers, but we would not be
#'   selecting workers).
#'
#' @param x \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#' @param queenTrait numeric (column position) or character (column name), trait
#'   that represents queen's effect on the colony value; if \code{NULL} then this contribution is 0
#' @param queenTraitFUN function, that will be applied to the queen effect
#'   values of workers, default is sum (see examples), but note that the correct
#'   function will depend on how you will setup simulation!
#' @param workersTrait numeric (column position) or character (column name), trait
#'   that represents workers' effect on the colony value; if \code{NULL} then this contribution is 0
#' @param workersTraitFUN function, that will be applied to the workers effect
#'   values of workers, default is sum (see examples), but note that the correct
#'   function will depend on how you will setup simulation!
#' @param use character, the measure to use for the calculation, being
#'   either "gv" (genetic value), "ebv" (estimated breeding value),
#'   or "pheno" (phenotypic value)
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[SIMplyBee]{calcInheritanceCriterion}} and
#'   \code{\link[SIMplyBee]{calcPerformanceCriterion}} and  as well as
#`   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return integer when \code{x} is
#'   \code{\link[SIMplyBee]{Colony-class}} and a named list when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, where names are colony IDs
#'
#' @references
#' \insertRef{du2021Short}{SIMplyBee}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' meanA <- c(10, 10 / SP$nWorkers)
#' varA <- c(1, 1 / SP$nWorkers)
#' corA <- matrix(data = c( 1.0, -0.5,
#'                         -0.5,  1.0), nrow = 2, byrow = TRUE)
#' SP$addTraitA(nQtlPerChr = 100, mean = meanA, var = varA, corA = corA,
#' name = c("queenTrait", "workersTrait"))
#' varE <- c(3, 3 / SP$nWorkers)
#' corE <- matrix(data = c(1.0, 0.3,
#'                         0.3, 1.0), nrow = 2, byrow = TRUE)
#' SP$setVarE(varE = varE, corE = corE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(colony)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(apiary)
#'
#' calcSelectionCriterion(colony,
#'                        queenTrait = 1, queenTraitFUN = sum,
#'                        workersTrait = 2, workersTraitFUN = sum)
#' calcSelectionCriterion(apiary,
#'                        queenTrait = 1, queenTraitFUN = sum,
#'                        workersTrait = 2, workersTraitFUN = sum)
#'
#' apiary[[2]] <- removeQueen(apiary[[2]])
#' calcSelectionCriterion(apiary, queenTrait = 1,
#'                        workersTrait = 2, workersTraitFUN = sum)
#'
#' @export
calcSelectionCriterion <- function(x, queenTrait = 1, queenTraitFUN = sum,
                                   workersTrait = 2, workersTraitFUN = sum,
                                   use = "gv", simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!use %in% c("gv", "ebv", "pheno")) {
    stop("Argument use must be 'gv', 'ebv', or 'pheno'!")
  }
  if (isColony(x)) {
    if(!isQueenPresent(x, simParamBee = simParamBee)) {
      stop("No queen in the Colony!")
    }
    if (!isWorkersPresent(x, simParamBee = simParamBee)) {
      stop("No workers in the Colony!")
    }
    if (is.null(queenTrait)) {
      queenEffect <- 0
    } else {
      queenEffect <- queenTraitFUN(slot(x@workers, use)[, queenTrait])
    }
    if (!is.null(workersTrait)) {
      workersEffect <- 0
    } else {
      workersEffect <- workersTraitFUN(slot(x@workers, use)[, workersTrait])
    }
    ret <- queenEffect + workersEffect
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      if (isQueenPresent(x[[colony]], simParamBee = simParamBee)) {
        ret[[colony]] <- calcSelectionCriterion(x[[colony]],
                                                queenTrait = queenTrait,
                                                queenTraitFUN = queenTraitFUN,
                                                workersTrait = workersTrait,
                                                workersTraitFUN = workersTraitFUN,
                                                use = use,
                                                simParamBee = simParamBee)
      } else {
        ret[colony] <- list(NULL)
      }
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

# get*Gv ----

#' @rdname getGv
#' @title Access genetic values of individuals in a caste
#'
#' @description Level 0 function that returns genetic values of individuals
#'   in a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with genetic values of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[AlphaSimR]{gv}} and
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return vector of phenotype values when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   and list of vectors of genetic values when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
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
#' # Input is a population
#' getGv(x = getQueen(colony))
#' queens <- getQueen(apiary, collapse = TRUE)
#' getGv(queens)
#'
#' # Input is a colony
#' getGv(colony, caste = "queen")
#' getQueenGv(colony)
#'
#' getGv(colony, caste = "workers")
#' getWorkersGv(colony)
#' # Same aliases exist for all the castes!
#'
#' # Get genetic values for all individuals
#' getGv(colony, caste = "all")
#' # Get all genetic values in a single matrix
#' getGv(colony, caste = "all", collapse = TRUE)
#'
#' # Input is a MultiColony - same behaviour as for the Colony!
#' getGv(apiary, caste = "queen")
#' getQueenGv(apiary)
#'
#' # Get the genetic values of all individuals either by colony or in a single matrix
#' getGv(apiary, caste = "all")
#' getGv(apiary, caste = "all", collapse = TRUE)
#' @export
getGv <- function(x, caste =NULL, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- gv(pop = x)
    rownames(ret) <- x@id
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getGv(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, use="order", simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret <- NULL
      } else {
        ret <- gv(pop = tmp)
        rownames(ret) <- tmp@id
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      tmp <- getGv(x = x[[colony]], caste = caste, nInd = nInd,
                   collapse = collapse, simParamBee = simParamBee)
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getGv Access genetic value of the queen
#' @export
getQueenGv <- function(x, collapse = FALSE, simParamBee = NULL) {
  ret <- getGv(x, caste = "queen", collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getGv Access genetic values of fathers
#' @export
getFathersGv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  ret <- getGv(x, caste = "fathers", nInd = nInd, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getGv Access genetic values of virgin queens
#' @export
getVirginQueensGv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  ret <- getGv(x, caste = "virginQueens", nInd = nInd, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getGv Access genetic values of workers
#' @export
getWorkersGv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  ret <- getGv(x, caste = "workers", nInd = nInd, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getGv Access genetic values of drones
#' @export
getDronesGv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  ret <- getGv(x, caste = "drones", nInd = nInd, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony genetic value from caste individuals' genetic values
#' @export
calcColonyGv <- function(x, FUN = mapCasteToColonyGv, simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

# get*Bv ----

#' @rdname getBv
#' @title Access breeding values of individuals in a caste
#'
#' @description Level 0 function that returns breeding values of individuals in
#'   a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with breeding valued of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[AlphaSimR]{bv}} and
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return vector of breeding values when \code{x} is \code{\link[SIMplyBee]{Colony-class}}
#'   and list of vectors of breeding values when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x} is
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' # Not exporting this function, since the theory behind it is not fully developed
getBv <- function(x, caste = NULL, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- bv(pop = x, simParam = simParamBee)
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getBv(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
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
      tmp <- getBv(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getBv Access breeding value of the queen
getQueenBv <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getBv(x,
               caste = "queen",
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getBv Access breeding values of fathers
getFathersBv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getBv(x,
               caste = "fathers", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getBv Access breeding values of virgin queens
getVirginQueensBv <- function(x, nInd = NULL,collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getBv(x,
               caste = "virginQueens", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getBv Access breeding values of workers
getWorkersBv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getBv(x,
               caste = "workers", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getBv Access breeding values of drones
getDronesBv <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getBv(x,
               caste = "drones", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony breeding value from caste individuals' breeding values
calcColonyBv <- function(x, FUN = mapCasteToColonyBv, simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

# get*Dd ----

#' @rdname getDd
#' @title Access dominance values of individuals in a caste
#'
#' @description Level 0 function that returns dominance values of
#'   individuals in a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with dominance values of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[AlphaSimR]{dd}} and
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return vector of dominance values when \code{x} is
#'   \code{\link[SIMplyBee]{Colony-class}} and list of vectors of dominance values when
#'   \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x}
#'   is \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' # Not exporting this function, since the theory behind it is not fully developed
getDd <- function(x, caste = NULL, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <- dd(pop = x, simParam = simParamBee)
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getDd(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
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
      tmp <- getDd(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getDd Access dominance value of the queen
getQueenDd <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getDd(x,
               caste = "queen",
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getDd Access dominance values of fathers
getFathersDd <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getDd(x,
               caste = "fathers", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getDd Access dominance values of virgin queens
getVirginQueensDd <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getDd(x,
               caste = "virginQueens", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getDd Access dominance values of workers
getWorkersDd <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getDd(x,
               caste = "workers", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getDd Access dominance values of drones
getDronesDd <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getDd(x,
               caste = "drones", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony dominance value from caste individuals' dominance values
calcColonyDd <- function(x, FUN = mapCasteToColonyDd, simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  calcColonyValue(x = x, FUN = FUN, simParamBee = simParamBee, ...)
}

# get*Aa ----

#' @rdname getAa
#' @title Access epistasis values of individuals in a caste
#'
#' @description Level 0 function that returns epistasis values of
#'   individuals in a caste.
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#' @param caste NULL or character, NULL when \code{x} is a \code{\link[AlphaSimR]{Pop-class}},
#'   and character when \code{x} is a \code{\link[SIMplyBee]{Colony-class}} or
#'    \code{\link[SIMplyBee]{MultiColony-class}} with the possible values of "queen", "fathers",
#'    "workers", "drones", "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed, otherwise a random sample
#' @param collapse logical, if the return value should be a single matrix
#'   with epistatic values of all the individuals
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link[AlphaSimR]{dd}} and
#'   \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
#'
#' @return vector of epistasis values when \code{x} is
#'   \code{\link[SIMplyBee]{Colony-class}} and list of vectors of epistasis values when
#'   \code{x} is \code{\link[SIMplyBee]{MultiColony-class}}, named by colony id when \code{x}
#'   is \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' # Not exporting this function, since the theory behind it is not fully developed
getAa <- function(x, caste = NULL, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste must be of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must not be negative!")
  }
  if (is.null(x)) {
    ret <- NULL
  } else if (isPop(x)) {
    ret <-  aa(pop = x, simParam = simParamBee)
  } else if (isColony(x)) {
    if (is.null(caste)) {
      caste <- "all"
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getAa(x = x, caste = caste, nInd = nInd,
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
      tmp <- getCastePop(x = x, caste = caste, nInd = nInd, simParamBee = simParamBee)
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
      tmp <- getAa(
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
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getAa Access epistasis value of the queen
getQueenAa <- function(x, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getAa(x,
               caste = "queen",
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getAa Access epistasis values of fathers
getFathersAa <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getAa(x,
               caste = "fathers", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getAa Access epistasis values of virgin queens
getVirginQueensAa <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getAa(x,
               caste = "virginQueens", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getAa Access epistasis values of workers
getWorkersAa <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getAa(x,
               caste = "workers", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getAa Access epistasis values of drones
getDronesAa <- function(x, nInd = NULL, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  ret <- getAa(x,
               caste = "drones", nInd = nInd,
               collapse = collapse,
               simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn calcColonyValue Calculate colony epistasis value from caste individuals' epistasis value
calcColonyAa <- function(x, FUN = mapCasteToColonyAa, simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
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
#' @param pop \code{\link[AlphaSimR]{Pop-class}}
#' @param alleles \code{NULL} or list;
#'   If \code{NULL}, then the function samples a heterozygous csd genotype for
#'   each virgin queen from all possible csd alleles.
#'   If not \code{NULL}, the user provides a list of length \code{nInd} with each
#'   node holding a matrix or a data.frame, each having two rows and n columns.
#'   Each row must hold one csd haplotype (allele) that will be assigned to a
#'   virgin queen. The n columns span the length of the csd locus as specified
#'   in \code{\link[SIMplyBee]{SimParamBee}}. The two csd alleles must be different to
#'   ensure heterozygosity at the csd locus.
#' @param simParamBee global simulation parameters.
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 10)
#'
#' SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 2)
#' tmp <- createVirginQueens(founderGenomes)
#' getCsdAlleles(tmp)
#'
#' SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 4)
#' tmp <- createVirginQueens(founderGenomes)
#' getCsdAlleles(tmp)
#'
#' @return Returns an object of \code{\link[AlphaSimR]{Pop-class}}
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

    alleles <- sapply(X = seq_len(pop@nInd),
                      FUN = function(x) {
                        list(alleles[sample(nAlleles, size = 2, replace = FALSE), , drop = FALSE])
                      })
  }

  if (pop@nInd != length(alleles)) {
    stop("The length of the allele list must match the number of individuals in the pop argument")
  }
  if (any(sapply(alleles, FUN = function(x) all(x[1, ] == x[2, ])))) {
    stop("You must provide two different alleles for each individual!")
  }

  # Pull out csd haplotype matrix
  csdH = pullMarkerHaplo(pop, markers = paste(simParamBee$csdChr, csdSites, sep="_"), simParam = simParamBee)
  # Prepare the haplotype matrix
  alleles <- as.matrix(do.call(rbind, alleles))
  rownames(alleles) <- rownames(csdH)
  colnames(alleles) <- colnames(csdH)

  pop <- setMarkerHaplo(pop, haplo=alleles, simParam = simParamBee)
  return(pop)
}

#' @rdname createCrossPlan
#' @title Create a plan for crossing virgin queens
#'
#' @description Level 0 function that creates a plan for crossing virgin queens or
#'   virgin colonies by sampling drones or drone producing colonies to mate with a the
#'   virgin queens/colonies either at random (\code{spatial = FALSE})
#'   or according to the distance between colonies (\code{spatial = TRUE}).
#'
#' @param x \code{\link[AlphaSimR]{Pop-class}} or \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}},
#'   the object with the virgin queens that need to be crossed. When \code{spatial = TRUE},
#'   the argument needs to be a \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}
#'   with the location set
#' @param drones \code{\link[AlphaSimR]{Pop-class}}, a population of drones (resembling a drone
#'   congregation area) available for mating. When \code{spatial = TRUE}, the user can not
#'   provide drones, but needs to provide drone producing colonies instead
#'   (see argument \code{droneColonies})
#' @param droneColonies \code{\link[SIMplyBee]{MultiColony-class}}, drone producing colonies
#'   available for mating. When \code{spatial = TRUE},
#'   the object needs to have the location set
#' @param nDrones, integer or function, number of drones to sample for each crossing. You need to provide
#'   this to provide this argument even when sampling drone producing colonies (otherwise, the default value
#'   will be used)
#' @param spatial logical, whether the drone producing colonies should be sampled according
#'   to their distance from the virgin colony (that is, in a radius)
#' @param radius numeric, the radius from the virgin colony in which to sample mating partners,
#'   only needed when \code{spatial = TRUE}
#' @param simParamBee \code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters
#' @param ... other arguments for \code{nDrones}, when \code{nDrones} is a function
#'
#' @return named list with names being virgin queens/colonies IDs with each
#'   list element holding the IDs of selected drones or drone producing colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 1000, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' # Create three virgin MultiColony objects with locations
#' virginColonies1 <- createMultiColony(basePop[1:30])
#' virginColonies1 <- setLocation(virginColonies1,
#'                                location = Map(c, runif(30, 0, 2*pi),
#'                                                  runif(30, 0, 2*pi)))
#' virginColonies2 <- createMultiColony(basePop[31:60])
#' virginColonies2 <- setLocation(virginColonies2,
#'                                location = Map(c, runif(30, 0, 2*pi),
#'                                                  runif(30, 0, 2*pi)))
#'
#' # Create drone colonies
#' droneColonies <- createMultiColony(basePop[121:200])
#' droneColonies <- setLocation(droneColonies,
#'                              location = Map(c, runif(80, 0, 2*pi),
#'                                                runif(80, 0, 2*pi)))
#'
#' # Create some drones to mate initial drone colonies with
#' DCA <- createDrones(basePop[201:300], nInd = 20)
#' # Cross initial virgin drone colonies to the DCA with a random cross plan
#' randomCrossPlan <- createCrossPlan(x = droneColonies,
#'                                    drones = DCA,
#'                                    nDrones = nFathersPoisson,
#'                                    spatial = FALSE)
#' droneColonies <- cross(droneColonies,
#'                        drones = DCA,
#'                        nDrones = nFathersPoisson,
#'                        crossPlan = randomCrossPlan)
#'
#' # Cross according to a cross plan that is created internally within the cross function
#' # The cross plan is created at random, regardless the location of the colonies
#' colonies1 <- cross(x = virginColonies1,
#'                    droneColonies = droneColonies,
#'                    nDrones = nFathersPoisson,
#'                    crossPlan = "create")
#'
#'
#' # Cross according to a spatial cross plan created internally according to the colonies' locations
#' colonies2 <- cross(x = virginColonies2,
#'                    crossPlan = "create",
#'                    droneColonies = droneColonies,
#'                    nDrones = nFathersPoisson,
#'                    spatial = TRUE,
#'                    radius = 1.5)
#' nFathers(colonies2)
#'
#'
#' @export
createCrossPlan <- function(x,
                            drones = NULL,
                            droneColonies = NULL,
                            nDrones = NULL,
                            spatial = FALSE,
                            radius,
                            simParamBee = NULL,
                            ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!is.null(drones) && !is.null(droneColonies)) {
    stop("You can either provide drones as a single Pop-class or droneColonies as
          a single MultiColony-class, but not both!")
  }
  if (spatial && !(isColony(x) | isMultiColony(x))) {
    stop("When creating a spatial cross plan, the x argument needs to be a
          Colony or a MultiColony object with set location!")
  }
  if (is.null(droneColonies) && spatial) {
    stop("When creating a spatial cross plan, you must provide droneColonies as a
         MultiColony object with location set!")
  }
  if (is.null(droneColonies) && !isPop(drones)) {
    stop("Argument drones must be a Pop class!")
  }
  if (spatial && !any(isColony(x), isMultiColony(x))) {
    stop("When creating a spatial cross plan, the x argument must be a Colony
         or MultiColony class with location set!")
  }
  drones <- drones[isDrone(x = drones, simParamBee = simParamBee)]
  virginId <- getId(x)

  if (is.function(nDrones)) {
    nDrones <- nDrones(n = length(virginId), ...)
  } else {
    nDrones <- rep(nDrones, length(virginId))
  }

  if (spatial) {
    virginLocations <- getLocation(x, collapse = TRUE)
    droneLocations <- getLocation(droneColonies, collapse = TRUE)

    spatialMatch <- RANN::nn2(droneLocations, virginLocations, searchtype = "radius", radius = radius)
    spatialMatch <- spatialMatch$nn.idx
    crossPlan <- lapply(1:nrow(spatialMatch),
                        FUN = function(x) rownames(droneLocations)[spatialMatch[x,]])
  } else  {
    if (is.null(nDrones)) {
      nDrones <- simParamBee$nFathers
    }
    if (!is.null(drones)) {
      fathersMatch <- rep.int(virginId, times = nDrones)
      fatherIDs <- sample(drones@id, size = length(fathersMatch), replace = FALSE)
      crossPlan <- base::split(fatherIDs, fathersMatch)
    } else {
      print("Cross plan, drone colonies")
      ids <- getId(droneColonies)
      crossPlan <- lapply(1:length(virginId), FUN = function(x) unique(sample(x = ids, size = nDrones[x], replace = TRUE)))
    }
  }
  names(crossPlan) <- virginId
  return(crossPlan)
}

#' @rdname mapLoci
#' @title Finds loci on a genetic map and return a list of positions
#'
#' @description Finds loci on a genetic map and return a list of positions.
#' This function is adopted from AlphaSimR (Gaynor et al., 2021).
#' @references
#' \insertRef{gaynor2021AlphaSimR}{SIMplyBee}
#'
#' @param markers character, vector of marker positions as "chr_position"
#' @param genMap list, genetic map
#'
#' @return A list with number of loci per chromosome and genetic positions of the markers
mapLoci = function(markers, genMap){
  # Check that the markers are present on the map
  genMapMarkerNames = unlist(lapply(genMap, names))
  stopifnot(all(markers%in%genMapMarkerNames))

  # Create lociPerChr and lociLoc
  lociPerChr = integer(length(genMap))
  lociLoc = vector("list", length(genMap))

  # Loop through chromosomes
  for(i in 1:length(genMap)){

    # Initialize lociLoc
    lociLoc[[i]] = integer()

    # Find matches if they exist
    take = match(names(genMap[[i]]), markers)
    lociPerChr[i] = length(na.omit(take))
    if(lociPerChr[i]>0L){
      lociLoc[[i]] = which(!is.na(take))
    }
  }
  lociLoc = unlist(lociLoc)

  return(list(lociPerChr=lociPerChr,
              lociLoc=lociLoc))
}


#' @rdname cor2cov
#' @title Convert correlation matrix to covariance matrix
#'
#' @description Convert correlation matrix to covariance matrix
#' This function is adopted from AlphaSimR (Gaynor et al., 2021).
#' @references
#' \insertRef{gaynor2021AlphaSimR}{SIMplyBee}
#'
#' @param corr correlation matrix
#' @param var vector of variances
#' @return covariance matrix
cor2cov <- function(corr, var) {
  corr <- as.matrix(corr)
  var  <- as.numeric(var)
  sd <- sqrt(var)
  cov <- corr * (sd %o% sd)
  return(cov)
}

#' Calculate coefficients to map across individual and colony variances
#'
#' Calculate the coefficients used to map variances in a fixed colony design.
#' Forward mappings in \code{mapIndToColonyVar} multiply individual components.
#' Inverse mappings in \code{mapColonyToIndVar} divide aggregate components.
#'
#' @param nW Positive integer scalar, number of workers in the colony.
#' @param nF Positive integer scalar, number of distinct mating fathers available
#'   to sire workers. With random allocation, some fathers may sire no workers.
#' @param nDPQ Positive integer scalar, number of distinct drone-producing queens
#'   (DPQs) represented among the mating fathers. Equal numbers of fathers per
#'   DPQ are assumed; nF must be divisible by nDPQ.
#' @param workersFUN Character scalar, "sum" or "mean", specifying aggregation
#'   of the workers' worker-effect trait values.
#' @param workerAllocation Character scalar, "random" for independent uniform
#'   sampling of a father for each worker, or "balanced" for exactly nW/nF
#'   workers per father. Balanced allocation requires nW divisible by nF.
#'
#' @return A list of three coefficients:
#'   \itemize{
#'   \item \code{worker}: multiplier converting individual worker-effect additive
#'     genetic variance to variance of the worker-group sum or mean.
#'   \item \code{queenWorker}: multiplier converting within-individual genetic
#'     covariance between queen-effect and worker-effect traits to covariance
#'     between the colony queen's queen-effect contribution and the worker-group
#'     worker-effect contribution. This coefficient does not include the factor
#'     of two used when calculating total colony variance.
#'   \item \code{environment}: multiplier converting individual worker-effect
#'     environmental variance to variance of the worker-group sum or mean,
#'     assuming environmental deviations are independent between workers.
#'   }
#'
#' @details The model assumes non-inbred colony queens and DPQs, mutually
#'   unrelated across those parents. Distinct fathers from the same DPQ may be
#'   brothers. The queen-effect variance multiplier is one and is not returned.
#'
#'   Let w be 1 for sums or 1/nW for means.
#'   Ordered distinct-worker pairs are partitioned into
#'   same father giving super-sisters with additive relationship of 3/4,
#'   distinct fathers from the same DPQ giving full-sisters with additive relationship of 1/2, and
#'   different DPQs giving half-sisters with additive relationship of 1/4.
#'   The worker coefficient is w^2 * (nW + relationship-weighted pair counts).
#'   Random allocation uses expected pair counts;
#'   balanced allocation uses exact pair counts.
#'   The queenWorker coefficient is nW*w/2, and
#'   the environment coefficient is nW*w^2.
#'
#'   Divisibility checks establish that the assumed equal allocation is possible;
#'   totals alone cannot verify actual family composition.
#'   These coefficients describe marginal colony components,
#'   not sample variances across colonies sharing relatives.
#'   Unequal contributions, general ancestry, and random colony sizes
#'   require a more general model.
#' @seealso \code{\link{mapIndToColonyVar}}, \code{\link{mapColonyToIndVar}}
#' @keywords internal
calcColonyAndIndCoefficients <- function(nW, nF, nDPQ, workersFUN,
                                         workerAllocation) {
  for (name in c("nW", "nF", "nDPQ")) {
    value <- get(name)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 1 || value != floor(value)) {
      stop(name, " must be a positive integer scalar.")
    }
  }
  if (length(workersFUN) != 1L || is.na(workersFUN) ||
      !workersFUN %in% c("sum", "mean")) {
    stop("workersFUN must be 'sum' or 'mean'.")
  }
  if (length(workerAllocation) != 1L || is.na(workerAllocation) ||
      !workerAllocation %in% c("random", "balanced")) {
    stop("workerAllocation must be 'random' or 'balanced'.")
  }
  # Equal father counts per DPQ are assumed.
  # Check that the supplied totals permit that allocation.
  if (nF %% nDPQ != 0) {
    stop("Equal fathers per DPQ requires nF divisible by nDPQ.")
  }
  if (workerAllocation == "balanced") {
    if (nW %% nF != 0) {
      stop("Balanced worker allocation requires nW divisible by nF.")
    }
    sameFather <- nW^2 / nF - nW
    sameDPQ <- nW^2 / nDPQ - nW^2 / nF
    differentDPQ <- nW^2 * (1 - 1 / nDPQ)
  } else {
    sameFather <- nW * (nW - 1) / nF
    sameDPQ <- nW * (nW - 1) * (1 / nDPQ - 1 / nF)
    differentDPQ <- nW * (nW - 1) * (1 - 1 / nDPQ)
  }
  weight <- if (workersFUN == "mean") 1 / nW else 1
  list(worker = weight^2 * (nW + 0.75 * sameFather +
                             0.5 * sameDPQ + 0.25 * differentDPQ),
       queenWorker = nW * weight / 2,
       environment = nW * weight^2)
}

#' Validate variance and correlation inputs
#'
#' Check named collections of scalar variance and correlation parameters used
#' by the individual/colony variance mappings. Invalid inputs raise an error.
#'
#' @param variances Named list of numeric, finite, nonnegative variance scalars.
#' @param correlations Named list of numeric, finite correlation scalars in
#'   the interval [-1, 1].
#' @return Invisibly returns \code{NULL} when all checked inputs are valid;
#'   otherwise stops with an error.
#' @keywords internal
validateVarsAndCors <- function(variances, correlations) {
  for (name in names(variances)) {
    value <- variances[[name]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
      stop(name, " must be a finite nonnegative scalar.")
    }
  }
  for (name in names(correlations)) {
    value <- correlations[[name]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        abs(value) > 1) {
      stop(name, " must be a finite correlation in [-1, 1].")
    }
  }
}

#' Stable conversion of a covariance and two variances to a correlation
#'
#' Divide a covariance by the product of the corresponding standard deviations,
#' but return \code{NA} when either variance (standard deviation) are zero.
#'
#' @param covariance Numeric scalar covariance between the two variables.
#' @param variance1 Numeric scalar variance of the first variable.
#' @param variance2 Numeric scalar variance of the second variable.
#' @return Numeric scalar correlation, or \code{NA_real_}
#'   if either variance is zero and the correlation is therefore undefined.
#' @keywords internal
covVar2Cor <- function(covariance, variance1, variance2) {
  if (variance1 == 0 || variance2 == 0) return(NA_real_)
  covariance / (sqrt(variance1) * sqrt(variance2))
}

#' @rdname mapIndToColonyVar
#' @title Map variance components between individuals and colonies
#'
#' @description Map individual queen-effect and worker-effect variance components
#'   to colony variance components, or recover individual variance components from
#'   separate queen and worker-group colony variance components.
#'
#' @param varAQueen Numeric scalar, additive genetic variance of
#'   the queen-effect trait among individual diploid bees in a base population.
#' @param varAWorker Numeric scalar, additive genetic variance of
#'   the worker-effect trait among individual diploid bees in a base population.
#' @param corAQueenWorker Numeric scalar, additive genetic correlation between
#'   the queen-effect trait and the worker-effect trait
#'   among individual diploid bees in a base population.
#'   This is the within-individual cross-trait correlation among individuals.
#' @param varEQueen Numeric scalar, environmental variance of
#'   the queen-effect trait among individual bees in a base population.
#' @param varEWorker Numeric scalar, environmental variance of
#'   the worker-effect trait among individual bees in a base population.
#' @param corEQueenWorker Numeric scalar, environmental correlation between
#'   the queen-effect trait and the worker-effect trait
#'   among individual diploid bees in a base population.
#'   This is the within-individual cross-trait correlation among individuals.
#' @param varAWorkerGroup Numeric scalar, additive genetic variance of
#'   the aggregated worker-effect trait among colonies in a base population.
#' @param corAQueenWorkerGroup Numeric scalar, additive genetic correlation between
#'   the queen's queen-effect trait and
#'   the aggregated workers' worker-effect trait
#'   among colonies in a base population.
#'   This is the cross-caste and cross-trait correlation among colonies.
#' @param varEWorkerGroup Numeric scalar, environmental variance of
#'   the aggregated worker-effect trait among colonies in a base population.
#' @param corEQueenWorkerGroup Numeric scalar, environmental correlation between
#'   the queen's queen-effect trait and
#'   the aggregated workers' worker-effect trait
#'   among colonies in a base population.
#'   This is the cross-caste and cross-trait correlation among colonies.
#' @param nW numeric, number of workers in the colony
#' @param nF numeric, number of fathers of the workers
#' @param nDPQ numeric, number of drone producing queens (DPQs)
#'   represented among the mating fathers
#' @param workersFUN character, either "sum" or "mean",
#'   indicating whether the workers' contributions are summed or averaged
#'   when forming the aggregated worker-effect contribution to colony value.
#' @param workerAllocation character, either "random" (default) or "balanced".
#'
#' @return Both functions return a flat list with the same named components.
#'
#'   \strong{Components already described as inputs}
#'   All components listed below are returned by both functions; the function
#'   labels indicate which inputs they correspond to.
#'   \itemize{
#'   \item Both functions: \code{varAQueen}, \code{varEQueen}, \code{nW},
#'     \code{nF}, \code{nDPQ}, \code{workersFUN}, and \code{workerAllocation}.
#'   \item \code{mapIndToColonyVar}: \code{varAWorker}, \code{varEWorker},
#'     \code{corAQueenWorker}, and \code{corEQueenWorker}.
#'   \item \code{mapColonyToIndVar}: \code{varAWorkerGroup},
#'     \code{varEWorkerGroup}, \code{corAQueenWorkerGroup}, and
#'     \code{corEQueenWorkerGroup}. The returned group correlations are
#'     \code{NA} if either corresponding variance is zero, even if zero
#'     was supplied as the correlation.
#'   }
#'
#'   \strong{Additional components}
#'   \itemize{
#'   \item \code{covAQueenWorker}: additive genetic covariance between
#'     the queen-effect trait and the worker-effect trait
#'     among individual diploid bees in a base population.
#'   \item \code{covAQueenWorkerGroup}: additive genetic covariance between
#'     the queen's queen-effect trait and
#'     the aggregated workers' worker-effect trait
#'     among colonies in a base population.
#'   \item \code{covEQueenWorker}: environmental covariance between
#'     the queen-effect trait and the worker-effect trait
#'     among individual diploid bees in a base population.
#'   \item \code{covEQueenWorkerGroup}: environmental covariance between
#'     the queen's queen-effect trait and
#'     the aggregated workers' worker-effect trait
#'     among colonies in a base population.
#'     Zero under the assumption of independent environmental deviations between bees.
#'   \item \code{varAColony} and \code{varEColony}: additive genetic
#'     and environmental variance of colony values.
#'     Each is the sum of the queen and worker-group variances plus twice their covariance.
#'   \item \code{h2Queen}: \code{varAQueen / (varAQueen + varEQueen)},
#'     heritability of the queen-effect trait among individual bees.
#'   \item \code{h2Worker}: \code{varAWorker / (varAWorker + varEWorker)},
#'     heritability of the worker-effect trait among individual bees.
#'   \item \code{h2WorkerGroup}: \code{varAWorkerGroup / (varAWorkerGroup + varEWorkerGroup)},
#'     heritability of the aggregated worker-effect trait among colonies.
#'   \item \code{h2Colony}: \code{varAColony / (varAColony + varEColony)},
#'     heritability of colony values, including genetic covariance between
#'     queen's queen trait and worker-group's worker trait in \code{varAColony}.
#'     All four heritabilities are \code{NA} when their denominator is zero.
#'   }
#'
#'   Covariances are not multiplied by two.
#'   Calculated correlations are \code{NA} if either corresponding variance is zero.
#'   In \code{mapColonyToIndVar}, \code{covEQueenWorker} and \code{corEQueenWorker}
#'   are both returned as \code{NA},
#'   because colony components do not identify
#'   within-individual environmental covariance or correlation.
#'
#' @details These mappings describe marginal variances under an additive model
#'   with non-inbred, mutually unrelated colony queens and drone-producing
#'   queens (DPQs), distinct fathers, and equal numbers of fathers per DPQ.
#'   They do not predict sample variance across colonies sharing relatives.
#'   Each diploid bee has genetic values for both the queen-effect and
#'   worker-effect traits. A colony value combines the queen's value for the
#'   queen-effect trait with the aggregated workers' values for the
#'   worker-effect trait. Thus Queen and Worker identify trait roles as well as
#'   their expressing castes; WorkerGroup always denotes an aggregate.
#'
#'   With random worker allocation,
#'   each worker independently samples a father uniformly.
#'   With balanced allocation,
#'   each father has exactly \code{nW/nF} workers,
#'   which requires \code{nW} to be divisible by \code{nF}.
#'   Both require \code{nF} divisible by \code{nDPQ}.
#'   The distinct-worker relationship is 3/4 for a shared father,
#'   1/2 for distinct fathers from the same DPQ, and 1/4 otherwise.
#'   Queen-offspring relationship is 1/2.
#'   Environmental deviations are independent between bees.
#'   Calculated correlations are \code{NA} when either variance is zero;
#'   individual correlations supplied to the forward mapping are retained.
#'
#'   The inverse divides worker-group variances and queen-worker-group
#'   covariance by the corresponding design coefficients.
#'   Both directions must use the same colony design and aggregation.
#'   A nonzero \code{corEQueenWorkerGroup} raises an error,
#'   as do colony components implying an individual genetic correlation outside [-1, 1].
#'   If either genetic component variance is zero,
#'   the inverse accepts \code{NA} or zero for \code{corAQueenWorkerGroup}.
#'   Similarly, \code{NA} or zero can be supplied for \code{corEQueenWorkerGroup}
#'   when either environmental component variance is zero.
#'   The zero-correlation restriction between bees does not apply to \code{corEQueenWorker},
#'   which describes environmental association between traits within the same bee.
#'
#'   Both results are flat lists. To pass a result to either mapping function,
#'   select the entries named in \code{names(formals(fun))} before using
#'   \code{do.call(fun, ...)}; the full result contains extra components.
#'   Before mapping an inverse result forward, supply an explicit
#'   \code{corEQueenWorker}, which the inverse cannot identify.
#'
#' @references
#' \insertRef{brascamp2014Methods}{SIMplyBee}
#' \insertRef{brascamp2019Note}{SIMplyBee}
#'
#' @examples
#' nW <- 100
#' nF <- 15
#' nDPQ <- 5
#'
#' # Mapping individual vars to colony with worker group mean
#' (colonyVars <- mapIndToColonyVar(
#'   varAQueen = 2, varAWorker = 0.01, corAQueenWorker = -0.5,
#'   varEQueen = 3, varEWorker = 0.02, corEQueenWorker = 0.3,
#'   nW = nW, nF = nF, nDPQ = nDPQ, workersFUN = "mean"))
#' # ... and inverse
#' (indVars <- do.call(mapColonyToIndVar,
#'   colonyVars[names(formals(mapColonyToIndVar))]))
#'
#' # Mapping colony vars with worker group sum to individual vars
#' (indVars <- mapColonyToIndVar(
#'   varAQueen = 2, varAWorkerGroup = 0.01, corAQueenWorkerGroup = -0.5,
#'   varEQueen = 3, varEWorkerGroup = 0.02, corEQueenWorkerGroup = 0.0,
#'   nW = nW, nF = nF, nDPQ = nDPQ, workersFUN = "sum"))
#' # ... and back to colony components, assuming zero within-bee
#' # environmental correlation (not identified by the inverse mapping).
#' indArgs <- indVars[names(formals(mapIndToColonyVar))]
#' indArgs$corEQueenWorker <- 0
#' (colonyVars <- do.call(mapIndToColonyVar, indArgs))
#' @export
mapIndToColonyVar <- function(varAQueen, varAWorker, corAQueenWorker,
                              varEQueen, varEWorker, corEQueenWorker,
                              nW, nF, nDPQ, workersFUN = "sum",
                              workerAllocation = "random") {
  validateVarsAndCors(list(varAQueen = varAQueen, varAWorker = varAWorker,
                           varEQueen = varEQueen, varEWorker = varEWorker),
                       list(corAQueenWorker = corAQueenWorker, corEQueenWorker = corEQueenWorker))
  k <- calcColonyAndIndCoefficients(nW, nF, nDPQ, workersFUN, workerAllocation)
  varAWorkerGroup <- k$worker * varAWorker
  covAQueenWorkerGroup <- k$queenWorker * corAQueenWorker * sqrt(varAQueen) * sqrt(varAWorker)
  corAQueenWorkerGroup <- covVar2Cor(covAQueenWorkerGroup, varAQueen, varAWorkerGroup)
  varAColony <- varAQueen + varAWorkerGroup + 2 * covAQueenWorkerGroup
  varEWorkerGroup <- k$environment * varEWorker
  covEQueenWorkerGroup <- 0
  # Correlation is zero unless either variance is zero, when it is undefined.
  corEQueenWorkerGroup <- covVar2Cor(covEQueenWorkerGroup, varEQueen, varEWorkerGroup)
  varEColony <- varEQueen + varEWorkerGroup
  covAQueenWorker <- corAQueenWorker * sqrt(varAQueen) * sqrt(varAWorker)
  covEQueenWorker <- corEQueenWorker * sqrt(varEQueen) * sqrt(varEWorker)
  list(
    varAQueen = varAQueen,
    varAWorker = varAWorker,
    covAQueenWorker = covAQueenWorker,
    corAQueenWorker = corAQueenWorker,
    varAWorkerGroup = varAWorkerGroup,
    covAQueenWorkerGroup = covAQueenWorkerGroup,
    corAQueenWorkerGroup = corAQueenWorkerGroup,
    varAColony = varAColony,
    varEQueen = varEQueen,
    varEWorker = varEWorker,
    covEQueenWorker = covEQueenWorker,
    corEQueenWorker = corEQueenWorker,
    varEWorkerGroup = varEWorkerGroup,
    covEQueenWorkerGroup = covEQueenWorkerGroup,
    corEQueenWorkerGroup = corEQueenWorkerGroup,
    varEColony = varEColony,
    h2Queen = if (varAQueen + varEQueen == 0) NA_real_ else varAQueen / (varAQueen + varEQueen),
    h2Worker = if (varAWorker + varEWorker == 0) NA_real_ else varAWorker / (varAWorker + varEWorker),
    h2WorkerGroup = if (varAWorkerGroup + varEWorkerGroup == 0) NA_real_ else varAWorkerGroup / (varAWorkerGroup + varEWorkerGroup),
    h2Colony = if (varAColony + varEColony == 0) NA_real_ else varAColony / (varAColony + varEColony),
    nW = nW,
    nF = nF,
    nDPQ = nDPQ,
    workersFUN = workersFUN,
    workerAllocation = workerAllocation
  )
}

#' @describeIn mapIndToColonyVar Recover individual variance components from
#'   separate queen and worker-group colony components.
#' @export
mapColonyToIndVar <- function(varAQueen, varAWorkerGroup, corAQueenWorkerGroup,
                              varEQueen, varEWorkerGroup, corEQueenWorkerGroup,
                              nW, nF, nDPQ, workersFUN = "sum",
                              workerAllocation = "random") {
  # A correlation is undefined if either component has zero variance
  if (length(corAQueenWorkerGroup) == 1L && is.na(corAQueenWorkerGroup) &&
      (isTRUE(varAQueen == 0) || isTRUE(varAWorkerGroup == 0))) corAQueenWorkerGroup <- 0
  if (length(corEQueenWorkerGroup) == 1L && is.na(corEQueenWorkerGroup) &&
      (isTRUE(varEQueen == 0) || isTRUE(varEWorkerGroup == 0))) corEQueenWorkerGroup <- 0
  validateVarsAndCors(list(varAQueen = varAQueen, varAWorkerGroup = varAWorkerGroup,
                           varEQueen = varEQueen, varEWorkerGroup = varEWorkerGroup),
                       list(corAQueenWorkerGroup = corAQueenWorkerGroup, corEQueenWorkerGroup = corEQueenWorkerGroup))
  if (corEQueenWorkerGroup != 0) {
    stop("Nonzero corEQueenWorkerGroup requires a shared environment model; environments are assumed independent between bees.")
  }
  k <- calcColonyAndIndCoefficients(nW, nF, nDPQ, workersFUN, workerAllocation)
  varAWorker <- varAWorkerGroup / k$worker
  covAQueenWorkerGroup <- corAQueenWorkerGroup * sqrt(varAQueen) * sqrt(varAWorkerGroup)
  covAQueenWorker <- covAQueenWorkerGroup / k$queenWorker
  corAQueenWorker <- covVar2Cor(covAQueenWorker, varAQueen, varAWorker)
  if (!is.na(corAQueenWorker) && abs(corAQueenWorker) > 1 + 1e-10) {
    stop("Colony genetic components imply an infeasible individual correlation outside [-1, 1].")
  }
  if (!is.na(corAQueenWorker)) corAQueenWorker <- max(-1, min(1, corAQueenWorker))
  varEWorker <- varEWorkerGroup / k$environment
  corAQueenWorkerGroup <- covVar2Cor(covAQueenWorkerGroup, varAQueen, varAWorkerGroup)
  covEQueenWorkerGroup <- 0
  # Correlation is zero unless either variance is zero, when it is undefined.
  corEQueenWorkerGroup <- covVar2Cor(covEQueenWorkerGroup, varEQueen, varEWorkerGroup)
  varAColony <- varAQueen + varAWorkerGroup + 2 * covAQueenWorkerGroup
  varEColony <- varEQueen + varEWorkerGroup
  covEQueenWorker <- NA_real_
  corEQueenWorker <- NA_real_
  list(
    varAQueen = varAQueen,
    varAWorker = varAWorker,
    covAQueenWorker = covAQueenWorker,
    corAQueenWorker = corAQueenWorker,
    varAWorkerGroup = varAWorkerGroup,
    covAQueenWorkerGroup = covAQueenWorkerGroup,
    corAQueenWorkerGroup = corAQueenWorkerGroup,
    varAColony = varAColony,
    varEQueen = varEQueen,
    varEWorker = varEWorker,
    covEQueenWorker = covEQueenWorker,
    corEQueenWorker = corEQueenWorker,
    varEWorkerGroup = varEWorkerGroup,
    covEQueenWorkerGroup = covEQueenWorkerGroup,
    corEQueenWorkerGroup = corEQueenWorkerGroup,
    varEColony = varEColony,
    h2Queen = if (varAQueen + varEQueen == 0) NA_real_ else varAQueen / (varAQueen + varEQueen),
    h2Worker = if (varAWorker + varEWorker == 0) NA_real_ else varAWorker / (varAWorker + varEWorker),
    h2WorkerGroup = if (varAWorkerGroup + varEWorkerGroup == 0) NA_real_ else varAWorkerGroup / (varAWorkerGroup + varEWorkerGroup),
    h2Colony = if (varAColony + varEColony == 0) NA_real_ else varAColony / (varAColony + varEColony),
    nW = nW,
    nF = nF,
    nDPQ = nDPQ,
    workersFUN = workersFUN,
    workerAllocation = workerAllocation
  )
}
