# Level 1 Pop Functions

#' @rdname getCastePop
#' @title Access individuals of a caste
#'
#' @description Level 1 function that returns individuals of a caste. These
#'   individuals stay in the colony (compared to \code{\link{pullCastePop}}).
#'
#' @param x Colony or Colonies
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed; if there are less individuals than requested,
#'   we return the ones available - this can return \code{NULL}
#' @param use character, all options provided by \code{\link{selectInd}} and
#'   \code{"order"} that selects \code{1:nInd} individuals
#'
#' @seealso \code{\link{getQueen}}, \code{\link{getFathers}},
#'   \code{\link{getVirginQueens}}, \code{\link{getWorkers}}, and
#'   \code{\link{getDrones}}
#'
#' @return when \code{x} is \code{\link{Colony-class}} return is
#'   \code{\link{Pop-class}} for \code{caste != "all"} or list for \code{caste
#'   == "all"} with nodes named by caste; when \code{x} is
#'   \code{\link{Colonies-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"} or named list of lists of
#'   \code{\link{Pop-class}} for \code{caste == "all"}
#'
#' @seealso \code{\link{getCasteId}} and \code{\link{getCaste}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#'
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony1 <- addVirginQueens(colony1, nInd = 4)
#' colony1 <- addDrones(colony1, nInd = 2)
#' colony2 <- addWorkers(colony2, nInd = 20)
#'
#' getCastePop(colony1, caste = "queen")
#' getQueen(colony1)
#'
#' drones@id
#' getCastePop(colony1, caste = "fathers")
#' getCastePop(colony1, caste = "fathers")@id
#' getCastePop(colony1, caste = "fathers", nInd = 2)@id
#' getCastePop(colony1, caste = "fathers", nInd = 2)@id
#' getFathers(colony1)
#' getFathers(colony1)@id
#' getFathers(colony1, nInd = 2)@id
#' getFathers(colony1, nInd = 2)@id
#'
#' getFathers(getQueen(colony1))
#'
#' getCastePop(colony1, caste = "virginQueens")
#' getCastePop(colony1, caste = "virginQueens")@id
#' getCastePop(colony1, caste = "virginQueens", nInd = 2)@id
#' getCastePop(colony1, caste = "virginQueens", nInd = 2)@id
#' getVirginQueens(colony1)
#' getVirginQueens(colony1)@id
#' getVirginQueens(colony1, nInd = 2)@id
#' getVirginQueens(colony1, nInd = 2)@id
#'
#' getCastePop(colony1, caste = "workers")
#' getCastePop(colony1, caste = "workers")@id
#' getCastePop(colony1, caste = "workers", nInd = 2)@id
#' getCastePop(colony1, caste = "workers", nInd = 2)@id
#' getWorkers(colony1)
#' getWorkers(colony1)@id
#' getWorkers(colony1, nInd = 2)@id
#' getWorkers(colony1, nInd = 2)@id
#'
#' getCastePop(colony1, caste = "drones")
#' getCastePop(colony1, caste = "drones")@id
#' getCastePop(colony1, caste = "drones", nInd = 2)@id
#' getCastePop(colony1, caste = "drones", nInd = 2)@id
#' getDrones(colony1)
#' getDrones(colony1)@id
#' getDrones(colony1, nInd = 2)@id
#' getDrones(colony1, nInd = 2)@id
#'
#' getCastePop(colony2, caste = "drones")
#' getDrones(colony2)
#'
#' apiary <- c(colony1, colony2)
#' getCastePop(apiary, caste = "queen")
#' getQueen(apiary)
#' getCastePop(apiary, caste = "queen")[[1]]@id
#' getCastePop(apiary, caste = "queen")[[2]]@id
#'
#' getCastePop(apiary, caste = "fathers")
#' getFathers(apiary)
#' getFathers(apiary)[[1]]@id
#' getFathers(apiary)[[2]]@id
#' getFathers(apiary, nInd = 2)
#'
#' getCastePop(apiary, caste = "virginQueens")
#' getVirginQueens(apiary)
#' getVirginQueens(apiary)[[1]]@id
#' getVirginQueens(apiary)[[2]]
#' getVirginQueens(apiary, nInd = 1)
#' getVirginQueens(apiary, nInd = 2)
#'
#' getCastePop(apiary, caste = "workers")
#' getWorkers(apiary)
#' getWorkers(apiary)[[1]]@id
#' getWorkers(apiary)[[2]]@id
#' getWorkers(apiary, nInd = 2)
#'
#' getCastePop(apiary, caste = "drones")
#' getDrones(apiary)
#' getDrones(apiary)[[1]]@id
#' getDrones(apiary)[[2]]
#' getDrones(apiary, nInd = 2)
#'
#' getCastePop(colony1, caste = "all")
#' getCastePop(colony2, caste = "all")
#' @export
getCastePop <- function(x, caste = "all", nInd = NULL, use = "order") {
  if (length(caste) > 1) {
    stop("Argument caste can be only of length 1!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCastePop(x = x, caste = caste, nInd = nInd, use = use)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          if (caste == "drones") {
            test <- isDrone(tmp)
            if (any(!test)) {
              tmp <- tmp[test]
              warning("Taking only drones that have not yet mated!")
            }
          }
          ret[[caste]] <- tmp
        }
      }
    } else {
      if (caste == "fathers") {
        pop <- x@queen@misc[[1]]$fathers
      } else {
        pop <- slot(x, caste)
      }
      if (is.null(pop)) {
        ret <- NULL
      } else {
        if (caste == "drones") {
          test <- isDrone(pop)
          if (any(!test)) {
            pop <- pop[test]
            warning("Taking only drones that have not yet mated!")
          }
        }
        if (is.null(nInd)) {
          nInd <- nInd(pop)
        }
        nIndRequested <- nInd
        nIndAvailable <- nInd(pop)
        if (nIndRequested > nIndAvailable) {
          nIndRequested <- nIndAvailable
        }
        if (use == "order") {
          ret <- pop[1:nIndRequested]
        } else {
          ret <- selectInd(pop = pop, nInd = nIndRequested, use = use)
        }
      }
    }
  } else if (isColonies(x)) {
    fun <- ifelse(caste == "all", lapply, sapply)
    ret <- fun(X = x@colonies, FUN = getCastePop, caste = caste, nInd = nInd, use = use)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCastePop Access the queen
#' @export
getQueen <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- getCastePop(x, caste = "queen", nInd = 1)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCastePop Access fathers (drones the queen mated with)
#' @export
getFathers <- function(x, nInd = NULL, use = "rand") {
  if (isPop(x)) {
    ret <- lapply(
      X = x@misc,
      FUN = function(z) {
        if (is.null(z$fathers)) {
          ret <- NULL
        } else {
          if (is.null(nInd)) {
            n <- nInd(z$fathers)
          }
          ret <- selectInd(pop = z$fathers, nInd = n, use = use)
        }
        return(ret)
      }
    )
    if (nInd(x) == 1) {
      ret <- ret[[1]]
    }
  } else if (isColony(x) | isColonies(x)) {
    ret <- getCastePop(x, caste = "fathers", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCastePop Access virgin queens
#' @export
getVirginQueens <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- getCastePop(x, caste = "virginQueens", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCastePop Access workers
#' @export
getWorkers <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- getCastePop(x, caste = "workers", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn getCastePop Access drones
#' @export
getDrones <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- getCastePop(x, caste = "drones", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname createVirginQueens
#' @title Creates virgin queens from the colony
#'
#' @description Level 1 function that creates the specified number of virgin
#'   queens from the colony by mating the colony queen and the fathers. If csd
#'   locus is active, it takes it into account and any csd homozygotes are
#'   removed and counted towards homozygous brood.
#'
#' @param x \code{link{MapPop-class}} or \code{\link{Colony-class}} or
#'   \code{\link{Colonies-class}}
#' @param nInd numeric or function, number of virgin queens; if \code{NULL} then
#'   \code{\link{SimParamBee}$nVirginQueens} is used; only used when \code{x} is
#'   \code{\link{Colony-class}} or \code{\link{Colonies-class}}, when \code{x}
#'   is \code{link{MapPop-class}} all individuals in \code{x} are converted
#'   into virgin queens
#' @param year numeric, year of birth for virgin queens
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return when \code{x} is \code{link{MapPop-class}} returns
#'   \code{virginQueens} (a \code{\link{Pop-class}});
#'   when \code{x} is \code{\link{Colony-class}} returns
#'   \code{virginQueens} (a \code{\link{Pop-class}});
#'   when \code{x} is \code{\link{Colonies-class}}
#'   return is a named list of \code{virginQueens} (a \code{\link{Pop-class}});
#'   named by colony ID
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$setTrackRec(isTrackRec = TRUE)
#' # Create virgin queens from the founder population (Map-Pop object)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' apiary <- c(colony1, colony2)
#'
#' (virginQueens <- createVirginQueens(colony1, nInd = 10))
#' colony1@queen@id
#' virginQueens@id
#' virginQueens@sex
#' virginQueens@misc
#' virginQueens@mother
#' virginQueens@father
#' SP$pedigree
#' SP$recHist
#' SP$recHist[[23]][[1]][1]
#' SP$recHist[[23]][[1]][2]
#' SP$caste
#'
#' createVirginQueens(apiary, nInd = 10)
#'
#' # Using a default in SP$nVirginQueens
#' # (just to have some virgin queens - change this to your needs!)
#' createVirginQueens(colony1)
#' createVirginQueens(apiary)
#'
#' # Specify own number
#' SP$nVirginQueens <- 15
#' createVirginQueens(colony1)
#' createVirginQueens(apiary)
#' # nVirginQueens will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' createVirginQueens(colony1, nInd = nVirginQueensPoisson)
#' createVirginQueens(apiary, nInd = nVirginQueensPoisson)
#' # nVirginQueens will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nVirginQueens <- nVirginQueensPoisson
#' createVirginQueens(colony1)
#' createVirginQueens(apiary)
#' # nVirginQueens will vary between function calls when a function is used
#' @export
# TODO: explore options for implementing difference between workers' and queens'
#       patrilines
#       https://github.com/HighlanderLab/SIMplyBee/issues/78
createVirginQueens <- function(x, nInd = NULL, year = NULL,
                               simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nInd)) {
    nInd <- simParamBee$nVirginQueens
  }
  # doing "if (is.function(nInd))" below
  if (isMapPop(x)) {
    ret <- newPop(x)
    ret@sex[] <- "F"
    simParamBee$changeCaste(id = ret@id, caste = "V")
    if (!is.null(year)) {
      ret <- setQueensYearOfBirth(x = ret, year = year)
    }
  } else if (isColony(x)) {
    if (is.function(nInd)) {
      nInd <- nInd(colony = x) # see nVirginQueensPoissonColonyStrength
    }
    ret <- createWorkers(x = x, nInd = nInd, exact = TRUE, simParamBee = simParamBee)$workers
    ret@sex[] <- "F"
    simParamBee$changeCaste(id = ret@id, caste = "V")
    if (!is.null(year)) {
      ret <- setQueensYearOfBirth(x = ret, year = year)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- createVirginQueens(
        x = x[[colony]],
        nInd = nInd, year = year, simParamBee = simParamBee
      )
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Map-Pop or Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname createWorkers
#' @title Creates workers from the colony
#'
#' @description Level 1 function that creates the specified number of workers
#'   from the colony by mating the colony queen and the fathers. If csd locus is
#'   active, it takes it into account and any csd homozygotes are removed and
#'   counted towards homozygous brood.
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param nInd numeric or function, number of workers created; if \code{NULL}
#'   then \code{\link{SimParamBee}$nWorkers} is used
#' @param exact logical, if the csd locus is active and exact is \code{TRUE},
#'   create the exactly specified number of viable workers (heterozygous on the
#'   csd locus)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return when \code{x} is \code{\link{Colony-class}} return is a list with two
#'   nodes named \code{workers} (a \code{\link{Pop-class}}) and \code{nHomBrood}
#'   (a numeric); when \code{x} is \code{\link{Colonies-class}} return is a list
#'   of lists named by colony ID
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' apiary <- c(colony1, colony2)
#'
#' (tmp <- createWorkers(colony1, nInd = 10))
#' colony1@queen@id
#' tmp$workers@id
#' tmp$workers@sex
#' tmp$workers@misc
#' tmp$workers@mother
#' tmp$workers@father
#' SP$pedigree
#' SP$recHist
#' SP$recHist[[23]][[1]][1]
#' SP$recHist[[23]][[1]][2]
#' SP$caste
#'
#' createWorkers(apiary, nInd = 10)
#'
#' # Using a default in SP$nWorkers
#' # (just to have some workers - change this to your needs!)
#' createWorkers(colony1)
#' createWorkers(apiary)
#'
#' # Specify own number
#' SP$nWorkers <- 15
#' createWorkers(colony1)
#' createWorkers(apiary)
#' # nWorkers will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' createWorkers(colony1, nInd = nWorkersPoisson)
#' createWorkers(apiary, nInd = nWorkersPoisson)
#' # nWorkers will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nWorkers <- nWorkersPoisson
#' createWorkers(colony1)
#' createWorkers(apiary)
#' # nWorkers will vary between function calls when a function is used
#'
#' # Inbred virgin queen with her brothers to generate csd homozygous brood
#' colony3 <- createColony(createVirginQueens(colony1, nInd = 1))
#' colony3 <- crossColony(colony3,
#'   drones = createDrones(colony1, nInd = 5),
#'   nFathers = 5
#' )
#' # Check the expected csd homozygosity
#' pHomBrood(colony3)
#'
#' # Evaluate a realised csd homozygosity
#' createWorkers(colony3, nInd = 100)
#' # nHomBrood will vary between function calls due to inheritance process
#' createWorkers(colony3, nInd = 100)
#' # nHomBrood will vary between function calls due to inheritance process
#' @export
createWorkers <- function(x, nInd = NULL, exact = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nInd)) {
    nInd <- simParamBee$nWorkers
  }
  # doing "if (is.function(nInd))" below
  if (isColony(x)) {
    if (!isQueenPresent(x)) {
      stop("Missing queen!")
    }
    if (!isQueenMated(x)) {
      stop("Unmated queen!")
    }
    if (is.function(nInd)) {
      nInd <- nInd(colony = x) # see nWorkersPoissonQueenFecundity
    }
    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("workers", "nHomBrood")
    workers <- beeCross(
      queen = getQueen(x), drones = getFathers(x),
      nProgeny = nInd, simParamBee = simParamBee
    )
    if (isCsdActive(simParamBee = simParamBee)) {
      sel <- isCsdHeterozygous(pop = workers, simParamBee = simParamBee)
      ret$workers <- workers[sel]
      ret$nHomBrood <- nInd - sum(sel)
      if (exact) {
        if (nInd(ret$workers) < nInd) {
          nMiss <- nInd - nInd(ret$workers)
          while (0 < nMiss) {
            workers <- beeCross(
              queen = getQueen(x),
              drones = getFathers(x),
              nProgeny = nMiss,
              simParamBee = simParamBee
            )
            sel <- isCsdHeterozygous(pop = workers, simParamBee = simParamBee)
            ret$workers <- c(ret$workers, workers[sel])
            ret$nHomBrood <- ret$nHomBrood + sum(!sel)
            nMiss <- nInd - nInd(ret$workers)
          }
        }
      }
    } else {
      ret$workers <- workers
      ret$nHomBrood <- NA
    }
    ret$workers@sex[] <- "F"
    simParamBee$addToCaste(id = ret$workers@id, caste = "W")
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- createWorkers(x[[colony]],
        nInd = nInd,
        simParamBee = simParamBee
      )
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname beeCross
#' @title Cross a queen and drones
#'
#' @description Level 1 function that crosses a queen and drones. Queen is
#'   diploid, while drones are double haploids so we use AlphaSimR diploid
#'   functionality to make this cross, but since drones are double haploids we
#'   get the desired outcome. This is an utility function, and you most likely
#'   want to use the \code{\link{crossColony}} or \code{\link{crossVirginQueen}}
#'   functions.
#'
#' @param queen \code{\link{Pop-class}}, with a single diploid individual
#' @param drones \code{\link{Pop-class}}, with one or more diploid (double
#'   haploid) individual(s)
#' @param nProgeny integer, number of progeny to create per cross
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Pop-class}} with diploid individuals
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' queen <- basePop[1]
#' drones <- createDrones(x = basePop[2], nInd = 5)
#' workers <- beeCross(queen, drones, nProgeny = 4)
#' workers@id
#' workers@mother
#' workers@father
#' SP$pedigree
#' SP$recHist
#' SP$recHist[[11]][[1]][1]
#' SP$recHist[[11]][[1]][2]
#' @export
beeCross <- function(queen, drones, nProgeny = 1, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (nInd(queen) > 1) {
    stop("At the moment we only cater for crosses with a single queen!")
  }
  ret <- randCross2(
    females = queen, males = drones,
    nCrosses = nProgeny, nProgeny = 1, balance = FALSE,
    simParam = simParamBee
  )
  return(ret)
}

#' @rdname beeCrossHaploDiploid
#' @title Cross a queen and drones
#'
#' @description Level 1 function that crosses a queen and drones. Drones are
#'   haploid, while the queen is diploid, so we first generate gametes (with
#'   recombination) from her and merge them with drone genomes (=gametes), where
#'   we randomly re-sample drones to get the desired number of progeny. This is
#'   an utility function, and you most likely want to use the
#'   \code{\link{crossColony}} function.
#'
#' @param queen \code{\link{Pop-class}}, with a single diploid individual
#' @param drones \code{\link{Pop-class}}, with one or more haploid individual(s)
#' @param nProgeny integer, number of progeny to create per cross
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details This would be the right approach to handle haplo-diploid inheritance
#'   in bees, but it causes a raft of downstream issues, since AlphaSimR assumes
#'   that individuals have the same ploidy. Hence, we don't use this function.
#'
#' @return \code{\link{Pop-class}} with diploid individuals
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' queen <- basePop[1]
#' drones <- reduceGenome(
#'   pop = basePop[2], nProgeny = 5, keepParents = FALSE,
#'   simRecomb = TRUE
#' )
#' workers <- SIMplyBee:::beeCrossHaploDiploid(queen, drones, nProgeny = 4)
#' workers@id
#' workers@mother
#' workers@father
#' SP$pedigree
#' SP$recHist
#' SP$recHist[[11]][[1]][1]
#' SP$recHist[[11]][[1]][2]
beeCrossHaploDiploid <- function(queen, drones, nProgeny = 1, simParamBee = NULL) {
  # An attempt to have drones properly haploid, but have hit AlphaSimR limits
  #   since a lot of the underlying C++ code assumes the same ploidy for all/most
  #   individuals, particularly for IBD tracking. Keeping the function here for
  #   future, but not exporting it.
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (nInd(queen) > 1) {
    stop("At the moment we only cater for crosses with a single queen!")
  }

  # Closely following AlphaSimR:::reduceGenome() and AlphaSimR:::mergeGenome()
  crossPlan <- cbind(
    match(x = queen@id, table = queen@id), # this handles more than one queen!
    match(
      x = sample(x = drones@id, size = nProgeny, replace = TRUE),
      table = drones@id
    )
  )

  # Meiosis on the queen's side
  createReducedGenome <- utils::getFromNamespace(
    x = "createReducedGenome",
    ns = "AlphaSimR"
  )
  queenGametes <- createReducedGenome(
    queen@geno, # this handles more than one queen!
    nProgeny,
    simParamBee$femaleMap,
    simParamBee$v,
    simParamBee$p,
    simParamBee$isTrackRec,
    queen@ploidy,
    simParamBee$femaleCentromere,
    simParamBee$quadProb,
    simParamBee$nThreads
  )
  dim(queenGametes$geno) <- NULL

  # Merge queen's gametes and drones (drones are haploid anyway)
  geno <- vector(mode = "list", length = simParamBee$nChr)
  for (chr in 1:simParamBee$nChr) {
    geno[[chr]] <- array(
      data = as.raw(0),
      dim = c(dim(queen@geno[[chr]])[1], 2, nProgeny)
    )
    for (prog in 1:nProgeny) {
      geno[[chr]][, 1, prog] <- queenGametes$geno[[chr]][, , crossPlan[prog, 1]] # this handles more than one queen!
      geno[[chr]][, 2, prog] <- drones@geno[[chr]][, , crossPlan[prog, 2]]
    }
  }

  rPop <- new(
    Class = "RawPop",
    nInd = as.integer(nProgeny),
    nChr = simParamBee$nChr,
    ploidy = 2L,
    nLoci = queen@nLoci,
    geno = geno
  )

  if (simParamBee$isTrackRec) {
    # Create history for haplotypes
    hist <- vector(mode = "list", length = 2L)
    hist[[1]] <- cbind(1L, 1L) # queen's contribution (will be rewritten below)
    hist[[2]] <- cbind(1L, 1L) # drones' contribution (just one chrom and no recomb)
    hist <- rep(list(hist), times = rPop@nChr) # replicate for chromosomes
    hist <- rep(list(hist), times = rPop@nInd) # replicate for individuals
    # Add queen's gamete recombinations
    for (prog in 1:nProgeny) {
      for (chr in 1:simParamBee$nChr) {
        hist[[prog]][[chr]][[1L]] <- queenGametes$recHist[[prog]][[chr]][[1L]]
      }
    }
  } else {
    hist <- NULL
  }

  return(newPop(
    rawPop = rPop,
    mother = queen@id[crossPlan[, 1]],
    father = drones@id[crossPlan[, 2]],
    simParam = simParamBee,
    iMother = queen@iid[crossPlan[, 1]],
    iFather = drones@iid[crossPlan[, 2]],
    femaleParentPop = queen,
    maleParentPop = drones,
    hist = hist
  ))
}

#' @rdname createDrones
#' @title Creates drones from the colony
#'
#' @description Level 1 function that creates drones from a population, colony,
#'   or colonies. Drones are double haploid and created from the diploid genome
#'   of a queen with recombination. Queen ID is stored as the father and
#'   mother of drones.
#'
#' @param x \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}; with \code{\link{Pop-class}}, its individuals
#'   must be virgin queens or queens (see \code{\link{createVirginQueens}}) -
#'   this case is so that we can initiate simulation by simulating drones from
#'   founding queens before we can create colonies
#' @param nInd numeric or function, number of drones; if \code{NULL} then
#'   \code{\link{SimParamBee}$nDrones} is used; when \code{x} is
#'   \code{\link{Pop-class}} the \code{nInd} is applied to every individual in
#'   the \code{x} (here population individuals serve as a queen, for example
#'   to initiate population from founding queens; see details). If \code{x}
#'   is a \code{\link{Colony-class}} or \code{\link{Colonies-class}}, then the
#'   \code{nInd} means the number of drones per colony.
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details When \code{x} is \code{\link{Pop-class}} this function creates
#'   \code{nInd} drones for each individual in \code{x}, which will amount to
#'   \code{nInd(x) * nInd} drones - this can be slow if either or both of
#'   \code{c(nInd(x), nInd)} is large; tune the numbers to your needs.
#'
#' @return when \code{x} is \code{\link{Pop-class}} or
#'   \code{\link{Colony-class}} return is a
#'   \code{\link{Pop-class}} with drones; when \code{x} is
#'   \code{\link{Colonies-class}} return is a list of \code{\link{Pop-class}}
#'   with drones - list nodes are named by colony ID
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$setTrackRec(isTrackRec = TRUE)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' basePop[1]@id
#' drones@id
#' drones@sex
#' drones@misc
#' drones@mother
#' drones@father
#' SP$pedigree
#' SP$recHist
#' SP$recHist[[13]][[1]][1]
#' SP$recHist[[13]][[1]][2]
#' SP$caste
#'
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' apiary <- c(colony1, colony2)
#'
#' (tmp <- createDrones(colony1, nInd = 10))
#' colony1@queen@id
#' tmp@id
#' tmp@sex
#' tmp@misc
#' tmp@mother
#' tmp@father
#' SP$pedigree
#' SP$recHist
#' SP$recHist[[23]][[1]][1]
#' SP$recHist[[23]][[1]][2]
#'
#' createDrones(apiary, nInd = 10)
#'
#' # Using a default in SP$nDrones
#' # (just to have some drones - change this to your needs!)
#' SP$nDrones
#' createDrones(colony1)
#' createDrones(apiary)
#'
#' # Specify own number
#' SP$nDrones <- 15
#' createDrones(colony1)
#' createDrones(apiary)
#' # nDrones will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' createDrones(colony1, nInd = nDronesPoisson)
#' createDrones(apiary, nInd = nDronesPoisson)
#' # nDrones will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nDrones <- nDronesPoisson
#' createDrones(colony1)
#' createDrones(apiary)
#' # nDrones will vary between function calls when a function is used
#' @export
createDrones <- function(x, nInd = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nInd)) {
    nInd <- simParamBee$nDrones
  }
  # doing "if (is.function(nInd))" below
  if (isPop(x)) {
    if (any(!(isVirginQueen(x) | isQueen(x)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
    if (is.function(nInd)) {
      nInd <- nInd(x = x) # see nDronesPoissonQueenFecundity
    }
    # Haploid version - causes all sorts of issues downstream
    # ret <- reduceGenome(pop = x, nProgeny = nInd, keepParents = FALSE,
    #                     simRecomb = TRUE, simParam = simParamBee)
    # keepParents = FALSE means that the queen will be stored as drones' parent,
    #   instead of storing queen's parents
    # Diploid version - a hack, but it works
    ret <- makeDH(pop = x, nDH = nInd, keepParents = FALSE, simParam = simParamBee)
    ret@sex[] <- "M"
    simParamBee$addToCaste(id = ret@id, caste = "D")
  } else if (isColony(x)) {
    if (!isQueenPresent(x)) {
      stop("Missing queen!")
    }
    if (is.function(nInd)) {
      nInd <- nInd(x = x) # see nDronesPoissonQueenFecundity
    }
    # Haploid version - causes all sorts of issues downstream
    # ret <- reduceGenome(pop = getQueen(x), nProgeny = nInd, keepParents = FALSE,
    #                     simRecomb = TRUE, simParam = simParamBee)
    # keepParents = FALSE means that the queen will be stored as drones' parent,
    #   instead of storing queen's parents
    # Diploid version - a hack, but it works
    ret <- makeDH(
      pop = getQueen(x), nDH = nInd, keepParents = FALSE,
      simParam = simParamBee
    )
    ret@sex[] <- "M"
    simParamBee$addToCaste(id = ret@id, caste = "D")
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- createDrones(
        x = x[[colony]], nInd = nInd,
        simParamBee = simParamBee
      )
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Pop, Colony, or Colonies class object!")
  }
  return(ret)
}

#' @rdname createDCA
#' @title Create a drone congregation area (DCA)
#'
#' @description Level 1 function that creates a population of drones from one or
#'   multiple colonies. Such a population is often referred to as a drone
#'   congregation area (DCA).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param nInd numeric, number of random drones to pull from each colony,
#'   if \code{NULL} all drones in a colony are pulled
#'
#' @return \code{\link{Pop-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#' createDCA(colony1)
#' createDCA(colony2)
#' createDCA(colony2, nInd = 10)@id
#' createDCA(colony2, nInd = 10)@id
#'
#' apiary <- c(colony1, colony2)
#' createDCA(apiary)
#' createDCA(apiary, nInd = 10)
#' @export
createDCA <- function(x, nInd = NULL) {
  if (isColony(x)) {
    DCA <- getDrones(x, nInd = nInd)
  } else if (isColonies(x)) {
    DCA <- getDrones(x, nInd = nInd)
    DCA <- mergePops(popList = DCA)
  } else {
    stop("Argument x must be a Colony of Colonies class object!")
  }
  return(DCA)
}

#' @rdname pullInd
#' @title Pull individuals from a population
#'
#' @description Level 1 function that pulls individuals from a population and
#'   update the population (these individuals don't stay in a population).
#'
#' @param pop \code{\link{Pop-class}}
#' @param nInd numeric, number of individuals to pull, if \code{NULL} pull all
#'   individuals
#' @param use character, all options provided by \code{\link{selectInd}}
#'
#' @return list with a node \code{pulled} holding \code{\link{Pop-class}} of
#'   pulled individuals and a node \code{remainder)} holding \code{\link{Pop-class}}
#'   of remaining individuals
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' pullInd(basePop, nInd = 2)
#' pullInd(basePop, nInd = 3)
#' pullInd(basePop)
#' @export
pullInd <- function(pop, nInd = NULL, use = "rand") {
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  if (is.null(nInd)) {
    nInd <- nInd(pop)
  }
  pulled <- selectInd(pop = pop, nInd = nInd, use = use)
  sel <- pop@id %in% pulled@id
  remainder <- pop[!sel]
  ret <- list(pulled = pulled, remainder = remainder)
  return(ret)
}

#' @rdname pullDroneGroupsFromDCA
#' @title Pulls drone groups from a Drone Congregation Area (DCA)
#'
#' @description Level 1 function that pulls drone groups from a Drone
#'   Congregation Area (DCA) to use them later in mating. Within the function
#'   drones are pulled (removed) from the DCA to reflect the fact that drones
#'   die after mating, so they can't be present in the DCA anymore. Be careful
#'   what you do with the DCA object outside function to avoid drone "copies".
#'
#' @param DCA \code{\link{Pop-class}}, population of drones
#' @param n integer, number of drone groups to be created
#' @param nFathers numeric of function, number of drones that a virgin queen
#'    mates with; if \code{NULL} then \code{\link{SimParamBee}$nFathers} is used
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list of \code{\link{Pop-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' colony1 <- addDrones(colony1, nInd = 10)
#' colony2 <- addDrones(colony2, nInd = 20)
#' apiary <- c(colony1, colony2)
#' DCA <- createDCA(apiary)
#' pullDroneGroupsFromDCA(DCA, n = 4, nFathers = 5)
#' @export
pullDroneGroupsFromDCA <- function(DCA, n, nFathers = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isPop(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  if (any(!isDrone(DCA))) {
    stop("Individuals in DCA must be drones!")
  }
  if (is.null(nFathers)) {
    nFathers <- simParamBee$nFathers
  }
  # doing "if (is.function(nFathers))" below
  ret <- vector(mode = "list", length = n)
  for (group in seq_len(n)) {
    if (is.function(nFathers)) {
      nF <- nFathers() # see nFathersPoisson
    } else {
      nF <- nFathers
    }
    if (nInd(DCA) < nF) {
      stop("We ran out of drones in the DCA!")
    }
    # TODO: We select drones for mating at random, should we use "use"?
    #       https://github.com/HighlanderLab/SIMplyBee/issues/205
    tmp <- pullInd(pop = DCA, nInd = nF)
    ret[[group]] <- tmp$pulled
    DCA <- tmp$remainder
  }
  return(ret)
}

#' @rdname pullCastePop
#' @title Pull individuals from a caste in a colony
#'
#' @description Level 1 function that pulls individuals from a caste in a
#'   colony. These individuals are removed from the colony (compared to
#'   \code{\link{getCaste}}).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{Colonies-class}}
#' @param caste character, "queen", "workers", "drones", or "virginQueens"
#' @param nInd numeric, number of individuals to pull, if \code{NULL} all
#'   individuals are pulled
#' @param use character, all options provided by \code{\link{selectInd}}
#'
#' @seealso \code{\link{pullQueen}}, \code{\link{pullVirginQueens}},
#'   \code{\link{pullWorkers}}, and \code{\link{pullDrones}}
#'
#' @return list of \code{\link{Pop-class}} and \code{\link{Colony-class}}
#'   when \code{x} is \code{\link{Colony-class}} and list of (a list of
#'   \code{\link{Pop-class}} named by colony id) and
#'   \code{\link{Colonies-class}} when \code{x} is
#'   \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' colony1 <- addVirginQueens(colony1, nInd = 10)
#' colony2 <- addVirginQueens(colony2, nInd = 5)
#' colony1 <- addWorkers(colony1, nInd = 10)
#' colony2 <- addWorkers(colony2, nInd = 20)
#' colony1 <- addDrones(colony1, nInd = 5)
#' colony2 <- addDrones(colony2, nInd = 6)
#' pullCastePop(colony1, caste = "queen")
#' pullQueen(colony1)
#'
#' pullCastePop(colony1, caste = "virginQueens")
#' pullCastePop(colony1, caste = "virginQueens", nInd = 2)
#' pullVirginQueens(colony1)
#' pullVirginQueens(colony1, nInd = 2)
#'
#' pullCastePop(colony1, caste = "workers")
#' pullCastePop(colony1, caste = "workers", nInd = 5)
#' pullWorkers(colony1)
#' pullWorkers(colony1, nInd = 5)
#'
#' pullCastePop(colony1, caste = "drones")
#' pullCastePop(colony1, caste = "drones", nInd = 5)
#' pullDrones(colony1)
#' pullDrones(colony1, nInd = 5)
#'
#' apiary <- c(colony1, colony2)
#'
#' pullCastePop(apiary, caste = "queen")
#' pullQueen(apiary)
#' nQueens(apiary)
#' nQueens(pullQueen(apiary)$colonies)
#'
#' pullCastePop(apiary, caste = "virginQueens")
#' pullVirginQueens(apiary)
#' nVirginQueens(apiary)
#' nVirginQueens(pullVirginQueens(apiary)$colonies)
#' nVirginQueens(pullVirginQueens(apiary, nInd = 5)$colonies)
#'
#' pullCastePop(apiary, caste = "workers")
#' pullWorkers(apiary)
#' nWorkers(apiary)
#' nWorkers(pullWorkers(apiary)$colonies)
#' nWorkers(pullWorkers(apiary, nInd = 5)$colonies)
#'
#' pullCastePop(apiary, caste = "drones")
#' pullDrones(apiary)
#' nDrones(apiary)
#' nDrones(pullDrones(apiary)$colonies)
#' nDrones(pullDrones(apiary, nInd = 5)$colonies)
#' @export
pullCastePop <- function(x, caste, nInd = NULL, use = "rand") {
  if (isColony(x)) {
    if (is.null(slot(x, caste))) {
      ret <- list(pulled = NULL, colony = x)
    } else {
      if (is.null(nInd)) {
        nInd <- nInd(slot(x, caste))
      }
      tmp <- pullInd(pop = slot(x, caste), nInd = nInd, use = use)
      slot(x, caste) <- tmp$remainder
      if (caste == "drones") {
        test <- isDrone(tmp$pulled)
        if (any(!test)) {
          tmp$pulled <- tmp$pulled[test]
          warning("Taking only drones that have not yet mated!")
        }
      }
      ret <- list(pulled = tmp$pulled, colony = x)
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("pulled", "colonies")
    ret$pulled <- vector(mode = "list", length = nCol)
    names(ret$pulled) <- getId(x)
    ret$colonies <- x
    for (colony in seq_len(nCol)) {
      tmp <- pullCastePop(x = x[[colony]], caste = caste, nInd = nInd, use = use)
      ret$pulled[[colony]] <- tmp$pulled
      ret$colonies[[colony]] <- tmp$colony
    }
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn pullCastePop Pull queen from a colony
#' @export
pullQueen <- function(x) {
  if (isColony(x) | isColonies(x)) {
    ret <- pullCastePop(x, caste = "queen")
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn pullCastePop Pull virgin queens from a colony
#' @export
pullVirginQueens <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- pullCastePop(x, caste = "virginQueens", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn pullCastePop Pull workers from a colony
#' @export
pullWorkers <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- pullCastePop(x, caste = "workers", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @describeIn pullCastePop Pull drones from a colony
#' @export
pullDrones <- function(x, nInd = NULL, use = "rand") {
  if (isColony(x) | isColonies(x)) {
    ret <- pullCastePop(x, caste = "drones", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Colony or Colonies class object!")
  }
  return(ret)
}

#' @rdname crossVirginQueen
#' @title Cross (mate) a virgin queen to a group drones
#'
#' @description Level 1 function that crosses (mates) a virgin queen to a group
#'   of drones. This function does not create any progeny, it only stores the
#'   mated drones (fathers) so we can later create progeny as needed.
#'
#' @param pop \code{\link{Pop-class}}, one or more virgin queens to be mated;
#'   \code{\link{isVirginQueen}} test will be run on these individuals
#' @param drones \code{\link{Pop-class}}, a group of drones that could be mated
#'   with virgin queen(s); if there is more than one virgin queen, then the
#'   \code{fathers} are partitioned into multiple groups of \code{nFathers} size
#'    using \code{\link{pullDroneGroupsFromDCA}}
#' @param nFathers numeric of function, number of drones that a virgin queen
#'    mates with; if \code{NULL} then \code{\link{SimParamBee}$nFathers} is used
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details This function changes caste for the mated drones to fathers, and
#'   mated virgin queens to queens. See examples. This means that you can not
#'   use these individuals in matings anymore!
#'
#' @seealso \code{\link{Colony-class}} on how we store the fathers along the
#'   queen.
#'
#' @return \code{\link{Pop-class}} with mated queen(s). The misc slot of the
#'   queens contains additional information about the number of workers, drones,
#'   and homozygous brood produced, and the expected percentage of csd homozygous
#'   brood.
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 20)
#'
#' virginQueen1 <- basePop[2]
#' (matedQueen1 <- crossVirginQueen(
#'   pop = virginQueen1,
#'   drones = drones[1:5],
#'   nFathers = 5
#' ))
#' isQueenMated(virginQueen1)
#' isQueenMated(matedQueen1)
#' nFathers(matedQueen1)
#' getFathers(matedQueen1)@id
#'
#' isDrone(drones[1:5])
#' isFather(drones[1:5])
#' isVirginQueen(matedQueen1)
#' isQueen(matedQueen1)
#'
#' virginQueen2 <- basePop[3]
#' (matedQueen2 <- crossVirginQueen(
#'   pop = virginQueen2,
#'   drones = drones[6:10],
#'   nFathers = 5
#' ))
#' isQueenMated(virginQueen2)
#' isQueenMated(matedQueen2)
#' nFathers(matedQueen2)
#' getFathers(matedQueen2)@id
#'
#' matedQueens <- crossVirginQueen(
#'   pop = c(basePop[4], basePop[5]),
#'   drones = drones[11:15], nFathers = 2
#' )
#' matedQueens
#' isQueenMated(matedQueens)
#' nFathers(matedQueens)
#' getFathers(matedQueens)
#'
#' # Inbred mated queen (mated with her own sons)
#' matedQueen3 <- crossVirginQueen(
#'   pop = basePop[1],
#'   drones = drones[16:20],
#'   nFathers = 5
#' )
#' # Check the expected csd homozygosity
#' pHomBrood(matedQueen3)
#' @export
crossVirginQueen <- function(pop, drones, nFathers = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  if (any(!isVirginQueen(pop))) {
    stop("Individuals in pop must be virgin queens!")
  }
  if (!isPop(drones)) {
    stop("Argument fathers must be a Pop class object!")
  }
  if (any(!isDrone(drones))) {
    stop("Argument drones must hold only drones, no fathers!")
  }
  if (is.null(nFathers)) {
    nFathers <- simParamBee$nFathers
  }
  # skipping "if (is.function(nFathers))" since we use it below or pass it to
  #   pullDroneGroupsFromDCA
  nVirginQueen <- nInd(pop)
  simParamBee$changeCaste(id = pop@id, caste = "Q")
  if (nVirginQueen == 1) {
    if (is.function(nFathers)) {
      n <- nFathers() # see nFathersPoisson
    } else {
      n <- nFathers
    }
    # TODO: In crossVirginQueens we select drones for mating at random, should
    #       we use "use"?
    #       https://github.com/HighlanderLab/SIMplyBee/issues/205
    fathers <- selectInd(pop = drones, nInd = n, use = "rand")
    simParamBee$changeCaste(id = fathers@id, caste = "F")
    pop@misc[[1]]$fathers <- fathers
  } else {
    fatherGroups <- pullDroneGroupsFromDCA(
      DCA = drones,
      n = nVirginQueen,
      nFathers = nFathers
    )
    for (queen in seq_len(nVirginQueen)) {
      simParamBee$changeCaste(id = fatherGroups[[queen]]@id, caste = "F")
      pop@misc[[queen]]$fathers <- fatherGroups[[queen]]
    }
  }

  pop <- setMisc(x = pop, node = "nWorkers", value = 0)
  pop <- setMisc(x = pop, node = "nDrones", value = 0)
  pop <- setMisc(x = pop, node = "nHomBrood", value = 0)
  if (isCsdActive(simParamBee = simParamBee)) {
    val <- calcQueensPHomBrood(x = pop)
  } else {
    val <- NA
  }
  pop <- setMisc(x = pop, node = "pHomBrood", value = val)
  return(pop)
}

#' @rdname setQueensYearOfBirth
#' @title Set the queen's year of birth
#'
#' @description Level 1 function that sets the queen's year of birth.
#'
#' @param x \code{\link{Pop-class}} (one or more than one queen),
#'   \code{\link{Colony-class}} (one colony), or
#'   \code{\link{Colonies-class}} (more colonies)
#' @param year integer, the year of the birth of the queen
#'
#' @return \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}} with queens having the year of birth set
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
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
setQueensYearOfBirth <- function(x, year) {
  if (isPop(x)) {
    if (any(!(isVirginQueen(x) | isQueen(x)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
    nInd <- nInd(x)
    x <- setMisc(x = x, node = "yearOfBirth", value = year)
  } else if (isColony(x)) {
    if (isQueenPresent(x)) {
      x@queen <- setMisc(x = x@queen, node = "yearOfBirth", value = year)
    } else {
      stop("Missing queen!") # TODO: should this be a warning?: https://github.com/HighlanderLab/SIMplyBee/issues/159
    }
  } else if (isColonies(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]]@queen <- setMisc(
        x = x[[colony]]@queen, node = "yearOfBirth",
        value = year
      )
    }
  } else {
    stop("Argument x must be a Pop, Colony or Colonies class object!")
  }
  return(x)
}

#' @describeIn setQueensYearOfBirth Set the queen's year of birth
#' @export
setQueensYOB <- setQueensYearOfBirth

#' @rdname setMisc
#' @title Set miscelaneous information in a population
#'
#' @description Level 1 function that sets the queen's year of birth.
#'
#' @param x \code{\link{Pop-class}}
#' @param node character, name of the node to set within the \code{x@misc} slot
#' @param value, value to be saved into \code{x@misc[[*]][[slot]]}; length of
#'   \code{value} should be equal to \code{nInd(x)}; if its length is 1, then
#'   it is repeated using \code{rep}
#'
#' @return \code{\link{Pop-class}} with \code{x@misc[[*]][[slot]]} set
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#' basePop@misc
#'
#' basePop <- setMisc(basePop, node = "info", value = c("A", "B", "C"))
#' basePop@misc
#'
#' basePop <- setMisc(basePop, node = "info", value = c("B", "C", "A"))
#' basePop@misc
#'
#' basePop <- setMisc(basePop, node = "info2", value = "A")
#' basePop@misc
#' @export
# TODO: move to AlphaSimR - track
#       https://github.com/gaynorr/AlphaSimR/pull/51
#       https://github.com/HighlanderLab/SIMplyBee/issues/144
setMisc <- function(x, node, value) {
  if (isPop(x)) {
    n <- nInd(x)
    if (length(value) == 1 && n > 1) {
      value <- rep(x = value, times = n)
    }
    for (ind in seq_len(n)) {
      x@misc[[ind]][[node]] <- value[ind]
    }
  } else {
    stop("Argument x must be a Pop class object!")
  }
  return(x)
}

#' @rdname getMisc
#' @title Get miscelaneous information in a population
#'
#' @description Get miscelaneous information in a population
#'
#' @param x \code{\link{Pop-class}}
#' @param node character, name of the node to get from the \code{x@misc} slot;
#'   if \code{NULL} the whole \code{x@misc} slot is returned
#'
#' @return The \code{x@misc} slot or its nodes \code{x@misc[[*]][[node]]}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParam$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' basePop <- setMisc(basePop, node = "info", value = 1)
#' basePop@misc
#' getMisc(x = basePop, node = "info")
#'
#' basePop <- setMisc(basePop, node = "info2", value = c("A", "B", "C"))
#' basePop@misc
#' getMisc(x = basePop, node = "info2")
#'
#' n <- nInd(basePop)
#' location <- vector(mode = "list", length = n)
#' for (ind in seq_len(n)) {
#'   location[[ind]] <- runif(n = 2, min = 0, max = 100)
#' }
#' location
#' basePop <- setMisc(basePop, node = "location", value = location)
#' basePop@misc
#' getMisc(x = basePop, node = "location")
#'
#' n <- nInd(basePop)
#' location <- vector(mode = "list", length = n)
#' for (ind in c(1, 3)) {
#'   location[[ind]] <- runif(n = 2, min = 0, max = 100)
#' }
#' location
#' basePop <- setMisc(basePop, node = "location", value = location)
#' basePop@misc
#' getMisc(x = basePop, node = "location")
#'
#' getMisc(x = basePop)
#' @export
# TODO: move to AlphaSimR - track
#       https://github.com/gaynorr/AlphaSimR/pull/51
#       https://github.com/HighlanderLab/SIMplyBee/issues/144
getMisc <- function(x, node = NULL) {
  if (isPop(x)) {
    if (is.null(node)) {
      ret <- x@misc
    } else {
      nInd <- nInd(x)
      ret <- vector(mode = "list", length = nInd)
      for (ind in seq_len(nInd)) {
        if (!is.null(x@misc[[ind]][[node]])) {
          ret[ind] <- x@misc[[ind]][node]
        }
      }
    }
  } else {
    stop("Argument x must be a Pop class object!")
  }
  return(ret)
}
