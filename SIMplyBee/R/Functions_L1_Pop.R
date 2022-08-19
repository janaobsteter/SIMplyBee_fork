# Level 1 Pop Functions

#' @rdname getCastePop
#' @title Access individuals of a caste
#'
#' @description Level 1 function that returns individuals of a caste. These
#'   individuals stay in the colony (compared to \code{\link{pullCastePop}}).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}},
#'   exceptionally \code{\link{Pop-class}} for calling \code{getFathers}
#'   on a queen population
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed; if there are less individuals than requested,
#'   we return the ones available - this can return \code{NULL}
#' @param use character, all options provided by \code{\link{selectInd}} and
#'   \code{"order"} that selects \code{1:nInd} individuals (meaning it always
#'   returns at least one individual, even if \code{nInd = 0})
#' @param removeFathers logical, removes \code{drones} that have already mated;
#'   set to \code{FALSE} if you would like to get drones for mating with multiple
#'   virgin queens, say via insemination
#'
#' @seealso \code{\link{getQueen}}, \code{\link{getFathers}},
#'   \code{\link{getVirginQueens}}, \code{\link{getWorkers}}, and
#'   \code{\link{getDrones}}
#'
#' @return when \code{x} is \code{\link{Colony-class}} return is
#'   \code{\link{Pop-class}} for \code{caste != "all"} or list for \code{caste
#'   == "all"} with nodes named by caste; when \code{x} is
#'   \code{\link{MultiColony-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"} or named list of lists of
#'   \code{\link{Pop-class}} for \code{caste == "all"}. You can merge
#'   all the populations in the list with \code{\link{mergePops}} function.
#'
#' @seealso \code{\link{getCasteId}} and \code{\link{getCaste}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])
#'
#' # Using defaults in SP$nWorkers & SP$nDrones
#' colony <- buildUp(x = colony, exact = TRUE)
#' apiary <- buildUp(x = apiary, exact = TRUE)
#'
#' # Using a default in SP$nVirginQueens
#' colony <- addVirginQueens(x = colony)
#' apiary <- addVirginQueens(x = apiary)
#' colony
#' apiary
#'
#' # Comparison of getCastePop() and getQueen()
#' getCastePop(colony, caste = "queen")
#' getQueen(colony)
#'
#' # Comparison of getCastePop() and getFathers()
#' getCastePop(colony, caste = "fathers")
#' getCastePop(colony, caste = "fathers")@id
#' getCastePop(colony, caste = "fathers", nInd = 2)@id
#'
#' getFathers(colony)
#' getFathers(colony)@id
#' getFathers(colony, nInd = 2)@id
#'
#' getFathers(getQueen(colony))
#'
#' # Comparison of getCastePop() and getVirginQueens()
#' getCastePop(colony, caste = "virginQueens")
#' getCastePop(colony, caste = "virginQueens")@id
#' getCastePop(colony, caste = "virginQueens", nInd = 2)@id
#'
#' getVirginQueens(colony)
#' getVirginQueens(colony)@id
#' getVirginQueens(colony, nInd = 2)@id
#'
#' # Comparison of getCastePop() and getWorkers()
#' getCastePop(colony, caste = "workers")
#' getCastePop(colony, caste = "workers")@id
#' getCastePop(colony, caste = "workers", nInd = 2)@id
#'
#' getWorkers(colony)
#' getWorkers(colony)@id
#' getWorkers(colony, nInd = 2)@id
#'
#' # Comparison of getCastePop() and getDrones()
#' getCastePop(colony, caste = "drones")
#' getCastePop(colony, caste = "drones")@id
#' getCastePop(colony, caste = "drones", nInd = 2)@id
#'
#' getDrones(colony)
#' getDrones(colony)@id
#' getDrones(colony, nInd = 2)@id
#'
#' # Comparisons on MultiColony class
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
#' mergePops(getVirginQueens(apiary))
#'
#' getCastePop(apiary, caste = "workers")
#' getWorkers(apiary)
#' getWorkers(apiary)[[1]]@id
#' getWorkers(apiary)[[2]]@id
#' getWorkers(apiary, nInd = 2)
#' mergePops(getWorkers(apiary))
#'
#' getCastePop(apiary, caste = "drones")
#' getDrones(apiary)
#' getDrones(apiary)[[1]]@id
#' getDrones(apiary)[[2]]
#' getDrones(apiary, nInd = 2)
#' mergePops(getDrones(apiary))
#'
#' getCastePop(apiary)
#' getCastePop(apiary, caste = "queen")
#' getCastePop(apiary, caste = "drones")
#' @export
getCastePop <- function(x, caste = "all", nInd = NULL, use = "order",
                        removeFathers = TRUE) {
  if (length(caste) > 1) {
    stop("Argument caste can be only of length 1!")
  }
  if (isColony(x)) {
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCastePop(x = x, caste = caste, nInd = nInd, use = use, removeFathers = removeFathers)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
    } else {
      if (caste == "fathers") {
        if (isQueenPresent(x)) {
          pop <- x@queen@misc[[1]]$fathers
        } else {
          pop <- NULL
        }
      } else {
        pop <- slot(x, caste)
      }
      if (is.null(pop)) {
        ret <- NULL
      } else {
        if (caste == "drones" && removeFathers) {
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
          warning("You are requesting more individuals than available.")
          nIndRequested <- nIndAvailable
        }
        if (nIndAvailable == 0) {
          start <- 0
        } else {
          start <- 1
        }
        if (use == "order") {
          ret <- pop[start:nIndRequested]
        } else {
          ret <- selectInd(pop = pop, nInd = nIndRequested, use = use)
        }
      }
    }
  } else if (isMultiColony(x)) {
    fun <- ifelse(caste == "all", lapply, sapply)
    ret <- fun(X = x@colonies, FUN = getCastePop, caste = caste, nInd = nInd, use = use)
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCastePop Access the queen
#' @export
getQueen <- function(x) {
  ret <- getCastePop(x, caste = "queen", nInd = 1)
  return(ret)
}

#' @describeIn getCastePop Access fathers (drones the queen mated with)
#' @export
getFathers <- function(x, nInd = NULL, use = "rand") {
  if (isPop(x)) { # DO WE WANT TO PUT THIS IN getCastePop???
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
  } else if (isColony(x) | isMultiColony(x)) {
    ret <- getCastePop(x, caste = "fathers", nInd = nInd, use = use)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCastePop Access workers
#' @export
getWorkers <- function(x, nInd = NULL, use = "rand") {
  ret <- getCastePop(x, caste = "workers", nInd = nInd, use = use)
  return(ret)
}

#' @describeIn getCastePop Access drones
#' @export
getDrones <- function(x, nInd = NULL, use = "rand", removeFathers = TRUE) {
  ret <- getCastePop(x,
                     caste = "drones", nInd = nInd, use = use,
                     removeFathers = removeFathers
  )
  return(ret)
}

#' @describeIn getCastePop Access virgin queens
#' @export
getVirginQueens <- function(x, nInd = NULL, use = "rand") {
  ret <- getCastePop(x, caste = "virginQueens", nInd = nInd, use = use)
  return(ret)
}

#' @rdname createCastePop
#' @title Creates caste population individuals from the colony
#'
#' @description Level 1 function that creates the specified number of caste
#'   individuals from the colony with a mated queens. If csd
#'   locus is active, it takes it into account and any csd homozygotes are
#'   removed and counted towards homozygous brood.
#'
#' @param x \code{link{MapPop-class}} (only if caste is "virginQueens"), or
#'   \code{Pop} (only if caste is "drones") or \code{\link{Colony-class}}
#'   or \code{\link{MultiColony-class}}
#' @param caste character, "workers", "drones", or "virginQueens"
#' @param nInd numeric or function, number of caste individuals; if \code{NULL} then
#'   \code{\link{SimParamBee}$nWorkers},  \code{\link{SimParamBee}$nDrones}
#'   or \code{\link{SimParamBee}$nVirginQueens} is used depending on the caste;
#'   only used when \code{x} is \code{\link{Colony-class}} or
#'   \code{\link{MultiColony-class}}, when \code{x} is \code{link{MapPop-class}}
#'   all individuals in \code{x} are converted into virgin queens
#' @param exact logical, only relevant when creating workers,
#'   if the csd locus is active and exact is \code{TRUE},
#'   create the exactly specified number of viable workers (heterozygous on the
#'   csd locus)
#' @param year numeric, year of birth for virgin queens
#' @param editCsd logical (only active when \code{x} is \code{link{MapPop-class}}),
#'   whether the csd locus should be edited to ensure heterozygosity at the csd
#'   locus (to get viable virgin queens); see \code{csdAlleles}
#' @param csdAlleles \code{NULL} or list (only active when \code{x} is \code{link{MapPop-class}});
#'   If \code{NULL}, then the function samples a heterozygous csd genotype for
#'   each virgin queen from all possible csd alleles.
#'   If not \code{NULL}, the user provides a list of length \code{nInd} with each
#'   node holding a matrix or a data.frame, each having two rows and n columns.
#'   Each row must hold one csd haplotype (allele) that will be assigned to a
#'   virgin queen. The n columns span the length of the csd locus as specified
#'   in \code{\link{SimParamBee}}. The two csd alleles must be different to
#'   ensure heterozygosity at the csd locus.
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return when \code{x} is \code{link{MapPop-class}} returns
#'   \code{virginQueens} (a \code{\link{Pop-class}});
#'   when \code{x} is \code{\link{Colony-class}} returns
#'   \code{virginQueens} (a \code{\link{Pop-class}});
#'   when \code{x} is \code{\link{MultiColony-class}}
#'   return is a named list of \code{virginQueens} (a \code{\link{Pop-class}});
#'   named by colony ID
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' # Create virgin queens on a MapPop
#' basePop <- createCastePop(founderGenomes, caste = "virginQueens")
#' # Or alias function: createVirginQueens(founderGenomes)
#'
#' #Create drones on a Pop
#' drones <- createCastePop(x = basePop[1], caste = "drones", nInd = 1000)
#' # Or alias: createDrones(x = basePop[1], nInd = 100)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])
#'
#' # Using default nInd in SP
#' colony@virginQueens <- createVirginQueens(colony)
#' colony@workers <- createWorkers(colony)$workers
#' colony@drones <- createDrones(colony)
#'
#' # These populations hold individual information
#' # Example on the virgin queens
#' virginQueens <- colony@virginQueens
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
#' # Specify own number
#' SP$nVirginQueens <- 15
#' SP$nWorkers <- 100
#' SP$nDrones <- 10
#' createVirginQueens(colony)
#' createVirginQueens(apiary)
#' createWorkers(colony)
#' createWorkers(apiary)
#' createDrones(colony)
#' createDrones(apiary)
#' # nVirginQueens will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' createVirginQueens(colony, nInd = nVirginQueensPoisson)
#' createVirginQueens(apiary, nInd = nVirginQueensPoisson)
#' createWorkers(colony, nInd = nWorkersPoisson)
#' createWorkers(apiary, nInd = nWorkersPoisson)
#' createDrones(colony, nInd = nDronesPoisson)
#' createDrones(apiary, nInd = nDronesPoisson)
#' # No. of individuals will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nVirginQueens <- nVirginQueensPoisson
#' SP$nWorkers <- nWorkersPoisson
#' SP$nDrones <- nDronesPoisson
#' createVirginQueens(colony)
#' createVirginQueens(apiary)
#' createWorkers(colony)
#' createWorkers(apiary)
#' createDrones(colony)
#' createDrones(apiary)
#' # No. of individuals will vary between function calls when a function is used
#'
#' # csd homozygosity - relevant when creating virgin queens
#' founderGenomes <- quickHaplo(nInd = 100, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
#' basePop <- createVirginQueens(founderGenomes, editCsd = FALSE)
#' nrow(getCsdAlleles(basePop, unique = TRUE))
#' all(isCsdHeterozygous(basePop))
#'
#' basePop <- createVirginQueens(founderGenomes, editCsd = TRUE)
#' nrow(getCsdAlleles(basePop, unique = TRUE))
#' all(isCsdHeterozygous(basePop))
#' @export
# TODO: explore options for implementing difference between workers' and queens'
#       patrilines
#       https://github.com/HighlanderLab/SIMplyBee/issues/78
createCastePop <- function(x, caste = NULL, nInd = NULL,
                           exact = TRUE, year = NULL,
                           editCsd = TRUE, csdAlleles = NULL,
                           simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nInd)) {
    if (caste == "virginQueens") {
      nInd <- simParamBee$nVirginQueens
    } else if (caste == "workers") {
      nInd <- simParamBee$nWorkers
    } else if (caste == "drones") {
      nInd <- simParamBee$nDrones
    }
  }
  if (is.function(nInd)) {
    nInd <- nInd(x)
  }
  # doing "if (is.function(nInd))" below
  if (isMapPop(x)) {
    if (caste != "virginQueens") { # Creating virgin queens if input  is a MapPop
      stop("MapPop-class can only be used to create virgin queens!")
    }
    ret <- newPop(x)
    if (!is.null(simParamBee$csdChr)) {
      if (editCsd) {
        ret <- editCsdLocus(ret, alleles = csdAlleles, simParamBee = simParamBee)
      }
    }
    ret@sex[] <- "F"
    simParamBee$changeCaste(id = ret@id, caste = "V")
    if (!is.null(year)) {
      ret <- setQueensYearOfBirth(x = ret, year = year)
    }
  } else if (isPop(x)) {
    if (caste != "drones") { #Creating drones if input is a Pop
      stop("Pop-class can only be used to create drones!")
    }
    if (any(!(isVirginQueen(x) | isQueen(x)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
    # Diploid version - a hack, but it works
    ret <- makeDH(pop = x, nDH = nInd, keepParents = FALSE, simParam = simParamBee)
    ret@sex[] <- "M"
    simParamBee$addToCaste(id = ret@id, caste = "D")
  } else if (isColony(x)) {
    if (!isQueenPresent(x)) {
      stop("Missing queen!")
    }
    if (caste == "workers") {
      ret <- vector(mode = "list", length = 2)
      names(ret) <- c("workers", "nHomBrood")
      workers <- combineBeeGametes(
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
              workers <- combineBeeGametes(
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
    } else if (caste == "virginQueens") { # Creating virgin queens if input is a Colony
      ret <- createCastePop(x = x, caste = "workers",
                            nInd = nInd, exact = TRUE, simParamBee = simParamBee)$workers
      ret@sex[] <- "F"
      simParamBee$changeCaste(id = ret@id, caste = "V")
      if (!is.null(year)) {
        ret <- setQueensYearOfBirth(x = ret, year = year)
      }
    } else if (caste == "drones") { # Creating drones if input is a Colony
      ret <- makeDH(
        pop = getQueen(x), nDH = nInd, keepParents = FALSE,
        simParam = simParamBee
      )
      ret@sex[] <- "M"
      simParamBee$addToCaste(id = ret@id, caste = "D")
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      ret[[colony]] <- createCastePop(
        x = x[[colony]], caste = caste,
        nInd = nInd, exact = exact,
        year = year,
        editCsd = TRUE, csdAlleles = NULL,
        simParamBee = simParamBee
      )
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Map-Pop (only for virgin queens),
         Pop (only for drones), Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn createCastePop Create workers from a colony
#' @export
createWorkers <- function(x, nInd = NULL, exact = FALSE, simParamBee = NULL) {
  ret <- createCastePop(x, caste = "workers", nInd = nInd,
                        exact = exact, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn createCastePop Create drones from a colony
#' @export
createDrones <- function(x, nInd = NULL, simParamBee = NULL) {
  ret <- createCastePop(x, caste = "drones", nInd = nInd,
                        simParamBee = simParamBee)
  return(ret)
}

#' @describeIn createCastePop Create virgin queens from a colony
#' @export
createVirginQueens <- function(x, nInd = NULL,
                               year = NULL,
                               editCsd = TRUE, csdAlleles = NULL,
                               simParamBee = NULL) {
  ret <- createCastePop(x, caste = "virginQueens", nInd = nInd,
                        year = year, editCsd = editCsd,
                        csdAlleles = csdAlleles, simParamBee = simParamBee)
  return(ret)
}

#' @rdname combineBeeGametes
#' @title Create diploid gametes from a mated queen
#'
#' @description Level 1 function that produces diploid offspring from a mated queen. Queen is
#'   diploid, while drones are double haploids so we use AlphaSimR diploid
#'   functionality to make this cross, but since drones are double haploids we
#'   get the desired outcome. This is an utility function, and you most likely
#'   want to use the \code{\link{cross}}
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
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$addTraitA(10)
#' SP$addSnpChip(5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' queen <- basePop[1]
#' drones <- createDrones(x = basePop[2], nInd = 5)
#' workers <- SIMplyBee:::combineBeeGametes(queen, drones, nProgeny = 4)
#' workers@id
#' workers@mother
#' workers@father
#' SP$pedigree
#' SP$recHist
#' SP$recHist[[11]][[1]][1]
#' SP$recHist[[11]][[1]][2]
combineBeeGametes <- function(queen, drones, nProgeny = 1, simParamBee = NULL) {
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

#' @rdname combineBeeGametesHaploDiploid
#' @title Create diploid gametes from a mated queen
#'
#' @description Level 1 function that produces diploid offspring from a mated queen. Drones are
#'   haploid, while the queen is diploid, so we first generate gametes (with
#'   recombination) from her and merge them with drone genomes (=gametes), where
#'   we randomly re-sample drones to get the desired number of progeny. This is
#'   an utility function, and you most likely want to use the
#'   \code{\link{cross}} function.
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
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$addTraitA(10)
#' SP$addSnpChip(5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' queen <- basePop[1]
#' drones <- reduceGenome(
#'   pop = basePop[2], nProgeny = 5, keepParents = FALSE,
#'   simRecomb = TRUE
#' )
#' workers <- SIMplyBee:::combineBeeGametesHaploDiploid(queen, drones, nProgeny = 4)
#' workers@id
#' workers@mother
#' workers@father
#' SP$pedigree
#' SP$recHist
#' SP$recHist[[11]][[1]][1]
#' SP$recHist[[11]][[1]][2]
combineBeeGametesHaploDiploid <- function(queen, drones, nProgeny = 1, simParamBee = NULL) {
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

#' @rdname createDCA
#' @title Create a drone congregation area (DCA)
#'
#' @description Level 1 function that creates a population of drones from a colony
#'   or multi-colony.  Such a population is often referred to as a drone
#'   congregation area (DCA).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param nInd numeric, number of random drones to pull from each colony,
#'   if \code{NULL} all drones in a colony are pulled
#' @param removeFathers logical, removes \code{drones} that have already mated;
#'   set to \code{FALSE} if you would like to get drones for mating with multiple
#'   virgin queens, say via insemination
#'
#' @details In reality, drones leave the colony to mate. They die after that.
#'   In this function we only get a copy of drones from \code{x}, for
#'   computational efficiency and ease of use. However, any mating will change
#'   the caste of drones to fathers, and they won't be available for future
#'   matings (see \code{\link{cross}}). Not unless
#'   \code{removeFathers = FALSE}.
#'
#' @return \code{\link{Pop-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$addTraitA(10)
#' SP$addSnpChip(5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])
#'
#' colony <- addDrones(colony, nInd = 10)
#' createDCA(colony)
#' createDCA(colony, nInd = 10)@id
#'
#' apiary <- addDrones(apiary)
#' createDCA(apiary)
#' createDCA(apiary, nInd = 10)
#' @export
createDCA <- function(x, nInd = NULL, removeFathers = TRUE) {
  if (isColony(x)) {
    DCA <- getDrones(x, nInd = nInd, removeFathers = removeFathers)
    if (is.null(DCA)) {
      warning("No available drones!")
    }
  } else if (isMultiColony(x)) {
    DCA <- getDrones(x, nInd = nInd, removeFathers = removeFathers)
    DCA <- DCA[sapply(DCA, FUN = function(z) !is.null(z))]
    if (length(DCA) != 0) {
      DCA <- mergePops(popList = DCA)
    } else {
      warning("No available drones!")
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
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
#'   pulled individuals and a node \code{remnant)} holding \code{\link{Pop-class}}
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
  remnant <- pop[!sel]
  ret <- list(pulled = pulled, remnant = remnant)
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
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' colony <- addDrones(colony, nInd = 100)
#'
#' # Create colony DCA
#' DCA <- createDCA(colony)
#' pullDroneGroupsFromDCA(DCA, n = 4, nFathers = 5)
#' pullDroneGroupsFromDCA(DCA, n = 5, nFathers = nFathersPoisson)
#'
#' @export
pullDroneGroupsFromDCA <- function(DCA, n, nFathers = NULL,
                                   simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isPop(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  # Keep only the drones (remove the fathers)
  DCA <- DCA[isDrone(DCA)]
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
    DCA <- tmp$remnant
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
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "workers", "drones", or "virginQueens"
#' @param nInd numeric, number of individuals to pull, if \code{NULL} all
#'   individuals are pulled
#' @param use character, all options provided by \code{\link{selectInd}}
#' @param removeFathers logical, removes \code{drones} that have already mated;
#'   set to \code{FALSE} if you would like to get drones for mating with multiple
#'   virgin queens, say via insemination
#'
#' @seealso \code{\link{pullQueen}}, \code{\link{pullVirginQueens}},
#'   \code{\link{pullWorkers}}, and \code{\link{pullDrones}}
#'
#' @return list of \code{\link{Pop-class}} and \code{\link{Colony-class}}
#'   when \code{x} is \code{\link{Colony-class}} and list of (a list of
#'   \code{\link{Pop-class}} named by colony id) and
#'   \code{\link{MultiColony-class}} when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$addTraitA(10)
#' SP$addSnpChip(5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = fatherGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 100, nDrones = 10, exact = TRUE)
#' colony <- addVirginQueens(x = colony, nInd = 3)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10, exact = TRUE)
#' apiary <- addVirginQueens(x = apiary, nInd = 3)
#'
#' # pullCastePop on Colony class
#' # We can't pull the queen and leave the colony queenless
#' pullCastePop(colony, caste = "virginQueens")
#' pullCastePop(colony, caste = "virginQueens", nInd = 2)
#' pullVirginQueens(colony)
#' pullVirginQueens(colony, nInd = 2)
#'
#' pullCastePop(colony, caste = "workers")
#' pullCastePop(colony, caste = "workers", nInd = 5)
#' pullWorkers(colony)
#' pullWorkers(colony, nInd = 5)
#'
#' pullCastePop(colony, caste = "drones")
#' pullCastePop(colony, caste = "drones", nInd = 5)
#' pullDrones(colony)
#' pullDrones(colony, nInd = 5)
#'
#' # pullCastePop on MultiColony class
#' pullCastePop(apiary, caste = "queen")
#' pullQueen(apiary)
#' nQueens(apiary)
#' nQueens(pullQueen(apiary)$remnant)
#'
#' pullCastePop(apiary, caste = "virginQueens")
#' pullVirginQueens(apiary)
#' nVirginQueens(apiary)
#'
#' pullCastePop(apiary, caste = "workers")
#' pullWorkers(apiary)
#' nWorkers(apiary)
#' nWorkers(pullWorkers(apiary)$remnant)
#'
#' pullCastePop(apiary, caste = "drones")
#' pullDrones(apiary)
#' nDrones(apiary)
#' nDrones(pullDrones(apiary)$remnant)
#' @export
pullCastePop <- function(x, caste, nInd = NULL, use = "rand",
                         removeFathers = TRUE) {
  if (length(caste) > 1) {
    stop("Argument caste can be only of length 1!")
  }
  if (isColony(x)) {
    if (is.null(slot(x, caste))) {
      ret <- list(pulled = NULL, remnant = x)
    } else {
      if (is.null(nInd)) {
        nInd <- nInd(slot(x, caste))
      }
      tmp <- pullInd(pop = slot(x, caste), nInd = nInd, use = use)
      slot(x, caste) <- tmp$remnant
      if (caste == "drones" && removeFathers) {
        test <- isDrone(tmp$pulled)
        if (any(!test)) {
          tmp$pulled <- tmp$pulled[test]
          warning("Taking only drones that have not yet mated!")
        }
      }
      ret <- list(pulled = tmp$pulled, remnant = x)
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("pulled", "remnant")
    ret$pulled <- vector(mode = "list", length = nCol)
    names(ret$pulled) <- getId(x)
    ret$remnant <- x
    for (colony in seq_len(nCol)) {
      tmp <- pullCastePop(x = x[[colony]], caste = caste, nInd = nInd, use = use)
      ret$pulled[[colony]] <- tmp$pulled
      ret$remnant[[colony]] <- tmp$remnant
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn pullCastePop Pull queen from a colony
#' @export
pullQueen <- function(x) {
  ret <- pullCastePop(x, caste = "queen")
  return(ret)
}

#' @describeIn pullCastePop Pull workers from a colony
#' @export
pullWorkers <- function(x, nInd = NULL, use = "rand") {
  ret <- pullCastePop(x, caste = "workers", nInd = nInd, use = use)
  return(ret)
}

#' @describeIn pullCastePop Pull drones from a colony
#' @export
pullDrones <- function(x, nInd = NULL, use = "rand", removeFathers = TRUE) {
  ret <- pullCastePop(x,
                      caste = "drones", nInd = nInd, use = use,
                      removeFathers = removeFathers
  )
  return(ret)
}

#' @describeIn pullCastePop Pull virgin queens from a colony
#' @export
pullVirginQueens <- function(x, nInd = NULL, use = "rand") {
  ret <- pullCastePop(x, caste = "virginQueens", nInd = nInd, use = use)
  return(ret)
}

#' @rdname cross
#' @title Cross (mate) virgin queen(s) as a population, of a colony, or
#'   of all given colonies
#'
#' @description Level 1 function that crosses (mates) a virgin queen to a group
#'   of drones. The virgin queen(s) could be within a population (\code{\link{Pop-class}}),
#'   in a colony (\code{\link{Colony-class}}), or multi-colony (\code{\link{MultiColony-class}}).
#'   This function does not create any progeny, it only stores the
#'   mated drones (fathers) so we can later create progeny as needed.
#'   When input is a  (\code{\link{Colony-class}}) or  (\code{\link{MultiColony-class}}),
#'   one virgin queens is selected at random, mated, and promoted to the queen of the colony.
#'   Other virgin queens are destroyed. Mated drones (fathers) are stored for
#'   producing progeny at a later stage.
#'
#' @param x \code{\link{Pop-class}} or code{\link{Colony-class}} or \code{\link{MultiColony-class}},
#'   one or more virgin queens / colonies to be mated;
#' @param fathers \code{\link{Pop-class}} or a list of \code{\link{Pop-class}},
#'   group(s) of drones that will be mated with virgin queen(s);
#'   if there is more than one virgin queen, the user has to provide
#'   a list of drone \code{\link{Pop-class}}. For this, the user can use
#'   \code{\link{pullDroneGroupsFromDCA}}
#' @param removeFathers logical, removes those \code{drones} that have already
#'   mated; set to \code{FALSE} if you would like to mate a drone to multiple
#'   virgin queens, say via insemination
#' @param checkMating character, throw a warning (when \code{checkMating = "warning"}),
#'  or stop error (when \code{checkMating = "error"}) when some matings fail (see
#'  Details)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details This function changes caste for the mated drones to fathers, and
#'   mated virgin queens to queens. See examples. This means that you can not
#'   use these individuals in matings anymore!
#'
#' @seealso \code{\link{Colony-class}} on how we store the fathers along the
#'   queen.
#'
#' @details If the supplied drone population is empty (has 0 individuals), which
#'   can happen in edge cases or when \code{\link{nFathersPoisson}} is used
#'   instead of \code{\link{nFathersTruncPoisson}}, then mating of a virgin
#'   queen will fail and she will stay virgin. This can happen for just a few
#'   of many virgin queens, which can be annoying to track down, but you can use
#'   \code{\link{isQueen}} or \code{\link{isVirginQueen}}  to find such virgin queens. You can use
#'   \code{checkMating} to alert you about this situation.
#'
#' @return \code{\link{Pop-class}} with mated queen(s). The misc slot of the
#'   queens contains additional information about the number of workers, drones,
#'   and homozygous brood produced, and the expected percentage of csd homozygous
#'   brood.
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 20, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 200)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 8, nFathers = nFathersPoisson)
#'
#' # If input is a Pop class of virgin queen(s)
#' virginQueen1 <- basePop[2]
#' isQueen(virginQueen1)
#' (matedQueen1 <- cross(
#'   x = virginQueen1,
#'   fathers = fatherGroups[[1]]
#' ))
#'
#' isQueen(virginQueen1)
#' isQueen(virginQueen1)
#' isQueen(matedQueen1)
#' isQueen(matedQueen1)
#' nFathers(matedQueen1)
#' getFathers(matedQueen1)@id
#'
#' isDrone(getFathers(matedQueen1))
#' isFather(getFathers(matedQueen1))
#' isVirginQueen(matedQueen1)
#' isQueen(matedQueen1)
#'
#' virginQueen2 <- basePop[3]
#' (matedQueen2 <- cross(
#'   x = virginQueen2,
#'   fathers = fatherGroups[[2]]
#' ))
#' isQueen(virginQueen2)
#' isQueen(matedQueen2)
#' nFathers(matedQueen2)
#' getFathers(matedQueen2)@id
#'
#' matedQueens <- cross(
#'   x = c(basePop[4], basePop[5]),
#'   fathers = fatherGroups[c(3, 4)]
#' )
#' matedQueens
#' isQueen(matedQueens)
#' isQueen(matedQueens)
#' nFathers(matedQueens)
#' getFathers(matedQueens)
#'
#' # Inbred mated queen (mated with her own sons)
#' matedQueen3 <- cross(
#'   x = basePop[1],
#'   fathers = fatherGroups[[5]]
#' )
#' # Check the expected csd homozygosity
#' pHomBrood(matedQueen3)
#'
#' # If input is a Colony or MultiColony class
#' # Create Colony and MultiColony class
#' colony <- createColony(basePop[6])
#' isVirginQueen(getVirginQueens(colony))
#' apiary <- createMultiColony(basePop[7:8], n = 2)
#' all(isVirginQueen(mergePops(getVirginQueens(apiary))))
#'
#' # Cross
#' colony <- cross(colony, fathers = fatherGroups[[6]])
#' isQueenPresent(colony)
#' apiary <- cross(apiary, fathers = fatherGroups[c(7, 8)])
#' all(isQueenPresent(apiary))
#' nFathers(apiary)
#'
#' # Try mating with drones that were already used for mating
#' colony <- createColony(basePop[9])
#' try((matedColony <- cross(x = colony, fathers = fatherGroups[[1]])))
#' # Create new drones and mate the colony with them
#' drones <- createDrones(x = basePop[1], nInd = 15)
#' all(isDrone(drones))
#' any(isFather(drones))
#' (matedColony <- cross(x = colony, fathers = drones))
#' isQueenPresent(matedColony)
#'
#' @export
cross <- function(x, fathers,
                  removeFathers = TRUE, checkMating = "error",
                  simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) {
    if (any(!isVirginQueen(x))) {
      stop("Individuals in pop must be virgin queens!")
    }
    if (nInd(x) != length(fathers)) {
      stop("Length of argument fathers should match the number of virgin queens!")
    }
    nVirginQueen <- nInd(x)

    if (nVirginQueen == 1) {
      if (!isPop(fathers)) {
        stop("Argument fathers must be a Pop class object!")
      }
      if (!all(isDrone(fathers))) {
        stop("Individuals in fathers must be drones!")
      }
      if (fathers@nInd > 0) {
        x@misc[[1]]$fathers <- fathers
        simParamBee$changeCaste(id = x@id, caste = "Q")
        simParamBee$changeCaste(id = fathers@id, caste = "F")

        x <- setMisc(x = x, node = "nWorkers", value = 0)
        x <- setMisc(x = x, node = "nDrones", value = 0)
        x <- setMisc(x = x, node = "nHomBrood", value = 0)
        if (isCsdActive(simParamBee = simParamBee)) {
          val <- calcQueensPHomBrood(x = x)
        } else {
          val <- NA
        }
        x <- setMisc(x = x, node = "pHomBrood", value = val)
      } else if (fathers@nInd == 0) {
        msg <- "Mating failed!"
        if (checkMating == "warning") {
          warning(msg)
        } else if (checkMating == "error") {
          stop(msg)
        }
      }
      ret <- x
    } else {
      ret <- list()
      for (queen in seq_len(nVirginQueen)) {
        ret[[queen]] <- cross(x[queen],
                              fathers = fathers[[queen]],
                              checkMating = checkMating, simParamBee = simParamBee
        )
      }
      ret <- mergePops(ret)
    }
  } else if (isColony(x)) {
    if (isQueenPresent(x)) {
      stop("Queen already present in the colony!")
    }
    if (!isVirginQueensPresent(x)) {
      stop("No virgin queen(s) in the colony to cross!")
    }
    virginQueen <- selectInd(x@virginQueens, nInd = 1, use = "rand")
    queen <- cross(
      x = virginQueen, fathers = fathers,
      removeFathers = removeFathers, checkMating = checkMating,
      simParamBee = simParamBee
    )
    if (isQueen(queen)) {
      x <- reQueen(x, queen)
      x <- removeVirginQueens(x)
    }
    ret <- x
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    if (nCol == 0) {
      ret <- createMultiColony()
    } else {
      ret <- createMultiColony(n = nCol)
      if (nCol != length(fathers)) {
        stop("Length of argument fathers should match the number of colonies!")
      }
      for (colony in seq_len(nCol)) {
        ret[[colony]] <- cross(
          x = x[[colony]],
          fathers = fathers[[colony]],
          removeFathers = removeFathers,
          checkMating = checkMating,
          simParamBee = simParamBee
        )
      }
    }
  }
  validObject(ret)
  return(ret)
}

#' @rdname setQueensYearOfBirth
#' @title Set the queen's year of birth
#'
#' @description Level 1 function that sets the queen's year of birth.
#'
#' @param x \code{\link{Pop-class}} (one or more than one queen),
#'   \code{\link{Colony-class}} (one colony), or
#'   \code{\link{MultiColony-class}} (more colonies)
#' @param year integer, the year of the birth of the queen
#'
#' @return \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{MultiColony-class}} with queens having the year of birth set
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#' SP$addTraitA(10)
#' SP$addSnpChip(5)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(x = colony, fathers = fatherGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])
#'
#' # Example on Colony class
#' getQueensYearOfBirth(colony)
#' getQueensYearOfBirth(apiary)
#'
#' queen1 <- getQueen(colony)
#' queen1 <- setQueensYearOfBirth(queen1, year = 2022)
#' getQueensYearOfBirth(queen1)
#'
#' colony <- setQueensYearOfBirth(colony, year = 2022)
#' getQueensYearOfBirth(colony)
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
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]]@queen <- setMisc(
        x = x[[colony]]@queen, node = "yearOfBirth",
        value = year
      )
    }
  } else {
    stop("Argument x must be a Pop, Colony or MultiColony class object!")
  }
  return(x)
}

