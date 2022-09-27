# ---- Level 3 MultiColony Functions ----

#' @rdname createMultiColony
#' @title Create MultiColony object
#'
#' @description Level 3 function that creates a set of colonies. Usually to
#'   start a simulation.
#'
#' @param x \code{\link{Pop-class}}, virgin queens or queens for the colonies
#'   (selected at random if there are more than \code{n} in \code{Pop}, while
#'    all are used when \code{n} is \code{NULL})
#' @param n integer, number of colonies to create (if only \code{n} is
#'   given then \code{\link{MultiColony-class}} is created with \code{n}
#'   \code{NULL}) individual colony - this is mostly useful for programming)
#' @param location list, location of the colonies as \code{c(x, y)}
#'
#' @details When both \code{x} and \code{n} are \code{NULL}, then a
#'   \code{\link{MultiColony-class}} with 0 colonies is created.
#'
#' @return \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' # Create 2 empty (NULL) colonies
#' apiary <- createMultiColony(n = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 virgin colonies
#' apiary <- createMultiColony(x = basePop, n = 2) # specify n
#' apiary <- createMultiColony(x = basePop[1:2]) # take all provided
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create mated colonies by crossing
#' apiary <- createMultiColony(x = basePop[1:2], n = 2)
#' drones <- createDrones(x = basePop[3], n = 30)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 2, nDrones = 15)
#' apiary <- cross(apiary, drones = droneGroups)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#' @export
createMultiColony <- function(x = NULL, n = NULL, location = NULL) {
  if (is.null(x)) {
    if (is.null(n)) {
      ret <- new(Class = "MultiColony")
    } else {
      ret <- new(Class = "MultiColony", colonies = vector(mode = "list", length = n))
    }
  } else {
    if (!isPop(x)) {
      stop("Argument x must be a Pop class object!")
    }
    if (any(!(isVirginQueen(x) | isQueen(x)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
    if (is.null(n)) {
      n <- nInd(x)
    }
    if (nInd(x) < n) {
      stop("Not enough individuals in the x to create n colonies!")
    }
    ret <- new(Class = "MultiColony", colonies = vector(mode = "list", length = n))
    for (colony in seq_len(n)) {
      ret[[colony]] <- createColony(
        x = x[colony],
        location = location[colony]
      )
    }
  }
  validObject(ret)
  return(ret)
}

#' @rdname selectColonies
#' @title Select colonies from MultiColony object
#'
#' @description Level 3 function that selects colonies from
#'   MultiColony object based on colony ID or random selection.
#'   Whilst user can provide can all arguments ID, p and n, not all are carried out.
#'   ID takes first priority. If no ID is provided, p takes precedence over n.
#'
#' @param multicolony \code{\link{MultiColony-class}}
#' @param ID character or numeric, ID of a colony (one or more) to be
#'   selected
#' @param n numeric, number of colonies to select
#' @param p numeric, percentage of colonies selected (takes precedence
#'   over \code{n})
#'
#' @return \code{\link{MultiColony-class}} with selected colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#' apiary <- createMultiColony(basePop[2:5], n = 4)
#' apiary <- cross(apiary, drones = droneGroups[1:4])
#' apiary2 <- createMultiColony(basePop[6:8])
#' getId(apiary)
#' getId(apiary2)
#'
#' getId(selectColonies(apiary, ID = 1))
#' getId(selectColonies(apiary, ID = "4"))
#' getId(selectColonies(apiary, ID = c(1, 2)))
#' getId(selectColonies(apiary, ID = c("3", "4")))
#' getId(selectColonies(apiary, ID = 3))
#' # ... alternative
#' getId(apiary[1])
#' getId(apiary[[1]])
#' getId(apiary["4"])
#' getId(apiary[["4"]])
#' getId(apiary[c(1, 2)])
#' getId(apiary[c("3", "4")])
#' getId(apiary[3])
#'
#' # Select a random number of colonies
#' selectColonies(apiary, n = 3)
#' # Select a percentage of colonies
#' selectColonies(apiary, p = 0.2)
#'
#' # Since selection is random, you would get a different set of colonies with
#' # each function call
#' getId(selectColonies(apiary, p = 0.5))
#' getId(selectColonies(apiary, p = 0.5))
#' getId(selectColonies(apiary, p = 0.5))
#' @export
selectColonies <- function(multicolony, ID = NULL, n = NULL, p = NULL) {
  # TODO: add use and trait argument to this function?
  #       the idea is that we could swarm/supersede/... colonies depending on a
  #       trait expression; this could be complicated - best to follow ideas at
  #       https://github.com/HighlanderLab/SIMplyBee/issues/105
  if (!is.null(ID)) {
    if (!(is.character(ID) | is.numeric(ID))) {
      stop("ID must be character or numeric!")
    }
    trueID <- ID %in% getId(multicolony)
    if (!all(trueID)) {
      ID <- ID[trueID]
      warning("ID parameter contains come invalid IDs!")
    }
  }
  if (!is.null(n)) {
    if (n > nColonies(multicolony)) {
      stop("n must not be larger than the number of colonies in multicolony!")
    } else if (n < 0) {
      stop("n must be non-negative!")
    }
  }
  if (!is.null(p)) {
    if (1 < p) {
      stop("p must not be higher than 1!")
    } else if (p < 0) {
      stop("p must not be less than 0!")
    }
  }
  if (!isMultiColony(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  if (!is.null(ID)) {
    ID <- as.character(ID)
    ret <- multicolony[ID]
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(multicolony)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    lSel <- sample.int(n = nCol, size = n)
    message(paste0("Randomly selecting colonies: ", n))
    if (length(lSel) > 0) {
      ret <- multicolony[lSel]
    } else {
      ret <- createMultiColony()
    }
  } else {
    stop("Provide either ID, n, or p!")
  }
  validObject(ret)
  return(ret)
}

#' @rdname pullColonies
#' @title Pull out some colonies from the MultiColony object
#'
#' @description Level 3 function that pulls out some colonies
#'   from the MultiColony based on colony ID or random selection.
#'
#' @param multicolony \code{\link{MultiColony-class}}
#' @param ID character or numeric, ID of a colony (one or more) to be pulled
#'   out
#' @param n numeric, number of colonies to select
#' @param p numeric, percentage of colonies pulled out (takes precedence
#'   over \code{n})
#'
#' @return list with two \code{\link{MultiColony-class}}, the \code{pulled}
#'   and the \code{remnant}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#' apiary <- createMultiColony(basePop[2:5], n = 4)
#' apiary <- cross(apiary, drones = droneGroups[1:4])
#' getId(apiary)
#'
#' tmp <- pullColonies(apiary, ID = c(1, 2))
#' getId(tmp$pulled)
#' getId(tmp$remnant)
#'
#' tmp <- pullColonies(apiary, ID = c("3", "4"))
#' getId(tmp$pulled)
#' getId(tmp$remnant)
#'
#' tmp <- pullColonies(apiary, n = 2)
#' getId(tmp$pulled)
#' getId(tmp$remnant)
#'
#' tmp <- pullColonies(apiary, p = 0.75)
#' getId(tmp$pulled)
#' getId(tmp$remnant)
#' @export
pullColonies <- function(multicolony, ID = NULL, n = NULL, p = NULL) {
  # TODO: add use and trait argument to this function?
  #       the idea is that we could swarm/supersede/... colonies depending on a
  #        trait expression; this could be complicated - best to follow ideas at
  #       https://github.com/HighlanderLab/SIMplyBee/issues/105
  if (!isMultiColony(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  if (!is.null(ID)) {
    trueID <- ID %in% getId(multicolony)
    if (!all(trueID)) {
      ID <- ID[trueID]
      warning("ID parameter contains come invalid IDs!")
    }
    pulled <- selectColonies(multicolony, ID) # selectColonies does the checking of the IDs
    remnant <- removeColonies(multicolony, ID)
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(multicolony)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    lPull <- sample.int(n = nCol, size = n)
    message(paste0("Randomly pulling colonies: ", n))
    if (length(lPull) > 0) {
      pulled <- multicolony[lPull]
      remnant <- multicolony[-lPull]
    } else {
      pulled <- createMultiColony()
      remnant <- multicolony
    }
  } else {
    stop("You must provide either ID, n, or p!")
  }
  ret <- list(pulled = pulled, remnant = remnant)
  validObject(ret$pulled)
  validObject(ret$remnant)
  return(ret)
}

#' @rdname removeColonies
#' @title Remove some colonies from the MultiColony object
#'
#' @description Level 3 function that removes some colonies
#'   from the MultiColony object based on their ID.
#'
#' @param multicolony \code{\link{MultiColony-class}}
#' @param ID character or numeric, ID of a colony (one or more) to be
#'   removed
#' @param n numeric, number of colonies to remove
#' @param p numeric, percentage of colonies removed (takes precedence
#'   over \code{n})
#' @return \code{\link{MultiColony-class}} with some colonies removed
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#' apiary <- createMultiColony(basePop[2:5], n = 4)
#' apiary <- cross(apiary, drones = droneGroups[1:4])
#' getId(apiary)
#'
#' getId(removeColonies(apiary, ID = 1))
#' getId(removeColonies(apiary, ID = "5"))
#'
#' getId(removeColonies(apiary, ID = c(1, 2)))
#' getId(removeColonies(apiary, ID = c("5", "6")))
#'
#' getId(removeColonies(apiary, ID = 5))
#' getId(removeColonies(apiary, ID = "9"))
#'
#' nColonies(apiary)
#' apiary <- removeColonies(apiary, ID = "2")
#' nColonies(apiary)
#'
#' @export
removeColonies <- function(multicolony,  ID = NULL, n = NULL, p = NULL) {
  if (!isMultiColony(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  if (!is.null(ID)) {
    trueID <- ID %in% getId(multicolony)
    if (!all(trueID)) {
      ID <- ID[trueID]
      warning("ID parameter contains come invalid IDs!")
    }
    ret <- selectColonies(multicolony, ID = getId(multicolony)[!getId(multicolony) %in% ID])
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(multicolony)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    ret <- selectColonies(multicolony, n = (nCol - n))
  } else {
    stop("You must provide either ID, n, or p!")
  }
  validObject(ret)
  return(ret)
}
