# Level 3 Colonies Functions

#' @rdname createColonies
#' @title Create colonies
#'
#' @description Level 3 function that creates a set of colonies. Usually to
#'   start a simulation.
#'
#' @param x \code{\link{Pop-class}}, virgin queens or queens for the colonies
#'   (selected at random if there are more than \code{n} in \code{Pop})
#' @param n integer, number of colonies to create (if only \code{nCol} is
#'   given then \code{\link{Colonies-class}} is created with \code{nCol} empty
#'   (\code{NULL}) individual colony) - this is mostly useful for programming)
#' @param location list, location of the colonies as \code{c(x, y)}
#'
#' @details When both \code{x} and \code{nCol} are \code{NULL}, then an empty
#'   \code{NULL} \code{\link{Colonies-class}} is created with 0 colonies.
#'
#' @return \code{\link{Colonies-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' # Create 2 empty (NULL) colonies
#' apiary <- createColonies(n = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 mated colonies
#' apiary <- createColonies(x = basePop, n = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 2 unmated/virgin colonies
#' apiary <- createColonies(x = basePop[1:2], n = 2)
#' drones <- createDrones(x = basePop[3], n = 50)
#' apiary <- crossColonies(apiary, drones = drones, nFathers = 15)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#' @export
createColonies <- function(x = NULL, n = NULL, location = NULL) {
  if (is.null(x)) {
    if (is.null(n)) {
      ret <- new(Class = "Colonies")
    } else {
      ret <- new(Class = "Colonies", colonies = vector(mode = "list", length = n))
    }
  } else {
    if (!isPop(x)) {
      stop("Argument x must be a Pop class object!")
    }
    if (any(!(isVirginQueen(x) | isQueen(x)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
    if (nInd(x) < n) {
      stop("Not enough individuals in the x to create n colonies!")
    }
    ret <- new(Class = "Colonies", colonies = vector(mode = "list", length = n))
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
#' @title Select individual colonies
#'
#' @description Level 3 function that selects individual colonies based on
#'   colony ID or random selection.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param ID character or numeric, name of a colony (one or more) to be
#'   selected; if character (numeric) colonies are selected based on their name
#'   (position)
#' @param n numeric, number of colonies to select
#' @param p numeric, probability of a colony being selected (takes precedence
#'   over \code{n})
#'
#' @return \code{\link{Colonies-class}} with selected colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[5:10], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[11:20], nFathers = 5)
#' colony3 <- createColony(x = basePop[4])
#' colony3 <- crossColony(colony3, drones = drones[21:30], nFathers = 5)
#' colony4 <- createColony(x = basePop[5])
#' colony4 <- crossColony(colony4, drones = drones[31:40], nFathers = 5)
#' apiary <- c(colony1, colony2, colony3, colony4)
#' getId(apiary)
#'
#' getId(selectColonies(apiary, ID = 1))
#' getId(selectColonies(apiary, ID = "5"))
#' getId(selectColonies(apiary, ID = c(1, 2)))
#' getId(selectColonies(apiary, ID = c("5", "6")))
#' getId(selectColonies(apiary, ID = 5))
#' getId(selectColonies(apiary, ID = "9"))
#' # ... alternative
#' getId(apiary[1])
#' getId(apiary[[1]])
#' getId(apiary["5"])
#' getId(apiary[["5"]])
#' getId(apiary[c(1, 2)])
#' getId(apiary[c("5", "6")])
#' getId(apiary[5])
#' getId(apiary["9"])
#'
#' getId(selectColonies(apiary, p = 0.5))
#' getId(selectColonies(apiary, p = 0.5))
#' getId(selectColonies(apiary, p = 0.5))
#' @export
selectColonies <- function(colonies, ID = NULL, n = NULL, p = NULL) {
  # TODO: add use and trait argument to this function?
  #       the idea is that we could swarm/supersede/... colonies depending on a
  #       trait expression; this could be complicated - best to follow ideas at
  #       https://github.com/HighlanderLab/SIMplyBee/issues/105
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!is.null(ID)) {
    # Testing because a logical vector recycles on colonies[ID]
    if (!(is.character(ID) | is.numeric(ID))) {
      stop("ID must be character or numeric!")
    }
    ret <- colonies[ID]
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(colonies)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    lSel <- sample.int(n = nCol, size = n)
    message(paste0("Randomly selecting colonies: ", n))
    if (length(lSel) > 0) {
      ret <- colonies[lSel]
    } else {
      ret <- NULL
    }
  } else {
    stop("Provide either ID, n, or p!")
  }
  validObject(ret)
  return(ret)
}

#' @rdname pullColonies
#' @title Pull out some colonies
#'
#' @description Level 3 function that pulls out some colonies based on colony
#'   ID or random selection.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param ID character or numeric, name of a colony (one or more) to be pulled
#'   out; if character (numeric) colonies are pulled out based on their name
#'   (position)
#' @param n numeric, number of colonies to select
#' @param p numeric, probability of a colony being pulled out (takes precedence
#'   over \code{n})
#'
#' @return list with two \code{\link{Colonies-class}}, the \code{pulledColonies}
#'   and the \code{remainingColonies}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[5:10], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[11:20], nFathers = 5)
#' colony3 <- createColony(x = basePop[4])
#' colony3 <- crossColony(colony3, drones = drones[21:30], nFathers = 5)
#' colony4 <- createColony(x = basePop[5])
#' colony4 <- crossColony(colony4, drones = drones[31:40], nFathers = 5)
#' apiary <- c(colony1, colony2, colony3, colony4)
#' getId(apiary)
#'
#' tmp <- pullColonies(apiary, ID = c(1, 2))
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, ID = c("5", "6"))
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, ID = 5)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, ID = "9")
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, n = 2)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#'
#' tmp <- pullColonies(apiary, p = 0.75)
#' getId(tmp$pulledColonies)
#' getId(tmp$remainingColonies)
#' @export
pullColonies <- function(colonies, ID = NULL, n = NULL, p = NULL) {
  # TODO: add use and trait argument to this function?
  #       the idea is that we could swarm/supersede/... colonies depending on a
  #        trait expression; this could be complicated - best to follow ideas at
  #       https://github.com/HighlanderLab/SIMplyBee/issues/105
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (!is.null(ID)) {
    pulledColonies <- selectColonies(colonies, ID)
    remainingColonies <- removeColonies(colonies, ID)
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(colonies)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    lPull <- sample.int(n = nCol, size = n)
    message(paste0("Randomly pulling colonies: ", n))
    if (length(lPull) > 0) {
      ids <- getId(colonies)
      pulledColonies <- selectColonies(colonies, ids[lPull])
      remainingColonies <- removeColonies(colonies, ids[lPull])
    } else {
      pulledColonies <- createColonies()
      remainingColonies <- colonies
    }
  } else {
    stop("You must provide either ID, n, or p!")
  }
  ret <- list(pulledColonies = pulledColonies, remainingColonies = remainingColonies)
  validObject(ret$pulledColonies)
  validObject(ret$remainingColonies)
  return(ret)
}

#' @rdname removeColonies
#' @title Remove some colonies
#'
#' @description Level 3 function that removes some colonies based on their ID.
#'
#' @param colonies \code{\link{Colonies-class}}
#' @param ID character or numeric, name of a colony (one or more) to be
#'   removed; if character (numeric) colonies are removed based on their name
#'   (position)
#'
#' @return \code{\link{Colonies-class}} with some colonies removed
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- crossColony(colony1, drones = drones[5:10], nFathers = 5)
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- crossColony(colony2, drones = drones[11:20], nFathers = 5)
#' colony3 <- createColony(x = basePop[4])
#' colony3 <- crossColony(colony3, drones = drones[21:30], nFathers = 5)
#' colony4 <- createColony(x = basePop[5])
#' colony4 <- crossColony(colony4, drones = drones[31:40], nFathers = 5)
#' apiary <- c(colony1, colony2, colony3, colony4)
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
#' @export
removeColonies <- function(colonies, ID) {
  if (!isColonies(colonies)) {
    stop("Argument colonies must be a Colonies class object!")
  }
  if (is.character(ID)) {
    ret <- colonies[!getId(colonies) %in% ID]
  } else if (is.numeric(ID)) {
    ret <- colonies[-ID]
  } else {
    stop("ID must be character or numeric!")
  }
  validObject(ret)
  return(ret)
}
