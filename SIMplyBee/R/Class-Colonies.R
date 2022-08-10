# Class Colonies ----

setClassUnion("integerOrNumeric", c("integer", "numeric"))
setClassUnion("integerOrNumericOrLogical", c("integer", "numeric", "logical"))
setClassUnion("integerOrNumericOrLogicalOrCharacter", c("integer", "numeric", "logical", "character"))

#' @rdname Colonies-class
#' @title Honeybee colonies
#'
#' @description An object holding a collection of honeybee colonies. It behaves
#'   like a list.
#'
#' @slot colonies list, a collection of \code{\link{Colony-class}} objects
#'
#' @param object \code{\link{Colonies-class}}
#' @param x \code{\link{Colonies-class}}
#' @param i integer, numeric, logical, or character, index or ID to select
#'   a colony (see examples)
#' @param j not used
#' @param drop not used
#' @param value \code{\link{Colony-class}} or \code{\link{Colonies-class}} to
#'   assign into \code{x} based on colony index or name \code{i}
#' @param ... \code{NULL}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#'
#' @seealso \code{\link{createColonies}}
#'
#' @return \code{\link{Colonies-class}} or \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
#' apiary <- createColonies(basePop[1:6], n = 6)
#' apiary <- cross(apiary, fathers = fatherGroups[1:6])
#' apiary
#' show(apiary)
#' is(apiary)
#' isColonies(apiary)
#'
#' getId(apiary)
#' apiary[1]
#' getId(apiary[1])
#' getId(apiary["2"])
#' getId(apiary[2])
#' getId(apiary[-1])
#' getId(apiary[5])
#'
#' getId(apiary)
#' getId(apiary[c(1, 3)])
#' getId(apiary[c("2", "4")])
#' getId(apiary[c(TRUE, FALSE, TRUE, FALSE)])
#' getId(apiary[c(TRUE, FALSE)]) # beware of recycling!
#' getId(apiary[c(5, 6)])
#' getId(apiary[c("6", "7")])
#'
#' apiary[[1]]
#' apiary[["2"]]
#' apiary[[3]]
#' apiary[["4"]]
#' try(apiary[[6]])
#' apiary[["7"]]
#'
#' getId(c(apiary[c(1, 3)], apiary[2]))
#' getId(c(apiary[2], apiary[c(1, 3)]))
#'
#' getId(c(apiary[2], apiary[0]))
#' getId(c(apiary[0], apiary[2]))
#'
#' getId(c(apiary[2], NULL))
#' getId(c(NULL, apiary[2]))
#'
#' apiary1 <- apiary[1:2]
#' apiary2 <- apiary[3:4]
#' getId(apiary1)
#' getId(apiary2)
#' apiary1[[1]] <- apiary2[[1]]
#' getId(apiary1)
#' try(apiary2[[1]] <- apiary2[[2]])
#'
#' apiary1 <- apiary[1:2]
#' apiary2 <- apiary[3:5]
#' getId(apiary1)
#' getId(apiary2)
#' apiary2[1:2] <- apiary1
#' getId(apiary2)
#' try(apiary2[1] <- apiary1)
#' try(apiary2[1:3] <- apiary1)
#' try(apiary2[1:2] <- apiary1[[1]])
#'
#' apiary2 <- apiary[3:5]
#' getId(apiary2)
#' try(apiary2[c("4", "5")] <- apiary1)
#' try(apiary2[c("4", "5")] <- apiary1)
#' @export
setClass(
  Class = "Colonies",
  slots = c(colonies = "list")
)

#' @describeIn Colonies-class Test if x is a Colonies class object
#' @export
isColonies <- function(x) {
  ret <- is(x, class2 = "Colonies")
  return(ret)
}

setClassUnion("ColoniesOrNULL", c("Colonies", "NULL"))

setValidity(Class = "Colonies", method = function(object) {
  errors <- character()
  ids <- getId(object)
  ids <- ids[!is.na(ids)]
  if (any(duplicated(ids))) {
    errors <- c(errors, "Some colonies are duplicated!")
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
})

#' @describeIn Colonies-class Show colonies object
setMethod(
  f = "show",
  signature(object = "Colonies"),
  definition = function(object) {
    cat("An object of class", classLabel(class(object)), "\n")
    cat("Number of colonies:", nColonies(object), "\n")
    cat("Number of empty (NULL) colonies:", nNULLColonies(object), "\n")
    invisible()
  }
)

#' @describeIn Colonies-class Combine multiple colony and colonies objects
setMethod(
  f = "c",
  signature(x = "Colonies"),
  definition = function(x, ...) {
    for (y in list(...)) {
      if (is(y, class2 = "NULL")) {
        # Do nothing
      } else if (isColony(y)) {
        x@colonies <- c(x@colonies, y)
      } else if (isColonies(y)) {
        x@colonies <- c(x@colonies, y@colonies)
      } else {
        stop("... must be a NULL, Colony or Colonies class object!")
      }
    }
    validObject(x)
    return(x)
  }
)

#' @describeIn Colonies-class Combine multiple colony and colonies objects
setMethod(
  f = "c",
  signature(x = "ColoniesOrNULL"),
  definition = function(x, ...) {
    if (is.null(x)) {
      colonies <- new(Class = "Colonies")
    } else {
      colonies <- new(Class = "Colonies", colonies = list(x))
    }
    for (y in list(...)) {
      if (is(y, class2 = "NULL")) {
        # Do nothing
      } else if (isColony(y)) {
        colonies@colonies <- c(colonies@colonies, y)
      } else if (isColonies(y)) {
        colonies@colonies <- c(colonies@colonies, y@colonies)
      } else {
        stop("... must be a NULL, Colony, or Colonies class object!")
      }
    }
    validObject(colonies)
    return(colonies)
  }
)

#' @describeIn Colonies-class Extract a colony (one or more!) with an integer/numeric/logical index (position) (return \code{\link{Colonies-class}})
setMethod(
  f = "[",
  signature(x = "Colonies", i = "integerOrNumericOrLogical", j = "ANY", drop = "ANY"),
  definition = function(x, i, j, drop) {
    x@colonies <- x@colonies[i]
    validObject(x)
    return(x)
  }
)

#' @describeIn Colonies-class Extract a colony (one or more!) with a character ID (name) (return \code{\link{Colonies-class}})
setMethod(
  f = "[",
  signature(x = "Colonies", i = "character", j = "ANY", drop = "ANY"),
  definition = function(x, i, j, drop) {
    # match returns integers, so we then call the x[integer] method
    ret <- x[match(x = i, table = getId(x))]
    validObject(ret)
    return(ret)
  }
)

#' @describeIn Colonies-class Extract a colony (just one!) with an integer/numeric/logical index (position) (return \code{\link{Colony-class}})
setMethod(
  f = "[[",
  signature(x = "Colonies", i = "integerOrNumericOrLogical"),
  definition = function(x, i) {
    n <- length(i)
    if (n > 1) {
      warning(paste("Selecting only the first colony out of ", n, " requested\n"))
    }
    # ...@colonies[[i[1L]]] is to get just one colony
    ret <- x@colonies[[i[1L]]]
    validObject(ret)
    return(ret)
  }
)

#' @describeIn Colonies-class Extract a colony (just one!) with a character ID (name) (return \code{\link{Colony-class}})
setMethod(
  f = "[[",
  signature(x = "Colonies", i = "character"),
  definition = function(x, i) {
    n <- length(i)
    if (n > 1) {
      warning(paste("Selecting only the first colony out of ", n, " requested\n"))
    }
    # x[i[1L]] calls x[character]
    # ...@colonies[[1L]] is to get just one colony
    ret <- x[i[1L]]@colonies[[1L]]
    validObject(ret)
    return(ret)
  }
)

#' @describeIn Colonies-class Assign colonies into colonies
# There is also [[<- method for colony in Class-Colony.R
setReplaceMethod(
  f = "[",
  signature(
    x = "Colonies", i = "integerOrNumericOrLogicalOrCharacter",
    j = "ANY", value = "Colonies"
  ),
  definition = function(x, i, j, value) {
    nCol <- nColonies(value)
    if (is.numeric(i)) {
      if (length(i) != nCol) {
        stop("Length of i (position index) does not match the number of assigned colonies!")
      }
      x@colonies[i] <- value@colonies
    } else if (is.logical(i)) {
      if (sum(i) != nCol) {
        stop("Number of TRUE values in i (position index) does not match the number of assigned colonies!")
      }
      x@colonies[i] <- value@colonies
    } else if (is.character(i)) {
      matches <- getId(x) %in% i
      if (sum(matches) != nCol) {
        stop("Number of matched colony names in x from i (name index) does not match the number of assigned colonies!")
      }
      x@colonies[matches] <- value@colonies
    }
    validObject(x)
    x
  }
)
