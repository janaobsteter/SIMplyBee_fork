# ---- Class MultiColony ----

setClassUnion("integerOrNumeric", c("integer", "numeric"))
setClassUnion("integerOrNumericOrLogical", c("integer", "numeric", "logical"))
setClassUnion("integerOrNumericOrLogicalOrCharacter", c("integer", "numeric", "logical", "character"))

#' @rdname MultiColony-class
#' @title Honeybee multicolony object
#'
#' @description An object holding a collection of honeybee colonies. It behaves
#'   like a list.
#'
#' @slot colonies list, a collection of \code{\link[SIMplyBee]{Colony-class}} objects
#'
#' @param object \code{\link[SIMplyBee]{MultiColony-class}}
#' @param x \code{\link[SIMplyBee]{MultiColony-class}}
#' @param i integer, numeric, logical, or character, index or ID to select
#'   a colony (see examples)
#' @param j not used
#' @param drop not used
#' @param value \code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}} to
#'   assign into \code{x} based on colony index or name \code{i}
#' @param ... \code{NULL}, \code{\link[SIMplyBee]{Colony-class}}, or
#'   \code{\link[SIMplyBee]{MultiColony-class}}
#'
#' @seealso \code{\link[SIMplyBee]{createMultiColony}}
#'
#' @return \code{\link[SIMplyBee]{MultiColony-class}} or \code{\link[SIMplyBee]{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#' apiary <- createMultiColony(basePop[1:6], n = 6)
#' apiary <- cross(apiary, drones = droneGroups[1:6])
#' apiary
#' show(apiary)
#' is(apiary)
#' isMultiColony(apiary)
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
  Class = "MultiColony",
  slots = c(colonies = "list")
)

#' @describeIn MultiColony-class Test if x is a MultiColony class object
#' @export
isMultiColony <- function(x) {
  ret <- is(x, class2 = "MultiColony")
  return(ret)
}

setClassUnion("MultiColonyOrNULL", c("MultiColony", "NULL"))

setValidity(Class = "MultiColony", method = function(object) {
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

# show ----
#' @describeIn MultiColony-class Show MultiColony object
setMethod(
  f = "show",
  signature(object = "MultiColony"),
  definition = function(object) {
    cat("An object of class", classLabel(class(object)), "\n")
    cat("Number of colonies:", nColonies(object), "\n")
    cat("Are empty:", nEmptyColonies(object), "\n")
    cat("Are NULL:", nNULLColonies(object), "\n")
    if (nColonies(object) > 1) {
      sel <- !(isNULLColonies(object) | isEmpty(object))
    } else {
      sel <- FALSE
    }
    if (any(sel)) {
      cat("Have split:", sum(hasSplit(object[sel])), "\n")
      cat("Have swarmed:", sum(hasSwarmed(object[sel])), "\n")
      cat("Have superseded:", sum(hasSuperseded(object[sel])), "\n")
      cat("Have collapsed:", sum(hasCollapsed(object[sel])), "\n")
      cat("Are productive:", sum(isProductive(object[sel])), "\n")
    } else {
      cat("Have split: 0 \n")
      cat("Have swarmed: 0 \n")
      cat("Have superseded: 0 \n")
      cat("Have collapsed: 0 \n")
      cat("Are productive: 0 \n")
    }
    invisible()
  }
)

# c ----
#' @describeIn MultiColony-class Combine multiple Colony and MultiColony objects
setMethod(
  f = "c",
  signature(x = "MultiColony"),
  definition = function(x, ...) {
    for (y in list(...)) {
      if (is(y, class2 = "NULL")) {
        # Do nothing
      } else if (isColony(y)) {
        x@colonies <- c(x@colonies, y)
      } else if (isMultiColony(y)) {
        x@colonies <- c(x@colonies, y@colonies)
      } else {
        stop("... must be a NULL, Colony or MultiColony class object!")
      }
    }
    validObject(x)
    return(x)
  }
)

#' @describeIn MultiColony-class Combine multiple Colony and MultiColony objects
setMethod(
  f = "c",
  signature(x = "MultiColonyOrNULL"),
  definition = function(x, ...) {
    if (is.null(x)) {
      multicolony <- new(Class = "MultiColony")
    } else {
      multicolony <- new(Class = "MultiColony", colonies = list(x))
    }
    for (y in list(...)) {
      if (is(y, class2 = "NULL")) {
        # Do nothing
      } else if (isColony(y)) {
        multicolony@colonies <- c(multicolony@colonies, y)
      } else if (isMultiColony(y)) {
        multicolony@colonies <- c(multicolony@colonies, y@colonies)
      } else {
        stop("... must be a NULL, Colony, or MultiColony class object!")
      }
    }
    validObject(multicolony)
    return(multicolony)
  }
)

# [ ----
#' @describeIn MultiColony-class Extract a colony (one or more!) with an integer/numeric/logical index (position) (return \code{\link[SIMplyBee]{MultiColony-class}})
setMethod(
  f = "[",
  signature(x = "MultiColony", i = "integerOrNumericOrLogical", j = "ANY", drop = "ANY"),
  definition = function(x, i, j, drop) {
    x@colonies <- x@colonies[i]
    validObject(x)
    return(x)
  }
)

#' @describeIn MultiColony-class Extract a colony (one or more!) with a character ID (name) (return \code{\link[SIMplyBee]{MultiColony-class}})
setMethod(
  f = "[",
  signature(x = "MultiColony", i = "character", j = "ANY", drop = "ANY"),
  definition = function(x, i, j, drop) {
    # match returns integers, so we then call the x[integer] method
    ret <- x[match(x = i, table = getId(x))]
    validObject(ret)
    return(ret)
  }
)

# [[ ----
#' @describeIn MultiColony-class Extract a colony (just one!) with an integer/numeric/logical index (position) (return \code{\link[SIMplyBee]{Colony-class}})
setMethod(
  f = "[[",
  signature(x = "MultiColony", i = "integerOrNumericOrLogical"),
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

#' @describeIn MultiColony-class Extract a colony (just one!) with a character ID (name) (return \code{\link[SIMplyBee]{Colony-class}})
setMethod(
  f = "[[",
  signature(x = "MultiColony", i = "character"),
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

# [<- ----
#' @describeIn MultiColony-class Assign colonies into MultiColony
# There is also [[<- method for colony in Class-Colony.R
setReplaceMethod(
  f = "[",
  signature(
    x = "MultiColony", i = "integerOrNumericOrLogicalOrCharacter",
    j = "ANY", value = "MultiColony"
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

# [[<- ----
#' @describeIn MultiColony-class Assign Colony into MultiColony
setReplaceMethod(
  f = "[[",
  signature(
    x = "MultiColony", i = "integerOrNumericOrLogicalOrCharacter",
    j = "ANY", value = "Colony"
  ),
  definition = function(x, i, j, value) {
    if (is.numeric(i)) {
      if (length(i) > 1) {
        stop("Length of numeric i (position index) must be 1 when value (assignment) is a Colony class object!")
      }
      x@colonies[[i]] <- value
    } else if (is.logical(i)) {
      if (sum(i) != 1) {
        stop("Number of TRUE values in i (position index) must be equal to 1 when value (assignment) is a Colony class object!")
      }
      x@colonies[i][[1L]] <- value
    } else if (is.character(i)) {
      if (length(i) > 1) {
        stop("Length of character i (name index) must be 1 when value (assignment) is a Colony class object!")
      }
      match <- getId(x) %in% i
      if (sum(match) != 1) {
        stop("Number of matched colony names in x from i (name index) must be equal to 1 when value (assignment) is a Colony class object!")
      }
      x@colonies[match][[1L]] <- value
    }
    validObject(x)
    x
  }
)
