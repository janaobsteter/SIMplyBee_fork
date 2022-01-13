# Class Colonies ----

setClassUnion("integerOrNumeric", c("integer", "numeric"))
setClassUnion("integerOrNumericOrLogical", c("integer", "numeric", "logical"))

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
#' @param ... \code{NULL}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#'
#' @seealso \code{\link{createColonies}}
#'
#' @return \code{\link{Colonies-class}} or \code{\link{Colony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- newPop(founderGenomes)
#'
#' drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 15)
#' colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
#' colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
#' colony3 <- createColony(queen = basePop[4], fathers = drones[11:15])
#' apiary <- c(colony1, colony2, colony3)
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
#'
#' getId(apiary)
#' getId(apiary[c(1, 3)])
#' getId(apiary[c("2", "4")])
#' getId(apiary[c(TRUE, FALSE, TRUE)])
#'
#' apiary[[1]]
#' apiary[["2"]]
#' apiary[[3]]
#' apiary[["4"]]
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
#' @export
setClass(Class = "Colonies",
         slots = c(colonies = "list"))

setValidity(Class = "Colonies", method = function(object) {
  errors <- character()
  ids <- getId(object)
  ids <- ids[!is.na(ids)]
  if (any(duplicated(ids))) {
    errors = c(errors, "Some colonies are duplicated!")
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
})

#' @describeIn Colonies-class Show colonies object
setMethod(f = "show",
          signature(object = "Colonies"),
          function (object) {
            cat("An object of class", classLabel(class(object)), "\n")
            cat("Number of colonies:", nColonies(object), "\n")
            cat("Number of empty (NULL) colonies:", nNULLColonies(object), "\n")
            invisible()
          }
)

#' @describeIn Colonies-class Extract a colony (one or more!) by integer/numeric/logical index (return \code{\link{Colonies-class}})
setMethod(f = "[",
          signature(x = "Colonies", i = "integerOrNumericOrLogical", j = "ANY", drop = "ANY"),
          function(x, i, j, drop) {
            x@colonies <- x@colonies[i]
            validObject(x)
            return(x)
          }
)

#' @describeIn Colonies-class Extract a colony (one or more!) by character ID (return \code{\link{Colonies-class}})
setMethod(f = "[",
          signature(x = "Colonies", i = "character", j = "ANY", drop = "ANY"),
          function(x, i, j, drop) {
            # match returns integers, so we then call the x[integer] method
            ret <- x[match(x = i, table = getId(x))]
            validObject(ret)
            return(ret)
          }
)

#' @describeIn Colonies-class Extract a colony (just one!) by integer/numeric index (return \code{\link{Colony-class}})
setMethod(f = "[[",
          signature(x = "Colonies", i = "integerOrNumericOrLogical"),
          function(x, i) {
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

#' @describeIn Colonies-class Extract a colony (just one!) by character ID (return \code{\link{Colony-class}})
setMethod(f = "[[",
          signature(x = "Colonies", i = "character"),
          function(x, i) {
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

#' @describeIn Colonies-class Combine multiple colony and colonies objects
setMethod(f = "c",
          signature(x = "Colonies"),
          function(x, ...) {
            for (y in list(...)) {
              if (class(y) == "NULL") {
                # Do nothing
              } else if (class(y) == "Colony") {
                x@colonies <- c(x@colonies, y)
              } else if (class(y) == "Colonies") {
                x@colonies <- c(x@colonies, y@colonies)
              } else {
                stop("... must be a NULL, Colony or Colonies class object!")
              }
            }
            validObject(x)
            return(x)
          }
)

setClassUnion("ColoniesOrNULL", c("Colonies", "NULL"))

#' @describeIn Colonies-class Combine multiple colony and colonies objects
setMethod(f = "c",
          signature(x = "ColoniesOrNULL"),
          function(x, ...) {
            if (is.null(x)) {
              colonies <- new(Class = "Colonies")
            } else {
              colonies <- new(Class = "Colonies", colonies = list(x))
            }
            for (y in list(...)) {
              if (class(y) == "NULL") {
                # Do nothing
              } else if (class(y) == "Colony") {
                colonies@colonies <- c(colonies@colonies, y)
              } else if (class(y) == "Colonies") {
                colonies@colonies <- c(colonies@colonies, y@colonies)
              } else {
                stop("... must be a NULL, Colony, or Colonies class object!")
              }
            }
            validObject(colonies)
            return(colonies)
          }
)

#' @describeIn Colonies-class Test if x is a Colonies class object
#' @export
isColonies <- function(x) {
  ret <- is(x, class2 = "Colonies")
  return(ret)
}
