# Class Colonies ----

#' @rdname Colonies
#' @title Honeybee colonies
#'
#' @description An object holding a collection of honeybee colonies. It behaves
#'   like a list.
#'
#' @slot colonies list, a collection of \code{\link{Colony-class}} objects
#'
#' @param x \code{\link{Colonies-class}}
#' @param i numeric or character, index or name to select a colony
#'
#' @seealso \code{\link{createColonies}}
#'
#' @return \code{\link{Colonies-class}}
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
#' getId(apiary)
#' apiary[1]
#' getId(apiary[1])
#' getId(apiary["2"])
#' getId(apiary[2])
#'
#' getId(apiary)
#' getId(apiary[c(1, 3)])
#' getId(apiary[c("2", "4")])
#'
#' apiary[[1]]
#' apiary[["2"]]
#' apiary[[3]]
#' apiary[["4"]]
#'
#' getId(c(apiary[c(1, 3)], apiary[2]))
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

#' @describeIn Colonies Show colonies object
setMethod(f = "show",
          signature(object = "Colonies"),
          function (object) {
            cat("An object of class", classLabel(class(object)), "\n")
            cat("Number of colonies:", nColonies(object), "\n")
            invisible()
          }
)

#' @describeIn Colonies Extract a colony (one or more!) by index (return \code{\link{Colonies-class}})
setClassUnion("integerOrNumeric", c("integer", "numeric"))
setMethod(f = "[",
          signature(x = "Colonies", i = "integerOrNumeric"),
          function(x, i) {
            x@colonies <- x@colonies[as.integer(i)]
            validObject(x)
            return(x)
          }
)

#' @describeIn Colonies Extract a colony (one or more!) by name (character) (return \code{\link{Colonies-class}})
setMethod(f = "[",
          signature(x = "Colonies", i = "character"),
          function(x, i) {
            ret <- x[match(x = i, table = getId(x))]
            validObject(ret)
            return(ret)
          }
)

#' @describeIn Colonies Extract a colony (just one!) by index (numeric) (return \code{\link{Colony-class}})
setMethod(f = "[[",
          signature(x = "Colonies", i = "integerOrNumeric"),
          function(x, i) {
            n <- length(i)
            if (n > 1) {
              warning(paste("Selecting only the first colony out of ", n, " requested\n"))
            }
            ret <- x@colonies[[i[1L]]]
            validObject(ret)
            return(ret)
          }
)

#' @describeIn Colonies Extract a colony (just one!) by name (character) (return \code{\link{Colony-class}})
setMethod(f = "[[",
          signature(x = "Colonies", i = "character"),
          function(x, i) {
            n <- length(i)
            if (n > 1) {
              warning(paste("Selecting only the first colony out of ", n, " requested\n"))
            }
            ret <- x[i[1L]]@colonies
            validObject(ret)
            return(ret)
          }
)

#' @describeIn Colonies Combine multiple colony and colonies objects
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

#' @describeIn Colonies Test if object is a Colonies class object
isColonies <- function(x) {
  ret <- is(x, class2 = "Colonies")
  return(ret)
}
