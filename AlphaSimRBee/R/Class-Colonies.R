# Class Colonies ----

#' @rdname Colonies
#' @title Colonies
#'
#' @description
#' The Colonies represents a set of colonies. It behaves like a list object.
#'
#' @param x Colonies, colonies
#' @param i numeric or character, index or name to select a colony
#'
#' @slot colonies, a collection of \code{\link{Colony-class}} objects
#'
#' @seealso \code{\link{selectColonies}}, \code{\link{addColonyToColonies}}
#'
#' @export
setClass("Colonies",
         slots = c(colonies = "list"))

#' @describeIn Colonies Show colonies object
setMethod("show",
          signature(object = "Colonies"),
          function (object) {
            cat("An object of class", classLabel(class(object)), "\n")
            cat("Number of colonies:", nColonies(object), "\n")
            invisible()
          }
)

#' @describeIn Colonies Extract a colony (one or more!) by index (return \code{\link{Colonies-class}})
setMethod("[",
          signature(x = "Colonies"),
          function(x, i) {
            x@colonies <- x@colonies[i]
            return(x)
          }
)

#' @describeIn Colonies Extract a colony (one or more!) by name (character) (return \code{\link{Colonies-class}})
setMethod("[",
          signature(x = "Colonies", i = "character"),
          function(x, i) {
            ret <- x[match(x = i, table = getId(x))]
            return(ret)
          }
)

#' @describeIn Colonies Extract a colony (just one!) by index (numeric) (return \code{\link{Colony-class}})
setMethod("[[",
          signature(x = "Colonies"),
          function(x, i) {
            n <- length(i)
            if (n > 1) {
              warning(paste("Selecting only the first colony out of ", n, " requested\n"))
            }
            ret <- x@colonies[[i[1]]]
            return(ret)
          }
)

#' @describeIn Colonies Extract a colony (just one!) by name (character) (return \code{\link{Colony-class}})
setMethod("[[",
          signature(x = "Colonies", i = "character"),
          function(x, i) {
            n <- length(i)
            if (n > 1) {
              warning(paste("Selecting only the first colony out of ", n, " requested\n"))
            }
            ret <- x[i[1]]@colonies
            return(ret)
          }
)

#' @describeIn Colonies Combine multiple colony and colonies
setMethod("c",
          signature(x = "Colonies"),
          function(x, ...) {
            for (y in list(...)) {
              if (class(y) == "NULL") {
                # Do nothing
              } else {
                if (class(y) == "Colony") {
                  x@colonies <- c(x@colonies, y)
                } else {
                  stopifnot(class(y) == "Colonies")
                  x@colonies <- c(x@colonies, y@colonies)
                }
              }
            }
            return(x)
          }
)

#' @describeIn Colonies Test if object is a Colonies class object
isColonies = function(x) {
  ret = is(x, class2 = "Colonies")
  return(ret)
}
