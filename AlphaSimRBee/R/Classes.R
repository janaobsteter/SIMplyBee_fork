# Class Colony ----

#' @rdname Colony
#' @method createColony
#' @title Creates a honeybee colony
#' @usage \method{createColony}(id, location, queen, drones, workers, virgin_queen, fathers, pheno, last_event)
#' @description List. Creates a honeybee colony as a list with the following elements:
#'   \id location, queen, drones, workers, virgin_queens, pheno, fathers, and last event
#'   \All elements of the list, expect for \code{last_even}, are assumed NULL if not specified
#'   \otherwise. \code{last_event} is set to "new_colony".
#' @slot id ID of the colony.
#' @slot location Numeric, location of the colony (x, y).
#' @slot queen AlphaSimR population object to become the queen of the colony (we use its misc slot for queen's age).
#' @slot drones AlphaSimR population object to become the drones of the colony.
#' @slot workers AlphaSimR population object to become the workers of the colony.
#' @slot virgin_queens AlphaSimR individual or population object to become the virgin queen(s) of the colony.
#' @slot pheno A matrix of the phenotypes of the colony
#' @slot swarm Logical, whether the colony has swarmed
#' @slot split Logical, whether the colony has split
#' @slot supersedure Logical, whether the colony has superseded
#' @slot collapse Logical, whether the colony has collapsed
#' @slot production Logical, whether the colony produces hive products
#' @slot last_event Character, the last event of the colony #TODO: WE probably don't need this
#' @slot misc A list, normally empty and exists solely as an open slot available for uses to store extra information about individuals.
#'
#' @example inst/examples/examples_createColony.R
#' @return Returns a \code{Colony} object
#' @export

setClassUnion("PopOrNULL", c("Pop", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClass("Colony",
         slots = c(id = "characterOrNULL",
                   location = "numericOrNULL",
                   queen = "PopOrNULL",
                   drones = "PopOrNULL",
                   workers = "PopOrNULL",
                   virgin_queens = "PopOrNULL",
                   pheno = "matrix",
                   split = "logical",
                   #remnant = "",
                   swarm = "logical",
                   supersedure = "logical",
                   collapse = "logical",
                   #rob = "logical",
                   production = "logical",
                   last_event = "character",
                   misc = "listOrNULL"
         ))

#' @describeIn Colony Show colony object
setMethod("show",
          signature(object = "Colony"),
          function(object) {
            cat("An object of class", classLabel(class(object)), "\n")
            cat("Id:", ifelse(is.null(object@id), NA, object@id),"\n")
            cat("Location:", ifelse(is.null(object@location), NA, object@location),"\n")
            cat("Queen:", ifelse(is.null(object@queen), NA, object@queen@id),"\n")
            cat("Fathers:", nFathers(object), "\n")
            cat("Virgin queens:", nVirginQueens(object),"\n")
            cat("Workers:", nWorkers(object), "\n")
            cat("Drones:", nDrones(object),"\n")
            cat("Split:", object@split, "\n")
            cat("Swarmed:", object@swarm, "\n")
            cat("Superseded:", object@supersedure, "\n")
            cat("Collapsed:", object@collapse, "\n")
            cat("Production:", object@production, "\n")
            invisible()
          }
)

#' @describeIn Colony Combine multiple colony objects into a set of colonies \code{\link{Colonies-class}}
setMethod("c",
          signature(x = "Colony"),
          function(x, ...) {
            colonies <- createColonies(x, ...)
            return(colonies)
          }
)

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
