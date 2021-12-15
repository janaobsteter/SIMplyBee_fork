#' @describeIn Pop Test if object is a Pop class object
# TOOD: I have provided this as PullRequest for AlphaSimR https://github.com/gaynorr/AlphaSimR/pull/31/commits/ecfa286a05f7e2f3f54ec5bc3a741b1786d183c4
#   once it gets incorporated there we should remove it here
isPop = function(x) {
  ret = is(x, class = "Pop")
  return(ret)
}

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
            cat("Id:", getId(object),"\n")
            cat("Location:", getLocation(object),"\n")
            cat("Queen:", getId(object@queen),"\n")
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

#' @describeIn Colony Test if object is a Colony class object
isColony = function(x) {
  ret = is(x, class2 = "Colony")
  return(ret)
}
