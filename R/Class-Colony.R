# ---- Class Colony ----

setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("integerOrNULL", c("integer", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("PopOrNULL", c("Pop", "NULL"))
setClassUnion("NULLOrPop", c("NULL", "Pop"))
setClassUnion("integerOrNumericOrLogicalOrCharacter", c("integer", "numeric", "logical", "character"))

#' @rdname Colony-class
#' @title Honeybee colony
#'
#' @description An object holding honeybee colony
#'
#' @slot id integer, unique ID of the colony
#' @slot location numeric, location of the colony (x, y)
#' @slot queen \code{\link{Pop-class}}, the queen of the colony (we use
#'   its misc slot for queen's age and drones (fathers) she mated with)
#' @slot virginQueens \code{\link{Pop-class}}, virgin queens of the
#'   colony
#' @slot drones \code{\link{Pop-class}}, drones of the colony
#' @slot workers \code{\link{Pop-class}}, workers of the colony
#' @slot split logical, has colony split
#' @slot swarm logical, has colony swarmed
#' @slot supersedure logical, has colony superseded
#' @slot collapse logical, has colony collapsed
#' @slot production logical, is colony productive
#' @slot last_event character, the last event of the colony TODO: revise https://github.com/HighlanderLab/SIMplyBee/issues/10
#' @slot misc list, available for storing extra information about the colony
#'
#' @param object \code{\link{Colony-class}}
#' @param x \code{\link{Colony-class}}
#' @param ... \code{NULL}, \code{\link{Colony-class}}, or
#'   \code{\link{MultiColony-class}}
#'
#' @seealso \code{\link{createColony}}
#'
#' @return \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- cross(colony1, drones = droneGroups[[1]])
#' colony2 <- createColony(x = basePop[3])
#' colony2 <- cross(colony2, drones = droneGroups[[2]])
#' colony3 <- createColony(x = basePop[4])
#' colony3 <- cross(colony3, drones = droneGroups[[3]])
#'
#' colony1
#' show(colony1)
#' is(colony1)
#' isColony(colony1)
#'
#' apiary <- c(colony1, colony2)
#' is(apiary)
#' isMultiColony(apiary)
#'
#' c(apiary, colony3)
#' c(colony3, apiary)
#' @export
setClass(
  Class = "Colony",
  slots = c(
    id = "integer",
    location = "numeric",
    queen = "PopOrNULL",
    workers = "PopOrNULL",
    drones = "PopOrNULL",
    virginQueens = "PopOrNULL",
    split = "logicalOrNULL",
    swarm = "logicalOrNULL",
    supersedure = "logicalOrNULL",
    collapse = "logicalOrNULL",
    production = "logicalOrNULL",
    last_event = "character",
    misc = "listOrNULL"
  )
)

#' @describeIn Colony-class Test if x is a Colony class object
#' @export
isColony <- function(x) {
  ret <- is(x, class2 = "Colony")
  return(ret)
}

setClassUnion("ColonyOrNULL", c("Colony", "NULL"))

setValidity(Class = "Colony", method = function(object) {
  errors <- character()
  if (isQueenPresent(object) && nInd(getQueen(object)) > 1) {
    errors <- c(errors, "There can be only one queen per colony!")
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
})

# show ----
#' @describeIn Colony-class Show colony object
setMethod(
  f = "show",
  signature(object = "Colony"),
  definition = function(object) {
    cat("An object of class", classLabel(class(object)), "\n")
    cat("Id:", getId(object), "\n")
    cat("Location:", getLocation(object), "\n")
    cat("Queen:", getId(object@queen), "\n")
    cat("Number of fathers:", nFathers(object), "\n")
    cat("Number of workers:", nWorkers(object), "\n")
    cat("Number of drones:", nDrones(object), "\n")
    cat("Number of virgin queens:", nVirginQueens(object), "\n")
    cat("Has split:", object@split, "\n")
    cat("Has swarmed:", object@swarm, "\n")
    cat("Has superseded:", object@supersedure, "\n")
    cat("Has collapsed:", object@collapse, "\n")
    cat("Is productive:", object@production, "\n")
    invisible()
  }
)

# c ----
#' @describeIn Colony-class Combine multiple colony objects
setMethod(
  f = "c",
  signature(x = "ColonyOrNULL"),
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

#' @rdname combineNULLAndPop
#' @title Combine a NULL and AlphaSimR population
#'
#' @param x NULL or \code{\link{Pop-class}}
#' @param ... list of NULL or \code{\link{Pop-class}} objects
#'
#' @description This combine \code{c()} method is a hack to combine NULL and an
#'   AlphaSimR population object \code{c(NULL, pop)} (\code{c(pop, NULL)} works
#'   already with AlphaSimR package code).
setMethod(
  f = "c",
  signature(x = "NULLOrPop"),
  definition = function(x, ...) {
    if (is.null(x)) {
      nList = length(list(...))
      pop <- list(...)[[1]]
      if (nList > 1) {
        for (y in list(...)[[2:nList]]) {
          if (is(y, class2 = "NULL")) {
            # Do nothing
          } else if (isPop(y)) {
            pop <- c(pop, y)
          } else {
            stop("... must be a NULL or Pop class object!")
          }
        }
      }
    }
    validObject(pop)
    return(pop)
  }
)
