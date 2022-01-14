# TOOD: I have provided this as PullRequest for AlphaSimR
#       https://github.com/gaynorr/AlphaSimR/pull/31/commits/ecfa286a05f7e2f3f54ec5bc3a741b1786d183c4
#       once it gets incorporated there we should remove it here
#       https://github.com/HighlanderLab/SIMplyBee/issues/60
isPop <- function(x) {
  ret <- is(x, class2 = "Pop")
  return(ret)
}

# Class Colony ----

setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("integerOrNULL", c("integer", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("PopOrNULL", c("Pop", "NULL"))

#' @rdname Colony-class
#' @title Honeybee colony
#'
#' @description An object holding honeybee colony
#'
#' @slot id character, ID of the colony, which is equal to the ID of the queen
#' @slot location numeric, location of the colony (x, y)
#' @slot queen \code{\link{Pop-class}}, the queen of the colony (we use
#'   its misc slot for queen's age and drones (fathers) she mated with)
#' @slot virginQueens \code{\link{Pop-class}}, virgin queens of the
#'   colony
#' @slot drones \code{\link{Pop-class}}, drones of the colony
#' @slot workers \code{\link{Pop-class}}, workers of the colony
#' @slot pheno matrix, phenotypes of the colony TODO: revise
#' @slot split logical, has colony split
#' @slot swarm logical, has colony swarmed
#' @slot supersedure logical, has colony superseded
#' @slot collapse logical, has colony collapsed TODO: revise
#' @slot production logical, is colony productive
#' @slot last_event character, the last event of the colony TODO: we probably don't need this
#' @slot misc list, available for storing extra information about the colony
#'
#' @param object \code{\link{Colony-class}}
#' @param x \code{\link{Colony-class}}
#' @param ... \code{NULL}, \code{\link{Colony-class}}, or
#'   \code{\link{Colonies-class}}
#'
#' @seealso \code{\link{createColony}}
#'
#' @return \code{\link{Colony-class}} or \code{\link{Colonies-class}}
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
#'
#' colony1
#' show(colony1)
#' is(colony1)
#' isColony(colony1)
#'
#' apiary <- c(colony1, colony2)
#' is(apiary)
#' isColonies(apiary)
#'
#' c(apiary, colony3)
#' c(colony3, apiary)
#'
#' @export
setClass(Class = "Colony",
         slots = c(id = "characterOrNULL",
                   location = "numericOrNULL",
                   queen = "PopOrNULL",
                   virginQueens = "PopOrNULL",
                   workers = "PopOrNULL",
                   drones = "PopOrNULL",
                   pheno = "matrix",
                   split = "logical",
                   #remnant = "",
                   swarm = "logicalOrNULL",
                   supersedure = "logicalOrNULL",
                   #rob = "logical",
                   collapse = "logicalOrNULL",
                   production = "logicalOrNULL",
                   last_event = "character",
                   misc = "listOrNULL"
         ))

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
  if (isQueenPresent(object) && !isQueenMated(object)) {
    errors <- c(errors, "Queen must be mated to be in the queen slot!")
  }
  if (!isQueenPresent(object) && !is.na(getId(object))) {
    errors <- c(errors, "When queen is absent, the colony ID should be NA!")
  }
  if (isQueenPresent(object) && getId(object) != getQueen(object)@id) {
    errors <- c(errors, "Colony and queen IDs don't match!")
  }
  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
})

#' @describeIn Colony-class Show colony object
setMethod(f = "show",
          signature(object = "Colony"),
          definition = function(object) {
            cat("An object of class", classLabel(class(object)), "\n")
            cat("Id:", getId(object),"\n")
            cat("Location:", getLocation(object),"\n")
            cat("Queen:", getId(object@queen),"\n")
            cat("Number of fathers:", nFathers(object), "\n")
            cat("Number of virgin queens:", nVirginQueens(object),"\n")
            cat("Number of workers:", nWorkers(object), "\n")
            cat("Number of drones:", nDrones(object),"\n")
            cat("Has split:", object@split, "\n")
            cat("Has swarmed:", object@swarm, "\n")
            cat("Has superseded:", object@supersedure, "\n")
            cat("Has collapsed:", object@collapse, "\n")
            cat("Is productive:", object@production, "\n")
            invisible()
          }
)

#' @describeIn Colony-class Combine multiple colony objects
setMethod(f = "c",
          signature(x = "ColonyOrNULL"),
          definition = function(x, ...) {
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

#' @describeIn Colonies-class Assign colony into colonies
setReplaceMethod(f = "[[",
                 signature(x = "Colonies", i = "integerOrNumericOrLogicalOrCharacter",
                           j = "ANY", value = "Colony"),
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
                 })
