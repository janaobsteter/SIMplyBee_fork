###############################################################################
# Class Colonies----

#' @title Colonies
#' #'
#' @description
#' The Colonies represents the list of the colonies.
#' It is designed to behave like a list of colonies.
#'
#' @param x a 'Colonies' object
#' @param i index of populations or colonies
#'
#' @slot colonies of \code{\link{Colony-class}} and/or
#' \code{Colonies-class}
#'
#'
#' @export
setClass("Colonies",
         slots=c(colonies="list"))


#' @describeIn Colonies Extract Colonies by index
setMethod("[",
          signature(x = "Colonies"),
          function(x, i){
            x@colonies = x@colonies[i]
            return(x)
          }
)

#' @describeIn Colonies Extract Colony by index
setMethod("[[",
          signature(x = "Colonies"),
          function (x, i){
            return(x@colonies[[i]])
          }
)

#' @describeIn Colonies Extract Colony by index
# setMethod("[[",
#           signature(x = "Colonies", i = "character"),
#           function (x, i){
#             tmp = selectColonies(x, ID = i)
#             return(tmp@colonies)
#           }
# )

#' @describeIn Colonies Combine multiple Coloniess
setMethod("c",
          signature(x = "Colonies"),
          function (x, ...){
            for(y in list(...)){
              if(class(y)=="NULL"){
                # Do nothing
              } else {
                if(class(y)=="Colony"){
                  x@colonies = c(x@colonies, y)
                }else{
                  stopifnot(class(y)=="Colonies")
                  x@colonies = c(x@colonies, y@colonies)
                }
              }
            }
            return(x)
          }
)


#' @describeIn 
setMethod("show",
          signature(object = "Colonies"),
          function (object){
            cat("An object of class",
                classLabel(class(object)), "\n")
            cat("Number of colonies:", nColonies(object), "\n")
            invisible()
          }
)




# Class Colony----

#' @rdname createColony
#' @method createColony
#' @title Creates a honeybee colony
#' @usage \method{createColony}(id, location, queen, drones, workers, virgin_queen, fathers, pheno, last_event)
#' @description List. Creates a honeybee colony as a list with the following elements:
#'   \id location, queen, drones, workers, virgin_queens, pheno, fathers, and last event
#'   \All elements of the list, expect for \code{last_even}, are assumed NULL if not specified
#'   \otherwise. \code{last_event} is set to "new_colony".
#'   # TODO: we will likely need queen age too - but that should go into colony@queen@misc slot!
# TODO: can also look at hive "strength" based on number of colony workers
#' @slot id ID of the colony.
#' @slot location Numeric, location of the colony (x, y).
#' @slot queen AlphaSimR population object to become the queen of the colony.
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
#' @return Returns AlphaSimR class "Colony" object.
#' @export

setClassUnion("PopOrNULL", c("Pop", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClass("Colony",
         slots=c(id="character",
                 location="numericOrNULL",
                 queen="PopOrNULL",
                 drones="PopOrNULL",
                 workers="PopOrNULL",
                 virgin_queens="PopOrNULL",
                 pheno="matrix",
                 swarm="logical",
                 split="logical",
                 #remnant="",
                 supersedure="logical",
                 collapse="logical",
                 #rob="logical",
                 production="logical",
                 last_event="character",
                 misc="listOrNULL"
         ))

#' @describeIn Colony Show colony summary
setMethod("c",
          signature(x = "Colony"),
          function (x, ...){
            Colonies = createColonies(x, ...)
            return(Colonies)
          }
)

#' @describeIn 
# setMethod("list",
#           signature(x = "Colony"),
#           function (x, ...){
#             Colonies = createColonies(x, ...)
#             return(Colonies)
#           }
# )

#' @describeIn 
setMethod("show",
          signature(object = "Colony"),
          function (object){
            cat("An object of class",
                classLabel(class(object)), "\n")
            cat("Id:", ifelse(!is.null(object@id), object@id, 0),"\n")
            cat("Location:", ifelse(!is.null(object@location), object@location, 0),"\n")
            cat("Queens:", ifelse(!is.null(object@queen), object@queen@nInd, 0),"\n")
            cat("Virgin queens:", nVirginQueens(object),"\n")
            cat("Drones:", nDrones(object),"\n")
            cat("Workers:", nWorkers(object), "\n")
            cat("Fathers:", nFathers(object), "\n")
            cat("Events:", paste(if(object@swarm) "swarm", if(object@split) "split", 
                                 if(object@supersedure) "supersede", if(object@collapse) "collapse"), "\n")
            cat("Production:", object@production, "\n")
            invisible()
          }
)
