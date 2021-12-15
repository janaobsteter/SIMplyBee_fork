#' @title Simulation parameters
#'
#' @description
#' Container for global simulation parameters. Saving this object
#' as SPBee will allow it to be accessed by function defaults.
#'
#' @export
SimParamBee <- R6Class(
  "SimParamBee",
  inherit = SimParam,
  public = list(
    #### Public ----

    #' @field csdChr Chromosome of the csd locus
    csdChr = "integer",

    #' @field csdPos Starting position (in bp) of the csd locus on the chosen chromosome
    csdPos = "numeric",

    #' @field nCsdAlleles number of sites (i.e. length) representing the csd locus
    #' (if affects the number of possible alleles)
    nCsdSites = "integer",

    #' @field nCsdHaplo Number of possible csd alleles
    nCsdHaplo = "integer",

    #' @field csdPosStart Starting absolute position (in bp) of the csd locus
    csdPosStart = "integer",

    #' @field csdPosStop Ending absolute position (in bp) of the csd locus
    csdPosStop = "integer",

    #' @description Starts the process of building a new simulation
    #' by creating a new SimParam object and assigning a founder
    #' population to the class. It is recommended that you save the
    #' object with the name "SP", because subsequent functions will
    #' check your global environment for an object of this name if
    #' their simParam arguments are NULL. This allows you to call
    #' these functions without explicitly supplying a simParam
    #' argument with every call.
    #'
    #' @param founderPop an object of \code{\link{MapPop-class}}
    #'
    #' @examples
    #' #Create founder haplotypes
    #' founderPop = quickHaplo(nInd=10, nChr=1, segSites=10)
    #'
    #' #Set simulation parameters
    #' SP = SimParam$new(founderPop)
    addCsd = function( csdChr = NULL, csdPos = NULL, nCsdHaplo = NULL){

      # Public items
      self$csdChr <- ifelse(is.null(csdChr), 3, csdChr)
      self$csdPos <- ifelse(is.null(csdPos), 0.865, csdPos)
      self$nCsdHaplo <- ifelse(is.null(nCsdHaplo), 32, nCsdHaplo)

      # self items
      self$nCsdSites <- log2(self$nCsdHaplo)

      if (self$nChr < self$csdChr) {
        stop("Csd chosen to be on chromosome ", self$csdChr, " but we only have ", self$nChr, " chromosome(s).")
      }

      nLoci = self$segSites[self$csdChr]
      self$csdPosStart <- floor(nLoci * self$csdPos)
      csdPosStop <- self$csdPosStart + self$nCsdSites
      if (csdPosStop > nLoci) {
        stop(paste0("Too few segregagting sites to simulate ", self$nCsdHaplo, " csd haplotypes at the given position!"))
      } else {
        self$csdPosStop = csdPosStop
      }


    }
  )
)

getNumThreads = AlphaSimR:::getNumThreads()
