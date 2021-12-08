#' @title Simulation parameters
#'
#' @description
#' Container for global simulation parameters. Saving this object
#' as SP will allow it to be accessed by function defaults.
#'
#' @export
SimParamBee = R6Class(
  "SimParamBee",
  public = list(
    #### Public ----

    #' @field csdChr Chromosome of the csd locus
    csdChr = "integer",

    #' @field csdPos Starting position (in bp) of the csd locus on the chosen chromosome
    csdPos = "numeric",

    #' @field nCsdAlleles number of sites (i.e. length) representing the csd locus
    #' (if affects the number of possible alleles)
    nCsdSites = "integer",


    #' @description Starts the process of building a new honeybee simulation
    #' by creating a new SimParamBee object and assigning a founder
    #' population to the class. It is recommended that you save the
    #' object with the name "SPBee", because subsequent functions will
    #' check your global environment for an object of this name if
    #' their simParamBee arguments are NULL. This allows you to call
    #' these functions without explicitly supplying a simParamBee
    #' argument with every call. If initializing the SPBee you should
    #' supply the number of sites to represent the csd locus since this
    #' dictates the number of possible csd alleles. If you choose 2 sites,
    #' the number of possible csd alleles is 2**2 = 4, if you choose 3
    #' sites, the possible number of alleles in 2**3 = 8, etc.
    #'
    #' @param founderPop an object of \code{\link{MapPop-class}}
    #'
    #' @examples
    #' #Create founder haplotypes
    #' founderPop = quickHaplo(nInd=10, nChr=1, segSites=10)
    #'
    #' #Set simulation parameters
    #' SP = SimParam$new(founderPop)
    #' #Set honeybee simulation parameters
    #' SPBee = SimParamBee$new(founderPop, nCsdSites = 3)
    #
    initialize = function(founderPop){
      stopifnot(class(founderPop)=="MapPop")

      # Public items
      self$nCsdSites = 3

      # Private items
      private$.csdChr = 3
      private$.csdPos =  0.865
      private$.csdHaplo = 2 ** self$nCsdSites

      invisible(self)
    },


    #I'VE LEFT THE BELOW IN TO SERVE AS A TEMPLATE IF WE DECIDE TO ADD FUNTIONS

    #' #' @description Changes how sexes are determined in the simulation.
    #' #' The default sexes is "no", indicating all individuals are hermaphrodites.
    #' #' To add sexes to the simulation, run this function with "yes_sys" or
    #' #' "yes_rand". The value "yes_sys" will systematically assign
    #' #' sexes to newly created individuals as first male and then female.
    #' #' Populations with an odd number of individuals will have one more male than
    #' #' female. The value "yes_rand" will randomly assign a sex to each
    #' #' individual.
    #' #'
    #' #' @param sexes acceptable value are "no", "yes_sys", or
    #' #' "yes_rand"
    #' #' @param force should the check for a running simulation be
    #' #' ignored. Only set to TRUE if you know what you are doing.
    #' #'
    #' #' @examples
    #' #' #Create founder haplotypes
    #' #' founderPop = quickHaplo(nInd=10, nChr=1, segSites=10)
    #' #'
    #' #' #Set simulation parameters
    #' #' SP = SimParam$new(founderPop)
    #' #' SP$setSexes("yes_sys")
    #' setSexes = function(sexes, force=FALSE){
    #'   if(!force){
    #'     private$.isRunning()
    #'   }
    #'   sexes = tolower(sexes)
    #'   if(sexes=="no"){
    #'     private$.sexes="no"
    #'   }else if(sexes=="yes_sys"){
    #'     private$.sexes="yes_sys"
    #'   }else if(sexes=="yes_rand"){
    #'     private$.sexes="yes_rand"
    #'   }else{
    #'     stop(paste0("sexes=",sexes," is not a valid option"))
    #'   }
    #'   invisible(self)
    #' }
  )
)

