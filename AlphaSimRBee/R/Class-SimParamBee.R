#' @title Simulation parameters
#'
#' @description
#' Container for global simulation parameters. Saving this object
#' as SPBee will allow it to be accessed by function defaults.
#'
#' @export
SimParamBee <- R6Class(
  "SimParamBee",
  inherit = "SimParam",
  public = list(
    #### Public ----

    #' @field csdChr Chromosome of the csd locus
    csdChr = "integer",

    #' @field csdPos Starting position (in bp) of the csd locus on the chosen chromosome
    csdPos = "numeric",

    #' @field nCsdAlleles number of sites (i.e. length) representing the csd locus
    #' (if affects the number of possible alleles)
    nCsdSites = "integer"
    )
)

