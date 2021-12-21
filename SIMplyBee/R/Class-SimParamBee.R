#' @rdname SimParamBee
#' @title Honeybee simulation parameters
#'
#' @description Container for global honeybee simulation parameters. Saving this
#'   object as \code{SP} will allow it to be accessed by SIMplyBee functions and
#'   many defaults can be used from the \code{SP}. \code{SimParamBee} inherits
#'   from AlphaSimR \code{\link{SimParam}}, so all \code{\link{SimParam}} slots
#'   and functions are available in addition to \code{SimParamBee}-specific
#'   slots and functions. Some \code{\link{SimParam}} functions could have
#'   upgraded behaviour as documented below in line with honeybee biology.
#'
#' @details This documentation shows details specific to \code{SimParamBee}. We
#'   suggest you also read all the options provided by the AlphaSimR
#'   \code{\link{SimParam}}. Below we show minimal usage cases for each
#'   \code{SimParamBee} function and before that we highlight key points of
#'   honeybee biology in relation to implementation in the SIMplyBee package.
#'
#' In honeybees, complementary sex determining (csd) locus impacts sex of
#' individuals on top of haplo-diploidy, where diploids are queens or workers
#' and haploids are drones. Heterozygous individuals at the csd locus become
#' queens or workers, while homozygous individuals at the csd locus become
#' unviable "drones". Hence genotype status at the csd locus is critical for
#' honeybee simulations. In SIMplyBee, the csd locus is implemented as a series
#' of bi-allelic SNP that don't recombine. Technically speaking these are
#' haplotypes, but since they don't recombine, we call them alleles. By varying
#' the number of SNP we can tune the number of csd alleles \code{nCsdHaplo}.
#' Individuals that are homozygous at the csd locus are not viable and removed
#' from simulation - see \code{\link{pHomBrood}}.
#'
#' @export
SimParamBee <- R6Class(
  "SimParamBee",
  inherit = SimParam,
  public = list(
    # Public ----

    #' @field csdChr integer, chromosome of the csd locus
    csdChr = "integerOrNULL",

    #' @field csdPos numeric, starting position of the csd locus on the
    #'   \code{csdChr} chromosome (relative at the moment, but could be in bp in
    #'   the future)
    csdPos = "numericOrNULL",

    #' @field nCsdAlleles integer, number of possible csd alleles
    nCsdAlleles = "integerOrNULL",

    #' @field nCsdSites integer, number of segregating sites representing the
    #'   csd locus
    nCsdSites = "integerOrNULL",

    #' @field csdPosStart integer, starting position of the csd locus (this is
    #'   worked out internally based on \code{csdPos})
    csdPosStart = "integerOrNULL",

    #' @field csdPosStop integer, ending position of the csd locus (this is
    #'   worked out internally based on \code{csdPosStart} and \code{nCsdSites})
    csdPosStop = "integerOrNULL",

    #' @description Starts the process of building a new simulation by creating
    #'   a new SimParamBee object and assigning a founder population to the
    #'   class. It is recommended that you save the object with the name "SP",
    #'   because subsequent functions will check your global environment for an
    #'   object of this name if their \code{simParamBee} arguments are
    #'   \code{NULL}. This allows you to call these functions without explicitly
    #'   supplying a \code{simParamBee} argument with every call.
    #'
    #' @param founderPop \code{\link{MapPop-class}}, founder population of
    #'   haplotypes
    #' @param csdChr integer, chromosome that will carry the csd locus, by
    #'   default 3, but if there are less chromosomes (for a simplified
    #'   simulation), the locus is put on the last available chromosome (1 or
    #'   2); if \code{NULL} then csd locus is ignored in the simulation
    #' @param csdPos numeric, starting position of the csd locus on the
    #'   \code{csdChr} chromosome (relative at the moment, but could be in bp in
    #'   the future)
    #' @param nCsdAlleles integer, number of possible csd alleles (this
    #'   determines how many segregating sites will be needed to represent the
    #'   csd locus from the underlying bi-allelic SNP; that is the minimum
    #'   number of bi-allelic SNP needed is \code{log2(nCsdAlleles)})
    #'
    #' @examples
    #' founderGenomes <- quickHaplo(nInd = 10, nChr = 3, segSites = 10)
    #' SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 2)
    #'
    #' # We need enough segregating sites
    #' try(SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 100))
    #' founderGenomes <- quickHaplo(nInd = 10, nChr = 3, segSites = 100)
    #' SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 100)
    #'
    #' # We can save the csd locus on chromosome 1 or 2, too, for quick simulations
    #' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
    #' SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 100)
    # TODO: use the max number of csd alleles found in literature and cite that
    #       https://github.com/HighlanderLab/SIMplyBee/issues/93
    initialize = function(founderPop, csdChr = 3, csdPos = 0.865, nCsdAlleles = 100) {
      # Get all the goodies from AlphaSimR::SimParam$new(founderPop)
      super$initialize(founderPop)

      # csd ----
      self$csdChr <- NULL
      if (!is.null(csdChr)) {
        # csd chromosome
        if (self$nChr < csdChr) {
          self$csdChr <- self$nChr
          # message(paste0("There are less than 3 chromosomes, so putting csd locus on chromosome ", self$csdChr, "!"))
        } else {
          self$csdChr <- csdChr
        }

        # csd position and sites
        self$csdPos <- csdPos
        self$nCsdAlleles <- nCsdAlleles
        self$nCsdSites <- ceiling(log2(self$nCsdAlleles))
        nLoci <- self$segSites[self$csdChr]
        self$csdPosStart <- floor(nLoci * self$csdPos)
        csdPosStop <- self$csdPosStart + self$nCsdSites - 1
        if (csdPosStop > nLoci) {
          stop(paste0("Too few segregagting sites to simulate ", self$nCsdAlleles, " csd alleles at the given position!"))
        } else {
          self$csdPosStop <- csdPosStop
        }
        genMap <- self$genMap
        # Cancel recombination in the csd region to get non-recombining haplotypes as csd alleles
        genMap[[self$csdChr]][self$csdPosStart:self$csdPosStop] <- 0
        self$switchGenMap(genMap)
      }
    }
  )
)

#' @rdname isSimParamBee
#' @title Test if x is a SimParamBee class object
#'
#' @description Test if x is a \code{\link{SimParamBee}} class object
#'
#' @param x \code{\link{SimParamBee}}
#'
#' @return logical
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' isSimParamBee(SP)
#'
#' @export
isSimParamBee <- function(x) {
  ret <- is(x, class2 = "SimParamBee")
  return(ret)
}

# TODO: remove this once AlphaSimR exports getNumThreads (pull request accepted)
# https://github.com/HighlanderLab/SIMplyBee/issues/75
getNumThreads <- AlphaSimR:::getNumThreads()
