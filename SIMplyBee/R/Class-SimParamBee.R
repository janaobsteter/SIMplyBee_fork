# TOOD: I have provided this as PullRequest for AlphaSimR
#       https://github.com/gaynorr/AlphaSimR/pull/52
#       once it gets incorporated there we should remove it here
#       https://github.com/HighlanderLab/SIMplyBee/issues/60
isPop <- function(x) {
  ret <- is(x, class2 = "Pop")
  return(ret)
}
setClassUnion("numericOrFunction", c("numeric", "function"))

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
#'   \code{SimParamBee} function. Before that, we highlight key points of
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
  classname = "SimParamBee",
  inherit = SimParam,

  # Public ----

  public = list(

    #' @field nWorkers numeric or function, default number of workers
    #'   to generate in a colony; if a function, it will be passed to other
    #'   functions and work with the internals of those functions - therefore
    #'   the function must be defined like \code{function(colony) someCode },
    #'   that is, it could operate on a colony or not, and return a single value
    nWorkers = "numericOrFunction",

    #' @field nDrones numeric or function, default number of drones
    #'   to generate in a colony; if a function, it will be passed to other
    #'   functions and work with the internals of those functions - therefore
    #'   the function must be defined like \code{function(colony) someCode },
    #'   that is, it could operate on a colony or not, and return a single value
    nDrones = "numericOrFunction",

    #' @field nVirginQueens numeric or function, default number of virgin queens
    #'   to generate in a colony; if a function, it will be passed to other
    #'   functions and work with the internals of those functions - therefore
    #'   the function must be defined like \code{function(colony) someCode },
    #'   that is, it could operate on a colony or not, and return a single value
    nVirginQueens = "numericOrFunction",

    #' @field nFathers numeric or function, default number of drones
    #'   a queen mates with; if a function, it will be passed to other
    #'   functions and work with the internals of those functions. The
    #'   function must return a single value.
    nFathers = "numericOrFunction",

    #' @field pSwarm numeric or a function, the percentage of workers that leave
    #'   with the old queen when the colony swarms; if a function, it will be passed to other
    #'   functions and work with the internals of those functions -  therefore
    #'   the function must be defined like \code{function(colony) someCode },
    #'   that is, it could operate on a colony or not, and return a single value
    #'   and return a single value.
    pSwarm = "numericOrFunction",

    #' @field pSplit numeric or a function, the percentage of workers that we remove
    #'   in a managed split; if a function, it will be passed to other
    #'   functions and work with the internals of those functions -  therefore
    #'   the function must be defined like \code{function(colony) someCode },
    #'   that is, it could operate on a colony or not, and return a single value
    #'   and return a single value.
    pSplit = "numericOrFunction",

    #' @description Starts the process of building a new simulation by creating
    #'   a new SimParamBee object and assigning a founder population to the this
    #'   object. It is recommended that you save the object with the name
    #'   \code{SP}, because subsequent functions will check your global
    #'   environment for an object of this name if their \code{simParamBee}
    #'   arguments are \code{NULL}. This allows you to call these functions
    #'   without explicitly supplying a \code{simParamBee} argument with every
    #'   call.
    #'
    #' @param founderPop \code{\link{MapPop-class}}, founder population of
    #'   genomes
    #'
    #' @param nWorkers numeric or function, default number of workers to
    #'   generate in a colony; the default value for this function is only to
    #'   have some workers to work with - you will want to change this!; see
    #'   also the \code{nWorkers} field description on using functions and their
    #'   examples in \code{\link{createWorkers}}
    #' @param nDrones numeric or function, default number of drones to
    #'   generate in a colony; the default value for this function is only to
    #'   have some drones to work with - you will want to change this!; see
    #'   also the \code{nDrones} field description on using functions and their
    #'   examples in \code{\link{createDrones}}
    #' @param nVirginQueens numeric or function, default number of virgin queens
    #'   to generate in a colony; the default value for this function is only to
    #'   have some virgin queens to work with - you will want to change this!;
    #'   see also the \code{nVirginQueens} field description on using functions
    #'   and their examples in \code{\link{createVirginQueens}}
    #'
    #' @param csdChr integer, chromosome that will carry the csd locus, by
    #'   default 3, but if there are less chromosomes (for a simplified
    #'   simulation), the locus is put on the last available chromosome (1 or
    #'   2); if \code{NULL} then csd locus is ignored in the simulation
    #' @param csdPos numeric, starting position of the csd locus on the
    #'   \code{csdChr} chromosome (relative at the moment, but could be in bp in
    #'   the future)
    #' @param nCsdAlleles integer, number of possible csd alleles (this
    #'   determines how many segregating sites will be needed to represent the
    #'   csd locus from the underlying bi-allelic SNP; the minimum number of
    #'   bi-allelic SNP needed is \code{log2(nCsdAlleles)}); if set to \code{0}
    #'   then \code{csdChr=NULL} is triggered
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
    initialize = function(founderPop,
                          nWorkers = 100, nDrones = 10,
                          nVirginQueens = 10,
                          csdChr = 3, csdPos = 0.865, nCsdAlleles = 100) {
      # Get all the goodies from AlphaSimR::SimParam$new(founderPop)
      super$initialize(founderPop)
      private$.versionSIMplyBee <- packageDescription("SIMplyBee")$Version

      # nWorkers, nDrones, and nVirginQueens ----

      self$nWorkers <- nWorkers
      self$nDrones <- nDrones
      self$nVirginQueens <- nVirginQueens

      # csd ----

      private$.csdChr <- NULL
      if (nCsdAlleles == 0) {
        csdChr <- NULL
      }
      if (!is.null(csdChr)) {
        # csd chromosome
        if (self$nChr < csdChr) {
          private$.csdChr <- self$nChr
          # message(paste0("There are less than 3 chromosomes, so putting csd locus on chromosome ", self$csdChr, "!"))
        } else {
          private$.csdChr <- csdChr
        }

        # csd position and sites
        private$.csdPos <- csdPos
        private$.nCsdAlleles <- nCsdAlleles
        private$.nCsdSites <- ceiling(log2(private$.nCsdAlleles))
        nLoci <- self$segSites[private$.csdChr]
        private$.csdPosStart <- floor(nLoci * private$.csdPos)
        csdPosStop <- private$.csdPosStart + private$.nCsdSites - 1
        if (csdPosStop > nLoci) {
          stop(paste0("Too few segregagting sites to simulate ", private$.nCsdAlleles, " csd alleles at the given position!"))
        } else {
          private$.csdPosStop <- csdPosStop
        }
        genMap <- self$genMap
        # Cancel recombination in the csd region to get non-recombining haplotypes as csd alleles
        genMap[[private$.csdChr]][private$.csdPosStart:private$.csdPosStop] <- 0
        self$switchGenMap(genMap)
      }

      invisible(self)
    }

  ),

  # Private ----

  private = list(
    .versionSIMplyBee = "character",
    .csdChr = "integerOrNULL",
    .csdPos = "numeric",
    .nCsdAlleles = "integer",
    .nCsdSites = "integer",
    .csdPosStart = "integer",
    .csdPosStop = "integer"
  ),

  # Active ----

  active = list(

    #' @field csdChr integer, chromosome of the csd locus
    csdChr = function(value) {
      if (missing(value)){
        private$.csdChr
      } else {
        stop("`$csdChr` is read only", call. = FALSE)
      }
    },

    #' @field csdPos numeric, starting position of the csd locus on the \code{csdChr}
    #'   chromosome (relative at the moment, but could be in bp in the future)
    csdPos = function(value) {
      if (missing(value)){
        private$.csdPos
      } else {
        stop("`$csdPos` is read only", call. = FALSE)
      }
    },

    #' @field nCsdAlleles integer, number of possible csd alleles
    nCsdAlleles = function(value) {
      if (missing(value)){
        private$.nCsdAlleles
      } else {
        stop("`$nCsdAlleles` is read only", call. = FALSE)
      }
    },

    #' @field nCsdSites integer, number of segregating sites representing the
    #'   csd locus
    nCsdSites = function(value) {
      if (missing(value)){
        private$.nCsdSites
      } else {
        stop("`$nCsdSites` is read only", call. = FALSE)
      }
    },

    #' @field csdPosStart integer, starting position of the csd locus
    csdPosStart = function(value) {
      if (missing(value)){
        private$.csdPosStart
      } else {
        stop("`$.csdPosStart` is read only", call. = FALSE)
      }
    },

    #' @field csdPosStop integer, ending position of the csd locus
    csdPosStop = function(value) {
      if (missing(value)){
        private$.csdPosStop
      } else {
        stop("`$csdPosStop` is read only", call. = FALSE)
      }
    },

    #' @field version list, versions of AlphaSimR and SIMplyBee packages used to
    #'   generate this object
    version = function(value) {
      if (missing(value)){
        list("AlphaSimR" = private$.version,
             "SIMplyBee" = private$.versionSIMplyBee)
      } else {
        stop("`$version` is read only", call. = FALSE)
      }
    }

  )

)

# isSimParamBee ----

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
