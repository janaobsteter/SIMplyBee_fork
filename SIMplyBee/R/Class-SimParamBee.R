# TODO: I have provided this as PullRequest for AlphaSimR
#       https://github.com/gaynorr/AlphaSimR/pull/52
#       once it gets incorporated there we should remove it here
#       https://github.com/HighlanderLab/SIMplyBee/issues/60
isPop <- function(x) {
  ret <- is(x, class2 = "Pop")
  return(ret)
}

# TODO: This should go to AlphaSimR too
#       https://github.com/HighlanderLab/SIMplyBee/issues/196
isMapPop <- function(x) {
  ret <- is(x, class2 = "MapPop")
  return(ret)
}

setClassUnion("numericOrFunction", c("numeric", "function"))

#' @rdname SimParamBee
#' @title Honeybee simulation parameters
#'
#' @description Container for global honeybee simulation parameters. Saving this
#'   object as \code{SP} will allow it to be accessed by SIMplyBee functions
#'   without repeatedly (and annoyingly!) typing out
#'   \code{someFun(..., simParamBee = SP)}. \code{SimParamBee} inherits
#'   from AlphaSimR \code{\link{SimParam}}, so all \code{\link{SimParam}} slots
#'   and functions are available in addition to \code{SimParamBee}-specific
#'   slots and functions. Some \code{\link{SimParam}} functions could have
#'   upgraded behaviour as documented in line with honeybee biology.
#'
#' @details This documentation shows details specific to \code{SimParamBee}. We
#'   suggest you also read all the options provided by the AlphaSimR
#'   \code{\link{SimParam}}. Below we show minimal usage cases for each
#'   \code{SimParamBee} function.
#'
#' See also \code{vignette(package = "SIMplyBee")} for descriptions of how
#'   SIMplyBee implements the specific honeybee biology.
#'
#' @export
SimParamBee <- R6Class(
  classname = "SimParamBee",
  inherit = SimParam,

  # Public ----

  public = list(

    # nWorkers field ----
    #' @field nWorkers numeric or function, a number of workers generated in a
    #'   colony - used in \code{\link{createWorkers}}, \code{\link{addWorkers}},
    #'   \code{\link{buildUpColony}}, and \code{\link{buildUpColonies}}.
    #'
    #'   The default value is 100, that is, queen generates 100 workers - this
    #'   is for a down-scaled simulation (for efficiency) assuming that this
    #'   represents ~60,000 workers in a full/strong colony (Seeley, 2019). This
    #'   value is set in \code{SimParamBee$new()} to have a number to work with
    #'   - you might want to change this!
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{nWorkers} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(x, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{x} and any following arguments MUST have a
    #'   default value. See \code{\link{nWorkersPoisson}},
    #'   \code{\link{nWorkersTruncPoisson}},
    #'   \code{\link{nWorkersPoissonQueenFecundity}}, or
    #'   \code{\link{nWorkersTruncPoissonQueenFecundity}} for examples. You will
    #'   likely want to define your own functions!
    nWorkers = "numericOrFunction",

    # nDrones field ----
    #' @field nDrones numeric or function, a number of drones generated in a
    #'   colony - used in \code{\link{createDrones}}, \code{\link{addDrones}},
    #'   \code{\link{buildUpColony}}, and \code{\link{buildUpColonies}}.
    #'
    #'   The default value is 10, that is, queen generates 10 drones - this is
    #'   for a down-scaled simulation (for efficiency) assuming that this
    #'   represents ~1,000 drones in a full/strong colony (Seeley, 2019). This
    #'   value is set in \code{SimParamBee$new()} to have a number to work with
    #'   - you might want to change this!
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{nDrones} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(x, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{x} and any following arguments MUST have a
    #'   default value. See \code{\link{nDronesPoisson}},
    #'   \code{\link{nDronesTruncPoisson}},
    #'   \code{\link{nDronesPoissonQueenFecundity}}, or
    #'   \code{\link{nDronesTruncPoissonQueenFecundity}} for examples. You will
    #'   likely want to define your own functions!
    nDrones = "numericOrFunction",

    # nVirginQueens field ----
    #' @field nVirginQueens numeric or function, a number of virgin queens
    #'   generated when a queen dies or other situations - used in
    #'   \code{\link{createVirginQueens}} and \code{\link{addVirginQueens}}.
    #'
    #'   The default value is 10, that is, when the queen dies, workers generate
    #'   10 new virgin queens (Seeley, 2019). This value is set in
    #'   \code{SimParamBee$new()} to have a number to work with - you might want
    #'   to change this!
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{nVirginQueens} is a function, it should work with internals
    #'   of other functions. Therefore, the function MUST be defined like
    #'   \code{function(colony, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{colony} and any following arguments MUST have a
    #'   default value. See \code{\link{nVirginQueensPoisson}},
    #'   \code{\link{nVirginQueensTruncPoisson}},
    #'   \code{\link{nVirginQueensPoissonColonyStrength}}, or
    #'   \code{\link{nVirginQueensTruncPoissonColonyStrength}} for examples.
    #'   You will likely want to define your own functions!
    nVirginQueens = "numericOrFunction",

    # nFathers field ----
    #' @field nFathers numeric or function, a number of drones a queen mates
    #'   with  - used in \code{\link{pullDroneGroupsFromDCA}},
    #'   \code{\link{crossVirginQueen}}, \code{\link{crossColony}}, and
    #'   \code{\link{crossColonies}}.
    #'
    #'   The default value is 15, that is, a virging queen mates on average with
    #'   15 drones (Seeley, 2019). This value is set in \code{SimParamBee$new()}
    #'   to have a number to work with - you might want to change this!
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{nFathers} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(arg = default) someCode }, that is, any arguments MUST
    #'   have a default value. We did not use the \code{colony} argument here,
    #'   because \code{nFathers} likely does not depend on the colony. Let us
    #'   know if we are wrong! See \code{\link{nFathersPoisson}} or
    #'   \code{\link{nFathersTruncPoisson}} for examples. You will likely want
    #'   to define your own functions!
    nFathers = "numericOrFunction",

    # pSwarm field ----
    #' @field pSwarm numeric or a function, the percentage of workers that leave
    #'   with the old queen when the colony swarms - used in
    #'   \code{\link{swarmColony}} and \code{\link{swarmColonies}}.
    #'
    #'   The default value is 0.50, that is, about a half of workers leave colony
    #'   in a swarm (Seeley, 2019). This value is set in \code{SimParamBee$new()}
    #'   to have a percentage to work with - you might want to change this!
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{pSwarm} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(colony, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{colony} and any following arguments MUST have a
    #'   default value. See \code{\link{pSwarmUnif}} or
    #'   \code{\link{pSwarmColonyStrength}} for examples. You will likely want
    #'   to define your own functions!
    pSwarm = "numericOrFunction",

    # pSplit field ----
    #' @field pSplit numeric or a function, the percentage of workers to be
    #'   removed in a managed split - used in \code{\link{splitColony}} and
    #'   \code{\link{splitColonies}}.
    #'
    #'   The default value is 0.30, that is, about a third of workers is put into
    #'   a split colony from a strong colony (Seeley, 2019). This value is set
    #'   in \code{SimParamBee$new()} to have a percentage to work with - you
    #'   might want to change this!
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{pSplit} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(colony, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{colony} and any following arguments MUST have a
    #'   default value. See \code{\link{pSplitUnif}} or
    #'   \code{\link{pSplitColonyStrength}} for examples. You will likely want
    #'   to define your own functions!
    pSplit = "numericOrFunction",

    # pDownsize field ----
    #' @field pDownsize numeric or a function, the percentage of workers to be
    #'   removed from the colony when downsizing, usually in autumn - used in
    #'   \code{\link{downsizeColony}} and \code{\link{downsizeColonies}}.
    #'
    #'   The default value is 0.85, that is, a majority of workers die before
    #'   autumn or all die but some winter workers are created (Seeley, 2019).
    #'   This value is set in \code{SimParamBee$new()} to have a percentage to
    #'   work with - you might want to change this!
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{pDownsize} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(colony, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{colony} and any following arguments MUST have a
    #'   default value. See \code{\link{pDownsizeUnif}} or
    #'   \code{\link{pDownsizeColonyStrength}} for examples. You will likely want
    #'   to define your own functions!
    pDownsize = "numericOrFunction",

    # phenoColony field ----
    #' @field phenoColony function, to set colony phenotypes - used in
    #'   \code{\link{setPhenoColony}} and \code{\link{setPhenoColonies}}.
    #'
    #'   This function should work with internals of others functions -
    #'   therefore the function MUST be defined like \code{function(colony, arg
    #'   = default) someCode }, that is, the first argument MUST be
    #'   \code{colony} and any following arguments MUST have a default value.
    #'   See \code{\link{phenoQueenPlusSumOfWorkers}} for an example. You will
    #'   likely want to define your own functions!
    phenoColony = "function",

    #' @description Starts the process of building a new simulation by creating
    #'   a new SimParamBee object and assigning a founder population of genomes
    #'   to the this object.
    #'
    #' @param founderPop \code{\link{MapPop-class}}, founder population of
    #'   genomes
    #' @param nWorkers see \code{\link{SimParamBee}} field \code{nWorkers}
    #' @param nDrones see \code{\link{SimParamBee}} field \code{nDrones}
    #' @param nVirginQueens see \code{\link{SimParamBee}} field \code{nVirginQueens}
    #' @param nFathers see \code{\link{SimParamBee}} field \code{nFathers}
    #' @param pSwarm see \code{\link{SimParamBee}} field \code{pSwarm}
    #' @param pSplit see \code{\link{SimParamBee}} field \code{pSplit}
    #' @param pDownsize see \code{\link{SimParamBee}} field \code{pDownsize}
    #' @param csdChr integer, chromosome that will carry the csd locus, by
    #'   default 3, but if there are less chromosomes (for a simplified
    #'   simulation), the locus is put on the last available chromosome (1 or
    #'   2); if \code{NULL} then csd locus is ignored in the simulation
    #' @param csdPos numeric, starting position of the csd locus on the
    #'   \code{csdChr} chromosome (relative at the moment, but could be in base
    #'   pairs in future)
    #' @param nCsdAlleles integer, number of possible csd alleles (this
    #'   determines how many segregating sites will be needed to represent the
    #'   csd locus from the underlying bi-allelic SNP; the minimum number of
    #'   bi-allelic SNP needed is \code{log2(nCsdAlleles)}); if set to \code{0}
    #'   then \code{csdChr=NULL} is triggered. By default we set \code{nCsdAlleles}
    #'   to 128, which is at the upper end of the reported number of csd alleles
    #'   (Lechner et al., 2014; Zareba et al., 2017; Bovo et al., 2021).
    #' @param phenoColony see \code{\link{SimParamBee}} field \code{phenoColony}
    #'
    #' @references
    #' Bovo et al. (2021) Application of Next Generation Semiconductor-Based
    #'   Sequencing for the Identification of Apis mellifera Complementary Sex
    #'   Determiner (csd) Alleles from Honey DNA. Insects, 12(10), 868.
    #'   \url{https://www.mdpi.com/2075-4450/12/10/868}
    #'
    #' Lechner et al. (2014) Nucleotide variability at its limit? Insights into
    #'  the number and evolutionary dynamics of the sex-determining specificities
    #'  of the honey bee Apis mellifera Molecular Biology and Evolution, 31,
    #'  272-287. \url{https://academic.oup.com/mbe/article/31/2/272/998263}
    #'
    #' Seeley (2019) The Lives of Bees: The Untold Story of the Honey
    #'   Bee in the Wild. Princeton: Princeton University Press.
    #'   \url{https://doi-org.ezproxy.is.ed.ac.uk/10.1515/9780691189383}
    #'
    #' Zareba et al. (2017) Uneven distribution of complementary sex determiner
    #'   (csd) alleles in Apis mellifera population. Scientific Reports, 7, 2317.
    #'   \url{https://doi.org/10.1038/s41598-017-02629-9}
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

    initialize = function(founderPop,
                          nWorkers = 100, nDrones = 100,
                          nVirginQueens = 10, nFathers = 15,
                          pSwarm = 0.5, pSplit = 0.3, pDownsize = 0.85,
                          csdChr = 3, csdPos = 0.865, nCsdAlleles = 128,
                          phenoColony = NULL) {
      # Get all the goodies from AlphaSimR::SimParam$new(founderPop)
      super$initialize(founderPop)
      private$.versionSIMplyBee <- packageDescription("SIMplyBee")$Version

      # nWorkers, nDrones, and nVirginQueens initialize ----

      self$nWorkers <- nWorkers
      self$nDrones <- nDrones
      self$nVirginQueens <- nVirginQueens
      self$nFathers <- nFathers
      self$pSwarm <- pSwarm
      self$pSplit <- pSplit
      self$pDownsize <- pDownsize

      # caste initialize ----

      private$.caste <- NULL

      # csd initialize ----

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

        # csd position and sitess
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

      # phenoColony initialize ----

      if (!is.null(phenoColony) && !is.function(phenoColony)) {
          stop("Argument phenoColony must be a function or NULL!")
      }
      self$phenoColony <- phenoColony

      invisible(self)
    },

    # Internal (public) ----

    #' @description Store caste information (for internal use only!)
    #'
    #' @param id character, individuals whose caste will be stored
    #' @param caste character, single "Q" for queens, "W" for workers, "D" for
    #'   drones, "V" for virgin queens, and "F" for fathers
    #'
    #' @examples
    #' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
    #' SP <- SimParamBee$new(founderGenomes)
    #' SP$setTrackPed(isTrackPed = TRUE)
    #' basePop <- createVirginQueens(founderGenomes)
    #'
    #' drones <- createDrones(x = basePop[1], nInd = 10)
    #' colony <- createColony(x = basePop[2])
    #' colony <- crossColony(colony, drones = drones[1:5], nFathers = 5)
    #' colony <- addWorkers(colony, nInd = 5)
    #' colony <- addDrones(colony, nInd = 5)
    #' colony <- addVirginQueens(colony, nInd = 2)
    #'
    #' SP$pedigree
    #' SP$caste
    addToCaste = function(id, caste) {
      tmp <- rep(x = caste[1], times = length(id))
      names(tmp) <- id
      private$.caste <- c(private$.caste, tmp)
      invisible(self)
    },

    #' @description Change caste information (for internal use only!)
    #'
    #' @param id character, individuals whose caste will be changed
    #' @param caste character, single "Q" for queens, "W" for workers, "D" for
    #'   drones, "V" for virgin queens, and "F" for fathers
    #'
    #' @examples
    #' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
    #' SP <- SimParamBee$new(founderGenomes)
    #' SP$setTrackPed(isTrackPed = TRUE)
    #' basePop <- createVirginQueens(founderGenomes)
    #' SP$pedigree
    #' SP$caste
    #'
    #' drones <- createDrones(x = basePop[1], nInd = 10)
    #' colony <- createColony(x = basePop[2])
    #' colony <- crossColony(colony, drones = drones[1:5], nFathers = 5)
    #' SP$pedigree
    #' SP$caste
    changeCaste = function(id, caste) {
      if (!is.character(id)) {
        stop("Argument id must be a character!")
      }
      private$.caste[id] <- caste[1]
      invisible(self)
    }
  ),

  # Private fields ----

  private = list(
    .versionSIMplyBee = "character",
    .caste = "character",
    .csdChr = "integerOrNULL",
    .csdPos = "numeric",
    .nCsdAlleles = "integer",
    .nCsdSites = "integer",
    .csdPosStart = "integer",
    .csdPosStop = "integer"
  ),

  # Active fileds ----

  active = list(

    #' @field caste character, caste information for every individual ever
    #'   created; active only when \code{SP$setTrackPed(isTrackPed = TRUE)}
    caste = function(value) {
      if (missing(value)) {
        factor(
          x = private$.caste, levels = c("Q", "F", "W", "D", "V"),
          labels = list("queen", "fathers", "workers", "drones", "virginQueens")
        )
      } else {
        stop("`$caste` is read only", call. = FALSE)
      }
    },

    #' @field csdChr integer, chromosome of the csd locus
    csdChr = function(value) {
      if (missing(value)) {
        private$.csdChr
      } else {
        stop("`$csdChr` is read only", call. = FALSE)
      }
    },

    #' @field csdPos numeric, starting position of the csd locus on the
    #'   \code{csdChr} chromosome (relative at the moment, but could be in base
    #'   pairs in the future)
    csdPos = function(value) {
      if (missing(value)) {
        private$.csdPos
      } else {
        stop("`$csdPos` is read only", call. = FALSE)
      }
    },

    #' @field nCsdAlleles integer, number of possible csd alleles
    nCsdAlleles = function(value) {
      if (missing(value)) {
        private$.nCsdAlleles
      } else {
        stop("`$nCsdAlleles` is read only", call. = FALSE)
      }
    },

    #' @field nCsdSites integer, number of segregating sites representing the
    #'   csd locus
    nCsdSites = function(value) {
      if (missing(value)) {
        private$.nCsdSites
      } else {
        stop("`$nCsdSites` is read only", call. = FALSE)
      }
    },

    #' @field csdPosStart integer, starting position of the csd locus
    csdPosStart = function(value) {
      if (missing(value)) {
        private$.csdPosStart
      } else {
        stop("`$.csdPosStart` is read only", call. = FALSE)
      }
    },

    #' @field csdPosStop integer, ending position of the csd locus
    csdPosStop = function(value) {
      if (missing(value)) {
        private$.csdPosStop
      } else {
        stop("`$csdPosStop` is read only", call. = FALSE)
      }
    },

    #' @field version list, versions of AlphaSimR and SIMplyBee packages used to
    #'   generate this object
    version = function(value) {
      if (missing(value)) {
        list(
          "AlphaSimR" = private$.version,
          "SIMplyBee" = private$.versionSIMplyBee
        )
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
#' @export
isSimParamBee <- function(x) {
  ret <- is(x, class2 = "SimParamBee")
  return(ret)
}

# nFunctions ----

#' @rdname nWorkersPoisson
#' @title Sample a number of workers
#'
#' @description Sample a number of workers when \code{nWorkers = NULL}
#'   (see \code{\link{SimParamBee}$nWorkers}) - this is just an example and
#'   you will likely want to define your own sampling functions!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param average numeric, average number of workers
#' @param queenFecundityTrait numeric, trait that represents queen's fecundity
#'   (defined in \code{\link{SimParamBee}} - see examples)
#' @param lowerLimit numeric, returned numbers will be above this value
#'
#' @details \code{nWorkersPoisson} samples from a Poisson distribution with a
#'   given average, which can return a value 0. \code{nDronesTruncPoisson}
#'   samples from a zero truncated Poisson distribution.
#'   \code{nWorkersPoissonQueenFecundity} samples from a
#'   Poisson distribution whose average is a function of queen's fecundity
#'   phenotype ((trait)). You need to set up the trait parameters (means and
#'   variances) via \code{\link{SimParamBee}} (see examples). Finally,
#'   \code{nWorkersTruncPoissonQueenFecundity} samples from from a zero truncated
#'   Poisson distribution whose average is a function of queen's fecundity.
#'
#' @seealso \code{\link{SimParamBee}} field \code{nWorkers}
#'
#' @return numeric, number of workers
#'
#' @examples
#' nWorkersPoisson()
#' nWorkersPoisson()
#' n <- nWorkersPoisson(n = 1000)
#' hist(n, breaks = seq(from = min(n), to = max(n)), xlim = c(0, 200))
#' table(n)
#'
#' # Example for nWorkersPoissonQueenFecundity()
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' average <- 100
#' h2 <- 0.1
#' SP$addTraitA(nQtlPerChr = 100, mean = average, var = average * h2)
#' SP$setVarE(varE = average * (1 - h2))
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony2 <- createColony(x = basePop[3])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' colony1@queen@pheno
#' colony2@queen@pheno
#' colony1 <- addWorkers(colony1, nInd = nWorkersPoissonQueenFecundity)
#' nWorkers(colony1)
#' colony2 <- addWorkers(colony2, nInd = nWorkersPoissonQueenFecundity)
#' nWorkers(colony2)
#'
#' # Logic behind nWorkersPoissonQueenFecundity()
#' average <- 100
#' queenFecundity <- rnorm(n = 1000, mean = average, sd = sqrt(average))
#' hist(queenFecundity, breaks = seq(from = 0, to = 200), xlim = c(0, 200))
#' n <- rpois(n = length(queenFecundity), lambda = queenFecundity)
#' hist(n, breaks = seq(from = 0, to = 200), xlim = c(0, 200))
#' r <- range(c(n, queenFecundity))
#' plot(n ~ queenFecundity, xlim = r, ylim = r)
#' abline(v = average)
#' abline(h = average)
#' abline(a = 0, b = 1)
#' @export
nWorkersPoisson <- function(colony, n = 1, average = 100) {
  return(rpois(n = n, lambda = average))
}

#' @describeIn nWorkersPoisson Sample a non-zero number of workers
#' @export
nWorkersTruncPoisson <- function(colony, n = 1, average = 100, lowerLimit = 0) {
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @describeIn nWorkersPoisson Sample a number of workers based on
#'   queen's fecundity trait (defined in \code{\link{SimParamBee}}!)
#' # TODO: Is using Poisson on top of queen's fecundity phenotype adding too
#' #       much variation?
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/243
#' @export
nWorkersPoissonQueenFecundity <- function(colony, n = 1,
                                          queenFecundityTrait = 1) {
  average <- colony@queen@pheno[, queenFecundityTrait]
  return(rpois(n = n, lambda = average))
}

#' @describeIn nWorkersPoisson Sample a number of workers based on
#'   queen's fecundity trait (defined in \code{\link{SimParamBee}}!)
#' # TODO: Is using Poisson on top of queen's fecundity phenotype adding too
#' #       much variation?
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/243
#' @export
nWorkersTruncPoissonQueenFecundity <- function(colony, n = 1,
                                               queenFecundityTrait = 1,
                                               lowerLimit = 0) {
  average <- colony@queen@pheno[, queenFecundityTrait]
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @rdname nDronesPoisson
#' @title Sample a number of drones
#'
#' @description Sample a number of drones when \code{nDrones = NULL}
#'   (see \code{\link{SimParamBee}$nDrones}) - this is just an example and
#'   you will likely want to define your own sampling functions!
#'
#' @param x \code{\link{Pop-class}} or \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param average numeric, average number of drones
#' @param queenFecundityTrait numeric, trait that represents queen's fecundity
#'   (defined in \code{\link{SimParamBee}} - see examples)
#' @param lowerLimit numeric, returned numbers will be above this value
#'
#' @details \code{nDronesPoisson} samples from a Poisson distribution with a
#'   given average, which can return a value 0. \code{nDronesTruncPoisson}
#'   samples from a zero truncated Poisson distribution.
#'   \code{nDronesPoissonQueenFecundity} samples from a
#'   Poisson distribution whose average is a function of queen's fecundity
#'   phenotype ((trait)). You need to set up the trait parameters (means and
#'   variances) via \code{\link{SimParamBee}} (see examples). Finally,
#'   \code{nDronesTruncPoissonQueenFecundity} samples from from a zero truncated
#'   Poisson distribution whose average is a function of queen's fecundity.
#'
#' @seealso \code{\link{SimParamBee}} field \code{nDrones}
#'
#' @return numeric, number of drones
#'
#' @examples
#' nDronesPoisson()
#' nDronesPoisson()
#' n <- nDronesPoisson(n = 1000)
#' hist(n, breaks = seq(from = min(n), to = max(n)), xlim = c(0, 200))
#' table(n)
#'
#' # Example for nDronesPoissonQueenFecundity()
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' average <- 100
#' h2 <- 0.1
#' SP$addTraitA(nQtlPerChr = 100, mean = average, var = average * h2)
#' SP$setVarE(varE = average * (1 - h2))
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 10)
#' colony1 <- createColony(x = basePop[2])
#' colony2 <- createColony(x = basePop[3])
#' colony1 <- crossColony(colony1, drones = drones[1:5], nFathers = 5)
#' colony2 <- crossColony(colony2, drones = drones[6:10], nFathers = 5)
#' colony1@queen@pheno
#' colony2@queen@pheno
#' colony1 <- addDrones(colony1, nInd = nDronesPoissonQueenFecundity)
#' nWorkers(colony1)
#' colony2 <- addDrones(colony2, nInd = nDronesPoissonQueenFecundity)
#' nWorkers(colony2)
#'
#' # Logic behind nDronesPoissonQueenFecundity()
#' average <- 100
#' queenFecundity <- rnorm(n = 1000, mean = average, sd = sqrt(average))
#' queenFecundity[queenFecundity < 0] <- 0
#' hist(queenFecundity, breaks = seq(from = 0, to = 200), xlim = c(0, 200))
#' n <- rpois(n = length(queenFecundity), lambda = queenFecundity)
#' hist(n, breaks = seq(from = 0, to = 200), xlim = c(0, 200))
#' r <- range(c(n, queenFecundity))
#' plot(n ~ queenFecundity, xlim = r, ylim = r)
#' abline(v = average)
#' abline(h = average)
#' abline(a = 0, b = 1)
#' @export
nDronesPoisson <- function(x, n = 1, average = 100) {
  return(rpois(n = n, lambda = average))
}

#' @describeIn nDronesPoisson Sample a non-zero number of drones
#' @export
nDronesTruncPoisson <- function(x, n = 1, average = 100, lowerLimit = 0) {
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @describeIn nDronesPoisson Sample a number of drones based on queen's
#'   fecundity trait (defined in \code{\link{SimParamBee}}!)
#' # TODO: Is using Poisson on top of queen's fecundity phenotype adding too
#' #       much variation?
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/243
#' @export
nDronesPoissonQueenFecundity <- function(x, n = 1,
                                         queenFecundityTrait = 1) {
  if (isPop(x)) {
    pheno <- x@pheno[, queenFecundityTrait]
  } else {
    pheno <- x@queen@pheno[, queenFecundityTrait]
  }
  average <- pheno
  return(rpois(n = n, lambda = average))
}

#' @describeIn nDronesPoisson Sample a non-zero number of drones based on
#'   queen's fecundity trait (defined in \code{\link{SimParamBee}}!)
#' # TODO: Is using Poisson on top of queen's fecundity phenotype adding too
#' #       much variation?
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/243
#' @export
nDronesTruncPoissonQueenFecundity <- function(x, n = 1,
                                              queenFecundityTrait = 1,
                                              lowerLimit = 0) {
  if (isPop(x)) {
    pheno <- x@pheno[, queenFecundityTrait]
  } else {
    pheno <- x@queen@pheno[, queenFecundityTrait]
  }
  average <- pheno
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @rdname nVirginQueensPoisson
#' @title Sample a number of virgin queens
#'
#' @description Sample a number of virgin queens when \code{nFathers = NULL}
#'   (see \code{\link{SimParamBee}$nVirginQueens}) - this is just an example and
#'   you will likely want to define your own sampling functions!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param average numeric, average number of virgin queens
#' @param lower numeric, lower (left) asymptote with respect to colony strength
#'   (see examples)
#' @param upper numeric, upper (right) asymptote with respect to colony strength
#'   (see examples)
#' @param rate numeric, growth rate with respect to colony strength
#'   (see examples)
#' @param nWorkersFull numeric, average number of workers in a full/strong colony
#' @param lowerLimit numeric, returned numbers will be above this value
#'
#' @details \code{nVirginQueensPoisson} samples from a Poisson distribution,
#'   which can return a value 0 (that would mean a failed colony recover after a
#'   queen dies), while \code{nVirginQueensTruncPoisson} samples from a
#'   truncated Poisson distribution (truncated at zero) to avoid failure.
#'
#'   The \code{nWorkersFull} default value used in this function is geared
#'   towards a situation where we simulate ~100 workers per colony (down-scaled
#'   simulation for efficiency). If you simulate more workers, you should change
#'   the default accordingly.
#'
#' @seealso \code{\link{SimParamBee}} field \code{nVirginQueens}
#'
#' @return numeric, number of virgin queens
#'
#' @examples
#' nVirginQueensPoisson()
#' nVirginQueensPoisson()
#' n <- nVirginQueensPoisson(n = 1000)
#' hist(n, breaks = seq(from = min(n), to = max(n)), xlim = c(0, 30))
#' table(n)
#'
#' nVirginQueensTruncPoisson()
#' nVirginQueensTruncPoisson()
#' n <- nVirginQueensTruncPoisson(n = 1000)
#' hist(n, breaks = seq(from = min(n), to = max(n)), xlim = c(0, 30))
#' table(n)
#'
#' # Example for nVirginQueensPoissonColonyStrength()
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones, nFathers = 5)
#' colony <- addWorkers(colony, nInd = 10)
#' nWorkers(colony) # weak colony
#' nVirginQueensPoissonColonyStrength(colony)
#' nVirginQueensPoissonColonyStrength(colony)
#' colony <- addWorkers(colony, nInd = 100)
#' nWorkers(colony) # strong colony
#' nVirginQueensPoissonColonyStrength(colony)
#' nVirginQueensPoissonColonyStrength(colony)
#'
#' # Logic behind nVirginQueensPoissonColonyStrength()
#' # Based https://en.wikipedia.org/wiki/Generalised_logistic_function
#' averageExp <- 10
#' lower <- 0
#' upper <- 20
#' rate <- 3
#' nWorkersFull <- 100
#' nW <- 0:400
#' x <- (nW - nWorkersFull) / nWorkersFull
#' average <- lower + ((upper - lower) / (1 + exp(-rate * x)))
#' n <- rpois(n = length(nW), lambda = average)
#' plot(n ~ nW)
#' abline(v = nWorkersFull)
#' abline(h = averageExp)
#' lines(average ~ nW)
#' @export
nVirginQueensPoisson <- function(colony, n = 1, average = 10) {
  return(rpois(n = n, lambda = average))
}

#' @describeIn nVirginQueensPoisson Sample a non-zero number of virgin queens
#' @export
nVirginQueensTruncPoisson <- function(colony, n = 1, average = 10, lowerLimit = 0) {
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @describeIn nVirginQueensPoisson Sample a number of virgin queens based on
#'   colony strength
#' @export
nVirginQueensPoissonColonyStrength <- function(colony, n = 1,
                                               lower = 0, upper = 20, rate = 3,
                                               nWorkersFull = 100) {
  nW <- nWorkers(colony)
  x <- (nW - nWorkersFull) / nWorkersFull
  average <- lower + ((upper - lower) / (1 + exp(-rate * x)))
  return(rpois(n = n, lambda = average))
}

#' @describeIn nVirginQueensPoisson Sample a non-zero number of virgin queens
#'   based on colony strength
#' @export
nVirginQueensTruncPoissonColonyStrength <- function(colony, n = 1,
                                                    lower = 0, upper = 20, rate = 3,
                                                    nWorkersFull = 100,
                                                    lowerLimit = 0) {
  nW <- nWorkers(colony)
  x <- (nW - nWorkersFull) / nWorkersFull
  average <- lower + ((upper - lower) / (1 + exp(-rate * x)))
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @rdname nFathersPoisson
#' @title Sample a number of fathers
#'
#' @description Sample a number of fathers when \code{nFathers = NULL} (see
#'   \code{\link{SimParamBee}$nFathers}) - this is just an example and you will
#'   likely want to define your own sampling functions!
#'
#' @param n integer, number of samples
#' @param average numeric, average number of fathers
#' @param lowerLimit numeric, returned numbers will be above this value
#'
#' @details \code{nFathersPoisson} samples from a Poisson distribution, which
#'   can return a value 0 (that would mean a failed queen mating), while
#'   \code{nFathersTruncPoisson} samples from a truncated Poisson distribution
#'   (truncated at zero) to avoid failed matings.
#'
#' @seealso \code{\link{SimParamBee}} field \code{nFathers}
#'
#' @return numeric, number of fathers
#'
#' @examples
#' nFathersPoisson()
#' nFathersPoisson()
#' n <- nFathersPoisson(n = 1000)
#' hist(n, breaks = seq(from = min(n), to = max(n)), xlim = c(0, 40))
#' table(n)
#'
#' nFathersTruncPoisson()
#' nFathersTruncPoisson()
#' n <- nFathersTruncPoisson(n = 1000)
#' hist(n, breaks = seq(from = min(n), to = max(n)), xlim = c(0, 40))
#' table(n)
#' @export
nFathersPoisson <- function(n = 1, average = 15) {
  return(rpois(n = n, lambda = average))
}

#' @describeIn nFathersPoisson Sample a non-zero number of fathers
#' @export
nFathersTruncPoisson <- function(n = 1, average = 15, lowerLimit = 0) {
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

# pFunctions ----

#' @rdname pSwarm
#' @title Sample the proportion of workers in a swarm
#'
#' @description Sample the proportion of workers in a swarm when \code{p = NULL}
#'   (see \code{\link{SimParamBee}$pSwarm}) - this is just an example and you
#'   will likely want to define your own sampling functions!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param min numeric, lower limit for \code{pSwarmUnif}
#' @param max numeric, upper limit for \code{pSwarmUnif}
#' @param nWorkersFull numeric, average number of workers in a full/strong
#'   colony for \code{pSplitColonyStrength} (actual number can go beyond this
#'   value)
#' @param scale numeric, scaling of numbers in \code{pSwarmColonyStrength}
#'   to avoid to narrow range when colonies have a large number of bees (in that
#'   case change \code{nWorkersFull} too!)
#'
#' @details \code{pSwarmUnif} samples from a uniform distribution between values
#'   0.4 and 0.6 irrespective of colony strength. \code{pSwarmColonyStrength}
#'   samples from a beta distribution with a mean of \code{a / (a + b)}, where
#'   \code{a = nWorkers + nWorkersFull} and \code{b = nWorkers}. This beta
#'   sampling mimics larger swarms for strong colonies and smaller swarms for
#'   weak colonies - see examples - this is just an example, not based on actual
#'   data!
#'   # TODO: should we flip the pattern?
#'   #       https://github.com/HighlanderLab/SIMplyBee/issues/250
#'
#'   The \code{nWorkersFull} default value used in this function is geared
#'   towards a situation where we simulate ~100 workers per colony (down-scaled
#'   simulation for efficiency). If you simulate more workers, you should change
#'   the default accordingly.
#'
#' @seealso \code{\link{SimParamBee}} field \code{pSwarm}
#'
#' @return numeric, proportion of workers in a swarm
#'
#' @examples
#' pSwarmUnif()
#' pSwarmUnif()
#' p <- pSwarmUnif(n = 1000)
#' hist(p, breaks = seq(from = 0, to = 1, by = 0.01), xlim = c(0, 1))
#'
#' # Example for pSwarmColonyStrength()
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones, nFathers = 5)
#' colony <- addWorkers(colony, nInd = 10)
#' nWorkers(colony) # weak colony
#' pSwarmColonyStrength(colony)
#' pSwarmColonyStrength(colony)
#' colony <- addWorkers(colony, nInd = 100)
#' nWorkers(colony) # strong colony
#' pSwarmColonyStrength(colony)
#' pSwarmColonyStrength(colony)
#'
#' # Logic behind pSwarmColonyStrength()
#' nWorkersFull <- 100
#' nWorkers <- 0:200
#' pSwarm <- 1 - rbeta(
#'   n = length(nWorkers),
#'   shape1 = nWorkers + nWorkersFull,
#'   shape2 = nWorkers
#' )
#' plot(pSwarm ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' pStay <- 1 - pSwarm
#' plot(pStay ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' # TODO: should we flip the pattern?
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/250
#' @export
pSwarmUnif <- function(colony, n = 1, min = 0.4, max = 0.6) {
  return(runif(n = n, min = min, max = max))
}

#' @describeIn pSwarm Sample the proportion of workers in a swarm based on
#'   colony strength
#' @export
pSwarmColonyStrength <- function(colony, n = 1, nWorkersFull = 100, scale = 1) {
  nW <- nWorkers(colony)
  pKeep <- rbeta(
    n = n,
    shape1 = (nW + nWorkersFull) / scale,
    shape2 = nW / scale
  )
  return(1 - pKeep)
}

#' @rdname pSplit
#' @title Sample the proportion of removed workers in a managed
#'   split
#'
#' @description Sample the proportion of removed workers in a managed split when
#'   \code{p = NULL} - (see \code{\link{SimParamBee}$pSplit}) - this is just an
#'   example and you will likely want to define your own sampling functions!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param min numeric, lower limit for \code{pSplitUnif}
#' @param max numeric, upper limit for \code{pSplitUnif}
#' @param nWorkersFull numeric, average number of workers in a full/strong
#'   colony for \code{pSplitColonyStrength} (actual number can go beyond this
#'   value)
#' @param scale numeric, scaling of numbers in \code{pSplitColonyStrength}
#'   to avoid to narrow range when colonies have a large number of bees (in that
#'   case change \code{nWorkersFull} too!)
#'
#' @details \code{pSplitUnif} samples from a uniform distribution between values
#'   0.2 and 0.4 irrespective of colony strength. \code{pSplitColonyStrength}
#'   samples from a beta distribution with mean \code{a / (a + b)}, where
#'   \code{a = nWorkers + nWorkersFull} and \code{b = nWorkers}. This beta
#'   sampling mimics larger splits for strong colonies and smaller splits for
#'   weak colonies - see examples - this is just an example, not based on actual
#'   data!
#'
#'   The \code{nWorkersFull} default value used in this function is geared
#'   towards a situation where we simulate ~100 workers per colony (down-scaled
#'   simulation for efficiency). If you simulate more workers, you should change
#'   the default accordingly.
#'
#' @seealso \code{\link{SimParamBee}} field \code{pSplit}
#'
#' @return numeric, proportion of removed workers
#'
#' @examples
#' pSplitUnif()
#' pSplitUnif()
#' p <- pSplitUnif(n = 1000)
#' hist(p, breaks = seq(from = 0, to = 1, by = 0.01), xlim = c(0, 1))
#'
#' # Example for pSplitColonyStrength()
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones, nFathers = 5)
#' colony <- addWorkers(colony, nInd = 10)
#' nWorkers(colony) # weak colony
#' pSplitColonyStrength(colony)
#' pSplitColonyStrength(colony)
#' colony <- addWorkers(colony, nInd = 100)
#' nWorkers(colony) # strong colony
#' pSplitColonyStrength(colony)
#' pSplitColonyStrength(colony)
#'
#' # Logic behind pSplitColonyStrength()
#' nWorkersFull <- 100
#' nWorkers <- 0:200
#' pSplit <- 1 - rbeta(
#'   n = length(nWorkers),
#'   shape1 = nWorkers + nWorkersFull,
#'   shape2 = nWorkers
#' )
#' plot(pSplit ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' pKeep <- 1 - pSplit
#' plot(pKeep ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' @export
pSplitUnif <- function(colony, n = 1, min = 0.2, max = 0.4) {
  return(runif(n = n, min = min, max = max))
}

#' @describeIn pSplit Sample the proportion of removed workers in a managed
#'   split based on colony strength
#' @export
pSplitColonyStrength <- function(colony, n = 1, nWorkersFull = 100, scale = 1) {
  nW <- nWorkers(colony)
  pKeep <- rbeta(
    n = n,
    shape1 = (nW + nWorkersFull) / scale,
    shape2 = nW / scale
  )
  return(1 - pKeep)
}

#' @rdname pDownsize
#' @title Sample the proportion of removed workers in downsizing
#'
#' @description Sample the proportion of removed workers in downsizing when
#'   \code{p = NULL} (see \code{\link{SimParamBee}$pDownsize}) - this is just an
#'   example and you will likely want to define your own sampling functions!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param min numeric, lower limit for \code{pDownsizeUnif}
#' @param max numeric, upper limit for \code{pDownsizeUnif}
#' @param nWorkersFull numeric, average number of workers in a full/strong
#'   colony for \code{pSplitColonyStrength} (actual number can go beyond this
#'   value)
#' @param scale numeric, scaling of numbers in \code{pDownsizeColonyStrength}
#'   to avoid to narrow range when colonies have a large number of bees (in that
#'   case change \code{nWorkersFull} too!)
#'
#' @details \code{pDownsizeUnif} samples from a uniform distribution between
#'   values 0.8 and 0.9 irrespective of colony strength.
#'   \code{pDownsizeColonyStrength} samples from a beta distribution with mean
#'   \code{a / (a + b)}, where \code{a = nWorkers} and \code{b = 3 * nWorkers +
#'   nWorkersFull}. This beta sampling mimics larger winter colonies for strong
#'   colonies and small winter colonies for weak colonies - see examples - this
#'   is just an example, not based on actual data!
#'
#'   The \code{nWorkersFull} default value used in this function is geared
#'   towards a situation where we simulate ~100 workers per colony (down-scaled
#'   simulation for efficiency). If you simulate more workers, you should change
#'   the default accordingly.
#'
#' @seealso \code{\link{SimParamBee}} field \code{pDownsize}
#'
#' @return numeric, proportion of removed workers
#'
#' @examples
#' pDownsizeUnif()
#' pDownsizeUnif()
#' p <- pDownsizeUnif(n = 1000)
#' hist(p, breaks = seq(from = 0, to = 1, by = 0.01), xlim = c(0, 1))
#'
#' # Example for pDownsizeColonyStrength()
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones, nFathers = 5)
#' colony <- addWorkers(colony, nInd = 10)
#' nWorkers(colony) # weak colony
#' pDownsizeColonyStrength(colony)
#' pDownsizeColonyStrength(colony)
#' colony <- addWorkers(colony, nInd = 100)
#' nWorkers(colony) # strong colony
#' pDownsizeColonyStrength(colony)
#' pDownsizeColonyStrength(colony)
#'
#' # Logic behind pDownsizeColonyStrength()
#' nWorkersFull <- 100
#' nWorkers <- 0:200
#' pRemove <- 1 - rbeta(
#'   n = length(nWorkers),
#'   shape1 = nWorkers,
#'   shape2 = 3 * nWorkers + nWorkersFull
#' )
#' plot(pRemove ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' pNew <- 1 - pRemove
#' plot(pNew ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' @export
pDownsizeUnif <- function(colony, n = 1, min = 0.8, max = 0.9) {
  return(runif(n = n, min = min, max = max))
}

#' @describeIn pDownsize Sample the proportion of removed workers in downsizing
#'   based on colony strength
#' @export
pDownsizeColonyStrength <- function(colony, n = 1, nWorkersFull = 100, scale = 1) {
  nW <- nWorkers(colony)
  pSurvive <- rbeta(
    n = n,
    shape1 = nW / scale,
    shape2 = (2 * nW + nWorkersFull) / scale
  )
  return(1 - pSurvive)
}

# phenoFunctions ----

#' @rdname phenoQueenPlusSumOfWorkers
#' @title Sample colony phenotype
#'
#' @description Sample colony phenotype when \code{colonyFUN = NULL} (see
#'   \code{\link{SimParamBee}$phenoColony}).
#'
#'   This is just an example. You can provide your own functions that satisfy
#'   your needs!
#'
#' @param colony \code{\link{Colony-class}}
#' @param queenTrait numeric, trait that represents queen's effect on the
#'   phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{0} then this effect is 0
#' @param workersTrait numeric, trait that represents workers's effect on the
#'   phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{0} then this effect is 0
#' @param checkProduction logical, does the phenotype depend on the production
#'   status of colony; if yes and production is not \code{TRUE}, the result is
#'   a 0
#'
#' @seealso \code{\link{SimParamBee}} field \code{phenoColony} and
#'   \code{\link{getEvents}}
#'
#' @return numeric matrix with a single value
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#'
#' # Define two traits that collectively affect colony honey yield:
#' # 1) queen's effect on colony honey yield
#' # 2) workers' effect on colony honey yield
#' # The traits will have negative genetic correlation and heritability of 0.25
#' meanP <- c(20, 0)
#' varA <- c(1, 1 / 10)
#' corA <- matrix(data = c(
#'   1.0, -0.5,
#'   -0.5, 1.0
#' ), nrow = 2, byrow = TRUE)
#' varE <- c(3, 3 / 10)
#' varA / (varA + varE)
#' SP$addTraitA(nQtlPerChr = 100, mean = meanP, var = varA, corA = corA)
#' SP$setVarE(varE = varE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 5)
#' colony <- createColony(x = basePop[2])
#' colony <- crossColony(colony, drones = drones, nFathers = 5)
#' colony <- buildUpColony(colony, nWorkers = 10)
#'
#' # Set phenotypes for all colony individuals
#' colony <- setPhenoColony(colony)
#'
#' # Queen's phenotype for both traits
#' pheno(getQueen(colony))
#' # TODO: use getQueensPheno(colony, caste = "queen")
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/26
#'
#' # Workers' phenotype for both traits
#' pheno(getWorkers(colony))
#' # TODO: use getWorkersPheno(colony, caste = "queen")
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/26
#'
#' # TODO: use getColonyPheno(colony) for all individuals
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/26
#'
#' # Set phenotypes for all colony individuals AND colony
#' colony <- setPhenoColony(colony,
#'   colonyFUN = phenoQueenPlusSumOfWorkers
#' )
#' pheno(colony)
#' # TODO: use getColonyPheno(colony) for all individuals and/or colony
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/26
#' @export
phenoQueenPlusSumOfWorkers <- function(colony, queenTrait = 1,
                                       workersTrait = 2,
                                       checkProduction = TRUE) {
  # TODO: should we add checks for other events too? say swarming?
  #       https://github.com/HighlanderLab/SIMplyBee/issues/255
  if (queenTrait > 0) {
    queenEff <- colony@queen@pheno[, queenTrait]
  } else {
    queenEff <- 0
  }
  if (workersTrait > 0) {
    sumOfWorkersEff <- sum(colony@workers@pheno[, workersTrait])
  } else {
    sumOfWorkersEff <- 0
  }
  colonyPheno <- queenEff + sumOfWorkersEff
  if (checkProduction) {
    if (!colony@production) {
      colonyPheno <- 0
    }
  }
  return(matrix(colonyPheno))
}
