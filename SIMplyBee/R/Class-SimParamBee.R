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
#'   \code{someFun(argument, simParamBee = SP)}. \code{SimParamBee} inherits
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
    #'   \code{\link{buildUp}}.
    #'
    #'   The default value is 100, that is, queen generates 100 workers - this
    #'   is for a down-scaled simulation (for efficiency) assuming that this
    #'   represents ~60,000 workers in a full/strong colony (Seeley, 2019). This
    #'   value is set in \code{SimParamBee$new()} to have a number to work with.
    #'
    #'   You can change this setting to your needs!
    #'
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{nWorkers} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(x, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{x} and any following arguments MUST have a
    #'   default value. See \code{\link{nWorkersPoisson}},
    #'   \code{\link{nWorkersTruncPoisson}}, or
    #'   \code{\link{nWorkersColonyPhenotype}} for examples.
    #'
    #'   You can provide your own functions that satisfy your needs!
    nWorkers = "numericOrFunction",

    # nDrones field ----
    #' @field nDrones numeric or function, a number of drones generated in a
    #'   colony - used in \code{\link{createDrones}}, \code{\link{addDrones}},
    #'   \code{\link{buildUp}}.
    #'
    #'   The default value is 100, that is, queen generates 100 drones - this is
    #'   for a down-scaled simulation (for efficiency) assuming that this
    #'   represents ~1,000 drones in a full/strong colony (Seeley, 2019). This
    #'   value is set in \code{SimParamBee$new()} to have a number to work with.
    #'
    #'   You can change this setting to your needs!
    #'
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{nDrones} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(x, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{x} and any following arguments MUST have a
    #'   default value. See \code{\link{nDronesPoisson}},
    #'   \code{\link{nDronesTruncPoisson}}, or
    #'   \code{\link{nDronesColonyPhenotype}} for examples.
    #'
    #'   You can provide your own functions that satisfy your needs!
    nDrones = "numericOrFunction",

    # nVirginQueens field ----
    #' @field nVirginQueens numeric or function, a number of virgin queens
    #'   generated when a queen dies or other situations - used in
    #'   \code{\link{createVirginQueens}} and \code{\link{addVirginQueens}}.
    #'
    #'   The default value is 10, that is, when the queen dies, workers generate
    #'   10 new virgin queens (Seeley, 2019). This value is set in
    #'   \code{SimParamBee$new()} to have a number to work with.
    #'
    #'   You can change this setting to your needs!
    #'
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{nVirginQueens} is a function, it should work with internals
    #'   of other functions. Therefore, the function MUST be defined like
    #'   \code{function(colony, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{colony} and any following arguments MUST have a
    #'   default value. See \code{\link{nVirginQueensPoisson}},
    #'   \code{\link{nVirginQueensTruncPoisson}}, or
    #'   \code{\link{nVirginQueensColonyPhenotype}} for examples.
    #'
    #'   You can provide your own functions that satisfy your needs!
    nVirginQueens = "numericOrFunction",

    # nFathers field ----
    #' @field nFathers numeric or function, a number of drones a queen mates
    #'   with  - used in \code{\link{pullDroneGroupsFromDCA}},
    #'   \code{\link{cross}}.
    #'
    #'   The default value is 15, that is, a virging queen mates on average with
    #'   15 drones (Seeley, 2019). This value is set in \code{SimParamBee$new()}
    #'   to have a number to work with.
    #'
    #'   You can change this setting to your needs!
    #'
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{nFathers} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(arg = default) someCode }, that is, any arguments MUST
    #'   have a default value. We did not use the \code{colony} argument here,
    #'   because \code{nFathers} likely does not depend on the colony. Let us
    #'   know if we are wrong! See \code{\link{nFathersPoisson}} or
    #'   \code{\link{nFathersTruncPoisson}} for examples.
    #'
    #'   You can provide your own functions that satisfy your needs!
    nFathers = "numericOrFunction",

    # swarmP field ----
    #' @field swarmP numeric or a function, the swarm proportion - the proportion
    #'   of workers that leave with the old queen when the colony swarms - used
    #'   in \code{\link{swarm}}.
    #'
    #'   The default value is 0.50, that is, about a half of workers leave colony
    #'   in a swarm (Seeley, 2019). This value is set in \code{SimParamBee$new()}
    #'   to have a proportion to work with.
    #'
    #'   You can change this setting to your needs!
    #'
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{swarmP} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(colony, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{colony} and any following arguments MUST have a
    #'   default value. See \code{\link{swarmPUnif}} or
    #'   \code{\link{swarmPColonyStrength}} for examples.
    #'
    #'   You can provide your own functions that satisfy your needs!
    swarmP = "numericOrFunction",

    # splitP field ----
    #' @field splitP numeric or a function, the split proportion - the
    #'   proportion of workers removed in a managed split - used in
    #'   \code{\link{split}}.
    #'
    #'   The default value is 0.30, that is, about a third of workers is put into
    #'   a split colony from a strong colony (Seeley, 2019). This value is set
    #'   in \code{SimParamBee$new()} to have a proportion to work with.
    #'
    #'   You can change this setting to your needs!
    #'
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{splitP} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(colony, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{colony} and any following arguments MUST have a
    #'   default value. See \code{\link{splitPUnif}} or
    #'   \code{\link{splitPColonyStrength}} for examples.
    #'
    #'   You can provide your own functions that satisfy your needs!
    splitP = "numericOrFunction",

    # downsizeP field ----
    #' @field downsizeP numeric or a function, the downsize proportion - the
    #'   proportion of workers removed from the colony when downsizing, usually
    #'   in autumn - used in \code{\link{downsize}}.
    #'
    #'   The default value is 0.85, that is, a majority of workers die before
    #'   autumn or all die but some winter workers are created (Seeley, 2019).
    #'   This value is set in \code{SimParamBee$new()} to have a proportion to
    #'   work with.
    #'
    #'   You can change this setting to your needs!
    #'
    #'   TODO: Is Seeley (2019) good citation for the defaults in SimParamBee?
    #'         https://github.com/HighlanderLab/SIMplyBee/issues/242
    #'
    #'   When \code{downsizeP} is a function, it should work with internals of
    #'   other functions. Therefore, the function MUST be defined like
    #'   \code{function(colony, arg = default) someCode }, that is, the first
    #'   argument MUST be \code{colony} and any following arguments MUST have a
    #'   default value. See \code{\link{downsizePUnif}} for example.
    #'
    #'   You can provide your own functions that satisfy your needs!
    downsizeP = "numericOrFunction",

    # colonyPheno field ----
    #' @field colonyPheno function, to set colony phenotypes - used in
    #'   \code{\link{setColonyPheno}}.
    #'
    #'   This function should work with internals of others functions -
    #'   therefore the function MUST be defined like \code{function(colony, arg
    #'   = default) someCode }, that is, the first argument MUST be
    #'   \code{colony} and any following arguments MUST have a default value.
    #'   See \code{\link{calcColonyPhenoFromCaste}} for an example.
    #'
    #'   You can provide your own functions that satisfy your needs!
    colonyPheno = "function",

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
    #' @param swarmP see \code{\link{SimParamBee}} field \code{swarmP}
    #' @param splitP see \code{\link{SimParamBee}} field \code{splitP}
    #' @param downsizeP see \code{\link{SimParamBee}} field \code{downsizeP}
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
    #' @param colonyPheno see \code{\link{SimParamBee}} field \code{colonyPheno}
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
                          swarmP = 0.5, splitP = 0.3, downsizeP = 0.85,
                          csdChr = 3, csdPos = 0.865, nCsdAlleles = 128,
                          colonyPheno = NULL) {
      # Get all the goodies from AlphaSimR::SimParam$new(founderPop)
      super$initialize(founderPop)
      private$.versionSIMplyBee <- packageDescription("SIMplyBee")$Version

      # nWorkers, nDrones, and nVirginQueens initialize ----

      self$nWorkers <- nWorkers
      self$nDrones <- nDrones
      self$nVirginQueens <- nVirginQueens
      self$nFathers <- nFathers
      self$swarmP <- swarmP
      self$splitP <- splitP
      self$downsizeP <- downsizeP

      # caste initialize ----

      private$.caste <- NULL

      # last ID initialize ----

      private$.lastColonyId <- 0L

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

      # colonyPheno initialize ----

      if (!is.null(colonyPheno) && !is.function(colonyPheno)) {
        stop("Argument colonyPheno must be a function or NULL!")
      }
      self$colonyPheno <- colonyPheno

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
    #' colony <- cross(colony, fathers = drones)
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
    #' colony <- cross(colony, fathers = drones)
    #' SP$pedigree
    #' SP$caste
    changeCaste = function(id, caste) {
      if (!is.character(id)) {
        stop("Argument id must be a character!")
      }
      private$.caste[id] <- caste[1]
      invisible(self)
    },

    #' @description A function to update the colony last
    #'   ID everytime we create a Colony-class with createColony.
    #'   For internal use only.
    #'
    #' @param lastColonyId integer, last colony ID assigned
    updateLastColonyId = function() {
      private$.lastColonyId = private$.lastColonyId + 1L
      invisible(self)
    }
  ),


  # Private fields ----

  private = list(
    .versionSIMplyBee = "character",
    .caste = "character",
    .lastColonyId = "integer",
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

    #' @field lastColonyId integer, ID of the last Colony object
    #'   created with \code{\link{createColony}}
    lastColonyId = function(value) {
      if (missing(value)) {
          private$.lastColonyId
      } else {
        stop("`$lastColonyId` is read only", call. = FALSE)
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

#' @rdname nWorkersFun
#' @title Sample a number of workers
#'
#' @description Sample a number of workers - used when \code{nInd = NULL}
#'   (see \code{\link{SimParamBee}$nWorkers}).
#'
#'   This is just an example. You can provide your own functions that satisfy
#'   your needs!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param average numeric, average number of workers
#' @param lowerLimit numeric, returned numbers will be above this value
#' @param queenTrait numeric, trait that represents queen's effect on the colony
#'   phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{0} then this effect is 0
#' @param workersTrait numeric, trait that represents workers's effect on the
#'   colony phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{0} then this effect is 0
#' @param checkProduction logical, does the phenotype depend on the production
#'   status of colony; if yes and production is not \code{TRUE}, the result is
#'   above \code{lowerLimit}
#' @param ... other arguments of \code{\link{calcColonyPhenoFromCaste}}
#'
#' @details \code{nWorkersPoisson} samples from a Poisson distribution with a
#'   given average, which can return a value 0. \code{nDronesTruncPoisson}
#'   samples from a zero truncated Poisson distribution.
#'
#'   \code{nWorkersColonyPhenotype} returns a number (above \code{lowerLimit})
#'   as a function of colony phenotype, say queen's fecundity. Colony phenotype
#'   is provided by \code{\link{calcColonyPhenoFromCaste}}. You need to set up
#'   traits influencing the colony phenotype and their parameters (mean and
#'   variances) via \code{\link{SimParamBee}} (see examples).
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
#' nWorkersTruncPoisson()
#' nWorkersTruncPoisson()
#' n <- nWorkersTruncPoisson(n = 1000)
#' hist(n, breaks = seq(from = min(n), to = max(n)), xlim = c(0, 200))
#' table(n)
#'
#' # Example for nWorkersColonyPhenotype()
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' average <- 100
#' h2 <- 0.1
#' SP$addTraitA(nQtlPerChr = 100, mean = average, var = average * h2)
#' SP$setVarE(varE = average * (1 - h2))
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 50)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 2, nFathers = 15)
#' colony1 <- createColony(x = basePop[2])
#' colony2 <- createColony(x = basePop[3])
#' colony1 <- cross(colony1, fathers = fatherGroups[[1]])
#' colony2 <- cross(colony2, fathers = fatherGroups[[2]])
#' colony1@queen@pheno
#' colony2@queen@pheno
#' createWorkers(colony1, nInd = nWorkersColonyPhenotype)
#' createWorkers(colony2, nInd = nWorkersColonyPhenotype)
#' @export
nWorkersPoisson <- function(colony, n = 1, average = 100) {
  return(rpois(n = n, lambda = average))
}

#' @describeIn nWorkersFun Sample a non-zero number of workers
#' @export
nWorkersTruncPoisson <- function(colony, n = 1, average = 100, lowerLimit = 0) {
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @describeIn nWorkersFun Sample a non-zero number of workers based on
#'   colony phenotype, say queen's fecundity
#' @export
nWorkersColonyPhenotype <- function(colony, queenTrait = 1, workersTrait = NULL,
                                    checkProduction = FALSE, lowerLimit = 0,
                                    ...) {
  ret <- round(calcColonyPhenoFromCaste(
    colony = colony,
    queenTrait = queenTrait,
    workersTrait = workersTrait,
    checkProduction = checkProduction,
    ...
  ))
  if (ret < (lowerLimit + 1)) {
    ret <- lowerLimit + 1
  }
  return(ret)
}

#' @rdname nDronesFun
#' @title Sample a number of drones
#'
#' @description Sample a number of drones - used when \code{nDrones = NULL}
#'   (see \code{\link{SimParamBee}$nDrones}).
#'
#'   This is just an example. You can provide your own functions that satisfy
#'   your needs!
#'
#' @param x \code{\link{Pop-class}} or \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param average numeric, average number of drones
#' @param lowerLimit numeric, returned numbers will be above this value
#' @param queenTrait numeric, trait that represents queen's effect on the colony
#'   phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{0} then this effect is 0
#' @param workersTrait numeric, trait that represents workers's effect on the
#'   colony phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{0} then this effect is 0
#' @param checkProduction logical, does the phenotype depend on the production
#'   status of colony; if yes and production is not \code{TRUE}, the result is
#'   above \code{lowerLimit}
#' @param ... other arguments of \code{\link{calcColonyPhenoFromCaste}}
#'
#' @details \code{nDronesPoisson} samples from a Poisson distribution with a
#'   given average, which can return a value 0.
#'
#'   \code{nDronesTruncPoisson} samples from a zero truncated Poisson
#'   distribution.
#'
#'   \code{nDronesColonyPhenotype} returns a number (above \code{lowerLimit}) as
#'   a function of colony phenotype, say queen's fecundity. Colony phenotype is
#'   provided by \code{\link{calcColonyPhenoFromCaste}}. You need to set up
#'   traits influencing the colony phenotype and their parameters (mean and
#'   variances) via \code{\link{SimParamBee}} (see examples).
#'
#'   When \code{x} is \code{\link{Pop-class}}, only \code{workersTrait} is not
#'   used, that is, only \code{queenTrait} is used.
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
#' nDronesTruncPoisson()
#' nDronesTruncPoisson()
#' n <- nDronesTruncPoisson(n = 1000)
#' hist(n, breaks = seq(from = min(n), to = max(n)), xlim = c(0, 200))
#' table(n)
#'
#' # Example for nDronesColonyPhenotype()
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' average <- 100
#' h2 <- 0.1
#' SP$addTraitA(nQtlPerChr = 100, mean = average, var = average * h2)
#' SP$setVarE(varE = average * (1 - h2))
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 50)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 2, nFathers = 15)
#' colony1 <- createColony(x = basePop[2])
#' colony2 <- createColony(x = basePop[3])
#' colony1 <- cross(colony1, fathers = fatherGroups[[1]])
#' colony2 <- cross(colony2, fathers = fatherGroups[[2]])
#' colony1@queen@pheno
#' colony2@queen@pheno
#' createDrones(colony1, nInd = nDronesColonyPhenotype)
#' createDrones(colony2, nInd = nDronesColonyPhenotype)
#' @export
nDronesPoisson <- function(x, n = 1, average = 100) {
  return(rpois(n = n, lambda = average))
}

#' @describeIn nDronesFun Sample a non-zero number of drones
#' @export
nDronesTruncPoisson <- function(x, n = 1, average = 100, lowerLimit = 0) {
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @describeIn nDronesFun Sample a non-zero number of drones based on
#'   colony phenotype, say queen's fecundity
#' @export
nDronesColonyPhenotype <- function(x, queenTrait = 1, workersTrait = NULL,
                                   checkProduction = FALSE, lowerLimit = 0,
                                   ...) {
  # This one is special because we cater drone production from base population
  #   virgin queens and colonies
  if (isPop(x)) {
    ret <- round(x@pheno[, queenTrait])
  } else {
    ret <- round(calcColonyPhenoFromCaste(
      colony = x,
      queenTrait = queenTrait,
      workersTrait = workersTrait,
      checkProduction = checkProduction,
      ...
    ))
  }
  if (ret < (lowerLimit + 1)) {
    ret <- lowerLimit + 1
  }
  return(ret)
}

#' @rdname nVirginQueensFun
#' @title Sample a number of virgin queens
#'
#' @description Sample a number of virgin queens - used when
#'   \code{nFathers = NULL} (see \code{\link{SimParamBee}$nVirginQueens}).
#'
#'   This is just an example. You can provide your own functions that satisfy
#'   your needs!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param average numeric, average number of virgin queens
#' @param lowerLimit numeric, returned numbers will be above this value
#' @param queenTrait numeric, trait that represents queen's effect on the colony
#'   phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{NULL} then this effect is 0
#' @param workersTrait numeric, trait that represents workers's effect on the
#'   colony phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{NULL} then this effect is 0
#' @param checkProduction logical, does the phenotype depend on the production
#'   status of colony; if yes and production is not \code{TRUE}, the result is
#'   above \code{lowerLimit}
#' @param ... other arguments of \code{\link{calcColonyPhenoFromCaste}}
#'
#' @details \code{nVirginQueensPoisson} samples from a Poisson distribution,
#'   which can return a value 0 (that would mean a colony will fail to raise a
#'   single virgin queen after the queen swarms or dies).
#'
#'   \code{nVirginQueensTruncPoisson} samples from a truncated Poisson
#'   distribution (truncated at zero) to avoid failure.
#'
#'   \code{nVirginQueensColonyPhenotype} returns a number (above
#'   \code{lowerLimit}) as a function of colony phenotype, say swarming
#'   tendency. Colony phenotype is provided by
#'   \code{\link{calcColonyPhenoFromCaste}}. You need to set up traits
#'   influencing the colony phenotype and their parameters (mean and variances)
#'   via \code{\link{SimParamBee}} (see examples).
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
#' # Example for nVirginQueensColonyPhenotype()
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' # TODO: Find some means and variances (Vg and Ve) for swarming in honeybees
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/259
#' meanP <- c(10, 0)
#' # setup variances such that the total phenotype variance will match the mean
#' varA <- c(3 / 2, 3 / 2 / SP$nWorkers)
#' corA <- matrix(data = c(
#'   1.0, -0.5,
#'   -0.5, 1.0
#' ), nrow = 2, byrow = TRUE)
#' varE <- c(7 / 2, 7 / 2 / SP$nWorkers)
#' varA / (varA + varE)
#' varP <- varA + varE
#' varP[1] + varP[2] * SP$nWorkers
#' SP$addTraitA(nQtlPerChr = 100, mean = meanP, var = varA, corA = corA)
#' SP$setVarE(varE = varE)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 50)
#' fatherGroups <- pullDroneGroupsFromDCA(drones, n = 2, nFathers = 15)
#' colony1 <- createColony(x = basePop[2])
#' colony2 <- createColony(x = basePop[3])
#' colony1 <- cross(colony1, fathers = fatherGroups[[1]])
#' colony2 <- cross(colony2, fathers = fatherGroups[[2]])
#' colony1 <- buildUp(colony1)
#' colony2 <- buildUp(colony2)
#' nVirginQueensColonyPhenotype(colony1)
#' nVirginQueensColonyPhenotype(colony2)
#' @export
nVirginQueensPoisson <- function(colony, n = 1, average = 10) {
  return(rpois(n = n, lambda = average))
}

#' @describeIn nVirginQueensFun Sample a non-zero number of virgin queens
#' @export
nVirginQueensTruncPoisson <- function(colony, n = 1, average = 10, lowerLimit = 0) {
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

#' @describeIn nVirginQueensFun Sample a non-zero number of virgin queens
#'   based on colony's phenotype, say, swarming tendency
#' @export
nVirginQueensColonyPhenotype <- function(colony, queenTrait = 1,
                                         workersTrait = 2,
                                         checkProduction = FALSE,
                                         lowerLimit = 0, ...) {
  ret <- round(calcColonyPhenoFromCaste(
    colony = colony,
    queenTrait = queenTrait,
    workersTrait = workersTrait, ...
  ))
  if (ret < (lowerLimit + 1)) {
    ret <- lowerLimit + 1
  }
  return(ret)
}

#' @rdname nFathersFun
#' @title Sample a number of fathers
#'
#' @description Sample a number of fathers - use when \code{nFathers = NULL}
#'   (see \code{\link{SimParamBee}$nFathers}).
#'
#'   This is just an example. You can provide your own functions that satisfy
#'   your needs!
#'
#' @param n integer, number of samples
#' @param average numeric, average number of fathers
#' @param lowerLimit numeric, returned numbers will be above this value
#'
#' @details \code{nFathersPoisson} samples from a Poisson distribution, which
#'   can return a value 0 (that would mean a failed queen mating).
#'
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

#' @describeIn nFathersFun Sample a non-zero number of fathers
#' @export
nFathersTruncPoisson <- function(n = 1, average = 15, lowerLimit = 0) {
  return(extraDistr::rtpois(n = n, lambda = average, a = lowerLimit))
}

# pFunctions ----

#' @rdname swarmPFun
#' @title Sample the swarm proportion - the proportion of workers that swarm
#'
#' @description Sample the swarm proportion - the proportion of workers that
#'   swarm - used when \code{p = NULL} (see \code{\link{SimParamBee}$swarmP}).
#'
#'   This is just an example. You can provide your own functions that satisfy
#'   your needs!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param min numeric, lower limit for \code{swarmPUnif}
#' @param max numeric, upper limit for \code{swarmPUnif}
#' @param nWorkersFull numeric, average number of workers in a full/strong
#'   colony for \code{splitPColonyStrength} (actual number can go beyond this
#'   value)
#' @param scale numeric, scaling of numbers in \code{swarmPColonyStrength}
#'   to avoid to narrow range when colonies have a large number of bees (in that
#'   case change \code{nWorkersFull} too!)
#'
#' @details \code{swarmPUnif} samples from a uniform distribution between values
#'   0.4 and 0.6 irrespective of colony strength.
#'
#'   \code{swarmPColonyStrength} samples from a beta distribution with a mean of
#'   \code{a / (a + b)}, where \code{a = nWorkers + nWorkersFull} and \code{b =
#'   nWorkers}. This beta sampling mimics larger swarms for strong colonies and
#'   smaller swarms for weak colonies - see examples. This is just an example,
#'   not based on actual data!
#'   # TODO: should we flip the pattern?
#'   #       https://github.com/HighlanderLab/SIMplyBee/issues/250
#'
#'   The \code{nWorkersFull} default value used in this function is geared
#'   towards a situation where we simulate ~100 workers per colony (down-scaled
#'   simulation for efficiency). If you simulate more workers, you should change
#'   the default accordingly.
#'
#' @seealso \code{\link{SimParamBee}} field \code{swarmP}
#'
#' @return numeric, swarm proportion
#'
#' @examples
#' swarmPUnif()
#' swarmPUnif()
#' p <- swarmPUnif(n = 1000)
#' hist(p, breaks = seq(from = 0, to = 1, by = 0.01), xlim = c(0, 1))
#'
#' # Example for swarmPColonyStrength()
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 15)
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = drones)
#' colony <- addWorkers(colony, nInd = 10)
#' nWorkers(colony) # weak colony
#' swarmPColonyStrength(colony)
#' swarmPColonyStrength(colony)
#' colony <- addWorkers(colony, nInd = 100)
#' nWorkers(colony) # strong colony
#' swarmPColonyStrength(colony)
#' swarmPColonyStrength(colony)
#'
#' # Logic behind swarmPColonyStrength()
#' nWorkersFull <- 100
#' nWorkers <- 0:200
#' swarmP <- 1 - rbeta(
#'   n = length(nWorkers),
#'   shape1 = nWorkers + nWorkersFull,
#'   shape2 = nWorkers
#' )
#' plot(swarmP ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' pStay <- 1 - swarmP
#' plot(pStay ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' # TODO: should we flip the pattern?
#' #       https://github.com/HighlanderLab/SIMplyBee/issues/250
#' @export
swarmPUnif <- function(colony, n = 1, min = 0.4, max = 0.6) {
  return(runif(n = n, min = min, max = max))
}

#' @describeIn swarmPFun Sample the swarm proportion - the proportion of
#'   workers that swarm based on the colony strength
#' @export
swarmPColonyStrength <- function(colony, n = 1, nWorkersFull = 100, scale = 1) {
  nW <- nWorkers(colony)
  pKeep <- rbeta(
    n = n,
    shape1 = (nW + nWorkersFull) / scale,
    shape2 = nW / scale
  )
  return(1 - pKeep)
}

#' @rdname splitPFun
#' @title Sample the split proportion - proportion of removed workers in a
#'   managed split
#'
#' @description Sample the split proportion - proportion of removed workers in a
#'   managed split - used when \code{p = NULL} - (see
#'   \code{\link{SimParamBee}$splitP}).
#'
#'   This is just an example. You can provide your own functions that satisfy
#'   your needs!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param min numeric, lower limit for \code{splitPUnif}
#' @param max numeric, upper limit for \code{splitPUnif}
#' @param nWorkersFull numeric, average number of workers in a full/strong
#'   colony for \code{splitPColonyStrength} (actual number can go beyond this
#'   value)
#' @param scale numeric, scaling of numbers in \code{splitPColonyStrength}
#'   to avoid to narrow range when colonies have a large number of bees (in that
#'   case change \code{nWorkersFull} too!)
#'
#' @details \code{splitPUnif} samples from a uniform distribution between values
#'   0.2 and 0.4 irrespective of colony strength.
#'
#'   \code{splitPColonyStrength} samples from a beta distribution with mean
#'   \code{a / (a + b)}, where \code{a = nWorkers + nWorkersFull} and \code{b =
#'   nWorkers}. This beta sampling mimics larger splits for strong colonies and
#'   smaller splits for weak colonies - see examples. This is just an example,
#'   based on practical experience!
#'
#'   The \code{nWorkersFull} default value used in this function is geared
#'   towards a situation where we simulate ~100 workers per colony (down-scaled
#'   simulation for efficiency). If you simulate more workers, you should change
#'   the default accordingly.
#'
#' @seealso \code{\link{SimParamBee}} field \code{splitP}
#'
#' @return numeric, split proportion
#'
#' @examples
#' splitPUnif()
#' splitPUnif()
#' p <- splitPUnif(n = 1000)
#' hist(p, breaks = seq(from = 0, to = 1, by = 0.01), xlim = c(0, 1))
#'
#' # Example for splitPColonyStrength()
#' founderGenomes <- quickHaplo(nInd = 2, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(x = basePop[1], nInd = 15)
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, fathers = drones)
#' colony <- addWorkers(colony, nInd = 10)
#' nWorkers(colony) # weak colony
#' splitPColonyStrength(colony)
#' splitPColonyStrength(colony)
#' colony <- addWorkers(colony, nInd = 100)
#' nWorkers(colony) # strong colony
#' splitPColonyStrength(colony)
#' splitPColonyStrength(colony)
#'
#' # Logic behind splitPColonyStrength()
#' nWorkersFull <- 100
#' nWorkers <- 0:200
#' splitP <- 1 - rbeta(
#'   n = length(nWorkers),
#'   shape1 = nWorkers + nWorkersFull,
#'   shape2 = nWorkers
#' )
#' plot(splitP ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' pKeep <- 1 - splitP
#' plot(pKeep ~ nWorkers, ylim = c(0, 1))
#' abline(v = nWorkersFull)
#' @export
splitPUnif <- function(colony, n = 1, min = 0.2, max = 0.4) {
  return(runif(n = n, min = min, max = max))
}

#' @describeIn splitPFun Sample the split proportion - the proportion of
#'   removed workers in a managed split based on the colony strength
#' @export
splitPColonyStrength <- function(colony, n = 1, nWorkersFull = 100, scale = 1) {
  nW <- nWorkers(colony)
  pKeep <- rbeta(
    n = n,
    shape1 = (nW + nWorkersFull) / scale,
    shape2 = nW / scale
  )
  return(1 - pKeep)
}

#' @rdname downsizePFun
#' @title Sample the downsize proportion - proportion of removed workers in
#'   downsizing
#'
#' @description Sample the downsize proportion - proportion of removed workers
#'   in downsizing - used when \code{p = NULL} (see
#'   \code{\link{SimParamBee}$downsizeP}).
#'
#'   This is just an example. You can provide your own functions that satisfy
#'   your needs!
#'
#' @param colony \code{\link{Colony-class}}
#' @param n integer, number of samples
#' @param min numeric, lower limit for \code{downsizePUnif}
#' @param max numeric, upper limit for \code{downsizePUnif}
#'
#' @seealso \code{\link{SimParamBee}} field \code{downsizeP}
#'
#' @return numeric, downsize proportion
#'
#' @examples
#' downsizePUnif()
#' downsizePUnif()
#' p <- downsizePUnif(n = 1000)
#' hist(p, breaks = seq(from = 0, to = 1, by = 0.01), xlim = c(0, 1))
#' @export
downsizePUnif <- function(colony, n = 1, min = 0.8, max = 0.9) {
  return(runif(n = n, min = min, max = max))
}

# phenoFunctions ----

#' @rdname calcColonyPhenoFromCaste
#' @title Sample colony phenotype based on caste phenotypes
#'
#' @description Sample colony phenotype based on caste phenotypes -
#'   used when \code{FUN = NULL} in \code{\link{setColonyPheno}} but
#'   \code{\link{SimParamBee$colonyPheno}} is set.
#'
#'   This is just an example - quite flexible one, though;) You can provide your
#'   own functions that satisfy your needs!
#'
#' @param colony \code{\link{Colony-class}}
#' @param queenTrait numeric, trait that represents queen's effect on the colony
#'   phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{NULL} then this effect is 0
#' @param queenFUN function, function that will be applied to the queen phenotype
#' @param workersTrait numeric, trait that represents workers' effect on the colony
#'   phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{NULL} then this effect is 0
#' @param workersFUN function, function that will be applied to the worker phenotypes
#' @param dronesTrait numeric, trait that represents drones' effect on the colony
#'   phenotype (defined in \code{\link{SimParamBee}} - see examples); if
#'   \code{NULL} then this effect is 0
#' @param dronesFUN function, function that will be applied to the drone phenotypes
#' @param combineFUN, function that will combine the queen, worker, and drone effects
#'   - the function expects that this function is defined as \code{function(q, w, d)}
#'   where \code{q} represents queen's effect, \code{q} represents workers' effect,
#'   \code{d} represents drones' effect
#' @param checkProduction logical, does the phenotype depend on the production
#'   status of colony; if yes and production is not \code{TRUE}, the result is
#'   a 0
#' @param ... arguments passed to \code{queenFUN}, \code{workersFUN}, or \code{combineFUN}
#'
#' @seealso \code{\link{SimParamBee}} field \code{colonyPheno}, and functions
#'   \code{\link{getEvents}} and \code{\link{setColonyPheno}} (and its example!)
#'
#' @return numeric matrix with a single value
#'
#' @export
calcColonyPhenoFromCaste <- function(colony,
                                     queenTrait = 1, queenFUN = function(x) x,
                                     workersTrait = 2, workersFUN = sum,
                                     dronesTrait = NULL, dronesFUN = NULL,
                                     combineFUN = function(q, w, d) q + w,
                                     checkProduction = TRUE,
                                    ...) {
  # TODO: should we add checks for other events too? say swarming?
  #       so that this function is useful for many traits
  #       https://github.com/HighlanderLab/SIMplyBee/issues/255
  if (is.null(queenTrait)) {
    queenEff <- 0
  } else {
    queenEff <- queenFUN(colony@queen@pheno[, queenTrait], ...)
  }
  if (is.null(workersTrait)) {
    workersEff <- 0
  } else {
    workersEff <- workersFUN(colony@workers@pheno[, workersTrait], ...)
  }
  if (is.null(dronesTrait)) {
    dronesEff <- 0
  } else {
    dronesEff <- dronesFUN(colony@drones@pheno[, dronesTrait], ...)
  }
  colonyPheno <- combineFUN(q = queenEff, w = workersEff, d = dronesEff, ...)
  if (checkProduction && !colony@production) {
    colonyPheno <- 0
  }
  return(matrix(colonyPheno))
}
