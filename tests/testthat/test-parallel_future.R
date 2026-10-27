# ---- Parallel (future) execution of MultiColony operations ----

runMultiColonyOps <- function(csdChr) {
  set.seed(42)
  founderGenomes <- quickHaplo(nInd = 20, nChr = 2, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = csdChr, nCsdAlleles = 8)
  SP$nThreads <- 1L
  SP$setTrackPed(TRUE)
  SP$nWorkers <- 20
  SP$nDrones <- 5
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1:4], nInd = 30, simParamBee = SP)
  queens <- cross(basePop[5:12], drones = drones, crossPlan = "create",
                  nDrones = 5, simParamBee = SP)
  apiary <- createMultiColony(queens, simParamBee = SP)
  apiary <- buildUp(apiary, simParamBee = SP)
  ret <- list()
  ret$buildUp <- apiary
  ret$pulled <- pullCastePop(apiary, caste = "workers", nInd = 3, simParamBee = SP)
  ret$removed <- removeWorkers(apiary, p = 0.5, simParamBee = SP)
  ret$swarm <- swarm(apiary[1:3], simParamBee = SP)
  ret$supersede <- supersede(apiary[4:5], simParamBee = SP)
  ret$split <- split(apiary[6:8], simParamBee = SP)
  ret$crossed <- cross(ret$supersede, droneColonies = ret$split$remnant,
                       nDrones = 3, crossPlan = "create", simParamBee = SP)
  ret$lastId <- SP$lastId
  ret$lastColonyId <- SP$lastColonyId
  ret$pedigree <- SP$pedigree
  ret$caste <- SP$caste
  ret
}

test_that("MultiColony operations give the same seeded results with sequential and multisession futures", {
  skip_on_cran()
  # Multisession workers load the installed SIMplyBee, which under
  # devtools::load_all() is not the code under test
  skip_if(pkgload::is_dev_package("SIMplyBee"),
          "SIMplyBee is loaded with load_all(); run via R CMD check")
  oplan <- future::plan()
  oopts <- options(future.rng.onMisuse = "error")
  on.exit({
    future::plan(oplan)
    options(oopts)
  }, add = TRUE)

  for (csdChr in list(NULL, 1)) {
    future::plan(future::sequential)
    seqRes <- runMultiColonyOps(csdChr)
    future::plan(future::multisession, workers = 2)
    parRes <- runMultiColonyOps(csdChr)
    compare <- names(seqRes)
    if (!is.null(csdChr)) {
      # With csd, sequential and multisession record different castes for the
      # discarded homozygous brood, which is not part of any population
      compare <- setdiff(compare, "caste")
    }
    for (n in compare) {
      expect_identical(parRes[[n]], seqRes[[n]], label = paste0(n, " (csdChr = ", format(csdChr), ")"))
    }
  }
})

test_that("createCastePop on MultiColony keeps simParamBee bookkeeping consistent with sequential futures", {
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan), add = TRUE)
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads <- 1L
  SP$setTrackPed(TRUE)
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1:2], nInd = 20, simParamBee = SP)
  queens <- cross(basePop[3:6], drones = drones, crossPlan = "create",
                  nDrones = 5, simParamBee = SP)

  lastColonyId <- SP$lastColonyId
  apiary <- createMultiColony(queens, simParamBee = SP)
  # Each colony gets exactly one new ID
  expect_equal(SP$lastColonyId, lastColonyId + 4)
  expect_equal(getId(apiary), lastColonyId + 1:4)

  apiary <- addWorkers(apiary, nInd = c(3, 1, 5, 2), simParamBee = SP)
  apiary <- addVirginQueens(apiary, nInd = 2, simParamBee = SP)
  # One caste record per individual, matching the pedigree
  expect_false(anyDuplicated(names(SP$caste)) > 0)
  expect_identical(length(SP$caste), as.integer(SP$lastId))
  expect_identical(nrow(SP$pedigree), as.integer(SP$lastId))
  expect_true(all(SP$caste[getCasteId(apiary, caste = "workers", collapse = TRUE, simParamBee = SP)] == "workers"))
  expect_true(all(SP$caste[getCasteId(apiary, caste = "virginQueens", collapse = TRUE, simParamBee = SP)] == "virginQueens"))
  expect_equal(unname(nWorkers(apiary, simParamBee = SP)), c(3, 1, 5, 2))
})

test_that("swarm on MultiColony keeps simParamBee records and colonies consistent", {
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan), add = TRUE)
  for (csdChr in list(NULL, 1)) {
    set.seed(7)
    founderGenomes <- quickHaplo(nInd = 30, nChr = 2, segSites = 100)
    SP <- SimParamBee$new(founderGenomes, csdChr = csdChr, nCsdAlleles = 16)
    SP$nThreads <- 1L
    SP$setTrackPed(TRUE)
    SP$setTrackRec(TRUE)
    SP$nWorkers <- 20
    SP$nDrones <- 5
    basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
    drones <- createDrones(basePop[1:5], nInd = 30, simParamBee = SP)
    queens <- cross(basePop[6:15], drones = drones, crossPlan = "create",
                    nDrones = 8, simParamBee = SP)
    apiary <- buildUp(createMultiColony(queens, simParamBee = SP), simParamBee = SP)
    oldQueens <- getCasteId(apiary, caste = "queen", collapse = TRUE, simParamBee = SP)
    lastId <- SP$lastId

    tmp <- swarm(apiary, simParamBee = SP)

    # Records of all created virgin queens (also the ones that did not prevail)
    expect_equal(SP$lastId, lastId + nColonies(apiary) * max(10, SP$nVirginQueens))
    expect_identical(nrow(SP$pedigree), as.integer(SP$lastId))
    expect_identical(length(SP$recHist), as.integer(SP$lastId))
    expect_identical(length(SP$caste), as.integer(SP$lastId))
    expect_identical(rownames(SP$pedigree), as.character(seq_len(SP$lastId)))
    expect_identical(names(SP$recHist), as.character(seq_len(SP$lastId)))
    newIds <- as.character((lastId + 1):SP$lastId)
    expect_true(all(SP$caste[newIds] == "virginQueens"))

    # Remnants have one virgin queen from their old queen, swarms the old queen
    expect_identical(getId(tmp$remnant), getId(apiary))
    expect_false(any(isQueenPresent(tmp$remnant, simParamBee = SP)))
    expect_true(all(nVirginQueens(tmp$remnant, simParamBee = SP) == 1))
    virginQueens <- getCasteId(tmp$remnant, caste = "virginQueens", collapse = TRUE, simParamBee = SP)
    expect_true(all(virginQueens %in% newIds))
    expect_equal(unname(SP$pedigree[virginQueens, "mother"]), as.integer(oldQueens))
    if (!is.null(csdChr)) {
      expect_true(all(isCsdHeterozygous(mergePops(getVirginQueens(tmp$remnant, simParamBee = SP)),
                                        simParamBee = SP)))
    }
    expect_identical(unname(getCasteId(tmp$swarm, caste = "queen", collapse = TRUE, simParamBee = SP)),
                     unname(oldQueens))
    # Workers are split between the swarm and the remnant
    expect_equal(unname(nWorkers(tmp$swarm, simParamBee = SP) + nWorkers(tmp$remnant, simParamBee = SP)),
                 unname(nWorkers(apiary, simParamBee = SP)))
    expect_false(any(getCasteId(tmp$swarm, caste = "workers", collapse = TRUE, simParamBee = SP) %in%
                       getCasteId(tmp$remnant, caste = "workers", collapse = TRUE, simParamBee = SP)))
  }
})

test_that("SimParamBee$slimClone() drops individual records but can create individuals", {
  founderGenomes <- quickHaplo(nInd = 20, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads <- 1L
  SP$setTrackPed(TRUE)
  SP$setTrackRec(TRUE)
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1:2], nInd = 50, simParamBee = SP)
  queen <- cross(basePop[3], drones = drones, simParamBee = SP)
  before <- list(SP$lastId, SP$pedigree, SP$recHist, SP$caste)

  slim <- SP$slimClone()
  expect_equal(nrow(slim$pedigree), 0)
  expect_length(slim$recHist, 0)
  expect_length(slim$caste, 0)
  expect_null(slim$founderPop)
  expect_identical(slim$lastId, SP$lastId)
  # The copy does not grow with the records (and does not enclose the original
  # object, which would serialise them too)
  sizeSP <- length(serialize(SP, NULL))
  sizeSlim <- length(serialize(slim, NULL))
  moreDrones <- createDrones(basePop[4:10], nInd = 1000, simParamBee = SP)
  expect_gt(length(serialize(SP, NULL)), 1.5 * sizeSP)
  expect_lt(length(serialize(SP$slimClone(), NULL)), 1.05 * sizeSlim)
  before <- list(SP$lastId, SP$pedigree, SP$recHist, SP$caste)

  offspring <- SIMplyBee:::combineBeeGametes(queen = queen, drones = getFathers(queen, simParamBee = slim),
                                 nProgeny = 3, simParamBee = slim)
  expect_equal(nInd(offspring), 3)
  expect_equal(nrow(slim$pedigree), 3)
  # The original is unchanged
  expect_identical(list(SP$lastId, SP$pedigree, SP$recHist, SP$caste), before)
})

test_that("supersede on MultiColony keeps simParamBee records and colonies consistent", {
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan), add = TRUE)
  for (csdChr in list(NULL, 1)) {
    set.seed(8)
    founderGenomes <- quickHaplo(nInd = 30, nChr = 2, segSites = 100)
    SP <- SimParamBee$new(founderGenomes, csdChr = csdChr, nCsdAlleles = 16)
    SP$nThreads <- 1L
    SP$setTrackPed(TRUE)
    SP$setTrackRec(TRUE)
    SP$nWorkers <- 20
    SP$nDrones <- 5
    basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
    drones <- createDrones(basePop[1:5], nInd = 30, simParamBee = SP)
    queens <- cross(basePop[6:15], drones = drones, crossPlan = "create",
                    nDrones = 8, simParamBee = SP)
    apiary <- buildUp(createMultiColony(queens, simParamBee = SP), simParamBee = SP)
    oldQueens <- getCasteId(apiary, caste = "queen", collapse = TRUE, simParamBee = SP)
    lastId <- SP$lastId

    superseded <- supersede(apiary, simParamBee = SP)

    expect_equal(SP$lastId, lastId + nColonies(apiary) * max(10, SP$nVirginQueens))
    expect_identical(nrow(SP$pedigree), as.integer(SP$lastId))
    expect_identical(length(SP$recHist), as.integer(SP$lastId))
    expect_identical(length(SP$caste), as.integer(SP$lastId))
    expect_identical(rownames(SP$pedigree), as.character(seq_len(SP$lastId)))
    newIds <- as.character((lastId + 1):SP$lastId)
    expect_true(all(SP$caste[newIds] == "virginQueens"))

    expect_identical(getId(superseded), getId(apiary))
    expect_true(all(hasSuperseded(superseded)))
    expect_false(any(isQueenPresent(superseded, simParamBee = SP)))
    expect_true(all(nVirginQueens(superseded, simParamBee = SP) == 1))
    virginQueens <- getCasteId(superseded, caste = "virginQueens", collapse = TRUE, simParamBee = SP)
    expect_true(all(virginQueens %in% newIds))
    expect_equal(unname(SP$pedigree[virginQueens, "mother"]), as.integer(oldQueens))
    if (!is.null(csdChr)) {
      expect_true(all(isCsdHeterozygous(mergePops(getVirginQueens(superseded, simParamBee = SP)),
                                        simParamBee = SP)))
    }
    # Workers and drones stay
    for (caste in c("workers", "drones")) {
      expect_identical(lapply(superseded@colonies, function(colony) slot(colony, caste)@id),
                       lapply(apiary@colonies, function(colony) slot(colony, caste)@id))
    }
  }
})

test_that("createPrevailingVirginQueens_internal handles one or several nInd values", {
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan), add = TRUE)
  founderGenomes <- quickHaplo(nInd = 12, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads <- 1L
  SP$setTrackPed(TRUE)
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1:2], nInd = 30, simParamBee = SP)
  queens <- cross(basePop[3:5], drones = drones, crossPlan = "create",
                  nDrones = 5, simParamBee = SP)
  apiary <- createMultiColony(queens, simParamBee = SP)
  createVQ <- SIMplyBee:::createPrevailingVirginQueens_internal

  lastId <- SP$lastId
  nInd <- c(2, 5, 3)
  virginQueens <- createVQ(apiary, nInd = nInd, simParamBee = SP)
  expect_equal(SP$lastId, lastId + sum(nInd))
  expect_identical(names(virginQueens), as.character(getId(apiary)))
  # Each colony's virgin queen comes from its own block of IDs and its queen
  idBlocks <- base::split(as.character(lastId + seq_len(sum(nInd))), rep(1:3, nInd))
  for (colony in 1:3) {
    expect_equal(nInd(virginQueens[[colony]]), 1)
    expect_true(virginQueens[[colony]]@id %in% idBlocks[[colony]])
    expect_equal(unname(SP$pedigree[virginQueens[[colony]]@id, "mother"]),
                 as.integer(apiary@colonies[[colony]]@queen@id))
  }
  expect_true(all(SP$caste[unlist(idBlocks)] == "virginQueens"))

  lastId <- SP$lastId
  virginQueens <- createVQ(apiary, nInd = 4, simParamBee = SP)
  expect_equal(SP$lastId, lastId + 3 * 4)
  expect_error(createVQ(apiary, nInd = c(1, 2), simParamBee = SP))
  expect_error(createVQ(apiary, nInd = c(1, 0, 2), simParamBee = SP))
})
