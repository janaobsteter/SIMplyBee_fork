


# ---- supersede ----

test_that("supersede", {
    founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    SP$nThreads = 1L
    basePop <- createVirginQueens(founderGenomes)
    drones <- createDrones(basePop[1], n = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

    # Create Colony and MultiColony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, drones = fatherGroups[[1]])
    colony <- buildUp(colony, nWorkers = 100)
    apiary <- createMultiColony(basePop[3:8], n = 6)
    apiary <- cross(apiary, drones = fatherGroups[2:7])
    apiary <- buildUp(apiary, nWorkers = 100)

  expect_true(isQueenPresent(colony))
  expect_true(all(isQueenPresent(apiary)))
  expect_false(isVirginQueensPresent(colony))
  expect_false(all(isVirginQueensPresent(apiary)))
    # Supersede a colony and an apiary
    colony <- supersede(colony)
    apiary <- supersede(apiary)

  expect_false(isQueenPresent(colony))
  expect_false(all(isQueenPresent(apiary)))
  expect_true(isVirginQueensPresent(colony))
  expect_true(all(isVirginQueensPresent(apiary)))
    #test error
    colony <- createColony(x = basePop[2])
    colony@queen <- NULL
  expect_error(supersede(colony))
})

# ---- split ----

test_that("split", {
    founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    SP$nThreads = 1L
    basePop <- createVirginQueens(founderGenomes)
    drones <- createDrones(basePop[1], n = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
    nWorkers = 100

    # Create Colony and MultiColony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, drones = fatherGroups[[1]])
    colony <- buildUp(colony, nWorkers = nWorkers)
    apiary <- createMultiColony(basePop[3:8], n = 6)
    apiary <- cross(apiary, drones = fatherGroups[2:7])
    apiary <- buildUp(apiary, nWorkers = 100)

  expect_error(split(colony, p = 1.2))
    # Split a colony
    tmp <- split(colony, p = 0.5)
    colony <- tmp$remnant
  expect_true(hasSplit(colony))
  expect_equal(nWorkers(colony), nWorkers/2)
    colony@workers <- NULL
  expect_error(split(colony, p = 0.5))
  expect_equal(length(tmp), 2)

    colony <- buildUp(colony, nWorkers = nWorkers)
    tmp <- split(colony, p = 1)
  expect_equal(nWorkers(tmp$split), 100)
    colony <- buildUp(colony, nWorkers = nWorkers)
    colony@queen <- NULL
  expect_error(split(colony, p = 0.5))
    tmp <- split(apiary)
  expect_equal(nColonies(tmp$split), 6)
  expect_equal(nColonies(tmp$remnant), 6)

})

# ---- resetEvents ----

test_that("resetEvents", {
    founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    SP$nThreads = 1L
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 100)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = 10)

    # Create and cross Colony and MultiColony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, drones = fatherGroups[[1]])
    apiary <- createMultiColony(basePop[4:5], n = 2)
    apiary <- cross(apiary, drones = fatherGroups[3:4])

    # Build-up - this sets Productive to TRUE
    colony <- buildUp(colony, nWorkers = 100)
    apiary <- buildUp(apiary, nWorkers = 100)
    # Supersede - this sets Supersede to TRUE
    colony <- supersede(colony)
    apiary <- supersede(apiary)

  expect_true(isProductive(colony))
  expect_true(all(isProductive(apiary)))
  expect_true(hasSuperseded(colony))
  expect_true(all(hasSuperseded(apiary)))

   colony <- resetEvents(colony)
   apiary <- resetEvents(apiary)

  expect_false(isProductive(colony))
  expect_false(all(isProductive(apiary)))
  expect_false(hasSuperseded(colony))
  expect_false(all(hasSuperseded(apiary)))
})

# ---- combine ----

test_that("Combine", {
    founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    SP$nThreads = 1L
    basePop <- createVirginQueens(founderGenomes)
    drones <- createDrones(basePop[1], n = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

    # Create weak and strong Colony and MultiColony class
    colony1 <- createColony(x = basePop[2])
    colony1 <- cross(colony1, drones = fatherGroups[[1]])
    colony2 <- createColony(x = basePop[3])
    colony2 <- cross(colony2, drones = fatherGroups[[2]])
    apiary1 <- createMultiColony(basePop[4:6], n = 3)
    apiary1 <- cross(apiary1, drones = fatherGroups[3:5])
    apiary2 <- createMultiColony(basePop[7:9], n = 3)
    apiary2 <- cross(apiary2, drones = fatherGroups[6:8])

    # Build-up
    colony1 <- buildUp(x = colony1, nWorkers = 100, nDrones = 20)
    colony2 <- buildUp(x = colony2, nWorkers = 20, nDrones = 5)
    apiary1 <- buildUp(x = apiary1, nWorkers = 100, nDrones = 20)
    apiary2 <- buildUp(x = apiary2, nWorkers = 20, nDrones = 5)

    colony3 <- combine(strong = colony1, weak = colony2)
    apiary3 <- combine(strong = apiary1, weak = apiary2)
  expect_equal(nWorkers(colony3),sum(nWorkers(colony1), nWorkers(colony2)))
  expect_equal(colony1@queen@id, colony3@queen@id)
  expect_equal(nWorkers(apiary3[[2]]),sum(nWorkers(apiary1[[2]]), nWorkers(apiary2[[2]])))
    colony1 <- NULL
    colony2 <- NULL
  expect_error(combine(strong = colony1, weak = colony2)) # discus the output
})

# ---- swarm ----

test_that("swarm", {
    founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    SP$nThreads = 1L
    basePop <- createVirginQueens(founderGenomes)
    drones <- createDrones(basePop[1], n = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
    nWorkers = 100
    # Create Colony and MultiColony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, drones = fatherGroups[[1]])
    colony <- buildUp(colony, nWorkers = nWorkers)
    apiary <- createMultiColony(basePop[3:8], n = 6)
    apiary <- cross(apiary, drones = fatherGroups[2:7])
    apiary <- buildUp(apiary, nWorkers = 100)

    # Swarm a colony
    tmp <- swarm(colony, p = 0.5)

  expect_true(isQueenPresent(tmp$swarm))
  expect_true(areVirginQueensPresent(tmp$remnant))
  expect_equal(colony@queen@id, tmp$swarm@queen@id)
  expect_equal(nWorkers(tmp$swarm), nWorkers/2)
  expect_equal(length(tmp), 2)
    colony <- buildUp(colony, nWorkers = nWorkers)
    tmp <- swarm(colony, p = 1)
  expect_equal(nWorkers(tmp$swarm), 100)
    colony <- tmp$swarm
    colony@queen <- NULL
  expect_error(swarm(colony, p = 0.4))

    #Apiary test
    tmp <- swarm(apiary, p = 0.5)
  expect_equal(nColonies(tmp$swarm), 6)
  expect_equal(nColonies(tmp$remnant), 6)
  expect_true(isQueenPresent(tmp$swarm[[4]]))
  expect_true(areVirginQueensPresent(tmp$remnant[[4]]))
})

# ---- collapse -----

test_that("collapse", {
    founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    SP$nThreads = 1L
    basePop <- createVirginQueens(founderGenomes)
    drones <- createDrones(basePop[1], n = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

    # Create Colony and MultiColony class
    colony <- createColony(x = basePop[1])
    colony <- cross(colony, drones = fatherGroups[[1]])
    apiary <- createMultiColony(x = basePop[2:10], n = 9)
    apiary <- cross(apiary, drones = fatherGroups[2:10])

    # Collapse
  expect_false(hasCollapsed(colony))
    colony <- collapse(colony)
  expect_true(hasCollapsed(colony))

  expect_false(all(hasCollapsed(apiary)))
    tmp <- pullColonies(apiary, n = 2)
    apiaryLost <- collapse(tmp$pulled)
  expect_true(all(hasCollapsed(apiaryLost)))
    apiaryLeft <- tmp$remnant
  expect_false(all(hasCollapsed(apiaryLeft)))
})

