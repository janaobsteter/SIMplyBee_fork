# Level 3 MultiColony Functions

# ---- createMultiColony ----

test_that("createMultiColony", {
  founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 100, simParamBee = SP)
  # Error if individuals x are not vq or q
  expect_error(createMultiColony(drones, n = 2))
  # Error if nInd x is < n
  expect_error(createMultiColony(basePop[3], n = 3))

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = drones, simParamBee = SP)
  # Error if x is not a Pop
  expect_error(createMultiColony(colony, n = 2))

  # Create 2 empty (NULL) colonies
  apiary <- createMultiColony(n = 2)
  expect_s4_class(createMultiColony(n = 2), "MultiColony")
  expect_null(createMultiColony(n = 2)[[1]])
  # Create 2 virgin colonies
  apiary <- createMultiColony(x = basePop[3:4], n = 2)
  # Create mated colonies by crossing
  apiary <- createMultiColony(x = basePop[4:5], n = 2)
  drones <- createDrones(x = basePop[6], n = 30, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 2, nDrones = 5, simParamBee = SP)
  apiary <- cross(apiary, drones = droneGroups, simParamBee = SP)
  # Error if x is not a Pop
  expect_error(createMultiColony(apiary, n = 2))

  #S4 check
  expect_s4_class(createMultiColony(x = basePop[4:5], n = 2), "MultiColony")
})

# ---- selectColonies ----

test_that("selectColonies", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1:4], nInd = 100, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)
  apiary <- createMultiColony(basePop[2:5], n = 4)
  apiary <- cross(apiary, drones = droneGroups[1:4], simParamBee = SP)

  # Error if argument multicolony isn't a multicolony class
  expect_error(selectColonies(basePop))

  # Error: argument ID must be a character or numeric
  expect_error(selectColonies(apiary, ID = TRUE))
  expect_error(selectColonies(apiary, ID = all))
  # Message : if ID isn't provided "Randomly selecting colonies"
  expect_message(selectColonies(apiary, n = 2))
  expect_message(selectColonies(apiary, p = 0.5))
  # Error: n / p/ ID must be provided
  expect_error(selectColonies(apiary))

  # Show how ID can be character or numeric
  expect_s4_class(selectColonies(apiary, ID = "1"), "MultiColony")
  expect_s4_class(selectColonies(apiary, ID = "1")[[1]], "Colony")
  expect_s4_class(selectColonies(apiary, ID = 1), "MultiColony")
  expect_s4_class(selectColonies(apiary, ID = 1)[[1]], "Colony")
  expect_s4_class(selectColonies(apiary, ID = c(1, 2)), "MultiColony")
  expect_s4_class(selectColonies(apiary, ID = c("1", "2"))[[1]], "Colony")
  # ID bug Github issue made

  #Show use of n and p arguments
  expect_s4_class(selectColonies(apiary, n = 1), "MultiColony")
  expect_s4_class(selectColonies(apiary, n = 1)[[1]], "Colony")
  expect_s4_class(selectColonies(apiary, n = 0), "MultiColony")
  expect_s4_class(selectColonies(apiary, p = 0.25), "MultiColony")
  expect_s4_class(selectColonies(apiary, p = 0.25)[[1]], "Colony")
  expect_s4_class(selectColonies(apiary, p = 0), "MultiColony")

  expect_equal(selectColonies(apiary, n = 1)[[1]]@queen@nInd, 1)
})

# ---- pullColonies ----

test_that("pullColonies", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  # Error if argument multicolony isn't a multicolony class
  expect_error(pullColonies(basePop))

  drones <- createDrones(x = basePop[1:4], nInd = 100, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)
  apiary <- createMultiColony(basePop[2:5], n = 4)
  apiary <- cross(apiary, drones = droneGroups[1:4], simParamBee = SP)

  # Error: argument ID must be a character or numeric
  expect_error(pullColonies(apiary, ID = TRUE))
  expect_error(pullColonies(apiary, ID = all))
  # Message : if ID isn't provided "Randomly selecting colonies"
  expect_message(pullColonies(apiary, n = 2))
  expect_message(pullColonies(apiary, p = 0.5))
  # Error: n / p/ ID must be provided
  expect_error(pullColonies(apiary))

  # Show how ID can be character or numeric.    Are both examples needed???
  expect_s4_class(pullColonies(apiary, ID = "1")$pulled, "MultiColony")
  expect_s4_class(pullColonies(apiary, ID = 1)$pulled, "MultiColony")
  expect_s4_class(pullColonies(apiary, ID = c(1, 2))$pulled, "MultiColony")
  # ID bug Github issue made
  #Show use of n and p arguments
  expect_s4_class(pullColonies(apiary, n = 1)$pulled, "MultiColony")
  expect_s4_class(pullColonies(apiary, p = 0.25)$pulled, "MultiColony")


  # Check if pull is working properly
  expect_equal(nColonies(pullColonies(apiary, ID = c(1, 2))$pulled), 2)
  expect_length(pullColonies(apiary, ID = c(1, 2)), 2)
  expect_equal(nColonies(pullColonies(apiary, n = 3)$pulled), 3)
  expect_equal(nColonies(pullColonies(apiary, p = 0.25)$pulled), 1)
})

# ---- removeColonies ----

test_that("removeColonies", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  # Error if argument multicolony isn't a multicolony class
  expect_error(removeColonies(basePop))

  drones <- createDrones(x = basePop[1:4], nInd = 100, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)
  apiary <- createMultiColony(basePop[2:5], n = 4)
  apiary <- cross(apiary, drones = droneGroups[1:4], simParamBee = SP)
  apiary2 <- createMultiColony(n = 0)

  # Error: argument ID must be a character or numeric
  expect_error(pullColonies(apiary, ID = TRUE))
  expect_error(pullColonies(apiary, ID = all))

  expect_s4_class(removeColonies(apiary, ID = 1), "MultiColony")
  expect_equal(nColonies(removeColonies(apiary, ID = 1)), 3)
  expect_s4_class(removeColonies(apiary, ID = "1"), "MultiColony")
  expect_equal(nColonies(removeColonies(apiary, ID = "1")), 3)
})

