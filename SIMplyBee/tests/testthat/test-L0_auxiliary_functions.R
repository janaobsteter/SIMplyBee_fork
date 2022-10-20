####----- nColonies ---- ####
test_that("nColonies", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  expect_equal(nColonies(createMultiColony(n = 2)), 2)
  expect_equal(nColonies(createMultiColony()), 0)
  expect_equal(nColonies(createMultiColony(n = 10)), 10)
})

####----- nCaste ---- ####
test_that("nCaste", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 45)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 15)
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = droneGroups[[1]])
  colony <- buildUp(colony, nDrones = 10, nWorkers = 20)
  expect_equal(nCaste(colony, caste = "queen"), 1)
  expect_equal(nCaste(colony, caste = "fathers"), 15)
  expect_equal(nCaste(colony, caste = "virginQueens"), 0)
  expect_equal(nCaste(colony, caste = "workers"), 20)
  expect_equal(nCaste(colony, caste = "drones"), 10)

  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
  apiary <- buildUp(apiary, nWorkers = 20, nDrones = 10)
  expect_equal(nCaste(apiary, caste = "queen"), c("2" = 1, "3" = 1))
  expect_equal(nCaste(apiary, caste = "fathers"), c("2" = 15, "3" = 15))
  expect_equal(nCaste(apiary, caste = "virginQueens"), c("2" = 0, "3" = 0))
  expect_equal(nCaste(apiary, caste = "workers"), c("2" = 20, "3" = 20))
  expect_equal(nCaste(apiary, caste = "drones"), c("2" = 10, "3" = 10))
})

####----- nQueens ---- ####
test_that("nQueens", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 50)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 15)
  colony1 <- createColony(x = basePop[2])
  colony1 <- cross(colony1, drones = droneGroups[[1]])
  colony2 <- createColony(x = basePop[3])
  colony2 <- cross(colony2, drones = droneGroups[[2]])
  expect_equal(nQueens(colony1), 1)
  expect_equal(nQueens(colony2), 1)
  colony2 <- removeQueen(colony2)
  expect_equal(nQueens(colony2), 0)
  apiary <- c(colony1, colony2)
  expect_equal(nQueens(apiary), c("1" = 1, "2" = 0))
})

####----- nFathers ---- ####
test_that("nFathers", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 100)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = 15)

  matedQueen1 <- cross(x = basePop[2], drones = droneGroups[[1]])
  matedQueen2 <- cross(x = basePop[3], drones = droneGroups[[2]])

  expect_equal(nFathers(matedQueen1), 15)
  expect_equal(nFathers(matedQueen2), 15)
  expect_equal(nFathers(c(matedQueen1, matedQueen2)), c(15, 15))

  colony1 <- createColony(x = basePop[4])
  colony1 <- cross(colony1, drones = droneGroups[[3]])
  colony2 <- createColony(x = basePop[5])
  colony2 <- cross(colony2, drones = droneGroups[[4]])
  expect_equal(nFathers(colony1), 15)
  expect_equal(nFathers(colony2), 15)
  expect_equal(nFathers(c(colony1, colony2)), c("1" = 15, "2" = 15))
})

####----- nDrones ---- ####
test_that("nDrones", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 50)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 15)
  colony1 <- createColony(x = basePop[2])
  colony1 <- cross(colony1, drones = droneGroups[[1]])
  colony2 <- createColony(x = basePop[3])
  colony2 <- cross(colony2, drones = droneGroups[[2]])

  expect_equal(nDrones(colony1), 0)
  colony1 <- addDrones(colony1, nInd = 5)
  colony2 <- addDrones(colony2, nInd = 10)
  expect_equal(nDrones(colony1), 5)
  expect_equal(nDrones(colony2), 10)
  expect_equal(nDrones(c(colony1, colony2)), c("1" = 5, "2" = 10))
  expect_error(nDrones(basePop))
})

####----- isQueenPresent ---- ####
test_that("isQueenPresent", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 50)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 15)
  colony1 <- createColony(x = basePop[2])
  colony1 <- cross(colony1, drones = droneGroups[[1]])
  colony2 <- createColony(x = basePop[3])
  colony2 <- cross(colony2, drones = droneGroups[[2]])
  colony3 <- createColony(x = basePop[4])
  apiary <- c(colony1, colony2, colony3)

  expect_true(isQueenPresent(colony1))
  expect_true(isQueenPresent(colony2))
  expect_equal(isQueenPresent(apiary), c("1" = TRUE, "2" = TRUE, "3" = FALSE))
  expect_false(isQueenPresent(removeQueen(colony1)))
  expect_false(isQueenPresent(supersede(colony2)))
})


####----- getCsd ---- ####
test_that("getCsd", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 50)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 15)
  colony1 <- createColony(x = basePop[2])
  colony1 <- cross(colony1, drones = droneGroups[[1]])
  colony2 <- createColony(x = basePop[3])
  colony2 <- cross(colony2, drones = droneGroups[[2]])
  colony1 <- addWorkers(colony1, nInd = 10)
  colony2 <- addWorkers(colony2, nInd = 20)
  colony1 <- addDrones(colony1, nInd = 2)
  colony2 <- addDrones(colony2, nInd = 4)
  apiary <- c(colony1, colony2)

  haplo <- getCsdAlleles(getQueen(colony1))
  geno <- getCsdGeno(getQueen(colony1))
  expect_equal(colSums(haplo), geno, ignore_attr = TRUE)
  # colSums returns a vector not a matrix, hence need ignore_attr = TRUE

  haplo <- getCsdAlleles(colony1)$queen
  geno <- getCsdGeno(colony1)$queen
  expect_equal(colSums(haplo), geno, ignore_attr = TRUE)
  # colSums returns a vector not a matrix, hence need ignore_attr = TRUE

  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  basePop <- newPop(founderGenomes)
  expect_error(getCsdAlleles(basePop))
  expect_error(getCsdGeno(basePop))
})

####----- isGenoHeterozygous ---- ####
test_that("isGenoHeterozygous", {
  geno <- matrix(
    data = c(
      0, 1, 2,
      0, 0, 0,
      2, 2, 2
    ),
    nrow = 3, byrow = TRUE
  )
  expect_true(SIMplyBee:::isGenoHeterozygous(geno[1, , drop = FALSE]))
  expect_false(SIMplyBee:::isGenoHeterozygous(geno[2, , drop = FALSE]))
  expect_false(SIMplyBee:::isGenoHeterozygous(geno[3, , drop = FALSE]))
  expect_equal(SIMplyBee:::isGenoHeterozygous(geno), c(TRUE, FALSE, FALSE))
})


####----- emptyNULL ---- ####
test_that("emptyNULL", {
   founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
   basePop <- createVirginQueens(founderGenomes, editCsd = FALSE)

   expect_true(isEmpty(new(Class = "Pop")))
   expect_true(isEmpty(basePop[0]))
   expect_false(isEmpty(basePop))

   emptyColony <- createColony()
   nonEmptyColony <- createColony(basePop[1])
   expect_true(isEmpty(emptyColony))
   expect_false(isEmpty(nonEmptyColony))

   emptyApiary <- createMultiColony(n = 3)
   emptyApiary1 <- c(createColony(), createColony())
   nonEmptyApiary <- createMultiColony(basePop[2:5], n = 4)

   expect_true(all(isEmpty(emptyApiary)))
   expect_true(all(isEmpty(emptyApiary1)))
   expect_true(!all(isEmpty(nonEmptyApiary)))
   expect_true(all(isNULLColonies(emptyApiary)))
   expect_true(!all(isNULLColonies(emptyApiary1)))
   expect_true(!all(isNULLColonies(nonEmptyApiary)))

   expect_equal(nEmptyColonies(emptyApiary), 3)
   expect_equal(nEmptyColonies(emptyApiary1), 2)
   expect_equal(nEmptyColonies(nonEmptyApiary), 0)
   expect_equal(nNULLColonies(emptyApiary), 3)
   expect_equal(nNULLColonies(emptyApiary1), 0)
   expect_equal(nNULLColonies(nonEmptyApiary), 0)
})
