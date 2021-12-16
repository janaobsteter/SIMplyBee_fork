test_that("nColonies", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes, simParam = SP)
  drones <- createFounderDrones(pop = basePop[2], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[1], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[1], fathers = drones[6:10])
  apiary <- c(colony1, colony2)
  expect_equal(nColonies(apiary), 2)
  expect_equal(nColonies(createColonies()), 0)
  expect_equal(nColonies(createColonies(n = 10)), 10)
})

test_that("nCaste", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes, simParam = SP)
  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
  colony1 <- addWorkers(colony1, nInd = 10)
  colony1 <- addDrones(colony1, nInd = 15)
  colony1 <- addVirginQueens(colony1, nInd = 3)
  colony2 <- addWorkers(colony2, nInd = 20)
  expect_equal(nCaste(colony1, caste = "queen"), 1)
  expect_equal(nCaste(colony1, caste = "fathers"), 5)
  expect_equal(nCaste(colony1, caste = "virgin_queens"), 3)
  expect_equal(nCaste(colony1, caste = "workers"), 10)
  expect_equal(nCaste(colony1, caste = "drones"), 15)

  apiary <- c(colony1, colony2)
  expect_equal(nCaste(apiary, caste = "queen"), c("2" = 1, "3" = 1))
  expect_equal(nCaste(apiary, caste = "fathers"), c("2" = 5, "3" = 5))
  expect_equal(nCaste(apiary, caste = "virgin_queens"), c("2" = 3, "3" = 1))
  expect_equal(nCaste(apiary, caste = "workers"), c("2" = 10, "3" = 20))
  expect_equal(nCaste(apiary, caste = "drones"), c("2" = 15, "3" = 0))
})

test_that("nQueens", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes, simParam = SP)
  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:8])
  expect_equal(nQueens(colony1), 1)
  expect_equal(nQueens(colony2), 1)
  colony2 <- removeQueen(colony2)
  expect_equal(nQueens(colony2), 0)
  apiary <- c(colony1, colony2)
  expect_equal(nQueens(apiary), c("2" = 1, "NA" = 0))
})

test_that("nDrones", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes, simParam = SP)
  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])

  expect_equal(nDrones(colony1), 0)
  colony1 <- addDrones(colony1, nInd =  5)
  colony2 <- addDrones(colony2, nInd = 10)
  expect_equal(nDrones(colony1), 5)
  expect_equal(nDrones(colony2), 10)
  expect_equal(nDrones(c(colony1, colony2)), c("2" = 5, "3" = 10))
  expect_error(nDrones(basePop))
})

test_that("getCsdHaploGeno", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, nCsdHaplo = 4)
  basePop <- newPop(founderGenomes, simParam = SP)

  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
  colony1 <- addWorkers(colony1, nInd = 10)
  colony2 <- addWorkers(colony2, nInd = 20)
  colony1 <- addDrones(colony1, nInd = 2)
  colony2 <- addDrones(colony2, nInd = 4)
  apiary <- c(colony1, colony2)

  haplo <- getCsdHaplo(getQueen(colony1), simParamBee = SP)
  geno  <- getCsdGeno(getQueen(colony1), simParamBee = SP)
  expect_equal(colSums(haplo), geno, ignore_attr = TRUE)
  # colSums returns a vector not a matrix, hence need ignore_attr = TRUE

  haplo <- getCsdHaplo(colony1, simParamBee = SP)$queen
  geno  <- getCsdGeno(colony1, simParamBee = SP)$queen
  expect_equal(colSums(haplo), geno, ignore_attr = TRUE)
  # colSums returns a vector not a matrix, hence need ignore_attr = TRUE

  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  basePop <- newPop(founderGenomes, simParam = SP)
  expect_error(getCsdHaplo(basePop))
  expect_error(getCsdGeno(basePop))
})

test_that("isGenoHeterozygous", {
  geno <- matrix(data = c(0, 1, 2,
                          0, 0, 0,
                          2, 2, 2),
                 nrow = 3, byrow = TRUE)
  expect_true(isGenoHeterozygous(geno[1, , drop = FALSE]))
  expect_false(isGenoHeterozygous(geno[2, , drop = FALSE]))
  expect_false(isGenoHeterozygous(geno[3, , drop = FALSE]))
  expect_equal(isGenoHeterozygous(geno), c(TRUE, FALSE, FALSE))
})
