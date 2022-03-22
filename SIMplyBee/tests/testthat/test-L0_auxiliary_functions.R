test_that("nColonies", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes)
  drones <- createFounderDrones(pop = basePop[2], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[1], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[2], fathers = drones[6:10])
  apiary <- c(colony1, colony2)
  expect_equal(nColonies(apiary), 2)
  expect_equal(nColonies(createColonies()), 0)
  expect_equal(nColonies(createColonies(nCol = 10)), 10)
})

test_that("nCaste", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes)
  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
  colony1 <- addWorkers(colony1, nInd = 10)
  colony1 <- addDrones(colony1, nInd = 15)
  colony1 <- addVirginQueens(colony1, nInd = 3)
  colony2 <- addWorkers(colony2, nInd = 20)
  expect_equal(nCaste(colony1, caste = "queen"), 1)
  expect_equal(nCaste(colony1, caste = "fathers"), 5)
  expect_equal(nCaste(colony1, caste = "virginQueens"), 3)
  expect_equal(nCaste(colony1, caste = "workers"), 10)
  expect_equal(nCaste(colony1, caste = "drones"), 15)

  apiary <- c(colony1, colony2)
  expect_equal(nCaste(apiary, caste = "queen"), c("2" = 1, "3" = 1))
  expect_equal(nCaste(apiary, caste = "fathers"), c("2" = 5, "3" = 5))
  expect_equal(nCaste(apiary, caste = "virginQueens"), c("2" = 3, "3" = 0))
  expect_equal(nCaste(apiary, caste = "workers"), c("2" = 10, "3" = 20))
  expect_equal(nCaste(apiary, caste = "drones"), c("2" = 15, "3" = 0))
})

test_that("nQueens", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes)
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

test_that("nFathers", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes)
  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  matedQueen1 <- crossVirginQueen(pop = basePop[2], fathers = drones[1:5])
  matedQueen2 <- crossVirginQueen(pop = basePop[3], fathers = drones[6:9])

  expect_equal(nFathers(matedQueen1), 5)
  expect_equal(nFathers(matedQueen2), 4)
  expect_equal(nFathers(c(basePop[2], matedQueen1, matedQueen2)), c(0, 5, 4))

  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:9])
  expect_equal(nFathers(colony1), 5)
  expect_equal(nFathers(colony2), 4)
  expect_equal(nFathers(c(colony1, colony2)), c("2" = 5, "3" = 4))
})

test_that("nDrones", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes)
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

test_that("isQueenMated", {
  founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes)
  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
  colony3 <- createColony(virginQueens = basePop[4])
  apiary <- c(colony1, colony2, colony3)

  expect_true(isQueenMated(getQueen(colony1)))
  expect_true(isQueenMated(colony1))
  expect_true(isQueenMated(colony2))
  expect_equal(isQueenMated(apiary), c("2" = TRUE, "3" = TRUE, `NA` = FALSE))
  expect_false(isQueenMated(removeQueen(colony1)))
  expect_false(isQueenMated(supersedeColony(colony2)))
  expect_equal(isQueenMated(c(getQueen(colony1), getQueen(colony2),
                        getVirginQueens(colony3))),
               c(TRUE, TRUE, FALSE))
})

test_that("getCsd", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 4)
  basePop <- newPop(founderGenomes)

  drones <- createDrones(x = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])
  colony1 <- addWorkers(colony1, nInd = 10)
  colony2 <- addWorkers(colony2, nInd = 20)
  colony1 <- addDrones(colony1, nInd = 2)
  colony2 <- addDrones(colony2, nInd = 4)
  apiary <- c(colony1, colony2)

  haplo <- getCsdAlleles(getQueen(colony1))
  geno  <- getCsdGeno(getQueen(colony1))
  expect_equal(colSums(haplo), geno, ignore_attr = TRUE)
  # colSums returns a vector not a matrix, hence need ignore_attr = TRUE

  haplo <- getCsdAlleles(colony1)$queen
  geno  <- getCsdGeno(colony1)$queen
  expect_equal(colSums(haplo), geno, ignore_attr = TRUE)
  # colSums returns a vector not a matrix, hence need ignore_attr = TRUE

  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  basePop <- newPop(founderGenomes)
  expect_error(getCsdAlleles(basePop))
  expect_error(getCsdGeno(basePop))
})

test_that("isGenoHeterozygous", {
  geno <- matrix(data = c(0, 1, 2,
                          0, 0, 0,
                          2, 2, 2),
                 nrow = 3, byrow = TRUE)
  expect_true(SIMplyBee:::isGenoHeterozygous(geno[1, , drop = FALSE]))
  expect_false(SIMplyBee:::isGenoHeterozygous(geno[2, , drop = FALSE]))
  expect_false(SIMplyBee:::isGenoHeterozygous(geno[3, , drop = FALSE]))
  expect_equal(SIMplyBee:::isGenoHeterozygous(geno), c(TRUE, FALSE, FALSE))
})

