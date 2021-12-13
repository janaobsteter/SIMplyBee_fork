test_that("nDrones", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = c(10, 20, 100))
  SP <- SimParam$new(founderGenomes)
  basePop <- newPop(founderGenomes)

  # Honeybees
  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])

  expect_equal(nDrones(colony1), 0)
  colony1 <- addDrones(colony1, nInd = 1000)
  expect_equal(nDrones(colony1), 1000)
  expect_error(nDrones(basePop))
})
