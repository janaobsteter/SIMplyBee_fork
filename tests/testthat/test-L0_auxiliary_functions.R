# ---- nColonies ----
test_that("nColonies", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  expect_equal(nColonies(createMultiColony(n = 2)), 2)
  expect_equal(nColonies(createMultiColony()), 0)
  expect_equal(nColonies(createMultiColony(n = 10)), 10)
})


# ---- nCaste ----
test_that("nCaste", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)

  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 45, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 10, simParamBee = SP)
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
  colony <- buildUp(colony, nDrones = 15, nWorkers = 20, simParamBee = SP)
  expect_equal(nCaste(colony, caste = "queen"), 1)
  expect_equal(nCaste(colony, caste = "drones"), 10)
  expect_equal(nCaste(colony, caste = "virginQueens"), 0)
  expect_equal(nCaste(colony, caste = "workers"), nWorkers(colony))
  expect_equal(nCaste(colony, caste = "fathers"), 10)

  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
  apiary <- buildUp(apiary, nWorkers = 20, nDrones = 10, simParamBee = SP)
  expect_equal(sum(nCaste(apiary, caste = "queen")), 2)
  expect_equal(sum(nCaste(apiary, caste = "drones")), 10)
  expect_equal(sum(nCaste(apiary, caste = "virginQueens")), 0)
  expect_equal(sum(nCaste(apiary, caste = "workers")), sum(nWorkers(apiary)))
  expect_equal(sum(nCaste(apiary, caste = "fathers")), 20)
})

# ---- nQueens ----

test_that("nQueens", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  colony1 <- createColony(x = basePop[1], simParamBee = SP)
  colony1 <- cross(colony1, drones = fatherGroups[[1]], simParamBee = SP)
  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = fatherGroups[c(2, 3)], simParamBee = SP)

  expect_equal(nQueens(colony1), 1)
  colony1 <- removeQueen(colony1, simParamBee = SP)
  expect_equal(nQueens(colony1), 0)
  expect_equal(sum(nQueens(apiary)), 2)
})

# ---- nDrones ----
test_that("nDrones", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  colony1 <- createColony(x = basePop[1], simParamBee = SP)
  colony1 <- cross(colony1, drones = fatherGroups[[1]], simParamBee = SP)
  colony2 <- createColony(x = basePop[2], simParamBee = SP)
  colony2 <- cross(colony2, drones = fatherGroups[[2]], simParamBee = SP)


  expect_equal(nDrones(colony1), 0)
  colony1 <- addDrones(colony1, nInd = 5, simParamBee = SP)
  colony2 <- addDrones(colony2, nInd = 10, simParamBee = SP)
  expect_equal(nDrones(colony1), 5)
  expect_equal(nDrones(colony2), 10)
  expect_equal(sum(nDrones(c(colony1, colony2))), 15)
  expect_error(nDrones(basePop))
})

# ---- isGenoHeterozygous ----

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

# ---- isCaste ----

test_that("isCaste", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)

  # get the queen that is a caste "queen" and ask if it is a caste "queen"
  # and drones and drones
  expect_true(isCaste(getQueen(colony), caste = "queen", simParamBee = SP))
  expect_true(all(isCaste(getDrones(colony), caste = "drones", simParamBee = SP)))
  expect_true(all(isCaste(getDrones(colony), caste = "drones", simParamBee = SP)))
  # get the queen that is a caste "queen" and test if it is a caste "workers",
  #test on virgin queen that is not present in a colony
  expect_false(isCaste(getQueen(colony), caste = "workers", simParamBee = SP))
  #test on virgin queen that is not present in a colony
  expect_null(isCaste(getVirginQueens(colony), caste = "virginQueens", simParamBee = SP))
  malePop <- c(getDrones(colony), getFathers(colony))
  expect_true(any(isCaste(malePop, caste = "drones", simParamBee = SP)))
  expect_true(any(isCaste(malePop, caste = "fathers", simParamBee = SP)))
  expect_false(all(isCaste(malePop, caste = "drones", simParamBee = SP)))
  expect_false(all(isCaste(malePop, caste = "fathers", simParamBee = SP)))
})

# ---- calcQueensPHomBrood ----

test_that("calcQueensPHomBrood", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)

  expect_error(calcQueensPHomBrood(colony@drones))
  expect_error(calcQueensPHomBrood(colony@workers))
  expect_true(is.numeric(calcQueensPHomBrood(colony@queen)))

  colony@queen <- NULL
  expect_error(calcQueensPHomBrood(colony@queen))
  apiary <- createMultiColony()
  colony@workers <- NULL
  colony@drones <- NULL
  colony@virginQueens <- NULL
  expect_error(calcQueensPHomBrood(colony))
  expect_equal((length(calcQueensPHomBrood(apiary))), 0)
})

# ---- pHomBrood ----

test_that("pHomBrood", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)

  expect_error(pHomBrood(colony@workers))
  expect_error(pHomBrood(colony@virginQueens))
  expect_error(pHomBrood(colony@drones))
  expect_true(is.numeric(pHomBrood(colony@queen)))

  colony@queen <- NULL
  expect_error(pHomBrood(colony@queen))
  apiary <- createMultiColony()
  colony@workers <- NULL
  colony@drones <- NULL
  colony@virginQueens <- NULL
  expect_error(pHomBrood(colony))
  expect_equal(length(pHomBrood(apiary)), 0)
})

# ---- nHomBrood -----

test_that("nHomBrood", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)

  expect_error(nHomBrood(colony@workers))
  expect_error(nHomBrood(colony@virginQueens))
  expect_error(nHomBrood(colony@drones))
  expect_true(is.numeric(nHomBrood(colony@queen)))

  colony@queen <- NULL
  expect_error(nHomBrood(colony@queen))
  apiary <- createMultiColony()
  colony@workers <- NULL
  colony@drones <- NULL
  colony@virginQueens <- NULL
  expect_error(nHomBrood(colony))
  expect_equal(length(nHomBrood(apiary)), 0)
})

# ---- isQueenPresent ----

test_that("isQueenPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony()
  colony2 <- createColony(simParamBee = SP)

  expect_true(isQueenPresent(colony))
  expect_false(isQueenPresent(apiary))
  expect_error(isQueenPresent(vec))
  expect_true(is.vector(isQueenPresent(apiary2)))
  expect_false(isQueenPresent(apiary2))
  expect_false(isQueenPresent(colony2))
})

# ---- isVirginQueensPresent ----

test_that("isVirginQueensPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony()
  colony2 <- createColony(simParamBee = SP)


  expect_true(isVirginQueensPresent(colony))
  expect_false(isVirginQueensPresent(apiary))
  expect_error(isVirginQueenPresent(vec))
  expect_true(is.vector(isVirginQueensPresent(apiary2)))
  expect_false(isVirginQueensPresent(apiary2))
  expect_false(isVirginQueensPresent(colony2))
})

# ---- isProductive ----

test_that("isProductive", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)

  expect_false(isProductive(colony))
  colony <- buildUp(x = colony, simParamBee = SP)
  expect_true(isProductive(colony))

  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = fatherGroups[c(2, 3)], simParamBee = SP)

  expect_false(all(isProductive(apiary)))
  apiary <- buildUp(x = apiary, simParamBee = SP)
  expect_true(all(isProductive(apiary)))

  colony <- createColony(simParamBee = SP)
  expect_false(isProductive(colony))
  colony <- NULL
  expect_error(isProductive(colony))

  apiary <- createMultiColony()
  expect_true(is.list(isProductive(apiary)))
})
# ---- reduceDroneHaplo ----

test_that("reduceDroneHaplo", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 2, simParamBee = SP)
  virginQueens <- c(basePop[2:3])
  vec <- c(1,2,"a")
  tmp <- getSegSiteHaplo(virginQueens, simParamBee = SP)
  df <- as.data.frame(tmp)
  tmpD <- getSegSiteHaplo(drones, simParamBee = SP)
  mix <- rbind(tmp, tmpD)

  expect_equal(nrow(reduceDroneHaplo(haplo = mix, pop = c(drones, virginQueens))), 6)
  expect_equal(nrow(reduceDroneHaplo(haplo = tmpD, pop = drones)), 2)
  expect_error(reduceDroneHaplo(haplo = vec, pop = drones))
  expect_error(reduceDroneHaplo(haplo = df, pop = drones))
  expect_true(is.matrix(reduceDroneHaplo(haplo = tmpD, pop = drones)))
  tmp <- reduceDroneHaplo(haplo = tmpD, pop = drones)
  expect_true(all(rle(as.vector(tmp))$values %in% 0:1))
})

# ---- reduceDroneGeno ----

test_that("reduceDroneGeno", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 2, simParamBee = SP)
  virginQueens <- c(basePop[2:3])
  vec <- c(1,2,"a")
  tmp <- getSegSiteGeno(virginQueens, simParamBee = SP)
  df <- as.data.frame(tmp)
  tmpD <- getSegSiteGeno(drones, simParamBee = SP)


  expect_equal(nrow(reduceDroneGeno(geno = tmpD, pop = drones)), 2)
  expect_error(reduceDroneGeno(geno = tmp, pop = queens))
  expect_error(reduceDroneGeno(geno = vec, pop = drones))
  expect_error(reduceDroneGeno(geno =  df, pop = drones))
  expect_true(is.matrix(reduceDroneGeno(geno = tmpD, pop = drones)))
  tmp <- reduceDroneGeno(geno = tmpD, pop = drones)
  expect_true(all(rle(as.vector(tmp))$values %in% 0:1))
})

# ---- getCsdAlleles ----

test_that("getCsdAlleles", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2],simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_true(is.list(getCsdAlleles(colony, simParamBee = SP)))
  expect_true(is.matrix(getCsdAlleles(getQueen(colony), simParamBee = SP)))

  # set CSD to NULL
  rm(SP)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_error(getCsdAlleles(colony, simParamBee = SP))

  # test unique and colapse
  rm(SP)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)
  expect_true(is.matrix(getCsdAlleles(colony, collapse = TRUE, simParamBee = SP)))
  expect_equal(nrow(getCsdAlleles(colony, collapse = TRUE, simParamBee = SP)),
               sum(nQueens(colony)*2, nDrones(colony), nWorkers(colony)*2, nFathers(colony),
                   nVirginQueens(colony)*2))
  expect_equal(nrow(getCsdAlleles(colony, collapse = TRUE, unique = TRUE, simParamBee = SP)),
               nrow(unique(getCsdAlleles(colony, collapse = TRUE, simParamBee = SP))))
  expect_true(nrow(getCsdAlleles(colony, collapse = TRUE, unique = TRUE, simParamBee = SP)) <= SP$nCsdAlleles)

})

# ---- getCsdGeno ----

test_that("getCsdGeno", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_true(is.list(getCsdGeno(colony, simParamBee = SP)))
  expect_true(is.matrix(getCsdGeno(getQueen(colony, simParamBee = SP))))
  expect_true(nCsdAlleles(colony, collapse = TRUE, simParamBee = SP) <= SP$nCsdAlleles)

  geno <- getCsdGeno(colony, simParamBee = SP)
  expect_equal(nrow(geno$fathers), 10)

  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_error(getCsdGeno(colony, simParamBee = SP))

})

# ---- isCsdHeterozygous ----

test_that("isCsdHeterozygous", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)
  colony@virginQueens <- createVirginQueens(colony, nInd = 1, simParamBee = SP)

  expect_true(isCsdHeterozygous(colony@queen, simParamBee = SP))
  expect_true(is.vector(isCsdHeterozygous(colony@workers, simParamBee = SP)))
  expect_false(all(isCsdHeterozygous(colony@drones, simParamBee = SP)))
  expect_true(isCsdHeterozygous(colony@virginQueens, simParamBee = SP))

  # set CSD to NULL
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_error(isCsdHeterozygous(colony@queen, simParamBee = SP))
})

# ---- nCsdAlleles ----

test_that("nCsdAlleles", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_equal(nCsdAlleles(colony@queen, simParamBee = SP), 2)
  expect_equal(nCsdAlleles(colony@workers[10], simParamBee = SP), 2)
  expect_equal(nCsdAlleles(colony@drones[9], simParamBee = SP), 1)
  expect_true(is.integer(nCsdAlleles(colony@queen, simParamBee = SP)))

  # set CSD to NULL
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_error(nCsdAlleles(colony@queen, simParamBee = SP), 2)

  #collapse argument
  nCsdAlleles <- 5
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = nCsdAlleles)
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)
  expect_true(is.numeric(nCsdAlleles(colony, collapse = TRUE, simParamBee = SP)))
  expect_true(nCsdAlleles(colony, collapse = TRUE, simParamBee = SP) <= nCsdAlleles)
})

# ---- calcBeeGRMIbs ----

test_that("calcBeeGRMIbs", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  SP$setTrackRec(TRUE)
  SP$setTrackPed(isTrackPed = TRUE)
  SP$addTraitA(10)
  SP$addSnpChip(5)

  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)
  apiary <- createMultiColony(basePop[2:3], n = 2)
  apiary <- cross(x = apiary, drones = fatherGroups[c(2, 3)], simParamBee = SP)
  apiary <- buildUp(x = apiary, simParamBee = SP)
  apiary <- addVirginQueens(x = apiary, nInd = 5, simParamBee = SP)

  genoQ <- getQueenSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoF <- getFathersSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoW <- getWorkersSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoD <- getDronesSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoV <- getVirginQueensSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoMeanW <- apply(X = genoW, MARGIN = 2, FUN = mean)
  genoMeanD <- apply(X = genoD, MARGIN = 2, FUN = mean)

  geno <- rbind(genoQ, genoF, genoW, genoD, genoV, genoMeanW, genoMeanD)
  n <- length(rownames(geno))
  rownames(geno)[c(n - 1, n)] <- c("mw", "md")

  sex <- getCasteSex(x = apiary[[1]], simParamBee = SP)
  sex <- c(
    sex$queen, sex$fathers, sex$workers, sex$drones, sex$virginQueens,
    "F", "M"
  )
  GRM <- calcBeeGRMIbs(x = geno, sex = sex)

  expect_true(is.matrix(GRM))

  # added a vector since x must be a matrix

  vec <- c(1,2,"a")

  expect_error(calcBeeGRMIbs(x = vec, sex = sex))

  # added A and B into the sex since it can contain only M and F

  sex <- getCasteSex(x = apiary[[1]], simParamBee = SP)
  sex <- c(
    sex$queen, sex$drones, sex$workers, sex$drones, sex$virginQueens,
    "A", "B"
  )

  expect_error(calcBeeGRMIbs(x = GRM, sex = sex))
})

# ---- editCsdLocus ----

test_that("editCsdLocus", {
  founderGenomes <- quickHaplo(nInd = 100, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, editCsd = FALSE, simParamBee = SP)
  nrow(getCsdAlleles(basePop, unique = TRUE, simParamBee = SP))
  all(isCsdHeterozygous(basePop, simParamBee = SP))

  basePopEdited <- SIMplyBee:::editCsdLocus(basePop)

  expect_true(isPop(basePopEdited))
  expect_true(all(isCsdHeterozygous(basePopEdited, simParamBee = SP)))
})

# ---- emptyNULL ----

####----- emptyNULL ---- ####
test_that("emptyNULL", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
  basePop <- createVirginQueens(founderGenomes, editCsd = FALSE, simParamBee = SP)

  expect_true(isEmpty(new(Class = "Pop")))
  expect_true(isEmpty(basePop[0]))
  expect_false(isEmpty(basePop))

  emptyColony <- createColony(simParamBee = SP)
  nonEmptyColony <- createColony(basePop[1], simParamBee = SP)
  expect_true(isEmpty(emptyColony))
  expect_false(isEmpty(nonEmptyColony))

  emptyApiary <- createMultiColony(n = 3)
  emptyApiary1 <- c(createColony(simParamBee = SP), createColony(simParamBee = SP))
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

# ---- isDronesPresent ----

test_that("isDronesPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony()

  expect_true(isDronesPresent(colony))
  expect_error(isDronesPresent(vec))
  expect_true(is.vector(isDronesPresent(apiary2)))
})

# ---- isFathersPresent ----

test_that("isFathersPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony()

  expect_true(isFathersPresent(colony))
  expect_false(isFathersPresent(apiary))
  expect_error(isFathersPresent(vec))
  expect_true(is.vector(isFathersPresent(apiary2)))
  queen <- colony@queen
  expect_error(isFathersPresent(queen))
})

# ---- isWorkersPresent ----

test_that("isWorkersPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony()

  expect_true(isWorkersPresent(colony))
  expect_false(isWorkersPresent(apiary))
  expect_error(isWorkersPresent(vec))
  expect_true(is.vector(isWorkersPresent(apiary2)))
})

# ---- isGenoHeterozygous ----
test_that("isGenoHeterozygous", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3, simParamBee = SP)
   colony <- addVirginQueens(x = colony, nInd = 4, simParamBee = SP)

   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3, simParamBee = SP)
   apiary <- addVirginQueens(x = apiary, nInd = 5, simParamBee = SP)

   # Caste members taken from Colony class
   (tmp <- getCsdGeno(getQueen(colony, simParamBee = SP)))
   SIMplyBee:::isGenoHeterozygous(tmp)

   (tmp <- getCsdGeno(getVirginQueens(colony, simParamBee = SP)))
   SIMplyBee:::isGenoHeterozygous(tmp)

   (tmp <- getCsdGeno(getWorkers(colony, simParamBee = SP)))
   SIMplyBee:::isGenoHeterozygous(tmp)

   # Caste members taken from MultiColony class
   (tmp <- getCsdGeno(getQueen(apiary[[1]]), simParamBee = SP))
   SIMplyBee:::isGenoHeterozygous(tmp)

   (tmp <- getCsdGeno(getVirginQueens(apiary[[1]]), simParamBee = SP))
   SIMplyBee:::isGenoHeterozygous(tmp)

   (tmp <- getCsdGeno(getWorkers(apiary[[1]]), simParamBee = SP))
   SIMplyBee:::isGenoHeterozygous(tmp)
})

# ---- getBV ----
test_that("getBV", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   SP$addTraitA(nQtlPerChr = 10, var = 1)
   SP$addSnpChip(5)
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3, simParamBee = SP)
   colony <- addVirginQueens(x = colony, nInd = 5, simParamBee = SP)

   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3, simParamBee = SP)
   apiary <- addVirginQueens(x = apiary, nInd = 5, simParamBee = SP)

   # Input is a population
   SIMplyBee:::getBv(x = getQueen(colony))
   queens <- getQueen(apiary, collapse = TRUE)
   SIMplyBee:::getBv(queens)

   # Input is a colony
   SIMplyBee:::getBv(colony, caste = "queen")
   SIMplyBee:::getQueenBv(colony)

   SIMplyBee:::getBv(colony, caste = "fathers")
   SIMplyBee:::getBv(colony, caste = "fathers", nInd = 2)
   SIMplyBee:::getBv(colony, caste = "fathers", nInd = 2) # random sample!
   SIMplyBee:::getFathersBv(colony)
   SIMplyBee:::getFathersBv(colony, nInd = 2)

   SIMplyBee:::getBv(colony, caste = "virginQueens")
   SIMplyBee:::getVirginQueensBv(colony)

   SIMplyBee:::getBv(colony, caste = "workers")
   SIMplyBee:::getWorkersBv(colony)

   SIMplyBee:::getBv(colony, caste = "drones")
   SIMplyBee:::getDronesBv(colony)

   # Get breeding values for all individuals
   SIMplyBee:::getBv(colony, caste = "all")
   # Get all breeding values in a single matrix
   SIMplyBee:::getBv(colony, caste = "all", collapse = TRUE)

   # Input is a MultiColony
   SIMplyBee:::getBv(apiary, caste = "queen")
   SIMplyBee:::getQueenBv(apiary)

   SIMplyBee:::getBv(apiary, caste = "fathers")
   SIMplyBee:::getBv(apiary, caste = "fathers", nInd = 2)
   SIMplyBee:::getBv(apiary, caste = "fathers", nInd = 2) # random sample!
   SIMplyBee:::getFathersBv(apiary)
   SIMplyBee:::getFathersBv(apiary, nInd = 2)

   SIMplyBee:::getBv(apiary, caste = "virginQueens")
   SIMplyBee:::getVirginQueensBv(apiary)

   SIMplyBee:::getBv(apiary, caste = "workers")
   SIMplyBee:::getWorkersBv(apiary)

   SIMplyBee:::getBv(apiary, caste = "drones")
   SIMplyBee:::getDronesBv(apiary)

   # Get the breeding values of all individuals either by colony or in a single matrix
   SIMplyBee:::getBv(apiary, caste = "all")
   SIMplyBee:::getBv(apiary, caste = "all", collapse = TRUE)
})

# ---- getDd ----
test_that("getDd", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3, simParamBee = SP)
   colony <- addVirginQueens(x = colony, nInd = 5, simParamBee = SP)

   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3, simParamBee = SP)
   apiary <- addVirginQueens(x = apiary, nInd = 5, simParamBee = SP)

   # Input is a population
   SIMplyBee:::getDd(x = getQueen(colony))
   queens <- getQueen(apiary, collapse = TRUE)
   SIMplyBee:::getDd(queens)

   # Input is a colony
   SIMplyBee:::getDd(colony, caste = "queen")
   SIMplyBee:::getQueenDd(colony)

   SIMplyBee:::getDd(colony, caste = "fathers")
   SIMplyBee:::getDd(colony, caste = "fathers", nInd = 2)
   SIMplyBee:::getDd(colony, caste = "fathers", nInd = 2) # random sample!
   SIMplyBee:::getFathersDd(colony)
   SIMplyBee:::getFathersDd(colony, nInd = 2)

   SIMplyBee:::getDd(colony, caste = "virginQueens")
   SIMplyBee:::getVirginQueensDd(colony)

   SIMplyBee:::getDd(colony, caste = "workers")
   SIMplyBee:::getWorkersDd(colony)

   SIMplyBee:::getDd(colony, caste = "drones")
   SIMplyBee:::getDronesDd(colony)

   # Get dominance valued for all individuals
   SIMplyBee:::getDd(colony, caste = "all")
   # Get all dominance values in a single matrix
   SIMplyBee:::getDd(colony, caste = "all", collapse = TRUE)

   # Input is a MultiColony
   SIMplyBee:::getDd(apiary, caste = "queen")
   SIMplyBee:::getQueenDd(apiary)

   SIMplyBee:::getDd(apiary, caste = "fathers")
   SIMplyBee:::getDd(apiary, caste = "fathers", nInd = 2)
   SIMplyBee:::getDd(apiary, caste = "fathers", nInd = 2) # random sample!
   SIMplyBee:::getFathersDd(apiary)
   SIMplyBee:::getFathersDd(apiary, nInd = 2)

   SIMplyBee:::getDd(apiary, caste = "virginQueens")
   SIMplyBee:::getVirginQueensDd(apiary)

   SIMplyBee:::getDd(apiary, caste = "workers")
   SIMplyBee:::getWorkersDd(apiary)

   SIMplyBee:::getDd(apiary, caste = "drones")
   SIMplyBee:::getDronesDd(apiary)

   # Get the dominance values of all individuals either by colony or in a single matrix
   SIMplyBee:::getDd(apiary, caste = "all")
   SIMplyBee:::getDd(apiary, caste = "all", collapse = TRUE)
})

# ---- getAa ----
test_that("getAa", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   SP$addTraitADE(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1, relAA = 0.5)
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3, simParamBee = SP)
   colony <- addVirginQueens(x = colony, nInd = 5, simParamBee = SP)

   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3, simParamBee = SP)
   apiary <- addVirginQueens(x = apiary, nInd = 5, simParamBee = SP)

   # Input is a population
   SIMplyBee:::getAa(x = getQueen(colony))
   queens <- getQueen(apiary, collapse = TRUE)
   SIMplyBee:::getAa(queens)

   # Input is a colony
   SIMplyBee:::getAa(colony, caste = "queen")
   SIMplyBee:::getQueenAa(colony)

   SIMplyBee:::getAa(colony, caste = "fathers")
   SIMplyBee:::getAa(colony, caste = "fathers", nInd = 2)
   SIMplyBee:::getAa(colony, caste = "fathers", nInd = 2) # random sample!
   SIMplyBee:::getFathersAa(colony)
   SIMplyBee:::getFathersAa(colony, nInd = 2)

   SIMplyBee:::getAa(colony, caste = "virginQueens")
   SIMplyBee:::getVirginQueensAa(colony)

   SIMplyBee:::getAa(colony, caste = "workers")
   SIMplyBee:::getWorkersAa(colony)

   SIMplyBee:::getAa(colony, caste = "drones")
   SIMplyBee:::getDronesAa(colony)

   # Get epistatic values for all individuals
   SIMplyBee:::getAa(colony, caste = "all")
   # Get all epistatic values in a single matrix
   SIMplyBee:::getAa(colony, caste = "all", collapse = TRUE)

   # Input is a MultiColony
   SIMplyBee:::getAa(apiary, caste = "queen")
   SIMplyBee:::getQueenAa(apiary)

   SIMplyBee:::getAa(apiary, caste = "fathers")
   SIMplyBee:::getAa(apiary, caste = "fathers", nInd = 2)
   SIMplyBee:::getAa(apiary, caste = "fathers", nInd = 2) # random sample!
   SIMplyBee:::getFathersAa(apiary)
   SIMplyBee:::getFathersAa(apiary, nInd = 2)

   SIMplyBee:::getAa(apiary, caste = "virginQueens")
   SIMplyBee:::getVirginQueensAa(apiary)

   SIMplyBee:::getAa(apiary, caste = "workers")
   SIMplyBee:::getWorkersAa(apiary)

   SIMplyBee:::getAa(apiary, caste = "drones")
   SIMplyBee:::getDronesAa(apiary)

   # Get the epistatic values of all individuals either by colony or in a single matrix
   SIMplyBee:::getAa(apiary, caste = "all")
   SIMplyBee:::getAa(apiary, caste = "all", collapse = TRUE)
})

# ---- editCsdLocus ----
test_that("editCsdLocus", {
   founderGenomes <- quickHaplo(nInd = 100, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, editCsd = FALSE, simParamBee = SP)
   nrow(getCsdAlleles(basePop, unique = TRUE, simParamBee = SP))
   all(isCsdHeterozygous(basePop, simParamBee = SP))

   basePopEdited <- SIMplyBee:::editCsdLocus(basePop)
   nrow(getCsdAlleles(basePopEdited, unique = TRUE, simParamBee = SP))
   all(isCsdHeterozygous(basePopEdited, simParamBee = SP))
})

# ---- getLocation ----
test_that("getLocation", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)

   expect_equal(getLocation(colony), c(0, 0))
   expect_equal(getLocation(apiary[[1]]), c(0, 0))
   expect_equal(getLocation(apiary), list("2" = c(0, 0), "3" = c(0, 0)))
   tmp <- matrix(data = 0, nrow = 2, ncol = 2, dimnames = list(c("2", "3"), NULL))
   expect_equal(getLocation(apiary, collapse = TRUE), tmp)

   loc <- c(123, 456)
   expect_equal(getLocation(setLocation(colony, location = loc)), loc)

   expect_equal(getLocation(setLocation(apiary, location = loc)),
                list("2" = loc, "3" = loc))
})

test_that("createCrossPlan", {
  founderGenomes <- quickHaplo(nInd = 1000, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  # Create three virgin MultiColony objects with locations
  virginColonies1 <- createMultiColony(basePop[1:2], simParamBee = SP)
  virginColonies1 <- setLocation(virginColonies1,
                                 location = Map(c, runif(2, 0, 2*pi),
                                                runif(2, 0, 2*pi)))
  virginColonies2 <- createMultiColony(basePop[3:4])
  virginColonies2 <- setLocation(virginColonies2,
                                 location = Map(c, runif(2, 0, 2*pi),
                                                runif(2, 0, 2*pi)))
  virginColonies3 <- createMultiColony(basePop[5:6])
  virginColonies3 <- setLocation(virginColonies3,
                                 location = Map(c, runif(2, 0, 2*pi),
                                                runif(2, 0, 2*pi)))

  # Create drone colonies
  droneColonies <- createMultiColony(basePop[7:9])
  droneColonies <- setLocation(droneColonies,
                               location = Map(c, runif(3, 0, 2*pi),
                                              runif(3, 0, 2*pi)))

  # Create some drones to mate initial drone colonies with
  DCA <- createDrones(basePop[10:12], nInd = 20)
  # Cross initial virgin drone colonies to the DCA with a random cross plan
  randomCrossPlan <- createCrossPlan(x = droneColonies,
                                     drones = DCA,
                                     nDrones = 15,
                                     spatial = FALSE,
                                     simParamBee = SP)
  expect_length(randomCrossPlan, 3)
  droneColonies <- cross(droneColonies,
                         drones = DCA,
                         nDrones = nFathersPoisson,
                         crossPlan = randomCrossPlan,
                         simParamBee = SP)

  expect_equal(as.vector(nFathers(droneColonies)), c(15, 15, 15))

  # Cross according to a spatial cross plan according to the colonies' locations
  crossPlanSpatial <- createCrossPlan(x = virginColonies1,
                                      droneColonies = droneColonies,
                                      nDrones = nFathersPoisson,
                                      spatial = TRUE,
                                      radius = 1.5)

  expect_length(crossPlanSpatial, 2)
  expect_error(createCrossPlan(x = droneColonies, droneColonies = virginColonies1))
})
