# ---- nColonies ----
test_that("nColonies", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  expect_equal(nColonies(createMultiColony(n = 2)), 2)
  expect_equal(nColonies(createMultiColony()), 0)
  expect_equal(nColonies(createMultiColony(n = 10)), 10)
})


# ---- nCaste ----
test_that("nCaste", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 45)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 10)
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = droneGroups[[1]])
  colony <- buildUp(colony, nDrones = 15, nWorkers = 20)
  expect_equal(nCaste(colony, caste = "queen"), 1)
  expect_equal(nCaste(colony, caste = "drones"), 15)
  expect_equal(nCaste(colony, caste = "virginQueens"), 0)
  expect_equal(nCaste(colony, caste = "workers"), nWorkers(colony))
  expect_equal(nCaste(colony, caste = "fathers"), 10)

  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
  apiary <- buildUp(apiary, nWorkers = 20, nDrones = 10)
  expect_equal(sum(nCaste(apiary, caste = "queen")), 2)
  expect_equal(sum(nCaste(apiary, caste = "drones")), 20)
  expect_equal(sum(nCaste(apiary, caste = "virginQueens")), 0)
  expect_equal(sum(nCaste(apiary, caste = "workers")), sum(nWorkers(apiary)))
  expect_equal(sum(nCaste(apiary, caste = "fathers")), 20)
})

# ---- nQueens ----

test_that("nQueens", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  colony1 <- createColony(x = basePop[1])
  colony1 <- cross(colony1, drones = fatherGroups[[1]])
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = fatherGroups[c(2, 3)])

  expect_equal(nQueens(colony1), 1)
  colony1 <- removeQueen(colony1)
  expect_equal(nQueens(colony1), 0)
  expect_equal(sum(nQueens(apiary)), 2)
})

# ---- nDrones ----
test_that("nDrones", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  colony1 <- createColony(x = basePop[1])
  colony1 <- cross(colony1, drones = fatherGroups[[1]])
  colony2 <- createColony(x = basePop[2])
  colony2 <- cross(colony2, drones = fatherGroups[[2]])


  expect_equal(nDrones(colony1), 0)
  colony1 <- addDrones(colony1, nInd = 5)
  colony2 <- addDrones(colony2, nInd = 10)
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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)

  # get the queen that is a caste "queen" and ask if it is a caste "queen"
  # and drones and drones
  expect_true(isCaste(getQueen(colony), caste = "queen"))
  expect_true(all(isCaste(getDrones(colony), caste = "drones")))
  expect_true(all(isCaste(getDrones(colony), caste = "drones")))
  # get the queen that is a caste "queen" and test if it is a caste "workers",
  #test on virgin queen that is not present in a colony
  expect_false(isCaste(getQueen(colony), caste = "workers"))
  #test on virgin queen that is not present in a colony
  expect_null(isCaste(getVirginQueens(colony), caste = "virginQueens"))
  malePop <- c(getDrones(colony), getFathers(colony))
  expect_true(any(isCaste(malePop, caste = "drones")))
  expect_true(any(isCaste(malePop, caste = "fathers")))
  expect_false(all(isCaste(malePop, caste = "drones")))
  expect_false(all(isCaste(malePop, caste = "fathers")))
})

# ---- calcQueensPHomBrood ----

test_that("calcQueensPHomBrood", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class object
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
  colony <- addVirginQueens(x = colony, nInd = 1)

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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class object
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
  colony <- addVirginQueens(x = colony, nInd = 1)

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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class object
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
  colony <- addVirginQueens(x = colony, nInd = 1)

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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class object
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- addVirginQueens(x = colony, nInd = 1)
  apiary <- createMultiColony(n = 1)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony()
  colony2 <- createColony()

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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class object
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
  colony <- addVirginQueens(x = colony, nInd = 1)
  apiary <- createMultiColony(n = 1)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony()
  colony2 <- createColony()


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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])

  expect_false(isProductive(colony))
  colony <- buildUp(x = colony)
  expect_true(isProductive(colony))

  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = fatherGroups[c(2, 3)])

  expect_false(all(isProductive(apiary)))
  apiary <- buildUp(x = apiary)
  expect_true(all(isProductive(apiary)))

  colony <- createColony()
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
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 2)
  virginQueens <- c(basePop[2:3])
  vec <- c(1,2,"a")
  tmp <- getSegSiteHaplo(virginQueens)
  df <- as.data.frame(tmp)
  tmpD <- getSegSiteHaplo(drones)
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
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 2)
  virginQueens <- c(basePop[2:3])
  vec <- c(1,2,"a")
  tmp <- getSegSiteGeno(virginQueens)
  df <- as.data.frame(tmp)
  tmpD <- getSegSiteGeno(drones)


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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)

  expect_true(is.list(getCsdAlleles(colony)))
  expect_true(is.matrix(getCsdAlleles(getQueen(colony))))

  # set CSD to NULL
  rm(SP)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)

  expect_error(getCsdAlleles(colony))

  # test unique and colapse
  rm(SP)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)
  expect_true(is.matrix(getCsdAlleles(colony, collapse = TRUE)))
  expect_equal(nrow(getCsdAlleles(colony, collapse = TRUE)),
               sum(nQueens(colony)*2, nDrones(colony), nWorkers(colony)*2, nFathers(colony),
                   nVirginQueens(colony)*2))
  expect_equal(nrow(getCsdAlleles(colony, collapse = TRUE, unique = TRUE)),
               nrow(unique(getCsdAlleles(colony, collapse = TRUE))))
  expect_true(nrow(getCsdAlleles(colony, collapse = TRUE, unique = TRUE)) <= SP$nCsdAlleles)

})

# ---- getCsdGeno ----

test_that("getCsdGeno", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)

  expect_true(is.list(getCsdGeno(colony)))
  expect_true(is.matrix(getCsdGeno(getQueen(colony))))
  expect_true(nCsdAlleles(colony, collapse = TRUE) <= SP$nCsdAlleles)

  geno <- getCsdGeno(colony)
  expect_equal(nrow(geno$fathers), 10)

  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)

  expect_error(getCsdGeno(colony))

})

# ---- isCsdHeterozygous ----

test_that("isCsdHeterozygous", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)
  colony@virginQueens <- createVirginQueens(colony, nInd = 1)

  expect_true(isCsdHeterozygous(colony@queen))
  expect_true(is.vector(isCsdHeterozygous(colony@workers)))
  expect_false(all(isCsdHeterozygous(colony@drones)))
  expect_true(isCsdHeterozygous(colony@virginQueens))

  # set CSD to NULL
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)

  expect_error(isCsdHeterozygous(colony@queen))
})

# ---- nCsdAlleles ----

test_that("nCsdAlleles", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)

  expect_equal(nCsdAlleles(colony@queen), 2)
  expect_equal(nCsdAlleles(colony@workers[10]), 2)
  expect_equal(nCsdAlleles(colony@drones[9]), 1)
  expect_true(is.integer(nCsdAlleles(colony@queen)))

  # set CSD to NULL
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)

  expect_error(nCsdAlleles(colony@queen), 2)

  #collapse argument
  nCsdAlleles <- 5
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = nCsdAlleles)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony)
  expect_true(is.numeric(nCsdAlleles(colony, collapse = TRUE)))
  expect_true(nCsdAlleles(colony, collapse = TRUE) <= nCsdAlleles)
})

# ---- calcBeeGRMIbs ----

test_that("calcBeeGRMIbs", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$setTrackRec(TRUE)
  SP$setTrackPed(isTrackPed = TRUE)
  SP$addTraitA(10)
  SP$addSnpChip(5)

  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
  apiary <- createMultiColony(basePop[2:3], n = 2)
  apiary <- cross(x = apiary, drones = fatherGroups[c(2, 3)])
  apiary <- buildUp(x = apiary)
  apiary <- addVirginQueens(x = apiary, nInd = 5)

  genoQ <- getQueenSegSiteGeno(apiary[[1]])
  genoF <- getFathersSegSiteGeno(apiary[[1]])
  genoW <- getWorkersSegSiteGeno(apiary[[1]])
  genoD <- getDronesSegSiteGeno(apiary[[1]])
  genoV <- getVirginQueensSegSiteGeno(apiary[[1]])
  genoMeanW <- apply(X = genoW, MARGIN = 2, FUN = mean)
  genoMeanD <- apply(X = genoD, MARGIN = 2, FUN = mean)

  geno <- rbind(genoQ, genoF, genoW, genoD, genoV, genoMeanW, genoMeanD)
  n <- length(rownames(geno))
  rownames(geno)[c(n - 1, n)] <- c("mw", "md")

  sex <- getCasteSex(x = apiary[[1]])
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

  sex <- getCasteSex(x = apiary[[1]])
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
  basePop <- createVirginQueens(founderGenomes, editCsd = FALSE)
  nrow(getCsdAlleles(basePop, unique = TRUE))
  all(isCsdHeterozygous(basePop))

  basePopEdited <- SIMplyBee:::editCsdLocus(basePop)

  expect_true(isPop(basePopEdited))
  expect_true(all(isCsdHeterozygous(basePopEdited)))
})

# ---- emptyNULL ----

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

# ---- isDronesPresent ----

test_that("isDronesPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

  # Create a Colony class object
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
  colony <- addVirginQueens(x = colony, nInd = 1)
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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class object
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
  colony <- addVirginQueens(x = colony, nInd = 1)
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
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create a Colony class object
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
  colony <- addVirginQueens(x = colony, nInd = 1)
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
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, drones = droneGroups[[1]])
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
   colony <- addVirginQueens(x = colony, nInd = 4)

   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
   apiary <- addVirginQueens(x = apiary, nInd = 5)

   # Caste members taken from Colony class
   (tmp <- getCsdGeno(getQueen(colony)))
   SIMplyBee:::isGenoHeterozygous(tmp)

   (tmp <- getCsdGeno(getVirginQueens(colony)))
   SIMplyBee:::isGenoHeterozygous(tmp)

   (tmp <- getCsdGeno(getWorkers(colony)))
   SIMplyBee:::isGenoHeterozygous(tmp)

   # Caste members taken from MultiColony class
   (tmp <- getCsdGeno(getQueen(apiary[[1]])))
   SIMplyBee:::isGenoHeterozygous(tmp)

   (tmp <- getCsdGeno(getVirginQueens(apiary[[1]])))
   SIMplyBee:::isGenoHeterozygous(tmp)

   (tmp <- getCsdGeno(getWorkers(apiary[[1]])))
   SIMplyBee:::isGenoHeterozygous(tmp)
})

# ---- getBV ----
test_that("getBV", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$addTraitA(nQtlPerChr = 10, var = 1)
   SP$addSnpChip(5)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, drones = droneGroups[[1]])
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
   colony <- addVirginQueens(x = colony, nInd = 5)

   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
   apiary <- addVirginQueens(x = apiary, nInd = 5)

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
   SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, drones = droneGroups[[1]])
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
   colony <- addVirginQueens(x = colony, nInd = 5)

   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
   apiary <- addVirginQueens(x = apiary, nInd = 5)

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
   SP$addTraitADE(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1, relAA = 0.5)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, drones = droneGroups[[1]])
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)
   colony <- addVirginQueens(x = colony, nInd = 5)

   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3)
   apiary <- addVirginQueens(x = apiary, nInd = 5)

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
   basePop <- createVirginQueens(founderGenomes, editCsd = FALSE)
   nrow(getCsdAlleles(basePop, unique = TRUE))
   all(isCsdHeterozygous(basePop))

   basePopEdited <- SIMplyBee:::editCsdLocus(basePop)
   nrow(getCsdAlleles(basePopEdited, unique = TRUE))
   all(isCsdHeterozygous(basePopEdited))
})

# ---- getLocation ----
test_that("getLocation", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   basePop <- createVirginQueens(founderGenomes)
   drones <- createDrones(x = basePop[1], nInd = 1000)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, drones = droneGroups[[1]])
   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)])

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
