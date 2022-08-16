
# ---- nColonies ----

test_that("nColonies", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  expect_equal(nColonies(createColonies(n = 2)), 2)
  expect_equal(nColonies(createColonies()), 0)
  expect_equal(nColonies(createColonies(n = 10)), 10)
})

# ---- nCaste ----

test_that("nCaste", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 45)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 3, nFathers = 15)
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, fathers = fatherGroups[[1]])
  colony <- buildUp(colony, nDrones = 10, nWorkers = 20)
  expect_equal(nCaste(colony, caste = "queen"), 1)
  expect_equal(nCaste(colony, caste = "fathers"), 15)
  expect_equal(nCaste(colony, caste = "virginQueens"), 0)
  expect_equal(nCaste(colony, caste = "workers"), 20)
  expect_equal(nCaste(colony, caste = "drones"), 10)

  apiary <- createColonies(basePop[3:4], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])
  apiary <- buildUp(apiary, nWorkers = 20, nDrones = 10)
  expect_equal(nCaste(apiary, caste = "queen"), c("3" = 1, "4" = 1))
  expect_equal(nCaste(apiary, caste = "fathers"), c("3" = 15, "4" = 15))
  expect_equal(nCaste(apiary, caste = "virginQueens"), c("3" = 0, "4" = 0))
  expect_equal(nCaste(apiary, caste = "workers"), c("3" = 20, "4" = 20))
  expect_equal(nCaste(apiary, caste = "drones"), c("3" = 10, "4" = 10))
})

# ---- nQueens ----

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

# ---- nFathers ----

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

# ---- nDrones ----

test_that("nDrones", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- newPop(founderGenomes)
  drones <- createFounderDrones(pop = basePop[1], nDronesPerQueen = 10)
  colony1 <- createColony(queen = basePop[2], fathers = drones[1:5])
  colony2 <- createColony(queen = basePop[3], fathers = drones[6:10])

  expect_equal(nDrones(colony1), 0)
  colony1 <- addDrones(colony1, nInd = 5)
  colony2 <- addDrones(colony2, nInd = 10)
  expect_equal(nDrones(colony1), 5)
  expect_equal(nDrones(colony2), 10)
  expect_equal(nDrones(c(colony1, colony2)), c("2" = 5, "3" = 10))
  expect_error(nDrones(basePop))
})

# ---- isQueenMated ----

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
  expect_equal(
    isQueenMated(c(
      getQueen(colony1), getQueen(colony2),
      getVirginQueens(colony3)
    )),
    c(TRUE, TRUE, FALSE)
  )
})

# ---- getCsd ----

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

# ---- isGenoHeterozygous ----

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

# ---- nNULLColonies ----

test_that("nNULLColonies", {
    founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    basePop <- createVirginQueens(founderGenomes)
    apiary <- createMultiColony(n = 3)

  # 3 empty colonies are created above, so I test if the output is an intiger
  # and if nNullColonies = 3
  expect_true(is.integer(nNULLColonies(apiary)))
  expect_equal(nNULLColonies(apiary), 3)
})

# ---- isCaste ----

test_that("isCaste", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

   # Create a Colony class
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, fathers = fatherGroups[[1]])
   colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)

  # get the queen that is a caste "queen" and ask if it is a caste "queen"
  # and drones and fathers
  expect_true(isCaste(getQueen(colony), caste = "queen"))
  expect_true(isCaste(getDrones(colony), caste = "drones"))
  expect_true(all(isCaste(getFathers(colony), caste = "fathers")))
  # get the queen that is a caste "queen" and test if it is a caste "workers"
  expect_false(isCaste(getQueen(colony), caste = "workers"))
  expect_null(isCaste(getVirginQueens(colony), caste = "virginQueens"))
  #dodaj še trote in fotre, pa na nečem, kar ni v družini
})

# ---- calcQueensPHomBrood ----

test_that("calcQueensPHomBrood", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class object
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
    colony <- addVirginQueens(x = colony, nInd = 1)

 expect_error(calcQueensPHomBrood(colony@drones))
 expect_error(calcQueensPHomBrood(colony@drones))# zbriši
 expect_true(is.numeric(calcQueensPHomBrood(colony@queen)))
 #dodaj še če matice ni, še na colony, colonies, empty colony
})

# ---- pHomBrood ----

test_that("pHomBrood", {
    founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class object
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
    colony <- addVirginQueens(x = colony, nInd = 1)

  expect_error(pHomBrood(colony@workers))
  expect_error(pHomBrood(colony@virginQueens))
  expect_error(pHomBrood(colony@drones))
  expect_true(is.numeric(pHomBrood(colony@queen)))
  #dodaj še če matice ni, še na colony, colonies, empty colony
})

# ---- nHomBrood -----

test_that("nHomBrood", {
    founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class object
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
    colony <- addVirginQueens(x = colony, nInd = 1)

  expect_error(nHomBrood(colony@workers))
  expect_error(nHomBrood(colony@virginQueens))
  expect_error(nHomBrood(colony@drones))
  expect_true(is.numeric(nHomBrood(colony@queen)))
  #dodaj še če matice ni, še na colony, colonies, empty colony
})

# ---- isQueenPresent ----

test_that("isQueenPresent", {
    founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class object
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
    colony <- addVirginQueens(x = colony, nInd = 1)
    apiary <- createMultiColony(n = 1)
    vec <- c(1,2,3,4)

  expect_true(isQueenPresent(colony))
  expect_false(isQueenPresent(apiary))
  expect_error(isQueenPresent(vec))
  #naredi še z multui koloni brz argumenta
})

# ---- isVirginQueensPresent ----

test_that("isVirginQueensPresent", {
    founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class object
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20)
    colony <- addVirginQueens(x = colony, nInd = 1)
    colony2 <- createMultiColony(n = 1)
    vec <- c(1,2,3,4)

  expect_true(isVirginQueensPresent(colony))
  expect_false(isVirginQueensPresent(colony2))
  expect_error(isQueenVirginPresent(vec))
  #naredi še z multui koloni brz argumenta
})

# ---- isProductive ----

test_that("isProductive", {
     founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
     SP <- SimParamBee$new(founderGenomes)
     basePop <- createVirginQueens(founderGenomes)

     drones <- createDrones(x = basePop[1], nInd = 1000)
     fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

     # Create a Colony and a MultiColony class
     colony <- createColony(x = basePop[2])
     colony <- cross(colony, fathers = fatherGroups[[1]])

   expect_false(isProductive(colony))
     colony <- buildUp(x = colony)
   expect_true(isProductive(colony))

     apiary <- createMultiColony(basePop[3:4], n = 2)
     apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])

   expect_false(all(isProductive(apiary)))
    apiary <- buildUp(x = apiary)
   expect_true(all(isProductive(apiary)))
   #še tuki dodaj prazne colony in colonies

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

 expect_error(reduceDroneHaplo(haplo = tmp, pop = queens))
 expect_error(reduceDroneHaplo(haplo = vec, pop = drones))
 expect_error(reduceDroneHaplo(haplo = df, pop = drones))
 expect_true(is.matrix(reduceDroneHaplo(haplo = tmpD, pop = drones)))
 #preveri, če ima matrika same nule in 1
})

# ---- reduceDroneGeno ----

test_that("reduceDroneGeno", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 2)
  virginQueens <- c(basePop[2:3])
  vec <- c(1,2,"a")
  tmp <- getSegSiteGeno(virginQueens)
  df <- as.data.frame(tmp)
  tmpD <- getSegSiteGeno(drones)

  expect_error(reduceDroneGeno(geno = tmp, pop = queens))
  expect_error(reduceDroneGeno(geno = vec, pop = drones))
  expect_error(reduceDroneGeno(geno =  df, pop = drones))
  expect_true(is.matrix(reduceDroneHaplo(haplo = tmpD, pop = drones)))
  #preveri, če ima matrika same nule in 1
})

# ---- getCsdAlleles ----

test_that("getCsdAlleles", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

   # Create a Colony class
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, fathers = fatherGroups[[1]])
   colony <- buildUp(x = colony)

 expect_true(is.list(getCsdAlleles(colony)))
 expect_true(is.matrix(getCsdAlleles(getQueen(colony))))

   # set CSD to NULL
   SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

   # Create a Colony class
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, fathers = fatherGroups[[1]])
   colony <- buildUp(x = colony)

 expect_error(getCsdAlleles(colony))
 #test unique and colapsed
})

# ---- getCsdGeno ----

test_that("getCsdGeno", {
    founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony)

  expect_true(is.list(getCsdGeno(colony)))
  expect_true(is.matrix(getCsdGeno(getQueen(colony))))

    # set CSD to NULL
    SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony)

  expect_error(getCsdGeno(colony))
  #test unique and colapsed
})

# ---- isCsdHeterozygous ----

test_that("isCsdHeterozygous", {
    founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony)

  expect_true(isCsdHeterozygous(colony@queen))
  expect_true(is.vector(isCsdHeterozygous(colony@workers)))

    # set CSD to NULL
    SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony)

  expect_error(isCsdHeterozygous(colony@queen))
  #test on drones, shuld be true
})

# ---- nCsdAlleles ----

test_that("nCsdAlleles", {
    founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
    SP <- SimParamBee$new(founderGenomes)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony)

  expect_equal(nCsdAlleles(colony@queen), 2)
  expect_equal(nCsdAlleles(colony@workers[10]), 2)
  expect_equal(nCsdAlleles(colony@drones[9]), 1)
  expect_true(is.integer(nCsdAlleles(colony@queen)))

    # set CSD to NULL
    SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
    basePop <- createVirginQueens(founderGenomes)

    drones <- createDrones(x = basePop[1], nInd = 1000)
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

    # Create a Colony class
    colony <- createColony(x = basePop[2])
    colony <- cross(colony, fathers = fatherGroups[[1]])
    colony <- buildUp(x = colony)

  expect_error(nCsdAlleles(colony@queen))
  #collapse argument
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
    fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)
    apiary <- createMultiColony(basePop[2:3], n = 2)
    apiary <- cross(x = apiary, fathers = fatherGroups[c(2, 3)])
    apiary <- buildUp(x = apiary)
    apiary <- addVirginQueens(x = apiary, nInd = 5)

    genoQ <- getQueensSegSiteGeno(apiary[[1]])
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

   # added A and B into the sex sice it can contain only M and F

    sex <- getCasteSex(x = apiary[[1]])
    sex <- c(
      sex$queen, sex$fathers, sex$workers, sex$drones, sex$virginQueens,
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
  #test if all individuals are heterozygous after editing

})

##### za vsak error se uprašimo, če si ga res želimo, ali bi bilo bolje imeti NULL


