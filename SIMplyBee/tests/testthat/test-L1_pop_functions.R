# ---- getCastePop ----

test_that("getCastePop", {
  founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 15)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, fathers = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)
  # Colony without workers
  colony1 <- removeWorkers(colony)

  fatherGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nFathers = 15)
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups)
  apiary <- addWorkers(apiary, nInd = 100)

  expect_warning(getCastePop(colony, caste = "drones"))
  suppressWarnings(expect_s4_class(getCastePop(colony, caste = "drones"), "Pop"))
  expect_s4_class(getCastePop(colony, caste = "workers"), "Pop")
  expect_s4_class(getCastePop(colony, caste = "queen"), "Pop")
  expect_null(getCastePop(colony, caste = "virginQueens"))
  suppressWarnings(expect_length(getCastePop(colony), 5))
  suppressWarnings(expect_length(getCastePop(colony1), 5))
  expect_type(getCastePop(apiary), "list")
  expect_length(getCastePop(apiary), 2)
  expect_equal(getCastePop(apiary, caste = "drones"), list("3" = NULL, "4" = NULL))

  # Test whether you pull out more individuals that available
  expect_equal(getCastePop(colony, caste = "workers", nInd = 10)@nInd, 10)
  expect_equal(getCastePop(colony, caste = "workers", nInd = 200)@nInd, 100)
})

# ---- createVirginQueens ----

test_that("createVirginQueens", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  # test on MapPop- test the caste it returns

  basePop <- createVirginQueens(founderGenomes)
  # test
  drones <- createDrones(basePop[1], n = 15)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, fathers = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)
  colony1 <- createColony(basePop[3])

  fatherGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nFathers = 15)
  apiary <- createMultiColony(basePop[4:5], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups)

  # Input = pop/colony        Output = pop class
  expect_s4_class(createVirginQueens(x= colony, nInd = 5), "Pop")
  expect_error(createVirginQueens(x= colony, nInd = 0))  # TODO GITHUB ISSUE
  expect_equal(createVirginQueens(x= colony, nInd = 5)@nInd, 5)
  # Input = multicolony       Output = named list (by colony ID) of pop class
  expect_type(createVirginQueens(x= apiary, nInd = 7), "list")
  expect_length(createVirginQueens(x= apiary), 2)
  # If no queen in colony
  expect_error(createVirginQueens(x= colony1, nInd = 5))
  # test on empty col / multi col
  # look at csd activity if editCsd = TRUE/FALSE #TODO
  # csdAlleles = NULL or list
  # if NULL = sample hetero csd genotype for each vq from all csd alleles
  # if not NULL = nInd provided with each node holding matrix/df
  # Potential error if provided incorrectly?
})

# ---- createDrones ----

test_that("createDrones", {
  founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 15)
  drones2 <- createDrones(basePop[2], n = 16)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[3])
  colony <- cross(colony, fathers = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)

  colony1  <- createColony(basePop[4])
  colony1 <- cross(colony1, fathers = drones2)

  fatherGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nFathers = 15)
  apiary <- createMultiColony(basePop[5:6], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups)

  # test whether caste is correct caste
  # Expect an NULL output if no drones are present in apiary
  expect_null(apiary@colonies$drones)
  # Expect error if x is not virgin queen or queen
  expect_error(createDrones(colony@workers))
  # Expect error if x = colony and queen isn't present
  expect_error(createDrones(colony1))
  #Check s4 class on colony
  expect_s4_class(createDrones(x= colony1, nInd = 5), "Pop")
  expect_error(createDrones(x = colony1, nInd = 0))
  expect_equal(createDrones(x= colony1, nInd = 5)@nInd, 5)

  expect_type(createDrones(x= apiary, nInd = 7), "list")
  expect_length(createDrones(x= apiary), 2)
})

# ---- beeCross ----

test_that("beeCross", {
  founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 15)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, fathers = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)

  fatherGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nFathers = 15)
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups)


  # Error if used on multicolony or colony class
  expect_error(beeCross(virginQueen = apiary, drones = fatherGroups))
  expect_error(beeCross(virginQueen = colony, drones = drones))
  #Error if more than one virginQueen is provided
  expect_error(beeCross(virginQueen = basePop[5:6], drones = drones))
  # test on basePop[0]
  #check the class and length
  expect_s4_class(beeCross(basePop[5], drones = drones), "Pop")
  expect_equal(beeCross(basePop[5], drones = drones, nProgeny = 5)@nInd, 5)
  expect_error(beeCross(basePop[5], drones = drones, nProgeny = 0))
})

# ---- pullCastePop ----

test_that("pullCastePop", {
  founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 15)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, fathers = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)
  # Colony without workers
  colony1 <- removeWorkers(colony)

  fatherGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nFathers = 15)
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups)
  apiary <- addWorkers(apiary, nInd = 100)

  # Warning- if pulling drones, pulls drones that are not yet mated
  expect_warning(pullCastePop(colony, caste = "drones"))
  suppressWarnings(expect_s4_class(pullCastePop(colony, caste = "drones")$pulled, "Pop"))
  suppressWarnings(expect_s4_class(pullCastePop(colony, caste = "drones")$pulled, "Colony"))

  expect_s4_class(pullCastePop(colony, caste = "workers")$pulled, "Pop")
  expect_s4_class(pullCastePop(colony, caste = "workers")$remnant, "Colony")

  expect_s4_class(pullCastePop(colony, caste = "queen")$pulled, "Pop")
  expect_null(pullCastePop(colony, caste = "virginQueens")$pulled)

  # test on fathers
  # test on empty apiary
  # Error is "caste" argument is missing
  expect_error(pullCastePop(colony))
  expect_error(pullCastePop(apiary))

  # type and length
  expect_type(pullCastePop(colony, caste = "workers"), "list")
  expect_type(pullCastePop(colony, caste = "queen"), "list")
  expect_type(pullCastePop(colony, caste = "drones"), "list")

  expect_type(pullCastePop(apiary, caste = "workers"), "list")
  expect_type(pullCastePop(apiary, caste = "queen"), "list")
  expect_type(pullCastePop(apiary, caste = "drones"), "list")

  expect_length(pullCastePop(colony, caste = "workers"), 2)
  expect_length(pullCastePop(colony, caste = "queen"), 2)
  expect_length(pullCastePop(colony, caste = "drones"), 2)

  expect_length(pullCastePop(apiary, caste = "workers"), 2)
  expect_length(pullCastePop(apiary, caste = "queen"), 2)
  expect_length(pullCastePop(apiary, caste = "drones"), 2)
})

# Test whether you pull out more individuals that available
expect_equal(pullCastePop(colony, caste = "workers", nInd = 10)$pulled@nInd, 10)

# ---- cross ----
test_that("cross", {
  founderGenomes <- quickHaplo(nInd = 7, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes, nInd = 100)
  drones <- createDrones(basePop[1], n = 1000)
  fatherGroups <- pullDroneGroupsFromDCA((drones), n = 3, nFathers = 15)


  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, fathers = fatherGroups[[1]])
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)
  #remove queen
  colony <- removeQueen(colony)
  apiary <- createMultiColony(basePop[3:4], n = 2)

  colony1 <- createColony(basePop[5])
  colony2 <- createColony(basePop[6])
  virginQueen <- basePop[7]

  # Error if x = pop and are not virgin queens
  expect_error(cross(colony@workers, fathers = fatherGroups[[2]]))
  expect_null(isVirginQueen(colony@virginQueens))
  # Error if fathers is not a pop
  expect_error(cross(colony1, fathers = colony))
  expect_true(isVirginQueen(colony1@virginQueens))
  # Error if fathers are not drones
  expect_error(cross(colony1, fathers = colony@workers))
  expect_true(isVirginQueen(colony1@virginQueens))
  # Error if length of fathers doesn't match virginqueens
  expect_error(cross(basePop[3:4], fathers = fatherGroups[1]))
  expect_true(isVirginQueen(basePop[3:4]))
  # Message if fathers == 0 "Mating failed"
  #TODO selectInd(colony, n = 0, use = "rand")

  #test check matings parameter
  # test mating a mated queen
  # If x = colony, queen must not be present
  expect_error(cross(colony, nFathers[1]))
  # If x = colony virgin queens must be present
  expect_error(cross(colony, fathers = fatherGroups))
  # If x = multicolony, length of fathers must match length of colonies
  expect_error(cross(apiary, fathers = fatherGroups[1]))
  # Cannot mate with already-mated drones
  expect_error(cross(colony1, fathers = drones))

  expect_s4_class(cross(colony2, fathers = drones2), "Colony") # Not working properly
  expect_s4_class(cross(virginQueen, fathers = drones3), "Pop") # Error of no virginQueen
  expect_s4_class(cross(apiary, fathers = fatherGroups[c(2,3)]), "MultiColony") #Same error

  expect_length(cross(virginQueen, fathers = drones3), 1)
})

# ---- setQueensYearOfBirth ----
test_that("setQueensYearOfBirth", {
  founderGenomes <- quickHaplo(nInd = 7, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes, nInd = 100)
  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

  colony <- createColony(x = basePop[2])
  colony <- cross(x = colony, fathers = fatherGroups[[1]])
  colony <- buildUp(colony)
  colony <- removeQueen(colony)
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])

  workers <- getWorkers(colony)

  colony1 <- createColony(x = basePop[5])
  colony1 <- cross(colony1, fathers = fatherGroups[[4]])
  queen1 <- getQueen(colony1)

  # Error if x = pop, and not a vq or q
  expect_error(setQueensYearOfBirth(workers))
  expect_error(setQueensYearOfBirth(drones))
  # Error if x = colony and no queen is present
  expect_error(setQueensYearOfBirth(colony))

  expect_s4_class(setQueensYearOfBirth(queen1, year = 2022), "Pop")
  expect_s4_class(setQueensYearOfBirth(colony1, year = 2022), "Colony")
  expect_s4_class(setQueensYearOfBirth(apiary, year = 2022), "MultiColony")

  expect_length(setQueensYearOfBirth(queen1, year = 2022), 1)
  expect_length(setQueensYearOfBirth(colony1, year = 2022), 1)
  expect_length(setQueensYearOfBirth(apiary, year = 2022), 1)
})


# ---- createDCA ----
test_that("createDCA", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$setTrackRec(TRUE)
  SP$setTrackPed(isTrackPed = TRUE)
  SP$addTraitA(10)
  SP$addSnpChip(5)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, fathers = fatherGroups[[1]])
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])

  colony1 <- createColony(x = basePop[5])
  colony1 <- cross(colony1, fathers = fatherGroups[[4]])
  colony1 <- buildUp(colony1)

  colony2 <- createColony(x = basePop[6])
  colony2 <- cross(colony2, fathers = selectInd(colony1@drones, nInd = 1, use = "rand"))

  apiary1 <- createMultiColony(basePop[7:8], n = 2)
  apiary1 <- cross(apiary1, fathers = fatherGroups[c(5, 6)])
  apiary1 <- buildUp(apiary1)

  # Warning if x = colony/multicolony and no drones available
  expect_warning(createDCA(colony))
  expect_warning(createDCA(apiary))

  # Talking only drones that have not yet mated
  expect_warning(createDCA(colony1))

  # Error if another caste is used
  expect_error(createDCA(x = colony1@workers))
  expect_error(createDCA(basePop[6]))

  expect_s4_class(createDCA(colony1, nInd = 10), "Pop")
  expect_length(createDCA(colony1, nInd =10), 1)
  expect_equal(createDCA(colony1, nInd =10)@nInd, 10)
})

# ---- pullDroneGroupsFromDCA ----
test_that("pullDroneGroupsFromDCA", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, fathers = fatherGroups[[1]])
  colony <- addDrones(colony, nInd = 100)

  colony2 <-  createColony(x = basePop[3])
  colony2 <- cross(colony2, fathers = selectInd(colony@drones, nInd = 1, use = "rand"))

  DCA <- createDCA(colony, nInd = 80)
  DCA2 <- createDCA(colony, nInd = 4)
  DCA3 <- createDCA(colony, nInd = 100, removeFathers = FALSE)

  # Error, DCA must be a Pop
  expect_error(pullDroneGroupsFromDCA(colony))
  # Error, n must be provided
  expect_error(pullDroneGroupsFromDCA(DCA))
  # Error, if nInd in DCA is smaller than nFathers
  expect_error(pullDroneGroupsFromDCA(DCA2, n = 8, nFathers = nFathersPoisson))
  #Error if DCA contains fathers (removed) and nInd is too small
  expect_error(pullDroneGroupsFromDCA(DCA3, n = 5, nFathers = 20))

  expect_type(pullDroneGroupsFromDCA(DCA, n = 5, nFathers = nFathersPoisson), "list")
  expect_length(pullDroneGroupsFromDCA(DCA, n = 2, nFathers = nFathersPoisson), 2)
  expect_s4_class(pullDroneGroupsFromDCA(DCA, n = 5, nFathers = nFathersPoisson)[[1]], "Pop")
})


