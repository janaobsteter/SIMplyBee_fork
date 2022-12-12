# ---- getCastePop ----

test_that("getCastePop", {
  founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 15)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, drones = drones)
  colony <- buildUp(colony, nWorkers = 200, nDrones = 100)
  # Colony without workers
  colony1 <- removeWorkers(colony)

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nDrones = 15)
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = dronesGroups)
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
  # Test whether you pull out more individuals that available
  expect_equal(getCastePop(colony, caste = "workers", nInd = 10)@nInd, 10)
  expect_equal(getCastePop(colony, caste = "workers", nInd = 100)@nInd, 100)
})

# ---- createVirginQueens ----

test_that("createVirginQueens", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  #check that output is virginqueens ?
  expect_true(all(isVirginQueen(createVirginQueens(founderGenomes))))

  basePop <- createVirginQueens(founderGenomes)
  expect_true(all(isVirginQueen(basePop)))

  drones <- createDrones(basePop[1], n = 15)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, drones = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)

  colony1 <- createColony(basePop[3])
  # If no queen in colony - queen is missing
  expect_error(createVirginQueens(x= colony1, nInd = 5))

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nDrones = 15)
  apiary <- createMultiColony(basePop[4:5], n = 2)
  apiary <- cross(apiary, drones = dronesGroups)
  # Input = multicolony       Output = named list (by colony ID) of pop class
  expect_type(createVirginQueens(x= apiary, nInd = 7), "list")
  expect_length(createVirginQueens(x= apiary), 2)

  # Error when testing on empty MultiColony
  apiary1 <- createMultiColony(n = 2)
  expect_error(createVirginQueens(apiary1, nInd = 5))

  #check that output is virginqueens ?
  expect_false(all(isVirginQueen(drones)))
  expect_false(all(isVirginQueen(basePop)))
  expect_error(createVirginQueens(basePop))

  # Input = pop/colony        Output = pop class
  expect_s4_class(createVirginQueens(x= colony, nInd = 5), "Pop")
  suppressWarnings(expect_error(createVirginQueens(x= colony, nInd = 0)))
  expect_equal(createVirginQueens(x= colony, nInd = 5)@nInd, 5)

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
  # Error: x can't be a MapPop
  expect_error(createDrones(founderGenomes))

  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 15)
  drones2 <- createDrones(basePop[2], n = 16)
  expect_true(all(isDrone(drones)))

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[3])
  colony <- cross(colony, drones = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)
  # Expect error if x is not virgin queen or queen
  expect_error(createDrones(colony@workers))

  #Check the caste
  expect_true(all(isDrone(colony@drones)))
  expect_false(all(isDrone(colony@workers)))
  expect_false(all(isDrone(colony@queen@misc[[1]]$fathers)))

  colony1  <- createColony(basePop[4])
  colony1 <- cross(colony1, drones = drones2)

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nDrones = 15)
  apiary <- createMultiColony(basePop[5:6], n = 2)
  apiary <- cross(apiary, drones = dronesGroups)

  # Expect error if x = colony and queen isn't present
  colony <- removeQueen(colony)
  expect_error(createDrones(colony))

  #Check s4 class on colony
  expect_s4_class(createDrones(x= colony1, nInd = 5), "Pop")
  expect_error(createDrones(x = colony1, nInd = 0))
  expect_equal(createDrones(x= colony1, nInd = 5)@nInd, 5)

  expect_type(createDrones(x= apiary, nInd = 7), "list")
  expect_length(createDrones(x= apiary), 2)
})

# ---- combineBeeGametes ----

test_that("combineBeeGametes", {
  founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 15)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, drones = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nDrones = 15)
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = dronesGroups)

  # Error if used on multicolony or colony class
  expect_error(combineBeeGametes(queen = apiary, drones = dronesGroups))
  expect_error(combineBeeGametes(queen = colony, drones = drones))
  #Error if more than one queen is provided
  expect_error(combineBeeGametes(queen = basePop[5:6], drones = drones))
  # AlphaSimR error - invalid crossPlan
  expect_error(combineBeeGametes(queen = basePop[0], drones = dronesGroups[[1]]))
  #check the class
  expect_s4_class(combineBeeGametes(basePop[5], drones = drones), "Pop")
  expect_equal(combineBeeGametes(basePop[5], drones = drones, nProgeny = 5)@nInd, 5)
  suppressWarnings(expect_error(combineBeeGametes(basePop[5], drones = drones, nProgeny = 0)))
})

# ---- pullCastePop ----

test_that("pullCastePop", {
  founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 15)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2])
  colony <- cross(colony, drones = drones)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)
  # Colony without workers
  colony1 <- removeWorkers(colony)

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony), n = 2, nDrones = 15)
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = dronesGroups)
  apiary <- addWorkers(apiary, nInd = 100)

  apiary1 <- createMultiColony(x = basePop[1], n = 0)
  apiary2 <- createMultiColony(n = 2)
  # test on empty apiary
  expect_type(pullCastePop(apiary1, caste = "queen", nInd = 1)$pulled, "list")
  expect_error(pullCastePop(apiary2, caste = "queen", nInd = 1))
  # Warning- if pulling drones, pulls drones that are not yet mated
  expect_warning(pullCastePop(colony, caste = "drones"))
  suppressWarnings(expect_s4_class(pullCastePop(colony, caste = "drones")$pulled, "Pop"))
  suppressWarnings(expect_s4_class(pullCastePop(colony, caste = "drones")$remnant, "Colony"))

  expect_s4_class(pullCastePop(colony, caste = "workers")$pulled, "Pop")
  expect_s4_class(pullCastePop(colony, caste = "workers")$remnant, "Colony")

  expect_s4_class(pullCastePop(colony, caste = "queen")$pulled, "Pop")
  expect_null(pullCastePop(colony, caste = "virginQueens")$pulled)

  # test on fathers - GitHub  issue made

  # Error is "caste" argument is missing
  expect_error(pullCastePop(colony))
  expect_error(pullCastePop(apiary))

  # type
  expect_type(pullCastePop(colony, caste = "workers"), "list")
  expect_type(pullCastePop(colony, caste = "queen"), "list")
  suppressWarnings(expect_type(pullCastePop(colony, caste = "drones"), "list"))

  expect_type(pullCastePop(apiary, caste = "workers"), "list")
  expect_type(pullCastePop(apiary, caste = "queen"), "list")
  expect_type(pullCastePop(apiary, caste = "drones"), "list")

  # Test whether you pull out more individuals that available
  expect_equal(pullCastePop(colony, caste = "workers", nInd = 10)$pulled@nInd, 10)

})

# ---- cross ----
test_that("cross", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes, nInd = 100)
  drones <- createDrones(basePop[1], n = 1000)
  dronesGroups <- pullDroneGroupsFromDCA(drones, n = 7, nDrones = 15)

  # Create Colony and MultiColony class
  colony <- createColony(basePop[2])
  colony <- cross(colony, drones = dronesGroups[[1]])
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100)
  #remove queen
  colony <- removeQueen(colony)
  # If x = colony virgin queens must be present
  expect_error(cross(colony, drones = dronesGroups))
  expect_null(isVirginQueen(colony@virginQueens))

  apiary <- createMultiColony(basePop[3:4], n = 2)
  colony1 <- createColony(basePop[5])
  colony2 <- createColony(basePop[6])
  virginQueen <- basePop[7]
  virginQueen2 <- basePop[8]

  # Error if x = pop and are not virgin queens
  expect_error(cross(colony@workers, drones = dronesGroups[[2]]))
  expect_null(isVirginQueen(colony@virginQueens))
  # Error if fathers is not a pop
  expect_error(cross(colony1, drones = colony))
  expect_true(isVirginQueen(colony1@virginQueens))
  # Error if fathers are not drones
  expect_error(cross(colony1, drones = colony@workers))
  expect_true(isVirginQueen(colony1@virginQueens))
  # Error if length of fathers doesn't match virginqueens
  expect_error(cross(basePop[3:4], drones = dronesGroups[1]))
  expect_true(all(isVirginQueen(basePop[3:4])))
  # If x = colony, queen must not be present
  expect_error(cross(colony, drones = nFathers[1]))
  expect_null(isVirginQueen(colony@virginQueens))

  # If x = multicolony, length of fathers must match length of colonies
  expect_error(cross(apiary, drones = dronesGroups[1]))
  expect_true(all(isVirginQueensPresent(apiary)))
  # Cannot mate with already-mated drones
  expect_error(cross(colony1, drones = drones))
  expect_true(isVirginQueen(colony1@virginQueens))

  # Must mate prior to S4 class check to change SP
  colony2 <- cross(colony2, drones = dronesGroups[[3]])
  expect_s4_class(colony2,"Colony")
  virginQueen <- cross(virginQueen, drones = dronesGroups[[4]])
  expect_s4_class(virginQueen, "Pop")
  apiary <- cross(apiary, drones = dronesGroups[c(5,6)])
  expect_s4_class(apiary, "MultiColony")

  # Error when mating a mated queen
  expect_error(cross(colony2, drones = dronesGroups[7]))

  # Message if fathers == 0 "Mating failed"
  expect_error(cross(virginQueen2, drones= selectInd(colony@drones,nInd = 0, use = "rand")))
  expect_warning(cross(virginQueen2, drones= selectInd(colony@drones,nInd = 0, use = "rand"), checkMating = "warning"))
})

# ---- setQueensYearOfBirth ----
test_that("setQueensYearOfBirth", {
  founderGenomes <- quickHaplo(nInd = 7, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes, nInd = 100)
  drones <- createDrones(x = basePop[1], nInd = 1000)
  dronesGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

  colony <- createColony(x = basePop[2])
  colony <- cross(x = colony, drones = dronesGroups[[1]])
  colony <- buildUp(colony)
  # Error if x = pop, and not a vq or q
  expect_error(setQueensYearOfBirth(colony@workers))
  expect_error(setQueensYearOfBirth(colony@drones))

  colony <- removeQueen(colony)
  # Error if x = colony and no queen is present
  expect_error(setQueensYearOfBirth(colony))

  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = dronesGroups[c(2, 3)])

  colony1 <- createColony(x = basePop[5])
  colony1 <- cross(colony1, drones = dronesGroups[[4]])
  queen1 <- getQueen(colony1)

  expect_s4_class(setQueensYearOfBirth(queen1, year = 2022), "Pop")
  expect_s4_class(setQueensYearOfBirth(colony1, year = 2022), "Colony")
  expect_s4_class(setQueensYearOfBirth(apiary, year = 2022), "MultiColony")
})

# ---- createDCA ----
test_that("createDCA", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  dronesGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = dronesGroups[[1]])
  apiary <- createMultiColony(basePop[3:4], n = 2)
  apiary <- cross(apiary, drones = dronesGroups[c(2, 3)])
  # Warning if x = colony/multicolony and no drones available
  expect_warning(createDCA(colony))
  expect_warning(createDCA(apiary))
  # Error if another caste is used
  expect_error(createDCA(basePop[6]))

  colony1 <- createColony(x = basePop[5])
  colony1 <- addDrones(colony, nInd = 100)

  colony2 <- createColony(x = basePop[6])
  colony2 <- cross(colony2, drones = selectInd(colony1@drones, nInd = 1, use = "rand"))
  # Warning : Taking only drones that have not yet mated
  expect_warning(createDCA(colony1))

  suppressWarnings(expect_s4_class(createDCA(colony1, nInd = 10), "Pop"))
  suppressWarnings(expect_equal(createDCA(colony1, nInd =10)@nInd, 10))

  #empty apiary
  apiary1 <- createMultiColony(n = 3)
  expect_error(createDCA(apiary1))
})

# ---- pullDroneGroupsFromDCA ----
test_that("pullDroneGroupsFromDCA", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 1000)
  expect_length(pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson), 10)
  dronesGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = dronesGroups[[1]])
  colony <- addDrones(colony, nInd = 100)

  DCA <- createDCA(colony, nInd = 80)
  # Error, DCA must be a Pop
  expect_error(pullDroneGroupsFromDCA(colony))
  # Error, n must be provided
  expect_error(pullDroneGroupsFromDCA(DCA))

  expect_type(pullDroneGroupsFromDCA(DCA, n = 5, nDrones = 8), "list")
  expect_s4_class(pullDroneGroupsFromDCA(DCA, n = 5, nDrones = 8)[[1]], "Pop")
  expect_true(all(isDrone(pullDroneGroupsFromDCA(DCA, n = 1, nDrones = 70)[[1]])))

  suppressWarnings(DCA2 <- createDCA(colony, nInd = 4))
  # Error, if nInd in DCA is smaller than nFathers
  expect_error(pullDroneGroupsFromDCA(DCA2, n =10, nDrones = 20))
})

test_that("combineBeeGametes", {
   founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$setTrackRec(TRUE)
   SP$setTrackPed(isTrackPed = TRUE)
   basePop <- createVirginQueens(founderGenomes)

   queen <- basePop[1]
   drones <- createDrones(x = basePop[2], nInd = 5)
   workers <- SIMplyBee:::combineBeeGametes(queen, drones, nProgeny = 4)
   workers@id
   workers@mother
   workers@father
   SP$pedigree
   SP$recHist
   SP$recHist[[11]][[1]][1]
   SP$recHist[[11]][[1]][2]
})

test_that("combineBeeGametesHaploidDiploid", {
   founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$setTrackRec(TRUE)
   SP$setTrackPed(isTrackPed = TRUE)
   basePop <- createVirginQueens(founderGenomes)

   queen <- basePop[1]
   drones <- reduceGenome(
     pop = basePop[2], nProgeny = 5, keepParents = FALSE,
     simRecomb = TRUE
   )
   workers <- SIMplyBee:::combineBeeGametesHaploDiploid(queen, drones, nProgeny = 4)
   workers@id
   workers@mother
   workers@father
   SP$pedigree
   SP$recHist
   SP$recHist[[11]][[1]][1]
   SP$recHist[[11]][[1]][2]
})
