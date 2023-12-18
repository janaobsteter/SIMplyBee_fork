
####----- CREATE COLONY ---- ####
test_that("createColony", {
   founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes)
   drones <- createDrones(x = basePop[1], nInd = 15)
   matedQueen <- cross(basePop[2], drones = drones)

   # Create empty colony
   expect_s4_class(createColony(), "Colony")
   # Create non-empty colony with a virgin queen
   expect_s4_class(createColony(x = basePop[3]), "Colony")
   # Create a colony with a mated queen
   expect_s4_class(createColony(x = matedQueen), "Colony")
   # Create a colony with a drones
   expect_error(createColony(x = drones[1]))
   # Set a location
   expect_s4_class(createColony(location = c(1,2)), "Colony")
})

####-----   REQUEEN ---- ####
test_that("reQueen", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 30)
  virginQueen <- basePop[2]
  virginQueen1 <- basePop[3]
  matedQueen <- cross(basePop[4], drones = drones[1:15])
  matedQueen1 <- cross(basePop[5], drones = drones[16:30])

  # Create virgin colony
  virginColony <- createColony(x = virginQueen)
  # Create mated colony
  matedColony <- createColony(x = matedQueen)
  # Requeen a virgin colony with a virgin queen
  expect_s4_class(reQueen(virginColony, queen = virginQueen1), "Colony")
  expect_false(isQueenPresent(reQueen(virginColony, queen = virginQueen1)))
  expect_true(isVirginQueensPresent(reQueen(virginColony, queen = virginQueen1)))
  # Try to requeen with a drone
  expect_error(isVirginQueensPresent(reQueen(virginColony, queen = drones[1])))
  # We can't requeen with a workers
  expect_error(reQueen(virginColony, queen = createWorkers(matedColony, nInd = 1)$workers))
  # It can accept multiple virgin queens, but not multiple queens
  expect_s4_class(reQueen(virginColony, queen = c(virginQueen, virginQueen)), "Colony")
  expect_error(reQueen(virginColony, queen = c(matedQueen, matedQueen)))

  # Requeen a virgin colony with a mated queen
  expect_s4_class(reQueen(virginColony, queen = matedQueen1), "Colony")
  expect_false(isVirginQueensPresent(reQueen(virginColony, queen = matedQueen1)))
  expect_true(isVirginQueensPresent(reQueen(virginColony, queen = matedQueen1, removeVirginQueens = FALSE)))

  # Requeen a mated colony with a virgin queen
  expect_s4_class(reQueen(matedColony, queen = virginQueen1), "Colony")
  expect_false(isQueenPresent(reQueen(matedColony, queen = virginQueen1)))

  # Requeen a mated colony with a mated queen
  expect_s4_class(reQueen(matedColony, queen = matedQueen1), "Colony")
  expect_true(isQueenPresent(reQueen(matedColony, queen = matedQueen1)))
})

####-----   ADD FUNCTIONS ---- ####
test_that("Add functions", {
   founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 100)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)

   # Create and cross Colony and MultiColony class
   emptyColony <- createColony()
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, drones = droneGroups[[1]])
   emptyApiary <- createMultiColony()
   apiary <- createMultiColony(basePop[4:5], n = 2)
   apiary <- cross(apiary, drones = droneGroups[3:4])

   # Add virgin queens to an empty colony --> missing queen error
   expect_error(addVirginQueens(emptyColony, nInd = 10))
   expect_error(addWorkers(emptyColony, nInd = 10))
   expect_error(addDrones(emptyColony, nInd = 10))
   expect_s4_class(addVirginQueens(apiary, nInd = 5), "MultiColony")
   expect_s4_class(addWorkers(apiary, nInd = 5), "MultiColony")
   expect_s4_class(addDrones(apiary, nInd = 5), "MultiColony")

   # Add virgin queens in the colony
   expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 5)), 5)
   expect_equal(nWorkers(addWorkers(colony, nInd = 5)), 5)
   expect_equal(nDrones(addDrones(colony, nInd = 5)), 5)
   colony <- addVirginQueens(colony, nInd = 5)
   colony <- addWorkers(colony, nInd = 5)
   colony <- addDrones(colony, nInd = 5)
   # Adding 0 individuals doesn't change the number
   suppressWarnings(expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 0)), 5))
   expect_warning(nWorkers(addWorkers(colony, nInd = 0)))
   suppressWarnings(expect_equal(nWorkers(addWorkers(colony, nInd = 0)), 5))
   expect_warning(nDrones(addDrones(colony, nInd = 0)))
   # Adding them in sums the number expect when new is TRUE
   expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 5)), 10)
   expect_equal(nWorkers(addWorkers(colony, nInd = 5)), 10)
   expect_equal(nDrones(addDrones(colony, nInd = 5)), 10)
   expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 5, new = TRUE)), 5)
   expect_equal(nWorkers(addWorkers(colony, nInd = 5, new = TRUE)), 5)
   expect_equal(nDrones(addDrones(colony, nInd = 5, new = TRUE)), 5)
   # If input is an apiary
   # Empty apiary - you can add, but nothing happens - returns an empty apiary
   expect_s4_class(addVirginQueens(emptyApiary, nInd = 5), "MultiColony")
   expect_s4_class(addWorkers(emptyApiary, nInd = 5), "MultiColony")
   expect_s4_class(addDrones(emptyApiary, nInd = 5), "MultiColony")
   # Non-empty apiary
   expect_s4_class(addVirginQueens(apiary, nInd = 5), "MultiColony")
   expect_s4_class(addWorkers(apiary, nInd = 5), "MultiColony")
   expect_s4_class(addDrones(apiary, nInd = 5), "MultiColony")
})

####----- BuildUp downSize ---- ####
test_that("BuildUpDownsize", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

   # Create a Colony and a MultiColony class
   emptyColony <- createColony()
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, drones = droneGroups[[1]])
   emptyApiary <- createMultiColony()
   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)])

   #BuildUp empty colony - missing queen error
   expect_error(buildUp(emptyColony))
   # Build up non-empty colony
   expect_s4_class(buildUp(colony, nWorkers = 100, nDrones = 10, exact = TRUE), "Colony")
   expect_equal(nWorkers(buildUp(colony, nWorkers = 100, nDrones = 10, exact = TRUE)), 100)
   expect_equal(nDrones(buildUp(colony, nWorkers = 100, nDrones = 10, exact = TRUE)), 10)
   colony <- buildUp(colony, nWorkers = 100, nDrones = 10, exact = TRUE)
   workerIDs <- getId(colony@workers)
   # if New = TRUE (deafult), add in fresh workers of a desired number (even if less that what's already in)
   expect_equal(nWorkers(buildUp(colony, nWorkers = 70, exact = TRUE)), 70)
   expect_equal(nDrones(buildUp(colony, nDrones = 5, exact = TRUE)), 5)
   # if New = FALSE and nInd is less than n individuals
   expect_equal(nWorkers(buildUp(colony, nWorkers = 70, exact = TRUE, new = FALSE)), 70)
   expect_equal(nDrones(buildUp(colony, nDrones = 5, exact = TRUE, new = FALSE)), 5)

   # Build Up an apiary
   # Empty apiary
   expect_s4_class(buildUp(emptyApiary), "MultiColony")
   # Non-empty apiary
   expect_equal(nColonies(buildUp(apiary)), 2)

   # Downsize EmptyColony works
   downsize(emptyColony)

   expect_s4_class(downsize(colony, p = 0.5), "Colony")
   expect_equal(nWorkers(downsize(colony, p = 0.5)), 50)
   expect_error(nWorkers(downsize(colony, n = 10)))
   expect_error(nWorkers(downsize(colony, p = 1.2)))
   workersIDs <- getId(colony@workers)
   expect_length(intersect(getId(getWorkers(downsize(colony, p = 0.1, new = TRUE))), workersIDs), 0)

   # Empty apiary
   expect_s4_class(downsize(emptyApiary), "MultiColony")
   # Non-empty apiary
   downsize(apiary)
})

####----- Replace Functions  ---- ####
test_that("replaceFunctions", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 100)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)

  # Create and cross Colony and MultiColony class
  emptyColony <- createColony()
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = droneGroups[[1]])
  colony <- buildUp(colony, nWorkers = 100, nDrones = 10)
  colony <- addVirginQueens(colony, nInd = 10)
  emptyApiary <- createMultiColony()
  apiary <- createMultiColony(basePop[4:5], n = 2)
  apiary <- cross(apiary, drones = droneGroups[3:4])
  apiary <- buildUp(apiary, nWorkers = 100, nDrones = 10)
  apiary <- addVirginQueens(apiary, nInd = 10)

  # Replace individuals in an empty colony/apiary --> missing queen error for Colony
  expect_error(replaceVirginQueens(emptyColony, p = 0.5))
  expect_error(replaceWorkers(emptyColony, p = 0))
  expect_error(replaceDrones(emptyColony, p = 1))
  expect_s4_class(replaceVirginQueens(emptyApiary, p = 0.5), "MultiColony")
  expect_s4_class(replaceWorkers(emptyApiary, p = 0), "MultiColony")
  expect_s4_class(replaceDrones(emptyApiary, p = 1), "MultiColony")

  # Replace individuals in the non-empty colony/apiary
  expect_s4_class(replaceVirginQueens(colony), "Colony")
  expect_s4_class(replaceWorkers(colony), "Colony")
  expect_s4_class(replaceDrones(colony), "Colony")
  expect_equal(nVirginQueens(replaceVirginQueens(colony, p = 1)), nVirginQueens(colony))
  expect_equal(nWorkers(replaceWorkers(colony, p = 0.5)), nWorkers(colony))
  expect_equal(nDrones(replaceDrones(colony, p = 0)), nDrones(colony))
  virginQueensIDs <- getId(colony@virginQueens)
  workerIDs <- getId(colony@workers)
  droneIDs <- getId(colony@drones)
  expect_length(intersect(getId(replaceVirginQueens(colony, p = 1)@virginQueens),
                         virginQueensIDs), 0)
  expect_length(intersect(getId(replaceWorkers(colony, p = 0.5)@workers),
                          workerIDs), nWorkers(colony)/2)
  expect_length(intersect(getId(replaceDrones(colony, p = 0)@drones),
                          droneIDs), nDrones(colony))
  expect_s4_class(replaceVirginQueens(apiary), "MultiColony")
  expect_s4_class(replaceWorkers(apiary), "MultiColony")
  expect_s4_class(replaceDrones(apiary), "MultiColony")
  expect_equal(nColonies(replaceVirginQueens(apiary, p = 1)), nColonies(apiary))
  expect_equal(nColonies(replaceWorkers(apiary, p = 0.5)), nColonies(apiary))
  expect_equal(nColonies(replaceDrones(apiary, p = 0)), nColonies(apiary))
})

####----- Remove functions  ---- ####
test_that("removeFunctions", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 100)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)

  # Create and cross Colony and MultiColony class
  emptyColony <- createColony()
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = droneGroups[[1]])
  colony <- buildUp(colony, nWorkers = 100, nDrones = 10)
  colony <- addVirginQueens(colony, nInd = 10)
  emptyApiary <- createMultiColony()
  apiary <- createMultiColony(basePop[4:5], n = 2)
  apiary <- cross(apiary, drones = droneGroups[3:4])
  apiary <- buildUp(apiary, nWorkers = 100, nDrones = 10)
  apiary <- addVirginQueens(apiary, nInd = 10)

  # Remove individuals in an empty colony/apiary --> missing queen error for Colony
  expect_s4_class(removeVirginQueens(emptyColony, p = 0.5), "Colony")
  expect_s4_class(removeWorkers(emptyColony, p = 0), "Colony")
  expect_s4_class(removeDrones(emptyColony, p = 1), "Colony")
  expect_s4_class(removeVirginQueens(emptyApiary, p = 0.5), "MultiColony")
  expect_s4_class(removeWorkers(emptyApiary, p = 0), "MultiColony")
  expect_s4_class(removeDrones(emptyApiary, p = 1), "MultiColony")

  # Remove individuals in the non-empty colony/apiary
  expect_s4_class(removeVirginQueens(colony), "Colony")
  expect_s4_class(removeWorkers(colony), "Colony")
  expect_s4_class(removeDrones(colony), "Colony")
  expect_equal(nVirginQueens(removeVirginQueens(colony, p = 1)), 0)
  expect_equal(nWorkers(removeWorkers(colony, p = 0.5)), nWorkers(colony)/2)
  expect_equal(nDrones(removeDrones(colony, p = 0)), nDrones(colony))

  expect_s4_class(removeVirginQueens(apiary), "MultiColony")
  expect_s4_class(removeWorkers(apiary), "MultiColony")
  expect_s4_class(removeDrones(apiary), "MultiColony")
  expect_equal(nColonies(removeVirginQueens(apiary, p = 1)), nColonies(apiary))
  expect_equal(nColonies(removeWorkers(apiary, p = 0.5)), nColonies(apiary))
  expect_equal(nColonies(removeDrones(apiary, p = 0)), nColonies(apiary))
})

####----- SetLocation ---- ####
test_that("setLocation", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes)

  emptyColony <- createColony()
  expect_s4_class(setLocation(emptyColony, location = c(1,1)), "Colony")
  expect_equal(setLocation(emptyColony, location = c(1,1))@location, c(1,1))

  emptyApiary <- createMultiColony(n = 3)
  apiary <- createMultiColony(basePop[1:3])

  expect_s4_class(setLocation(emptyApiary, location = c(1,2)), "MultiColony")
  expect_error(setLocation(emptyApiary, location = list(1,2))) # Lengths do not match
  expect_s4_class(setLocation(emptyApiary, location = list(1:2, 3:4, 4:5)), "MultiColony") #Not setting anything, if all are NULL!!!!
  expect_s4_class(setLocation(apiary, location = c(1,2)), "MultiColony")
  expect_error(setLocation(apiary, location = list(1,2))) # Lengths do not match
  expect_s4_class(setLocation(apiary, location = list(1:2, 3:4, 4:5)), "MultiColony")
})



