
#createColony
#reQueen
#Add functions
#Builup
#downsize
#Reaplce functions
#Remove functions
#Reset events & collapse
#swarm
#supersede
#Split
#Combine
#setLocation
#setColonyPheno

test_that("createColony", {
   founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   basePop <- createVirginQueens(founderGenomes)
   drones <- createDrones(x = basePop[1], nInd = 15)
   matedQueen <- cross(basePop[2], fathers = drones)

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

test_that("reQueen", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(x = basePop[1], nInd = 30)
  virginQueen <- basePop[2]
  virginQueen1 <- basePop[3]
  matedQueen <- cross(basePop[4], fathers = drones[1:15])
  matedQueen1 <- cross(basePop[5], fathers = drones[16:30])

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

test_that("Add functions", {
   founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 100)
   fatherGroups <- pullDroneGroupsFromDCA(drones, n = 5, nFathers = nFathersPoisson)

   # Create and cross Colony and MultiColony class
   emptyColony <- createColony()
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, fathers = fatherGroups[[1]])
   emptyApiary <- createMultiColony()
   apiary <- createMultiColony(basePop[4:5], n = 2)
   apiary <- cross(apiary, fathers = fatherGroups[3:4])

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
   expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 0)), 5)
   expect_equal(nWorkers(addWorkers(colony, nInd = 0)), 5)
   expect_equal(nDrones(addDrones(colony, nInd = 0)), 5)
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

test_that("BuildUpDownsize", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   basePop <- createVirginQueens(founderGenomes)

   drones <- createDrones(x = basePop[1], nInd = 1000)
   fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nFathers = nFathersPoisson)

   # Create a Colony and a MultiColony class
   emptyColony <- createColony()
   colony <- createColony(x = basePop[2])
   colony <- cross(colony, fathers = fatherGroups[[1]])
   emptyApiary <- createMultiColony()
   apiary <- createMultiColony(basePop[3:4], n = 2)
   apiary <- cross(apiary, fathers = fatherGroups[c(2, 3)])

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
   expect_warning(buildUp(colony, nWorkers = 70, exact = TRUE, new = FALSE))
   suppressWarnings(expect_equal(nWorkers(buildUp(colony, nWorkers = 70, exact = TRUE, new = FALSE)), 100))
   expect_warning(buildUp(colony, nDrones = 5, exact = TRUE, new = FALSE))
   suppressWarnings(expect_equal(nDrones(buildUp(colony, nDrones = 5, exact = TRUE, new = FALSE)), 10))

   # Build Up an apiary
   # Empty apiary
   expect_s4_class(buildUp(emptyApiary), "MultiColony")
   # Non-empty apiary
   expect_equal(nColonies(buildUp(apiary)), 2)

   # Downsize
   # EmptyColony
   expect_error(downsize(emptyColony))
   expect_s4_class(downsize(colony, p = 0.5), "Colony")
   expect_equal(nWorkers(downsize(colony, p = 0.5)), 50)
   expect_error(nWorkers(downsize(colony, n = 10)))
   expect_error(nWorkers(downsize(colony, p = 1.2)))
   workersIDs <- getId(colony@workers)
   expect_length(intersect(getId(getWorkers(downsize(colony, p = 0.1, new = TRUE))), workersIDs), 0)

   # Empty apiary
   expect_s4_class(downsize(emptyApiary), "MultiColony")
   # Non-empty apiary
   downsize(apiary) # !!!!!!!!!!!!!
})

test_that("replaceFunctions", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 100)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 5, nFathers = nFathersPoisson)

  # Create and cross Colony and MultiColony class
  emptyColony <- createColony()
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, fathers = fatherGroups[[1]])
  emptyApiary <- createMultiColony()
  apiary <- createMultiColony(basePop[4:5], n = 2)
  apiary <- cross(apiary, fathers = fatherGroups[3:4])

  # Add virgin queens to an empty colony --> missing queen error
  expect_error(replaceWorkers(emptyColony, p = 0.5))
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
  expect_warning(nVirginQueens(addVirginQueens(colony, nInd = 0)))
  expect_warning(nWorkers(addWorkers(colony, nInd = 0)))
  expect_warning(nDrones(addDrones(colony, nInd = 0)))
  suppressWarnings(expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 0)), 5))
  suppressWarnings(expect_equal(nWorkers(addWorkers(colony, nInd = 0)), 5))
  suppressWarnings(expect_equal(nDrones(addDrones(colony, nInd = 0)), 5))
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
  expect_equal(nColonies(addVirginQueens(emptyApiary, nInd = 5)), 0)
  expect_s4_class(addWorkers(emptyApiary, nInd = 5), "MultiColony")
  expect_equal(nColonies(addWorkers(emptyApiary, nInd = 5)), 0)
  expect_s4_class(addDrones(emptyApiary, nInd = 5), "MultiColony")
  expect_equal(nColonies(addDrones(emptyApiary, nInd = 5)), 0)
  # Non-empty apiary
  expect_s4_class(addVirginQueens(apiary, nInd = 5), "MultiColony")
  expect_s4_class(addWorkers(apiary, nInd = 5), "MultiColony")
  expect_s4_class(addDrones(apiary, nInd = 5), "MultiColony")

})




