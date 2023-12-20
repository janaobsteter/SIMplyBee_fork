# ---- Create Colony ----

test_that("createColony", {
   founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
   drones <- createDrones(x = basePop[1], nInd = 15, simParamBee = SP)
   matedQueen <- cross(basePop[2], drones = drones, simParamBee = SP)

   # Create empty colony
   expect_s4_class(createColony(simParamBee = SP), "Colony")
   # Create non-empty colony with a virgin queen
   expect_s4_class(createColony(x = basePop[3], simParamBee = SP), "Colony")
   # Create a colony with a mated queen
   expect_s4_class(createColony(x = matedQueen, simParamBee = SP), "Colony")
   # Create a colony with a drones
   expect_error(createColony(x = drones[1], simParamBee = SP))
})

# ----  ReQueen ----

test_that("reQueen", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 30, simParamBee = SP)
  virginQueen <- basePop[2]
  virginQueen1 <- basePop[3]
  matedQueen <- cross(basePop[4], drones = drones[1:15], simParamBee = SP)
  matedQueen1 <- cross(basePop[5], drones = drones[16:30], simParamBee = SP)

  # Create virgin colony
  virginColony <- createColony(x = virginQueen, simParamBee = SP)
  # Create mated colony
  matedColony <- createColony(x = matedQueen, simParamBee = SP)
  # Requeen a virgin colony with a virgin queen
  expect_s4_class(reQueen(virginColony, queen = virginQueen1), "Colony")
  expect_false(isQueenPresent(reQueen(virginColony, queen = virginQueen1)))
  expect_true(isVirginQueensPresent(reQueen(virginColony, queen = virginQueen1)))
  # Try to requeen with a drone
  expect_error(isVirginQueensPresent(reQueen(virginColony, queen = drones[1])))
  # We can't requeen with a workers
  expect_error(reQueen(virginColony, queen = createWorkers(matedColony, nInd = 1, simParamBee = SP)$workers))
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

# ----  Add Functions ----

test_that("Add functions", {
   founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 100, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson, simParamBee = SP)

   # Create and cross Colony and MultiColony class
   emptyColony <- createColony(simParamBee = SP)
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   emptyApiary <- createMultiColony(simParamBee = simParamBee)
   apiary <- createMultiColony(basePop[4:5], n = 2, simParamBee = simParamBee)
   apiary <- cross(apiary, drones = droneGroups[3:4], simParamBee = SP)

   # Add virgin queens to an empty colony --> missing queen error
   expect_error(addVirginQueens(emptyColony, nInd = 10, simParamBee = SP))
   expect_error(addWorkers(emptyColony, nInd = 10, simParamBee = SP))
   expect_error(addDrones(emptyColony, nInd = 10, simParamBee = SP, simParamBee = SP))
   expect_s4_class(addVirginQueens(apiary, nInd = 5, simParamBee = SP), "MultiColony")
   expect_s4_class(addWorkers(apiary, nInd = 5, simParamBee = SP), "MultiColony")
   expect_s4_class(addDrones(apiary, nInd = 5, simParamBee = SP, simParamBee = SP), "MultiColony")

   # Add virgin queens in the colony
   expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 5, simParamBee = SP)), 5)
   expect_equal(nWorkers(addWorkers(colony, nInd = 5, simParamBee = SP)), 5)
   expect_equal(nDrones(addDrones(colony, nInd = 5, simParamBee = SP)), 5)
   colony <- addVirginQueens(colony, nInd = 5, simParamBee = SP)
   colony <- addWorkers(colony, nInd = 5, simParamBee = SP)
   colony <- addDrones(colony, nInd = 5, simParamBee = SP)
   # Adding 0 individuals doesn't change the number
   suppressWarnings(expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 0, simParamBee = SP)), 5))
   expect_warning(nWorkers(addWorkers(colony, nInd = 0, simParamBee = SP)))
   suppressWarnings(expect_equal(nWorkers(addWorkers(colony, nInd = 0, simParamBee = SP)), 5))
   expect_warning(nDrones(addDrones(colony, nInd = 0, simParamBee = SP)))
   # Adding them in sums the number expect when new is TRUE
   expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 5, simParamBee = SP)), 10)
   expect_equal(nWorkers(addWorkers(colony, nInd = 5, simParamBee = SP)), 10)
   expect_equal(nDrones(addDrones(colony, nInd = 5, simParamBee = SP)), 10)
   expect_equal(nVirginQueens(addVirginQueens(colony, nInd = 5, new = TRUE, simParamBee = SP)), 5)
   expect_equal(nWorkers(addWorkers(colony, nInd = 5, new = TRUE, simParamBee = SP)), 5)
   expect_equal(nDrones(addDrones(colony, nInd = 5, new = TRUE, simParamBee = SP)), 5)
   # If input is an apiary
   # Empty apiary - you can add, but nothing happens - returns an empty apiary
   expect_s4_class(addVirginQueens(emptyApiary, nInd = 5, simParamBee = SP), "MultiColony")
   expect_s4_class(addWorkers(emptyApiary, nInd = 5, simParamBee = SP), "MultiColony")
   expect_s4_class(addDrones(emptyApiary, nInd = 5, simParamBee = SP), "MultiColony")
   # Non-empty apiary
   expect_s4_class(addVirginQueens(apiary, nInd = 5, simParamBee = SP), "MultiColony")
   expect_s4_class(addWorkers(apiary, nInd = 5, simParamBee = SP), "MultiColony")
   expect_s4_class(addDrones(apiary, nInd = 5, simParamBee = SP), "MultiColony")
})

# ---- BuildUp downSize ----

test_that("BuildUpDownsize", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   emptyColony <- createColony(simParamBee = SP)
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   emptyApiary <- createMultiColony(simParamBee = simParamBee)
   apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = simParamBee)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)

   #BuildUp empty colony - missing queen error
   expect_error(buildUp(emptyColony, simParamBee = SP))
   # Build up non-empty colony
   expect_s4_class(buildUp(colony, nWorkers = 100, nDrones = 10, exact = TRUE, simParamBee = SP), "Colony")
   expect_equal(nWorkers(buildUp(colony, nWorkers = 100, nDrones = 10, exact = TRUE, simParamBee = SP)), 100)
   expect_equal(nDrones(buildUp(colony, nWorkers = 100, nDrones = 10, exact = TRUE, simParamBee = SP)), 10)
   colony <- buildUp(colony, nWorkers = 100, nDrones = 10, exact = TRUE, simParamBee = SP)
   workerIDs <- getId(colony@workers)
   # if New = TRUE (deafult), add in fresh workers of a desired number (even if less that what's already in)
   expect_equal(nWorkers(buildUp(colony, nWorkers = 70, exact = TRUE, simParamBee = SP)), 70)
   expect_equal(nDrones(buildUp(colony, nDrones = 5, exact = TRUE, simParamBee = SP)), 5)
   # if New = FALSE and nInd is less than n individuals
   expect_equal(nWorkers(buildUp(colony, nWorkers = 70, exact = TRUE, new = FALSE, simParamBee = SP)), 70)
   expect_equal(nDrones(buildUp(colony, nDrones = 5, exact = TRUE, new = FALSE, simParamBee = SP)), 5)

   # Build Up an apiary
   # Empty apiary
   expect_s4_class(buildUp(emptyApiary, simParamBee = SP), "MultiColony")
   # Non-empty apiary
   expect_equal(nColonies(buildUp(apiary, simParamBee = SP)), 2)

   # Downsize EmptyColony works
   downsize(emptyColony, simParamBee = SP)

   expect_s4_class(downsize(colony, p = 0.5, simParamBee = SP), "Colony")
   expect_equal(nWorkers(downsize(colony, p = 0.5, simParamBee = SP)), 50)
   expect_error(nWorkers(downsize(colony, n = 10, simParamBee = SP)))
   expect_error(nWorkers(downsize(colony, p = 1.2, simParamBee = SP)))
   workersIDs <- getId(colony@workers)
   expect_length(intersect(getId(getWorkers(downsize(colony, p = 0.1, new = TRUE, simParamBee = SP))), workersIDs), 0)

   # Empty apiary
   expect_s4_class(downsize(emptyApiary, simParamBee = SP), "MultiColony")
   # Non-empty apiary
   downsize(apiary, simParamBee = SP)
})

# ---- Replace Functions  ----

test_that("replaceFunctions", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 100, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson, simParamBee = SP)

  # Create and cross Colony and MultiColony class
  emptyColony <- createColony(simParamBee = SP)
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 10, simParamBee = SP)
  colony <- addVirginQueens(colony, nInd = 10, simParamBee = SP)
  emptyApiary <- createMultiColony(simParamBee = simParamBee)
  apiary <- createMultiColony(basePop[4:5], n = 2, simParamBee = simParamBee)
  apiary <- cross(apiary, drones = droneGroups[3:4], simParamBee = SP)
  apiary <- buildUp(apiary, nWorkers = 100, nDrones = 10, simParamBee = SP)
  apiary <- addVirginQueens(apiary, nInd = 10, simParamBee = SP)

  # Replace individuals in an empty colony/apiary --> missing queen error for Colony
  expect_error(replaceVirginQueens(emptyColony, p = 0.5, simParamBee = SP))
  expect_error(replaceWorkers(emptyColony, p = 0, simParamBee = SP))
  expect_error(replaceDrones(emptyColony, p = 1, simParamBee = SP))
  expect_s4_class(replaceVirginQueens(emptyApiary, p = 0.5, simParamBee = SP), "MultiColony")
  expect_s4_class(replaceWorkers(emptyApiary, p = 0, simParamBee = SP), "MultiColony")
  expect_s4_class(replaceDrones(emptyApiary, p = 1, simParamBee = SP), "MultiColony")

  # Replace individuals in the non-empty colony/apiary
  expect_s4_class(replaceVirginQueens(colony, simParamBee = SP), "Colony")
  expect_s4_class(replaceWorkers(colony, simParamBee = SP), "Colony")
  expect_s4_class(replaceDrones(colony, simParamBee = SP), "Colony")
  expect_equal(nVirginQueens(replaceVirginQueens(colony, p = 1, simParamBee = SP)), nVirginQueens(colony))
  expect_equal(nWorkers(replaceWorkers(colony, p = 0.5, simParamBee = SP)), nWorkers(colony))
  expect_equal(nDrones(replaceDrones(colony, p = 0, simParamBee = SP)), nDrones(colony))
  virginQueensIDs <- getId(colony@virginQueens)
  workerIDs <- getId(colony@workers)
  droneIDs <- getId(colony@drones)
  expect_length(intersect(getId(replaceVirginQueens(colony, p = 1, simParamBee = SP)@virginQueens),
                         virginQueensIDs), 0)
  expect_length(intersect(getId(replaceWorkers(colony, p = 0.5, simParamBee = SP)@workers),
                          workerIDs), nWorkers(colony)/2)
  expect_length(intersect(getId(replaceDrones(colony, p = 0, simParamBee = SP)@drones),
                          droneIDs), nDrones(colony))
  expect_s4_class(replaceVirginQueens(apiary, simParamBee = SP), "MultiColony")
  expect_s4_class(replaceWorkers(apiary, simParamBee = SP), "MultiColony")
  expect_s4_class(replaceDrones(apiary, simParamBee = SP), "MultiColony")
  expect_equal(nColonies(replaceVirginQueens(apiary, p = 1, simParamBee = SP)), nColonies(apiary))
  expect_equal(nColonies(replaceWorkers(apiary, p = 0.5, simParamBee = SP)), nColonies(apiary))
  expect_equal(nColonies(replaceDrones(apiary, p = 0, simParamBee = SP)), nColonies(apiary))
})

# ---- Remove functions  ----
test_that("removeFunctions", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 100, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson, simParamBee = SP)

  # Create and cross Colony and MultiColony class
  emptyColony <- createColony(simParamBee = SP)
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 10, simParamBee = SP)
  colony <- addVirginQueens(colony, nInd = 10, simParamBee = SP)
  emptyApiary <- createMultiColony(simParamBee = simParamBee)
  apiary <- createMultiColony(basePop[4:5], n = 2, simParamBee = simParamBee)
  apiary <- cross(apiary, drones = droneGroups[3:4], simParamBee = SP)
  apiary <- buildUp(apiary, nWorkers = 100, nDrones = 10, simParamBee = SP)
  apiary <- addVirginQueens(apiary, nInd = 10, simParamBee = SP)

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

# ---- SetLocation ----
test_that("setLocation", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 50)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 1000, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 4, nDrones = 10, simParamBee = SP)
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
  apiary <- createMultiColony(basePop[3:5], simParamBee = SP)
  apiary <- cross(apiary, drones = droneGroups[2:4], simParamBee = SP)

  loc <- c(1, 1)
  expect_equal(getLocation(setLocation(colony, location = loc)), loc)

  expect_equal(getLocation(setLocation(apiary, location = loc)),
               list("2" = loc, "3" = loc, "4" = loc))

  locList <- list("2" = c(0, 0), "3" = c(1, 1), "4" = c(2, 2))
  expect_equal(getLocation(setLocation(apiary, location = locList)), locList)

  locDF <- data.frame(x = c(0, 1, 2), y = c(0, 1, 2))
  expect_equal(getLocation(setLocation(apiary, location = locDF)), locList)

  emptyColony <- createColony(simParamBee = SP)
  expect_s4_class(setLocation(emptyColony, location = c(1,1)), "Colony")
  expect_equal(setLocation(emptyColony, location = c(1,1))@location, c(1,1))

  emptyApiary <- createMultiColony(n = 3, simParamBee = simParamBee)
  apiary <- createMultiColony(basePop[1:3], simParamBee = simParamBee)

  expect_s4_class(setLocation(emptyApiary, location = c(1,2)), "MultiColony")
  expect_error(setLocation(emptyApiary, location = list(1,2))) # Lengths do not match
  expect_s4_class(setLocation(emptyApiary, location = list(1:2, 3:4, 4:5)), "MultiColony") #Not setting anything, if all are NULL!!!!
  expect_s4_class(setLocation(apiary, location = c(1,2)), "MultiColony")
  expect_s4_class(setLocation(apiary, location = list(1:2, 3:4, 4:5)), "MultiColony")
})



# ---- Supersede ----

test_that("supersede", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create Colony and MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 100, simParamBee = SP)
  apiary <- createMultiColony(basePop[3:8], simParamBee = SP)
  apiary <- cross(apiary, drones = fatherGroups[2:7], simParamBee = SP)
  apiary <- buildUp(apiary, nWorkers = 100, simParamBee = SP)

  expect_true(isQueenPresent(colony))
  expect_true(all(isQueenPresent(apiary)))
  expect_false(isVirginQueensPresent(colony))
  expect_false(all(isVirginQueensPresent(apiary)))
  # Supersede a colony and an apiary
  colony <- supersede(colony)
  apiary <- supersede(apiary)

  expect_false(isQueenPresent(colony))
  expect_false(all(isQueenPresent(apiary)))
  expect_true(isVirginQueensPresent(colony))
  expect_true(all(isVirginQueensPresent(apiary)))
  #test error
  colony <- createColony(x = basePop[2])
  colony@queen <- NULL
  expect_error(supersede(colony))
})

# ---- Split ----

test_that("split", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
  nWorkers = 100

  # Create Colony and MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  colony <- buildUp(colony, nWorkers = nWorkers)
  apiary <- createMultiColony(basePop[3:8], n = 6)
  apiary <- cross(apiary, drones = fatherGroups[2:7])
  apiary <- buildUp(apiary, nWorkers = 100)

  expect_error(split(colony, p = 1.2))
  # Split a colony
  tmp <- split(colony, p = 0.5)
  colony <- tmp$remnant
  expect_true(hasSplit(colony))
  expect_equal(nWorkers(colony), nWorkers/2)
  colony@workers <- NULL
  expect_error(split(colony, p = 0.5))
  expect_equal(length(tmp), 2)

  colony <- buildUp(colony, nWorkers = nWorkers)
  tmp <- split(colony, p = 1)
  expect_equal(nWorkers(tmp$split), 100)
  colony <- buildUp(colony, nWorkers = nWorkers)
  colony@queen <- NULL
  expect_error(split(colony, p = 0.5))
  tmp <- split(apiary)
  expect_equal(nColonies(tmp$split), 6)
  expect_equal(nColonies(tmp$remnant), 6)

})

# ---- ResetEvents ----

test_that("resetEvents", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes)

  drones <- createDrones(x = basePop[1], nInd = 100)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = 10)

  # Create and cross Colony and MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(colony, drones = fatherGroups[[1]])
  apiary <- createMultiColony(basePop[4:5], n = 2)
  apiary <- cross(apiary, drones = fatherGroups[3:4])

  # Build-up - this sets Productive to TRUE
  colony <- buildUp(colony, nWorkers = 100)
  apiary <- buildUp(apiary, nWorkers = 100)
  # Supersede - this sets Supersede to TRUE
  colony <- supersede(colony)
  apiary <- supersede(apiary)

  expect_true(isProductive(colony))
  expect_true(all(isProductive(apiary)))
  expect_true(hasSuperseded(colony))
  expect_true(all(hasSuperseded(apiary)))

  colony <- resetEvents(colony)
  apiary <- resetEvents(apiary)

  expect_false(isProductive(colony))
  expect_false(all(isProductive(apiary)))
  expect_false(hasSuperseded(colony))
  expect_false(all(hasSuperseded(apiary)))
})

# ---- Combine ----

test_that("Combine", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create weak and strong Colony and MultiColony class
  colony1 <- createColony(x = basePop[2])
  colony1 <- cross(colony1, drones = fatherGroups[[1]])
  colony2 <- createColony(x = basePop[3])
  colony2 <- cross(colony2, drones = fatherGroups[[2]])
  apiary1 <- createMultiColony(basePop[4:6], n = 3)
  apiary1 <- cross(apiary1, drones = fatherGroups[3:5])
  apiary2 <- createMultiColony(basePop[7:9], n = 3)
  apiary2 <- cross(apiary2, drones = fatherGroups[6:8])

  # Build-up
  colony1 <- buildUp(x = colony1, nWorkers = 100, nDrones = 20)
  colony2 <- buildUp(x = colony2, nWorkers = 20, nDrones = 5)
  apiary1 <- buildUp(x = apiary1, nWorkers = 100, nDrones = 20)
  apiary2 <- buildUp(x = apiary2, nWorkers = 20, nDrones = 5)

  colony3 <- combine(strong = colony1, weak = colony2)
  apiary3 <- combine(strong = apiary1, weak = apiary2)
  expect_equal(nWorkers(colony3),sum(nWorkers(colony1), nWorkers(colony2)))
  expect_equal(colony1@queen@id, colony3@queen@id)
  expect_equal(nWorkers(apiary3[[2]]),sum(nWorkers(apiary1[[2]]), nWorkers(apiary2[[2]])))
  colony1 <- NULL
  colony2 <- NULL
  expect_error(combine(strong = colony1, weak = colony2)) # discus the output
})

# ---- Swarm ----

test_that("swarm", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)
  nWorkers = 100
  # Create Colony and MultiColony class
  colony <- createColony(x = basePop[2])
  colony <- cross(x = colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(colony, nWorkers = nWorkers, simParamBee = SP)
  apiary <- createMultiColony(basePop[3:8], n = 6, simParamBee = SP)
  apiary <- cross(x = apiary, drones = fatherGroups[2:7], simParamBee = SP)
  apiary <- buildUp(apiary, nWorkers = 100, simParamBee = SP)

  # Swarm a colony
  tmp <- swarm(colony, p = 0.5, simParamBee = SP)

  expect_true(isQueenPresent(tmp$swarm, simParamBee = SP))
  expect_true(areVirginQueensPresent(tmp$remnant, simParamBee = SP))
  expect_equal(colony@queen@id, tmp$swarm@queen@id)
  expect_equal(nWorkers(tmp$swarm, simParamBee = SP), nWorkers/2)
  expect_equal(length(tmp), 2)
  colony <- buildUp(colony, nWorkers = nWorkers, simParamBee = SP)
  tmp <- swarm(colony, p = 1, simParamBee = SP)
  expect_equal(nWorkers(tmp$swarm, simParamBee = SP), 100)
  colony <- tmp$swarm
  colony@queen <- NULL
  expect_error(swarm(colony, p = 0.4, simParamBee = SP))

  #Apiary test
  tmp <- swarm(apiary, p = 0.5, simParamBee = SP)
  expect_equal(nColonies(tmp$swarm), 6)
  expect_equal(nColonies(tmp$remnant), 6)
  expect_true(isQueenPresent(tmp$swarm[[4]], simParamBee = SP))
  expect_true(areVirginQueensPresent(tmp$remnant[[4]], simParamBee = SP))
})

# ---- Collapse -----

test_that("collapse", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes)
  drones <- createDrones(basePop[1], n = 1000)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)

  # Create Colony and MultiColony class
  colony <- createColony(x = basePop[1])
  colony <- cross(colony, drones = fatherGroups[[1]])
  apiary <- createMultiColony(x = basePop[2:10], n = 9)
  apiary <- cross(apiary, drones = fatherGroups[2:10])

  # Collapse
  expect_false(hasCollapsed(colony))
  colony <- collapse(colony)
  expect_true(hasCollapsed(colony))

  expect_false(all(hasCollapsed(apiary)))
  tmp <- pullColonies(apiary, n = 2)
  apiaryLost <- collapse(tmp$pulled)
  expect_true(all(hasCollapsed(apiaryLost)))
  apiaryLeft <- tmp$remnant
  expect_false(all(hasCollapsed(apiaryLeft)))
})


