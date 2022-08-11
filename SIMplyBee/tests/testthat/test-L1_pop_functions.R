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
  apiary <- createColonies(basePop[3:4], n = 2)
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
