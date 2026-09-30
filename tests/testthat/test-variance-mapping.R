test_that("individual-to-colony round trip recovers the identifiable individual components", {
  for (workersFUN in c("sum", "mean")) {
    for (workerAllocation in c("random", "balanced")) {
      referenceColony <- NULL
      for (corE in c(0, 0.7)) {
        # Start with individual inputs, independently of the inverse mapping
        indArgs <- list(
          varAQueen = 1,
          varAWorker = 1,
          corAQueenWorker = -0.5,
          varEQueen = 1,
          varEWorker = 1,
          corEQueenWorker = corE,
          nW = if (workerAllocation == "balanced") 120 else 100,
          nF = 15,
          nDPQ = 5,
          workersFUN = workersFUN,
          workerAllocation = workerAllocation
        )
        colonyVars <- do.call(mapIndToColonyVar, indArgs)
        indVars <- do.call(
          mapColonyToIndVar,
          colonyVars[names(formals(mapColonyToIndVar))]
        )

        expect_equal(colonyVars[names(indArgs)], indArgs)
        identifiableInputs <- setdiff(names(indArgs), "corEQueenWorker")
        expect_equal(indVars[identifiableInputs], indArgs[identifiableInputs])
        expect_equal(indVars$covAQueenWorker, -0.5)
        expect_equal(colonyVars$covEQueenWorker, corE)
        expect_equal(colonyVars$corEQueenWorker, corE)
        expect_equal(colonyVars$covEQueenWorkerGroup, 0)
        expect_equal(colonyVars$corEQueenWorkerGroup, 0)

        # An unknown within-bee association must not become an assumed zero
        expect_identical(indVars$covEQueenWorker, NA_real_)
        expect_identical(indVars$corEQueenWorker, NA_real_)
        expect_false(identical(indVars, colonyVars))
        expect_identical(names(indVars), names(colonyVars))
        recoverable <- setdiff(
          names(colonyVars),
          c("covEQueenWorker", "corEQueenWorker")
        )
        # Numerical equality allows floating-point rounding in the inverse
        expect_equal(indVars[recoverable], colonyVars[recoverable])

        # Changing within-bee corE must leave colony components unchanged
        if (is.null(referenceColony)) {
          referenceColony <- colonyVars[recoverable]
        } else {
          expect_equal(colonyVars[recoverable], referenceColony)
        }
      }
    }
  }
})

test_that("colony-to-individual round trip recovers the supplied colony components", {
  for (workersFUN in c("sum", "mean")) {
    for (workerAllocation in c("random", "balanced")) {
      # Start with colony inputs, independently of the forward mapping
      colonyArgs <- list(
        varAQueen = 1,
        varAWorkerGroup = 1,
        corAQueenWorkerGroup = -0.5,
        varEQueen = 1,
        varEWorkerGroup = 1,
        corEQueenWorkerGroup = 0,
        nW = if (workerAllocation == "balanced") 120 else 100,
        nF = 15,
        nDPQ = 5,
        workersFUN = workersFUN,
        workerAllocation = workerAllocation
      )
      indVars <- do.call(mapColonyToIndVar, colonyArgs)
      expect_identical(indVars$covEQueenWorker, NA_real_)
      expect_identical(indVars$corEQueenWorker, NA_real_)

      # The within-bee environmental assumption does not affect colony values
      for (corE in c(0, 0.7)) {
        indArgs <- indVars[names(formals(mapIndToColonyVar))]
        indArgs$corEQueenWorker <- corE
        colonyVars <- do.call(mapIndToColonyVar, indArgs)

        expect_equal(colonyVars[names(colonyArgs)], colonyArgs)
        expect_equal(colonyVars$covAQueenWorkerGroup, -0.5)
        expect_equal(colonyVars$varAColony, 1)
        expect_equal(colonyVars$covEQueenWorkerGroup, 0)
        expect_equal(colonyVars$varEColony, 2)
        expect_equal(colonyVars$corEQueenWorker, corE)
        expect_equal(
          colonyVars$covEQueenWorker,
          corE * sqrt(indVars$varEQueen * indVars$varEWorker)
        )
        recoverable <- setdiff(
          names(indVars),
          c("covEQueenWorker", "corEQueenWorker")
        )
        expect_identical(names(colonyVars), names(indVars))
        expect_equal(colonyVars[recoverable], indVars[recoverable])
      }
    }
  }
})
