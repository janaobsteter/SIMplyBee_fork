---
editor_options: 
  markdown: 
    wrap: 72
---

# SIMplyBee version 0.5.0

-   2026-29-09

## Major changes

-   swarm/split/supersede do no longer store the year of the queen

-   colonies with high inbreeding that do not produce a viable virgin
    queens in max(10, SP\$nVirginQueens) attempts are removed in
    swarm/supersede

-   split no longer creates virgin queens in the split colonies but
    returns colonies with workers and meta data, but no virgin queens

-   createMultiColony() no longer creates an empty apiary, but it adds
    empty colonies with IDs

-   we no longer support creating an exact number of workers with the csd functionality turned on. Due to inbreeding at the csd locus, some workers are not viable. Previous funcionality re-ran the creation of workers until a required number was available. We no longer support this, so the number of returned workers
is the number of viable workers.

-   the 'cross' function now removes unsuccessfully mated queens. This can primarily happen when creating a spatial cross plan. If there are no drone producing colonies in the specified radius, the mating fails and the queens/colonies are removed from the object. This is to ensure all further functionality works well.

## New features
- added functionality to map individual-level variance to colony-level variance and vice-versa
  with functions 'mapIndToColonyVar' and 'mapColonyToIndVar'

-   parallelised all the major functions (so they run on
    simParamBee\$nThreads cores) with PSOCK system. Since the parallelisation setup within functions
    takes additional time, we recommend using a single threads for a small number of colonies

-   MultiColony operations parallelised with `future` now send each worker
    only its own chunk of colonies, instead of the whole MultiColony and the
    calling function's environment (including, for example, all drones in
    `cross()`). This reduces memory use on parallel workers, which could
    previously be killed (e.g., by HPC memory limits) with errors such as
    `FutureInterruptError` or `FutureLaunchError` ("error writing to
    connection"). Only creating individuals (`createCastePop()` and the
    virgin queens in `swarm()` and `supersede()`), `pullCastePop()`, and
    `removeCastePop()` on MultiColony objects now run in parallel; cheap
    bookkeeping steps (e.g., adding the fathers in `cross()`, `reQueen()`,
    `setLocation()`, `setEvents()`, `combine()`, and pulling workers in
    `swarm()` and `split()`) run sequentially. Seeded results after
    `cross()` differ from previous development versions, since `cross()` no
    longer consumes random numbers for setting up parallel streams.

-   MultiColony functions now send parallel workers much less data, which
    reduces run time and memory use, especially with `plan(multisession)`:

    -   `createCastePop()` (and hence `addWorkers()`, `addDrones()`,
        `addVirginQueens()`, and `buildUp()`) sends each worker only the
        colonies' queens (with their fathers) and a slimmed copy of
        `SimParamBee` without the pedigree, recombination, and caste
        records; the records of the new individuals are added to
        `SimParamBee` in the main R session
    -   `swarm()` and `supersede()` create the virgin queens on the
        workers and return only the one that prevails in each colony
        (a Colony uses the same code, without parallel workers)
    -   `swarm()` and `split()` pull the workers in the main R session and
        no longer send colonies to workers

    Run times with 1000 colonies (16 chromosomes, 100 workers per colony),
    sequential / `plan(multisession, workers = 4)`, in seconds:

    | function                     | before      | now        |
    |------------------------------|-------------|------------|
    | `buildUp()`                  | 68.7 / 31.8 | 22.6 / 7.0 |
    | `addVirginQueens(nInd = 10)` | 57.0 / 23.9 | 7.1 / 2.2  |
    | `swarm()`                    | 36.0 / 35.3 | 11.3 / 6.7 |
    | `split()`                    | 16.3 / 21.7 | 2.5 / 2.4  |
    | `supersede()`                | 27.3 / 14.8 | 7.3 / 2.8  |
    | `removeWorkers(p = 0.3)`     |             | 1.5 / 2.2  |

    Parallel futures pay off when there is a lot of work per colony (e.g.,
    creating many workers or virgin queens per colony), but not for every
    function or simulation size (e.g., removing or pulling individuals is
    faster sequentially), so we recommend timing a few steps of your
    simulation with `plan(sequential)` and `plan(multisession)`.

-   MultiColony functions now use `future.apply::future_mapply()` instead of
    `future_lapply()` to send colonies to parallel workers, which halved the
    time of `buildUp()`, `swarm()`, and `supersede()` with
    `plan(multisession)` and reduced the time of `removeCastePop()` and
    `pullCastePop()` from about 10 to 2 seconds for 1000 colonies. Seeded
    results are unchanged.

-   seeded results of `swarm()`, `split()`, and `supersede()` on a
    MultiColony differ from previous versions, because random numbers are
    now drawn in a different order (virgin queens are created and selected
    on the parallel workers, and workers are pulled in the main R session);
    `removeCastePop(p = 1)` on a MultiColony no longer consumes random
    numbers. Sequential and parallel futures still give the same results.
    Seeded results of `swarm()` and `supersede()` on a Colony also differ,
    since the virgin queens are created from the queen's fathers in a random
    order (as for a MultiColony).

## Bug fixes

-   with `plan(sequential)`, `createCastePop()` on a MultiColony no longer
    duplicates caste records in `SimParamBee` and `createMultiColony()` no
    longer advances the colony ID counter twice; sequential and parallel
    futures now give the same seeded results

-   `supersede()` on a MultiColony no longer generates random numbers in
    parallel workers without a declared parallel-safe seed

-   `cross()` and `buildUp()` now pass `simParamBee` to all internal calls
    instead of relying on a global `SP` object

-   `swarm()` and `supersede()` now use `SP$nVirginQueens` when it is a
    function (e.g., `nVirginQueensPoisson()`), with one value per colony
    for a MultiColony; previously `swarm()` failed and `supersede()` used
    the number of virgin queens already in the colony instead

-   `swarm()` now samples the swarm location within `radius` of the parent
    colony; previously, for a Colony, the location was sampled around the
    parent's x coordinate only, and for a MultiColony the sampled locations
    were not set on the swarms

-   `createCrossPlan()` with a single drone producing colony now samples
    only that colony; previously it sampled from colony IDs 1 to that
    colony's ID

-   `createCastePop()` on a MultiColony, and `swarm()` and `supersede()`
    on a Colony or MultiColony, now record all created virgin queens,
    including csd homozygous ones that are removed, as virgin queens in
    `SimParamBee` (previously they were recorded as workers with
    `plan(sequential)` or on a Colony)

-   recombination records (`SP$recHist`) of individuals created on a
    MultiColony with parallel futures are now named by their IDs
    (previously, e.g., `"1.341"` instead of `"341"`)


# SIMplyBee version 0.4.1

-   2024-09-19

## Bug fixes

-   locations of the colonies in the D_Crossing vignettes were previously
sampled by random. This caused that on some runs some queens were left unmated,
which caused an error. We now read in the locations from a csv file.


# SIMplyBee version 0.4.0

-   2024-08-23

## Major changes

-   createColony() and createMultiColony() do not have the location
    argument anymore; use setLocation() instead; by default, location is
    now c(0, 0) PR#500


## New features
-   In setLocation(MultiColony) we can set one location (numeric) or
    multiple (list or data.frame) PR#500
-   getLocation(MultiColony) got the collapse argument
    Commit#f4e629c3e8920948ad576eae3615a86b26300790

-   We can now sample location of a swarm - see sampleLocation and
    radius arguments in swarm() PR#500 PR#502

-   New function rcircle() to sample a random point within a circle with
    a given radius PR#502

-   Removed the function createRandomCrossPlan() - that option is now
    included in the newly added createCrossPlan() fuction

-   Added the **functionality for spatially-aware mating** of honeybee
    colonies. The new functionality is included in the
    createCrossPlan(), that allows to create the cross plan according to
    the colonies' locations, and in the cross() function, that crosses
    bees according to their location. This did not break the old
    functionality of the cross function.
    
-  Unified setMisc and getMisc behaviour to comply with the new AlphaSimR behaviour

-  Added new C++ function isHeterozygous() to speed up the SIMplyBee function isCsdHeterozygous() 

## Bug fixes

-   Bug fix - get\*Haplo() functions were returning diploid drones when
    input was a Pop-class

# SIMplyBee version 0.3.0

-   2022-12-05 First public/CRAN version of the package

## Major changes

-   \~2 years of work went into this first public version!

## New features

-   Many;) See the vignettes and help pages.

## Bug fixes

-   Many bugs and issues squashed - see
    <https://github.com/HighlanderLab/SIMplyBee/issues>. Please
    contribute to the development of this package.
