---
editor_options:
  markdown:
    wrap: 72
---

# SIMplyBee version 0.5.0

- 2026-29-09

## Major changes

- `calcColonyValue()` now passes each Colony to the first argument of its
    callback by position. Callbacks may name that argument `colony`, `x`, or
    another name. This also applies to `SimParamBee$colonyValueFUN`.

- Colony IDs are now character, consistently with AlphaSimR individual IDs.
    `getId()` returns character IDs, including for empty collections and missing
    entries, so `apiary[getId(apiary)]` selects by ID/name and not by position!
    Colony IDs are independent of queen IDs and remain unchanged when queens
    are removed or replaced. Crossing plans now support non-numeric donor-colony IDs and
    correctly map reordered colony names to queen IDs.

- swarm/split/supersede do no longer store the year of the queen

- colonies with high inbreeding that do not produce a viable virgin
    queens in max(10, SP\$nVirginQueens) attempts are removed in
    swarm/supersede

- split no longer creates virgin queens in the split colonies but
    returns colonies with workers and meta data, but no virgin queens

- createMultiColony() no longer creates an empty apiary, but it adds
    empty colonies with IDs

- we no longer support creating an exact number of workers with the csd
    functionality turned on. Due to inbreeding at the csd locus, some workers
    are not viable. Previous funcionality re-ran the creation of workers until
    a required number was available. We no longer support this, so the number of
    returned workers is the number of viable workers.

- the 'cross' function now removes unsuccessfully mated queens.
    This can primarily happen when creating a spatial cross plan.
    If there are no drone producing colonies in the specified radius,
    the mating fails and the queens/colonies are removed from the object.
    This is to ensure all further functionality works well.

- cross plans with drone producing colonies now hold one colony ID per drone
    (a colony ID can repeat), and `cross()` uses them as given: each entry is
    one drone (father). `createCrossPlan()` returns `nDrones` colony IDs per
    virgin queen; hand-made plans must list a drone producing colony / drone father for every
    worker that will be created (for
    example, `c("5", "5")` for two drones from colony 5). Previously, a plan
    listed the available colonies and `cross()` sampled `nDrones` from them.
    We do recommend creating cross plans through `createCrossPlan()` function and
    not manually. 

## New features

- `createCrossPlan()` and `cross()` gain `nDPC` and `DPCAllocation` to limit
    the number of drone producing colonies available to each virgin queen,
    either a random or the nearest `nDPC` colonies for spatial cross plans (still in the 
    specified radius).


- added functionality to map individual-level variance to colony-level variance and vice-versa
  with functions `mapIndToColonyVar()` and `mapColonyToIndVar()` functions. This is neccesary
  to set the correct individual-level variance components that would result in the required
  colony-level variance components.

- Parallelised major colony operations with `future` and `future.apply`.
    New vignette describes how to use parallelisation. 

## Other

- Added a shared `inst/REFERENCES.bib` bibliography for package help and
    vignettes.

## Bug fixes

- `cross()` with drone producing colonies no longer mates queens with 0
    drones when all sampled drone producing colonies produce the same number
    of drones.

- `createCrossPlan` capped the spatial matches to 10 drone producing colonies. Now,
   it samples from all available drone producing colonies in the specified radius (
    unless specifying the `nDPC` parameter)

- `createCrossPlan()` with `drones` now assigns drones to the right virgin
    queens when IDs are not in sorted order, and uses `SimParamBee$nFathers`
    when `nDrones` is `NULL`.

- `getFathers()` with `collapse = TRUE` on a MultiColony no longer fails.


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

- `calcQueensPHomBrood()`, `pHomBrood()`, and `nHomBrood()` now warn and
    return `NA_real_` for queenless colonies, allowing `MultiColony` summaries
    to continue.

- `nFathers()` now lists all supported classes, including `Pop`, in its
    invalid-input error message and documents queen population input.

- Brood summaries now use numeric missing values and return `numeric(0)`
    for empty populations. Inactive csd metadata also uses numeric missing
    values. `getId(NULL)` now returns `NA_character_`.

- Fixed the inconsistent use of `simParamBee` in some functions.


# SIMplyBee version 0.4.1

- 2024-09-19

## Bug fixes

- locations of the colonies in the D_Crossing vignettes were previously
sampled by random. This caused that on some runs some queens were left unmated,
which caused an error. We now read in the locations from a csv file.

# SIMplyBee version 0.4.0

- 2024-08-23

## Major changes

- createColony() and createMultiColony() do not have the location
    argument anymore; use setLocation() instead; by default, location is
    now c(0, 0) PR#500

## New features

- In setLocation(MultiColony) we can set one location (numeric) or
    multiple (list or data.frame) PR#500
- getLocation(MultiColony) got the collapse argument
    Commit#f4e629c3e8920948ad576eae3615a86b26300790

- We can now sample location of a swarm - see sampleLocation and
    radius arguments in swarm() PR#500 PR#502

- New function rcircle() to sample a random point within a circle with
    a given radius PR#502

- Removed the function createRandomCrossPlan() - that option is now
    included in the newly added createCrossPlan() fuction

- Added the **functionality for spatially-aware mating** of honeybee
    colonies. The new functionality is included in the
    createCrossPlan(), that allows to create the cross plan according to
    the colonies' locations, and in the cross() function, that crosses
    bees according to their location. This did not break the old
    functionality of the cross function.

- Unified setMisc and getMisc behaviour to comply with the new AlphaSimR behaviour

- Added new C++ function isHeterozygous() to speed up the SIMplyBee function isCsdHeterozygous()

## Bug fixes

- Bug fix - get\*Haplo() functions were returning diploid drones when
    input was a Pop-class

# SIMplyBee version 0.3.0

- 2022-12-05 First public/CRAN version of the package

## Major changes

- \~2 years of work went into this first public version!

## New features

- Many;) See the vignettes and help pages.

## Bug fixes

- Many bugs and issues squashed - see
    <https://github.com/HighlanderLab/SIMplyBee/issues>. Please
    contribute to the development of this package.
