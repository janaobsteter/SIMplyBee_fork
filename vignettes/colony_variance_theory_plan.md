# Colony Variance Theory Plan

This note records the current plan for issue
[#49](https://github.com/HighlanderLab/SIMplyBee/issues/49) and the two draft
vignettes `F2_Variance_calculations.Rmd` and
`F2_Variance_calculations_functions.Rmd`.

The goal is to turn published colony-level variance components into
individual-level queen and worker variance components that can be passed to
SIMplyBee/AlphaSimR. The forward direction is identifiable once the colony value
mapping and family structure are fixed. The reverse direction is usually
underdetermined, so the function must expose the missing assumptions rather than
pretend there is a unique solution.

## Sources Studied

- GitHub issue #49, including the 2025-12-10 meeting note on six individual
  variance parameters and the underdetermined reverse problem.
- The linked historical commit `e135438f97c48ad7deabf10641a4c6f4a3f19fe8`,
  which added the first `Colony_variance_calculus.Rmd` draft.
- `vignettes/F2_Variance_calculations.Rmd`, the derivation-heavy draft.
- `vignettes/F2_Variance_calculations_functions.Rmd`, the first function sketch.
- `vignettes/F_Quantitative_Genetics.Rmd`, which defines the default
  individual-to-colony value mapping used in examples.
- `R/Class-SimParamBee.R` and `R/Functions_L0_auxilary.R`, especially
  `mapCasteToColonyValue()`, `calcColonyGv()`, and `calcColonyPheno()`.

## Model To Standardise

For one colony trait, the current default mapping is

```text
C = Q + W
```

where `Q` is the queen contribution and `W` is the aggregated worker
contribution. By default SIMplyBee uses `W = sum_i W_i`, but the theory should
also support `W = mean_i W_i` because some published worker-group estimates are
closer to mean worker effects than total worker production.

For any aggregate with worker weights `a_i`,

```text
W = sum_i a_i W_i
```

where `a_i = 1` for a worker sum and `a_i = 1 / n_w` for a worker mean.

The core genetic variance equation is

```text
Var(G_C) =
  sigma_gq^2
  + K_w sigma_gw^2
  + L_qw sigma_gq,gw
```

with

```text
K_w = sum_i a_i^2 + sum_{i != j} a_i a_j r_ij
L_qw = sum_i a_i
```

Here `r_ij` is the additive genetic relationship, or the corresponding
covariance multiplier, between workers `i` and `j`. For the default sum mapping,
`K_w = n_w + n_w (n_w - 1) rbar_w` and `L_qw = n_w`. For the mean mapping,
`K_w = (1 / n_w) * (1 + (n_w - 1) rbar_w)` and `L_qw = 1`.

## Theory Patches

1. Use distinct-worker pair counts.

   The current vignettes often use `n_w^2 / n_f` pair counts and then also add
   the individual worker variance term. That double-counts the diagonal worker
   terms. The clean formulation is:

   ```text
   Var(sum_i W_i) = n_w sigma_w^2
                    + n_w (n_w - 1) rbar_w sigma_w^2
   ```

   This is clearer and avoids small but systematic over-counting.

2. Standardise relationship classes.

   Use these default genetic relationship multipliers unless a source requires
   different assumptions:

   ```text
   same father / supersisters: 0.75
   same DPQ, different father: 0.50
   different DPQ / half-sisters through the colony queen only: 0.25
   unrelated workers: 0
   ```

   Then compute `rbar_w` from probabilities or counts. For balanced groups with
   `n_w` workers, `n_f` fathers, and `n_DPQ` drone-producing queens:

   ```text
   n_SS = n_w^2 / n_f - n_w
   n_FS = n_w^2 / n_DPQ - n_w^2 / n_f
   n_HS = n_w (n_w - 1) - n_SS - n_FS

   rbar_w = (0.75 n_SS + 0.50 n_FS + 0.25 n_HS) /
            (n_w (n_w - 1))
   ```

   If `n_DPQ` is unavailable, start with the simpler father-only model:
   same-father pairs have relationship 0.75 and different-father pairs have
   relationship 0.25.

3. Keep genetic and environmental aggregation separate.

   The phenotypic derivation in the draft currently mixes `covP` and genetic
   relationship terms. Under the current SIMplyBee mapping, environmental
   deviations are sampled on individuals. The environmental correlation between
   queen and worker traits applies within an individual, but the colony phenotype
   uses the queen's queen-trait phenotype and the workers' worker-trait
   phenotypes from different individuals. Therefore the default colony
   environmental variance should be:

   ```text
   Var(E_C) = sigma_eq^2 + K_e sigma_ew^2
   ```

   where `K_e = sum_i a_i^2`, because distinct individual environmental
   deviations are independent by default. For a sum, `K_e = n_w`; for a mean,
   `K_e = 1 / n_w`.

   Do not include worker-worker environmental covariance or queen-worker
   environmental covariance unless we add an explicit common-colony environment
   model.

4. Treat `corE` carefully in examples.

   `corE` is still relevant for phenotypes of different traits measured on the
   same individual, but it is not automatically a covariance between a queen's
   environment and her workers' environments in the current colony value mapping.
   The F2 vignettes should say this explicitly.

5. Define reported colony covariance terms precisely.

   If literature reports a queen-worker colony covariance as
   `Cov(G_Q, G_W)`, then for the sum mapping:

   ```text
   Cov(G_Q, G_W) = 0.5 n_w sigma_gq,gw
   ```

   and the contribution to `Var(G_C)` is `2 Cov(G_Q, G_W)`.

   If literature reports the full variance contribution `2 Cov(G_Q, G_W)`, then
   the inverse scaling differs by a factor of two. The function API should force
   this distinction.

## Reverse Mapping Policy

The reverse function should not return a single "true" individual parameter set
unless enough constraints are supplied.

### Case A: Literature Reports Colony Genetic Components

Inputs:

```text
V_gQ_colony = Var(G_Q)
V_gW_colony = Var(G_W)
C_gQW_colony = Cov(G_Q, G_W)
n_w, n_f, n_DPQ or rbar_w
workersFUN = "sum" or "mean"
```

Then the genetic inverse is identifiable:

```text
sigma_gq^2 = V_gQ_colony
sigma_gw^2 = V_gW_colony / K_w
sigma_gq,gw = C_gQW_colony / (0.5 L_qw)
```

Validate the resulting genetic covariance matrix is positive semidefinite:

```text
abs(sigma_gq,gw) <= sqrt(sigma_gq^2 sigma_gw^2)
```

### Case B: Literature Reports Only Total Colony Genetic Variance

Input:

```text
V_gC_colony = Var(G_C)
```

This is one equation with three genetic unknowns. Require user assumptions, for
example:

```text
queen_share_g = target share of V_gC assigned to queen contribution
worker_share_g = target share assigned to worker aggregate contribution
cor_gq_gw = individual queen-worker genetic correlation
```

or accept fixed values for any two of `sigma_gq^2`, `sigma_gw^2`, and
`sigma_gq,gw`, then solve the third.

### Case C: Literature Reports Colony Environmental Variance

Under the current SIMplyBee mapping, use a default split between queen and
worker aggregate environmental variance:

```text
V_eC_colony = sigma_eq^2 + K_e sigma_ew^2
```

A reasonable API is:

```text
env_share_queen = p
env_share_worker = 1 - p

sigma_eq^2 = p V_eC_colony
sigma_ew^2 = (1 - p) V_eC_colony / K_e
sigma_eq,ew = 0
```

Default proposal: `p = 0.5`, with a warning that this is an assumption. Also
allow `p` to be user supplied.

Do not implement the three-way environmental split from the issue note as the
default unless we also implement an explicit common-colony environmental
covariance model. Otherwise we would estimate a parameter that SIMplyBee's
current phenotype mapping does not use.

### Case D: User Wants Environmental Covariance

Support this only as an optional future extension:

```text
Var(E_C) = sigma_eq^2 + K_e sigma_ew^2 + L_e sigma_eq,ew
```

This needs a clear biological interpretation, for example shared apiary or
colony environment, and an implementation path for simulating that common
environment. It should not be conflated with AlphaSimR's within-individual
`corE`.

## Proposed Functions

1. `calcWorkerRelatedness()`

   Purpose: calculate `rbar_w` from either explicit worker pedigree/father
   groups or summary assumptions.

   Initial arguments:

   ```text
   nWorkers
   nFathers = NULL
   nDPQ = NULL
   relationship = c(super = 0.75, full = 0.50, half = 0.25)
   exact = TRUE
   ```

2. `mapIndividualToColonyVar()`

   Purpose: forward-map individual variances to expected colony variances.

   Initial arguments:

   ```text
   varGQueen, varGWorker, covGQueenWorker
   varEQueen = 0, varEWorker = 0, covEQueenWorker = 0
   nWorkers
   rbarWorkers = NULL
   nFathers = NULL
   nDPQ = NULL
   workersFUN = c("sum", "mean")
   envModel = c("independent", "common")
   ```

   Return a named list or data frame with:

   ```text
   varGColony
   varEColony
   varPColony
   queenGContribution
   workerGContribution
   queenWorkerGContribution
   queenEContribution
   workerEContribution
   queenWorkerEContribution
   K_w, K_e, L_qw, rbarWorkers
   ```

3. `mapColonyToIndividualVar()`

   Purpose: reverse-map reported colony variances into individual simulation
   parameters, with explicit assumptions.

   Initial modes:

   ```text
   geneticMode = c("components", "shares", "fixed")
   envMode = c("share", "fixed", "none")
   ```

   The function should return both estimates and an `assumptions` field. It
   should error when the system is underdetermined and no policy is supplied.

## Implementation Order

1. Polish theory in the two F2 vignettes:
   replace pair-count equations with `K_w`, `K_e`, and `L_qw`; correct the
   phenotypic/environmental section; define `sum` versus `mean` worker mapping.

2. Add a small internal helper for worker relationship multipliers and test it
   with simple known cases:
   one father, many fathers without DPQ, and fathers nested within DPQs.

3. Add the forward function and tests comparing expected variance to simulated
   variance under large `nColonies`, fixed `nWorkers`, fixed `nFathers`, and
   simple additive traits.

4. Add the reverse function in conservative modes:
   `geneticMode = "components"` and `envMode = "share"` first. Add the more
   flexible underdetermined policies only after the component mode is tested.

5. Update `F_Quantitative_Genetics.Rmd` examples to call the helper rather than
   hand-scaling worker variances by `SP$nWorkers` without explaining the
   relationship multiplier.

6. Add `NEWS.md` after user-facing functions are exported.

## Open Decisions

- Naming: decide whether these should be exported user-facing functions or
  initially internal helpers used in vignettes.
- Default worker mapping: keep `workersFUN = "sum"` aligned with
  `mapCasteToColonyValue()`, but document when `mean` better matches published
  worker-group estimates.
- Literature convention: for each target paper, record whether reported
  queen-worker covariance is `Cov(Q, W)` or `2 Cov(Q, W)`.
- DPQ availability: decide whether `nDPQ` should be required for the full model
  or optional with a father-only fallback.
- Common environment: defer unless there is a concrete simulation design for
  shared colony/apiary environmental deviations.

## Recommended Starting Point

Start with the forward mapping and the component-based inverse. That gives a
defensible, testable path:

```text
published colony components -> individual genetic variances
published colony residual variance + explicit split -> individual environmental variances
```

For papers that report only total colony genetic or phenotypic variance, the
function should require shares, fixed parameters, or correlations supplied by
the user. This is the honest way to handle the underdetermined system and avoids
embedding arbitrary assumptions as hidden defaults.
