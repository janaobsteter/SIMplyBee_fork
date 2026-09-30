# AGENTS.md

## Scope

These notes apply to how we manage this git repository of the `SIMplyBee` package.
SIMplyBee aims to provide an easy to use simulation platform
to simulate honeybee breeding programmes by building upon the AlphaSimR packages.
So, we prioritise aligning the SIMplyBee R API with the underlying AlphaSimR R API
where this is appropriate, and deviate or level-up where we need a honeybee specific approach.

## The way of working

* We strive for planned work so we have a plan of what we want to change.
* We strive for minimal changes, unless needed otherwise.
* We provide clear examples for new functionality so useRs can be quickly
  onboarded.
* We add or update tests for every behavior change.
* We run R CMD check for every code change.
* We keep local quality gates green before handoff.
* We update `NEWS.md` for user-visible behavior or API changes.

## Permissions and authorization

Standing authorization for this repository:

* All commands explicitly shown in this document are pre-authorized for
  repository work (including inline commands and code-block commands,
  with task-specific substitutions where placeholders are shown).
* Agents should run these documented commands directly without asking for
  extra confirmation in chat.
* If sandboxing blocks an allowed command, agents should submit the required
  unsandboxed/escalated tool request directly with a brief justification.
* Agents should ask in chat only if a required escalation is denied or a
  platform policy still blocks execution after escalation.
* Agents may use `curl` (or equivalent read-only HTTP tools) for repository
  and upstream references on:
  `github.com`, `api.github.com`, `raw.githubusercontent.com`,
  `github.com/HighlanderLab/SIMplyBee`, `SIMplyBee.info`,
  `cran.r-project.org`, `cranchecks.info`, `badges.cranchecks.info`,
  `r-pkg.org`, `cranlogs.r-pkg.org`, `img.shields.io`,
  `codecov.io`, `app.codecov.io`, and `highlanderlab.r-universe.dev`
  (including issues, pull requests, comments, events, metadata, and docs).
* For the above domains, agents should execute `curl` directly without asking
  for extra confirmation in chat; if sandboxing requires escalation, submit the
  escalated tool request immediately and continue.
* To minimise repeated platform permission prompts, prefer these canonical
  command forms (same flags and flag order):

```sh
curl -sS --max-time 15 <url>
curl -I -sS --max-time 15 <url>
```

* This standing authorization does not override explicit user instructions or
  allow destructive commands that were not requested by the user.

## Definition of done

A task is done when all applicable items below are completed:

* Added/updated user-facing examples and tests for new functionality.
* Run applicable formatting/lint checks described below and `git diff --check`.
* `Rscript -e "devtools::test()"`
  for package tests.
* `Rscript -e "devtools::check()"`
  for full package checks.
* Updated `NEWS.md` for user-visible changes.

### Task-class quality gates

* For docs/config-only changes (for example `AGENTS.md`, README text, `NEWS.md`
  text-only edits, comments, formatting-only), `devtools::test()` and
  `devtools::check()` are not required unless package behavior is affected.
* For behavior-changing R/C++ work, run focused tests first (for example
  `devtools::test(filter = '...')`), then run full package checks before
  handoff (`devtools::test()` and `devtools::check()`, plus applicable lint checks).
* If full checks are intentionally skipped, explicitly report what was skipped,
  why, and which focused checks were run.

## Worktree and file hygiene

Default for non-trivial code tasks is to use a dedicated git worktree:

```sh
git fetch origin --prune
git worktree add ../SIMplyBee_wt_<task> -b <branch-name> origin/<base-branch>
```

Choose `<base-branch>` from the task or PR target. The README distinguishes
`main` (pre-CRAN stable work) from `devel` (development); do not assume `main`
is always the correct base. Reuse an existing task worktree when appropriate.

Within this workflow, follow these rules:

* In the shared root worktree, limit edits to small docs/meta/config work.
* For behavior-changing R/C++ tasks, use a dedicated worktree by default.
* A branch can be checked out in only one worktree at a time.
* If the shared root is dirty or conflicting edits appear, stop and move work
  into a dedicated worktree.
* In a dedicated worktree, edit only task-related files and do not revert or
  overwrite unrelated edits.
* If syncing an existing branch before substantial work, run:

```sh
git fetch origin --prune
git rebase origin/<base-branch>
```

* Inspect staged and unstaged changes first. Preserve existing work; do not
  stash, rebase, or change branches in a shared dirty checkout as routine setup.
* If conflicts occur, preserve local edits, resolve conflicts carefully, and
  report conflicted files plus the chosen resolution.
* By default, edit files freely but do not run `git add`, `git commit`,
  or `git push` unless explicitly requested.
* If a command leaves files staged unintentionally, report that in handoff.

Remove a task worktree only once its work is preserved and it is clean; never
force removal to discard uncommitted work. After merge/finish:

```sh
git worktree remove ../SIMplyBee_wt_<task>
git worktree prune
```

## Issue triage workflow

For issue exploration, closure recommendations, or "what is left to do?" tasks:

* Read issue body, comments, and events/timeline.
* Check linked commits/PRs and map issue checklist items to current files/tests.
* Report what is done, what is missing, and a concrete recommendation
  (close, keep open, or split follow-up issue).
* Include direct references to supporting files/tests/commits.

## Quality toolchain

The checkout has `air.toml` and `jarl.toml`.

Use Air and Jarl when installed, scoped to changed R files, for example:

```sh
air format R/Functions_L0_auxilary.R
jarl check R/Functions_L0_auxilary.R
git diff --check
```

Avoid repository-wide formatting churn. Check tool help before using unfamiliar
flags. Report unavailable checks rather than silently installing or upgrading
tools. If a tool is missing from `PATH`, also check user-local bin directories:

```sh
which <tool> || PATH="$HOME/.local/bin:$HOME/bin:$PATH" which <tool>
```

### Coverage with covr

Use `covr` for test-coverage checks on behavior-changing work:

```sh
Rscript -e "cov <- covr::package_coverage(clean = TRUE); print(cov)"
```

### GitHub Actions (CI)

CI runs on push and pull request and acts as the remote quality gate:

* `.github/workflows/R-CMD-check.yaml`: multi-platform R CMD check matrix.
* `.github/workflows/test-coverage.yaml`: `covr` coverage run and Codecov upload.
* `.github/workflows/document.yaml`: regenerates roxygen documentation on R-source
  pushes and can commit the generated changes back to the branch.
* `.github/workflows/pkgdown.yaml`: builds the website and deploys on non-PR runs.

Check each workflow's triggers before assuming it runs for a particular branch.
Regenerate documentation locally when needed and inspect its diff before handoff.

Local work should pass local checks before relying on CI feedback.

## Generated files and source-of-truth rules

Do not edit generated files by hand:

* `R/RcppExports.R`
* `src/RcppExports.cpp`
* `NAMESPACE`
* Files in `man/` generated from roxygen comments
* Generated website output in `docs/`; edit its sources in `R/`, `vignettes/`,
  `README.md`, and `_pkgdown.yml` instead

Regenerate as needed:

```sh
Rscript -e "Rcpp::compileAttributes()"
Rscript -e "devtools::document()"
```

## R CMD Check

### Preferred way to R CMD Check

Run faster package checks from the package directory:

```sh
Rscript -e "devtools::check(vignette = FALSE)"
```
Run this for every code change so changes are evaluated
in the same package context (build, docs, and tests).

Run slower package checks from the package directory:

```sh
Rscript -e "devtools::check()"
```

### Check environment and reporting

A fast check with `vignette = FALSE` is an intermediate check, not a replacement
for the full check when vignettes or package behavior change. Vignettes use
knitr/rmarkdown; CI sets up Pandoc. Check R, compiler, Pandoc, and LaTeX
availability as relevant to the actual failure. Do not assume Quarto is required
or hard-code an IDE installation path.

If compiler detection fails inside the sandbox, inspect the error first; it
may be a sandbox restriction rather than a missing compiler. Request escalated
execution when needed under the standing authorization above.

At handoff, record commands run, errors/warnings/notes, and any skipped checks
with reasons. Compare failures with the unchanged base when needed to distinguish
existing problems from regressions. Never claim a check passes based on an older
run or assume a fixed expected result for this machine.

## Testing

We strive for very good testing with `testthat`.

- Add or update `testthat` tests for every behavior change.
- Prefer focused regression tests for bug fixes.
- Keep tests runnable via package tests and checks.
- Keep stochastic tests reproducible with explicit seeds and small populations.
  Set `SP$nThreads <- 1L` where appropriate; pass `simParamBee = SP` explicitly
  to avoid dependence on global state. Restore any global options or parallel
  plans changed by a test.
- Prefer independently derived expectations, biological invariants, and small
  enumerated cases over tests that repeat the implementation. For simulation
  estimates, use tolerances justified by sampling error.
- Cover relevant haplodiploid, caste, empty-colony, and single/multiple-colony
  cases when changing population or colony operations.
- Guard genuinely optional external dependencies with explicit skips; do not
  hide failures in required package dependencies such as AlphaSimR.

For testing use:

- `Rscript -e "devtools::test()"`
  for package tests, or a focused run such as
  `Rscript -e "devtools::test(filter = 'L2_colony_functions')"`.

Tests are also run as part of R CMD check.

## Research, examples, and references

* Keep development plans and exploratory or standalone validation scripts in
  `dev/`; consult `dev/README.md` and the relevant topic plan before continuing
  existing research. Update that plan when completing planned work.
* Put automated regression tests in `tests/testthat/` and user tutorials in
  `vignettes/`. Keep expensive simulation experiments out of routine tests.
* Check `.Rbuildignore` before assuming a vignette or development script runs
  in package checks; validate excluded material explicitly when changing it.
* Keep scientific assumptions explicit, especially individual versus colony
  quantities, sum versus mean worker effects, mating/worker allocation,
  relatedness, and covariance conventions. Keep equations, code, and examples
  consistent and cite sources for model changes.
* Use `inst/REFERENCES.bib` for package citations and `literature/README.md` for
  the local literature archive. Verify references rather than inventing them.

## Proofreading

If asked to proofread, act as an expert proofreader and editor
with a deep understanding of clear, engaging, and well-structured writing.
Work paragraph by paragraph,
always starting by making a TODO list
that includes individual items for each heading.
Fix spelling, grammar, and other minor problems without asking. Label any unclear, confusing, or ambiguous sentences with
a TODO comment.
Only report what you have changed.
