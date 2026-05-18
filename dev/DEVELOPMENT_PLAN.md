# fairenough — development plan

Goal: get `fairenough` to a CRAN-submittable state while shrinking the
maintenance surface and adding test coverage on the headline LLM feature.

Ordering: most → least important. Earlier phases unblock later ones.
Tick boxes as items land.

## Phase 1 — Foundation (do first, no logic changes)

- [x] **1.1 Remove `tests/palmerpenguins` git submodule.**
  `git submodule deinit -f tests/palmerpenguins && git rm -f tests/palmerpenguins && rm -rf .git/modules/tests/palmerpenguins`.
  Edit `.gitmodules` (delete entry; remove file if empty), `.gitignore`
  (`!tests/palmerpenguins`), `.Rbuildignore` (`tests/palmerpenguins`).
  The submodule's content reappears in Phase 5 as a vignette. *(15 min)*

- [x] **1.2 Drop `rv`.** Delete `rproject.toml`, `rv.lock`, and the `rv/`
  ignore entries from `.gitignore` / `.Rbuildignore`. Document the dev
  workflow as `devtools::install_deps(dependencies = TRUE)` in a
  `CONTRIBUTING.md` (or a section of `README.Rmd`). CRAN doesn't care
  about lockfiles. *(15 min)*

- [x] **1.3 Add `R-CMD-check` GitHub Actions workflow.**
  `usethis::use_github_action_check_standard()` (matrix of release/devel/
  oldrel × OS). Enable the badge in `README.Rmd`. **No further work makes
  sense without this** — it is the feedback loop for everything below.
  *(10 min + first green run)*

## Phase 2 — Correctness bugs (CRAN-blocking, user-affecting)

- [x] **2.1 Stop mutating global `options()`** in `get_base_path` /
  `get_raw_dir` (R/utils.R:21, 41, 57, 70). Replace with a
  package-internal env: `.fairenough_state <- new.env(parent = emptyenv())`,
  with getter/setter on it. CRAN explicitly forbids persistent session
  state mutation. *(1 hr)*

- [x] **2.2 Confirm-before-rename in `setup_package`** (R/setup_package.R:172).
  Currently silently `file.rename`s files matched by extension into
  `data_raw/`. Add a confirmation prompt and an abort when files would
  be overwritten. *(30 min)*

- [x] **2.3 Standardize error emission.** Replace plain `stop()` at
  `build_license.R:181, 186, 192` and `gendict.R:522` with
  `cli::cli_abort`. Replace soft-deprecated `usethis::ui_yeah` at
  `utils.R:287` with `cli::cli_inform` + a `prompt_confirm` call. *(20 min)*

- [x] **2.4 Sync `inst/CITATION` with `DESCRIPTION` authors.** Adriana
  is `aut, cre` in `DESCRIPTION` but missing from `inst/CITATION`.
  Decide whether `CITATION.cff` or `inst/CITATION` is the source of
  truth and regenerate the other (typically `CITATION.cff` →
  `cffr::cff_write_citation()` → `inst/CITATION`). Resolution:
  `DESCRIPTION` is the source of truth; `CITATION.cff` regenerated via
  `cffr::cff_write()` and `inst/CITATION` regenerated via
  `cffr::as_bibentry()` + `cffr::cff_write_citation()`. The pre-existing
  `inst/CITATION` had to be deleted before regen because cffr otherwise
  embeds its content as a `preferred-citation` block and round-trips
  the stale author list. *(15 min)*

- [x] **2.5 Fix `prompt_multi_select(allow_other = TRUE)` runtime bug**
  in `collect_metadata` (Zenodo communities prompt). `prompt_multi_select`
  does not accept `allow_other`. Either add the argument to its
  signature (and implement) or drop the call. Surfaced by R CMD check
  "checking R code for possible problems" NOTE. *(15 min)*

- [x] **2.6 Strip non-ASCII characters from `R/utils.R`.** Surfaced as a
  CRAN-blocking WARNING. Run `tools::showNonASCIIfile("R/utils.R")` to
  locate them; replace with ASCII equivalents or `\uxxxx` escapes per
  CRAN's portability rule. *(15 min)*

## Phase 3 — Public API surface (CRAN reviewers will ask)

- [x] **3.1 Mark internals `@keywords internal` and prune NAMESPACE
  from 44 → ~15 exports.** Demote: the four `prompt_*` helpers; the
  five field validators (`validate_url/email/orcid/date/package_name`);
  `ensure_directory`, `use_template`, `get_base_path`, `get_raw_dir`,
  `validate_data_frame`, `validate_file_path`, `filter_supported_files`,
  `is_supported_file_type`, `get_supported_extensions`. Make the
  `validate_*_completed` family consistent (export all 5 or none).
  Resolution: 18 functions demoted as listed. Family policy: exported
  all 5 (`validate_setup_completed` already exported, added `@export`
  to the other 4). Templates (`inst/templates/README.{Rmd,qmd}`)
  rewritten to use `here::here()` and `readr::read_csv()` so generated
  user packages no longer need fairenough as a render-time dependency
  (decoupling beyond what plan strictly required). Tests calling bare
  `is_supported_file_type()` / `filter_supported_files()` updated to
  use `fairenough:::` prefix. NAMESPACE: 44 → 29 exports (further
  prune of `*_package` / `*_data` aliases is left for a follow-up).
  *(2 hr — must run R CMD check after to catch external uses)*

- [x] **3.2 Add missing `@param` docs.** R CMD check WARNING flagged
  five undocumented args: `build()` `good_practice`; `build_readme()`
  `quarto`; `check_description_exists()` `base_path`; `gendict()`
  `verbose` and `...`; `validators` (the shared Rd) `x`. `build()`
  `good_practice` was a dead arg — dropped instead of documented; see
  6.8 for the orphan it surfaced. *(20 min)*

- [x] **3.3 Audit unused `Imports`.** R CMD check NOTE listed
  `fontawesome`, `gt`, `knitr`, `pkgdown`, `roxygen2`, `stringr` as
  declared in `Imports:` but never imported from. For each: confirm it
  is truly unused, then drop from `DESCRIPTION` or move to `Suggests`.
  `knitr` stays in `Suggests` after Phase 5 (CRAN convention for
  `VignetteBuilder` packages — vignettes run at build time, not user
  runtime). `rmarkdown` is already a real `Imports` (used by
  `build_readme()`'s Rmarkdown branch) and stays there. Resolution:
  dropped `roxygen2` (only metadata, transitively pulled by `devtools`);
  moved `fontawesome`, `gt`, `knitr`, `pkgdown`, `stringr` to
  `Suggests`. Templates' existing `requireNamespace()` loop already
  handles missing optional packages with a clear stop() message.
  *(30 min)*

## Phase 4 — Tests (must pass with no network / no API keys)

- [ ] **4.1 Replace empty `test-basic.R`** (currently `expect_true(TRUE)`).
  Delete or use for tiny smoke checks. *(5 min)*

- [ ] **4.2 Add a fake `chat` mock** + skip patterns. Pattern: a small
  stand-in object satisfying the `ellmer::chat` interface used in
  `gendict()`; `chat$chat()` returns canned strings. Add
  `testthat::skip_if(Sys.getenv("OPENAI_API_KEY") == "")` to any test
  that talks to a real provider. *(1 hr)*

- [ ] **4.3 Add `gendict` unit test** using the mock. Asserts the
  returned tibble shape, that variable names match input columns, and
  the `description` column is non-empty. *(1 hr)*

- [ ] **4.4 Add an end-to-end `fairenough()` test** on a tiny synthetic
  CSV in `withr::local_tempdir()`. No `chat` arg → tests the no-LLM path.
  Assert the resulting directory has `DESCRIPTION`, `R/`, `data/`,
  `inst/extdata/`. *(1.5 hr)*

## Phase 5 — Vignette: palmerpenguins demo

- [ ] **5.1 Create a vignette** demonstrating the full pipeline on
  penguins: `vignettes/palmerpenguins-demo.Rmd`. Walks through setup →
  process → collect → generate → build, ending with the resulting
  package layout. Use `eval = FALSE` for the `chat` step (or the mock
  from 4.2) so it does not need an API key in CI. **This replaces the
  role of the deleted submodule.** *(3 hr)*

- [ ] **5.2 Resolve the `VignetteBuilder` warning.** Once 5.1 lands,
  `DESCRIPTION`'s `VignetteBuilder: knitr` is justified. Update
  `inst/doc` handling in `.gitignore` if needed. *(15 min)*

- [ ] **5.3 Add `@examples` to public functions.** Minimum: `fairenough`,
  `setup`, `process`, `collect`, `generate`, `build`. Use `\dontrun{}`
  for anything filesystem-mutating. *(1 hr)*

## Phase 6 — Refactors (non-blocking; big readability wins)

- [ ] **6.1 Strip ~25 `cat("DEBUG: ...")` scaffolding lines** in
  `gendict()` (R/gendict.R:298–460). Quickest win in this phase, no
  behavior change. *(5 min)*

- [ ] **6.2 De-duplicate `build_roxygen` + `devtools::install`.**
  `wrappers.R::build()` calls `build_roxygen()` (R/wrappers.R:333) and
  then `build_package()` which calls it again (R/build_package.R:23).
  Same with install: R/build_package.R:76 + R/build_package.R:534. Pick
  a single owner per stage. *(30 min)*

- [ ] **6.3 Split `utils.R` (872 lines)** along its natural boundaries:
  `R/config.R` (path/option helpers), `R/io.R` (read_data, file-format
  predicates), `R/validation.R` (the ~550-line check_*/format_checklist/
  validate_*_completed framework). Pure file moves, no logic changes.
  *(1.5 hr)*

- [ ] **6.4 De-duplicate license-URL tables.** Three sources today:
  `LICENSE_CONFIG` in `build_license.R`, inline tables at
  `collect_metadata.R:259` and `:757`. Make `LICENSE_CONFIG` the single
  source. *(30 min)*

- [ ] **6.5 De-duplicate `build_readme()` qmd/Rmd branches**
  (R/build_package.R:348–377 vs 378–410). Extract
  `.render_readme(template, format)`. *(30 min)*

- [ ] **6.6 Decompose `collect_metadata()`** (R/collect_metadata.R:36,
  449 lines). Extract `.collect_package_info`, `.collect_authors`,
  `.collect_license`, `.collect_publication`, `.collect_coverage`,
  `.collect_related`. Top-level becomes ~50 lines of orchestration.
  *(3 hr)*

- [ ] **6.7 Decompose `save_metadata()`** (R/collect_metadata.R:498,
  192 lines). Split out the `Config/*` JSON-encoded fields writer into
  `.save_config_fields`. *(1 hr)*

- [ ] **6.8 Resolve orphaned `.validate_package()`** (R/build_package.R:103).
  Surfaced during 3.2: `.validate_package()` accepts and uses
  `good_practice` (calls `goodpractice::gp()`) but has no callers
  anywhere in the package. Decide: (a) wire it up — add `good_practice`
  to `build_package()`'s signature and have it call `.validate_package`,
  then forward from `build()`; or (b) delete `.validate_package()` and
  its Rd. Either way, document the decision in commit. Plan item 7.4
  (run `goodpractice::gp()` on `fairenough` itself) is a *developer*
  check and doesn't depend on this. *(30 min)*

## Phase 7 — Pre-CRAN polish

- [ ] **7.1 Clean up `.Rbuildignore`.** `^README\.Rmd$` is duplicated
  (lines 4 and 19); mixed escaping conventions. Rewrite as a single
  consistent block. *(10 min)*

- [ ] **7.2 Add `cran-comments.md` and `NEWS.md`.** Standard CRAN
  submission artifacts. *(20 min)*

- [ ] **7.3 Run `R CMD check --as-cran` clean.** Zero errors, zero
  warnings. Notes only with explicit justification. *(unbounded —
  depends what surfaces)*

- [ ] **7.4 Run `goodpractice::gp()` on `fairenough` itself.** It is in
  Imports — eat the dog food. Address what it surfaces. *(1 hr)*

- [ ] **7.5 Update `README.Rmd` badges.** Replace the placeholder block
  with real R-CMD-check / lifecycle / CRAN-status badges. *(15 min)*

- [x] **7.6 Re-tighten R-CMD-check `error-on`.** Phase 1.3 set
  `error-on: '"error"'` in `.github/workflows/R-CMD-check.yaml` so the
  workflow could merge while Phase 2/3 cleanups landed. Once
  `devtools::check()` is 0W/0N, delete the line (default `"warning"`)
  or set `error-on: '"note"'` for CRAN-strict gating. *(2 min)*

## Effort summary

~26–32 hours of focused work across the seven phases. Phase 1 is ~40
minutes and unblocks everything below.

A baseline `devtools::check()` after Phase 1 returned 0 errors,
3 warnings, 3 notes — items 2.5, 2.6, 3.2, 3.3 address these directly.

## Workflow notes

- Work happens on the `dev` branch; PRs into `main` per the existing
  pattern (the merge-from-dev history is visible in `git log main`).
- Each phase fits one PR; large items inside a phase (3.1, 6.6) can be
  their own PR.
- Tick boxes here as items land; commit the tick alongside the work so
  the diff is self-documenting.
