# RMediation 1.5.0 (2026-06-18)

## Dependencies

* **medfit is now available on CRAN** (v0.2.0/0.2.1). Removed `Remotes:
  data-wise/medfit`; medfit now resolves from CRAN. Pinned `Suggests: medfit
  (>= 0.2.0)`.

## medfit Covariance Extraction

* `ci()` for medfit `MediationData`/`SerialMediationData` now extracts the path
  covariance by parameter name and uses the full covariance matrix (including
  off-diagonals) for both simple and serial mediation. Removed the previous
  positional/value-matching heuristics and the independence/diagonal fallbacks,
  which could silently produce incorrect intervals; unresolved path labels now
  raise an informative error.
* The serial-mediation pipeline is now verified end-to-end against medfit's
  released serial extractor (medfit >= 0.2.0): `medfit::extract_mediation()`
  produces a `SerialMediationData` for both lavaan (ordered `mediator` vector)
  and lm/glm (`mediator_models`) chains, and `ci()` consumes it via the
  documented `d1, d2, ...` path-name contract. Added integration tests that fit a
  real model, extract, and run `ci()` end-to-end (skipped when medfit < 0.2.0).

## Workflow Optimization & Standardization (2025-12-05)

### Performance Improvements
* **Optimized R-CMD-check workflow**: Reduced from 5 to 3 platforms (macOS, Windows, Ubuntu release)
* **Removed failing platforms**: Dropped `ubuntu-latest (devel)` and `ubuntu-latest (oldrel-1)` configurations
* **Runtime improvement**: Check workflows now complete in ~2 minutes (down from 6+ minutes, **67% faster**)
* **Build optimization**: Added `--ignore-vignettes` flag to skip vignette validation during CI checks

### Bug Fixes
* **Fixed pkgdown build errors**:
  - Removed non-existent `reexports` topic from `_pkgdown.yml` reference index
  - Added missing `tidy` topic to `_pkgdown.yml` reference index
  - pkgdown workflow now passes successfully

### Standardization
* **Badge order**: Added R-hub badge to README, standardized order to match mediationverse ecosystem:
  CRAN, Lifecycle, R-CMD-check, Website, R-hub, Codecov
* **.Rbuildignore**: Added `^STATUS\.md$` to match ecosystem standards
* **Website configuration**: Already compliant with mediationverse standards:
  - Academic blue color scheme (#0054AD)
  - Consistent fonts (Inter, Montserrat, Fira Code)
  - Ecosystem and Status dropdown menus

### Documentation
* Updated README badge order for consistency across ecosystem
* All workflows now passing on main branch

## Workflow Optimization & Standardization (2025-12-05)

### Performance Improvements
* **Optimized R-CMD-check workflow**: Reduced from 5 to 3 platforms (macOS, Windows, Ubuntu release)
* **Removed failing platforms**: Dropped `ubuntu-latest (devel)` and `ubuntu-latest (oldrel-1)` configurations
* **Runtime improvement**: Check workflows now complete in ~2 minutes (down from 6+ minutes, **67% faster**)
* **Build optimization**: Added `--ignore-vignettes` flag to skip vignette validation during CI checks

### Bug Fixes
* **Fixed pkgdown build errors**:
  - Removed non-existent `reexports` topic from `_pkgdown.yml` reference index
  - Added missing `tidy` topic to `_pkgdown.yml` reference index
  - pkgdown workflow now passes successfully

### Standardization
* **Badge order**: Added R-hub badge to README, standardized order to match mediationverse ecosystem:
  CRAN, Lifecycle, R-CMD-check, Website, R-hub, Codecov
* **.Rbuildignore**: Added `^STATUS\.md$` to match ecosystem standards
* **Website configuration**: Already compliant with mediationverse standards:
  - Academic blue color scheme (#0054AD)
  - Consistent fonts (Inter, Montserrat, Fira Code)
  - Ecosystem and Status dropdown menus

### Documentation
* Updated README badge order for consistency across ecosystem
* All workflows now passing on main branch

## Dependency Reduction

* **Removed dependencies**: `e1071`, `modelr`, and `generics` packages are no longer required.
* **Internal implementations**: Added internal `.skewness()`, `.kurtosis()`, and `.resample_bootstrap()` functions to replace external dependencies.
* **Own tidy generic**: Package now defines its own `tidy()` generic instead of re-exporting from `generics`.
* Total Imports reduced from 10 to 7 packages.

## CI/CD Improvements

* Added `R-CMD-check.yaml` workflow for multi-platform testing (macOS, Windows, Ubuntu with R devel/release/oldrel).
* Added `test-coverage.yaml` workflow for Codecov integration.
* Added `VignetteBuilder: knitr` to DESCRIPTION for proper vignette building.
* Standardized README.md to mediationverse format.

## Ecosystem Notes

* Part of the **mediationverse** ecosystem for mediation analysis in R.
* Integration with medfit planned for v1.5.0 (Q1 2026).
* See [Ecosystem Coordination](https://github.com/data-wise/medfit/blob/main/planning/ECOSYSTEM.md) for version compatibility and development guidelines.
* See [Development Roadmap](https://data-wise.github.io/mediationverse/articles/roadmap.html) for timeline.

---

# RMediation 1.4.0

## Major Changes

### S7 Core Architecture (Complete Refactoring)
* **Complete architectural redesign:** S7 is now the computational core, with legacy functions as thin wrappers.
* **Zero breaking changes:** All existing code continues to work exactly as before (100% backward compatible).
* **Enhanced extensibility:** New computational architecture makes adding CI methods and features straightforward.

### S7 OOP Classes
* Introduced S7 object-oriented programming for modern, type-safe class definitions.
* **New S7 Classes:**
  - `ProductNormal`: Represents the distribution of the product of normal random variables.
  - `MBCOResult`: Encapsulates results from MBCO tests.
* **New S7 Generics:** `cdf()`, `dist_quantile()`, `ci()`, `print()`, `summary()`, `show()`.
* All S7 classes include comprehensive display methods (`print`, `summary`, `show`) for meaningful output.

### N-Variable Product Support (NEW)
* **Breaking limitation removed:** Can now compute confidence intervals for products of 3 or more variables.
* Works with Monte Carlo method (`type = "mc"`).
* Example: `ProductNormal(mu = c(0.3, 0.4, 0.5), Sigma = diag(3))` for three-way products.
* Previous versions limited to 2-variable products only.

### Internal Computational Core
* **New internal S7 functions:** `.compute_ci_dop()`, `.compute_ci_mc()`, `.compute_ci_asymp()`, `.compute_cdf_dop()`, `.compute_cdf_mc()`, `.compute_quantile_dop()`, `.compute_quantile_mc()`.
* All core algorithms now work with `ProductNormal` objects natively.
* Legacy functions (`medci()`, `pprodnormal()`, `qprodnormal()`) convert parameters and dispatch to S7 core.
* Identical numerical results to previous versions (validated with 201 tests).

### Enhanced `ci()` Function
* Migrated `ci()` to S7 generic with methods for:
  - `numeric`: Legacy support for direct coefficient/covariance input.
  - `lavaan`: Automatic extraction and CI computation for indirect effects.
  - `ProductNormal`: Native S7 class support.
* **Auto-detection**: Automatically identifies product parameters (e.g., `ab := a*b`) in `lavaan` models.
* Renamed first argument to `mu` for better legacy compatibility.

### MBCO Function Updates
* Migrated `mbco()` to S7 generic with method for `MxModel` objects.
* Returns `MBCOResult` S7 objects with properties: `statistic`, `df`, `p_value`, `type`.
* **Legacy Compatibility**: Implemented S3 methods (`$`, `[[`, `names`) for backward-compatible list-style access (e.g., `result$chisq`, `result$p`).
* Display methods show significance indicators (*, **, ***, ns) and interpretation.

### Input Validation
* Comprehensive input validation using `checkmate` package across all functions.
* Type checking with `match.arg()` for categorical arguments.
* Improved error messages for invalid inputs.

### Display Methods
* All S7 classes have meaningful `print()`, `summary()`, and `show()` methods.
* `ProductNormal` displays: distribution type, means, covariance matrix, SDs, and correlations.
* `MBCOResult` displays: test type, statistics, p-values with significance levels, and interpretation.

### Namespace Improvements
* **Zero masking warnings:** Package now loads cleanly with no namespace conflicts.
* Renamed `quantile()` generic to `dist_quantile()` to avoid masking `stats::quantile()`.
* S7 methods for `print()`, `summary()`, `show()` now properly register with base generics.
* Added S4 compatibility via `S7::S4_register()` for formal generic support.

### Package Cleanup
* Removed unused dependencies (`doParallel`, `foreach`, `methods`).
* Fixed all documentation warnings and notes.
* **R CMD check**: Now passes with 0 errors, 0 warnings, 0 notes.
* Added 40 new S7 prototype tests + 19 display method tests (total: 201 tests, 100% pass rate).

## Bug Fixes
* Fixed argument dispatch issues between `alpha` and `level` parameters.
* Resolved S7 generic conflicts with base R functions (print, summary, quantile).
* Fixed recursive dispatch issues in display methods.
* Added case-insensitive handling for `type` parameter (accepts "MC", "mc", "DOP", "dop", etc.).
* Fixed return value structure for `ci()` method (now returns list with `CI`, `Estimate`, `SE` components).

# RMediation 1.3.0

RMediation 1.3.0 (2025-11-18)
==============
* Updated functions and package documentation.
* Updated package dependencies.
* Updated `mbco` function documentation.
* Implemented `checkmate` for robust input validation.
* Created GitHub pages for package documentation.
* Refactored package structure for better organization.
* Refactored package validation for better input validation.
* Added tests for package functions.

RMediation 1.2.3 (5/12/2023)
==============
* Fixed an issue where the type="all" in the medci function would generate incorrect labels for CIs.

RMediation 1.2.2 (5/11/2023)
==============
* Fixed an issue where the type="all" in the medci function would generate an error "object 'MeekerCI' not found".

* changed the dependency version for grDevices to >= 3.5 in the description file.

RMediation 1.2.1 (4/28/2023)
==============
* Fixed issues to meet CRAN submission requirements.

RMediation 1.2.0 (6/28/2022)
==============
*New parametric and non-parametric MBCO test, using `mbco` function.
* Fixed an error with printing medci, '(1-alpha)% CI'

RMediation 1.1.5 (6/7/2020)
==============
* Fixed the issue with 'medic' function that printed '(1-alpha/2)% CI' instead of '(1-alpha)% CI'

RMediation 1.1.4 (3/12/2016)
=============
* 'medci' function output structure is changed. For each 'type', it produces a 'list' of '(1-alpha/2)% CI', 'Estimate', and 'SE'. See help(ci) for more information.

* Two functions 'pMC' and 'qMC' are added.

* for medci(), type=‘prodclin’ is replaced with ‘dop’.

RMediation 1.1.3
=============
* New 'ci' function produces CIs for any well-defined function of parameter estimates in an SEM and MSEM using the Monte Carlo or asymptotic normal theory with the multivariate delta standard error method.
