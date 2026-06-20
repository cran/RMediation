# RMediation <img src="man/figures/logo.png" align="right" height="139" alt="RMediation website" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/RMediation)](https://CRAN.R-project.org/package=RMediation)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/data-wise/rmediation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/data-wise/rmediation/actions/workflows/R-CMD-check.yaml)
[![Website](https://github.com/data-wise/rmediation/actions/workflows/pkgdown.yaml/badge.svg)](https://data-wise.github.io/rmediation/)
[![R-hub](https://github.com/data-wise/rmediation/actions/workflows/rhub.yaml/badge.svg)](https://github.com/data-wise/rmediation/actions/workflows/rhub.yaml)
[![Codecov](https://codecov.io/gh/data-wise/rmediation/graph/badge.svg)](https://app.codecov.io/gh/data-wise/rmediation)
[![r-universe](https://data-wise.r-universe.dev/badges/RMediation)](https://data-wise.r-universe.dev/RMediation)
<!-- badges: end -->

## Overview

**RMediation** computes confidence intervals for indirect effects in mediation analysis. It implements statistically rigorous methods that account for the non-normal distribution of the product of regression coefficients—addressing the well-known limitations of normal-theory approaches like the Sobel test.

The package supports:

- **Single mediator models** (X → M → Y)
- **Sequential/serial mediators** (X → M₁ → M₂ → Y)
- **Parallel mediators** with complex indirect effects
- **Any product of normal random variables** for custom applications

## Why RMediation?

Traditional methods (e.g., Sobel test) assume the indirect effect *ab* follows a normal distribution. This assumption is often violated because the product of two normal variables is *not* normally distributed—especially when effect sizes are small or sample sizes are modest.

**RMediation** provides methods that correctly handle this non-normality:

| Method | Best For | Speed |
|--------|----------|-------|
| **DOP** (Distribution of Product) | Two-variable products; publication-quality results | Fast |
| **Monte Carlo** | Complex models; 3+ variable products | Moderate |
| **MBCO** (Model-Based Constrained Optimization) | Hypothesis testing; bootstrap inference | Slower |

## Installation

### From CRAN (Stable)

```r
install.packages("RMediation")
```

### From GitHub (Development)

```r
# Using pak (recommended)
pak::pak("Data-Wise/rmediation@dev")

# Or using remotes
remotes::install_github("Data-Wise/rmediation", ref = "dev")
```

### From r-universe (development binaries)

The development version is also published at the
[Data-Wise r-universe](https://data-wise.r-universe.dev/RMediation) as a
pre-built binary (no compiler needed):

```r
install.packages(
  "RMediation",
  repos = c("https://data-wise.r-universe.dev", "https://cloud.r-project.org")
)
```

## Usage

### Basic Example: Single Mediator

When you have coefficient estimates and standard errors (e.g., from a published study):

```r
library(RMediation)

# Path coefficients: a = 0.5 (SE = 0.08), b = 0.6 (SE = 0.04)
# Compute 95% CI for the indirect effect ab
medci(mu.x = 0.5, mu.y = 0.6,
      se.x = 0.08, se.y = 0.04,
      rho = 0, type = "dop")

#> 95% CI for ab: [0.178, 0.422]
#> Estimate: 0.300, SE: 0.062
```

### Sequential Mediators (3+ Variables)

For models with multiple sequential mediators (X → M₁ → M₂ → Y), use the `ci()` function with a formula:

```r
# Four-path indirect effect: b1 * b2 * b3 * b4
ci(mu = c(b1 = 1.0, b2 = 0.7, b3 = 0.6, b4 = 0.45),
   Sigma = c(0.05, 0, 0, 0,    # Covariance matrix (lower triangle)
             0.05, 0, 0,
             0.03, 0,
             0.03),
   quant = ~ b1 * b2 * b3 * b4,
   type = "MC",
   n.mc = 1e5)
```

### With lavaan Models

RMediation integrates directly with [lavaan](https://lavaan.ugent.be/) SEM models:

```r
library(lavaan)
library(RMediation)

# Define mediation model with labeled paths
model <- '
  m ~ a*x
  y ~ b*m + c*x

  # Define indirect effect
  ab := a*b
'

fit <- sem(model, data = mydata)

# Compute CI for indirect effect (auto-detects 'ab := a*b')
ci(fit, type = "dop")
```

### Serial Mediation via medfit

For serial chains (`X → M1 → M2 → Y`), let [medfit](https://data-wise.github.io/medfit/)
fit the model and extract the coefficients + covariance, then pass the result
straight to `ci()`. medfit is an optional (`Suggests`) dependency.

```r
library(RMediation)

# medfit extracts a SerialMediationData object from a lavaan or lm/glm fit...
mu <- medfit::extract_mediation(
  fit, treatment = "X", mediator = c("M1", "M2"), outcome = "Y"
)

# ...and RMediation computes the CI for the serial indirect effect a * d1 * b
ci(mu, level = 0.95, type = "MC")
```

See `vignette("serial-mediation-with-medfit")` for the full lavaan and lm/glm
workflow.

### Using the S7 ProductNormal Class

For programmatic use, create `ProductNormal` objects directly:

```r
# Define distribution of two correlated normal variables
pn <- ProductNormal(
  mu = c(0.5, 0.3),
  Sigma = matrix(c(0.01, 0.002,
                   0.002, 0.01), 2, 2)
)

# Compute confidence interval
ci(pn, level = 0.95, type = "dop")

# Get CDF and quantiles
cdf(pn, q = 0.15)
dist_quantile(pn, p = 0.975)

# View summary
summary(pn)
```

### Hypothesis Testing with MBCO

For formal hypothesis testing of indirect effects using bootstrap:

```r
# Requires OpenMx model
library(OpenMx)

# ... define your OpenMx mediation model ...

# Run MBCO test (H0: indirect effect = 0)
mbco(model, effect = "ab", n.boot = 1000, type = "parametric")
```

## Key Functions

| Function | Purpose |
|----------|---------|
| `medci()` | CI for product of two normal variables |
| `ci()` | CI for any function of parameters (formulas, lavaan, ProductNormal) |
| `mbco()` | Bootstrap hypothesis tests via MBCO |
| `pprodnormal()` | CDF of product of two normals |
| `qprodnormal()` | Quantiles of product of two normals |
| `ProductNormal()` | S7 class for product distributions |

## Part of the Mediationverse

**RMediation** is part of the [**mediationverse**](https://data-wise.github.io/mediationverse/)—a suite of R packages for comprehensive mediation analysis:

| Package | Purpose |
|---------|---------|
| [**mediationverse**](https://data-wise.github.io/mediationverse/) | Meta-package: install and load all packages |
| [**medfit**](https://data-wise.github.io/medfit/) | Model fitting and coefficient extraction |
| [**probmed**](https://data-wise.github.io/probmed/) | Probabilistic effect size (P_med) |
| **RMediation** | Confidence intervals (DOP, Monte Carlo, MBCO) |
| [**medrobust**](https://github.com/data-wise/medrobust) | Sensitivity analysis | [github](https://github.com/data-wise/medrobust) |
| [**medsim**](https://github.com/Data-Wise/medsim) | Simulation infrastructure |

Install the entire ecosystem:

```r
pak::pak("Data-Wise/mediationverse")
library(mediationverse)  # Loads all packages
```

## Citation

If you use RMediation in your research, please cite:

```
Tofighi, D., & MacKinnon, D. P. (2011). RMediation: An R package for mediation
analysis confidence intervals. Behavior Research Methods, 43, 692-700.
https://doi.org/10.3758/s13428-011-0076-x
```

For Monte Carlo CIs in SEM:

```
Tofighi, D., & MacKinnon, D. P. (2016). Monte Carlo confidence intervals for
complex functions of indirect effects. Structural Equation Modeling: A
Multidisciplinary Journal, 23, 194-205.
https://doi.org/10.1080/10705511.2015.1057284
```

For sequential mediation and CI method comparisons:

```
Tofighi, D., & Kelley, K. (2020). Indirect effects in sequential mediation
models: Evaluating methods for hypothesis testing and confidence interval
formation. Multivariate Behavioral Research, 55(2), 188-210.
https://doi.org/10.1080/00273171.2019.1618545
```

For MBCO and bootstrap methods:

```
Tofighi, D., & Kelley, K. (2020). Improved inference in mediation analysis:
Introducing the model-based constrained optimization procedure.
Psychological Methods, 25(4), 496-515. https://doi.org/10.1037/met0000259

Tofighi, D. (2020). Bootstrap model-based constrained optimization tests of
indirect effects. Frontiers in Psychology, 10, 2989.
https://doi.org/10.3389/fpsyg.2019.02989
```

## Getting Help

- [Package documentation](https://data-wise.github.io/rmediation/)
- [Report issues](https://github.com/data-wise/rmediation/issues)
- [Mediationverse roadmap](https://data-wise.github.io/mediationverse/articles/roadmap.html)

## License

GPL-3
