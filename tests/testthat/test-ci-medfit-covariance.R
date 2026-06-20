#' Tests for name-based covariance extraction from medfit objects
#'
#' These tests cover the path -> covariance resolver used by
#' \code{ci_mediation_data()} and \code{ci_serial_mediation_data()}.

skip_if_not_installed("medfit")

# ---- fixtures ---------------------------------------------------------------

make_md <- function(cov_ab = 0) {
  vcov_mat <- matrix(
    c(0.01,  cov_ab, 0,
      cov_ab, 0.02,  0,
      0,      0,     0.015),
    nrow = 3, ncol = 3,
    dimnames = list(c("a", "b", "c_prime"),
                    c("a", "b", "c_prime"))
  )
  medfit::MediationData(
    a_path = 0.5, b_path = 0.4, c_prime = 0.1,
    estimates = c(a = 0.5, b = 0.4, c_prime = 0.1),
    vcov = vcov_mat,
    sigma_m = NULL, sigma_y = NULL,
    treatment = "X", mediator = "M", outcome = "Y",
    mediator_predictors = character(0), outcome_predictors = character(0),
    data = NULL, n_obs = 200L, converged = TRUE, source_package = "manual"
  )
}

make_serial <- function(off_diag = 0) {
  # 2 mediators -> d_path length 1. Paths order: a, d1, b.
  od <- off_diag
  vcov_mat <- matrix(
    c(0.01, od,   od,
      od,   0.02, od,
      od,   od,   0.015),
    nrow = 3, ncol = 3,
    dimnames = list(c("a", "d1", "b"),
                    c("a", "d1", "b"))
  )
  medfit::SerialMediationData(
    a_path = 0.5, d_path = 0.3, b_path = 0.4, c_prime = 0.1,
    estimates = c(a = 0.5, d1 = 0.3, b = 0.4),
    vcov = vcov_mat,
    sigma_mediators = NULL, sigma_y = NULL,
    treatment = "X", mediators = c("M1", "M2"), outcome = "Y",
    mediator_predictors = list("X", c("X", "M1")),
    outcome_predictors = character(0),
    data = NULL, n_obs = 200L, converged = TRUE, source_package = "manual"
  )
}

# ---- 1. covariance present is extracted by name -----------------------------

test_that(".extract_path_vcov reads the true off-diagonal cov(a, b)", {
  md <- make_md(cov_ab = 0.003)
  Sigma <- RMediation:::.extract_path_vcov(md, c("a", "b"))
  expect_equal(dim(Sigma), c(2L, 2L))
  expect_equal(Sigma["a", "b"], 0.003)
  expect_equal(Sigma["a", "a"], 0.01)
  expect_equal(Sigma["b", "b"], 0.02)
})

# ---- 2. cross-method agreement (dop vs MC vs medci) -------------------------

test_that("ci() dop and MC agree with each other and with medci", {
  cov_ab <- 0.003
  md <- make_md(cov_ab = cov_ab)

  res_dop <- RMediation::ci(md, type = "dop")
  set.seed(42)
  res_mc <- RMediation::ci(md, type = "MC", n.mc = 2e5)

  # dop and MC agree within ~2% on each CI bound
  expect_equal(as.numeric(res_dop$CI), as.numeric(res_mc$CI),
               tolerance = 0.02)

  # both agree with a direct medci() call carrying the same correlation.
  # medci(type = "dop") returns a list whose first element is the CI vector
  # (named "<level>% CI"), so reference it positionally.
  se_a <- sqrt(0.01)
  se_b <- sqrt(0.02)
  rho <- cov_ab / (se_a * se_b)
  ref <- RMediation::medci(mu.x = 0.5, mu.y = 0.4, se.x = se_a, se.y = se_b,
                           rho = rho, alpha = 0.05, type = "dop")
  expect_equal(as.numeric(res_dop$CI), as.numeric(ref[[1]]),
               tolerance = 1e-6)
})

# ---- 3. serial uses the FULL covariance, not a diagonal approximation -------

test_that("serial ci uses full covariance matching a reference simulation", {
  off_diag <- 0.004
  smd <- make_serial(off_diag = off_diag)
  all_paths <- c(0.5, 0.3, 0.4)

  Sigma_full <- matrix(
    c(0.01, off_diag, off_diag,
      off_diag, 0.02, off_diag,
      off_diag, off_diag, 0.015),
    nrow = 3, ncol = 3
  )
  Sigma_diag <- diag(diag(Sigma_full))

  set.seed(7)
  res <- RMediation::ci(smd, type = "MC", n.mc = 1e5)

  # The CI must DIFFER from a diagonal-only approximation: this proves the
  # off-diagonal covariances are actually used (not silently dropped).
  set.seed(7)
  draws_diag <- MASS::mvrnorm(n = 1e5, mu = all_paths, Sigma = Sigma_diag)
  prod_diag <- apply(draws_diag, 1, prod)
  ci_diag <- unname(stats::quantile(prod_diag, probs = c(0.025, 0.975)))

  expect_false(isTRUE(all.equal(unname(res$CI), ci_diag, tolerance = 1e-6)))

  # Non-tautological accuracy check: compare against an INDEPENDENT large-n
  # Monte Carlo reference using a DIFFERENT seed and the FULL Sigma. Reusing
  # the same seed for both legs would make this a tautology; a different seed
  # with a loose tolerance absorbs MC noise while still catching a wrong Sigma.
  set.seed(20240601)
  ref_draws <- MASS::mvrnorm(n = 2e5, mu = all_paths, Sigma = Sigma_full)
  ref_product <- apply(ref_draws, 1, prod)
  ref_ci <- unname(stats::quantile(ref_product, probs = c(0.025, 0.975)))
  expect_equal(unname(res$CI), ref_ci, tolerance = 0.03)
})

# ---- 3b. serial preserves the backward-compatible return shape --------------

test_that("ci_serial_mediation_data preserves the original return shape", {
  smd <- make_serial(off_diag = 0.004)

  set.seed(11)
  res <- RMediation::ci(smd, type = "MC", n.mc = 2000)

  # Original shape (spec 4.4): CI (named lower/upper), Estimate, SE, type,
  # level, n.mc, k.
  expect_named(res, c("CI", "Estimate", "SE", "type", "level", "n.mc", "k"))
  expect_named(res$CI, c("lower", "upper"))
  expect_lt(res$CI[["lower"]], res$CI[["upper"]])
  expect_equal(res$Estimate, 0.5 * 0.3 * 0.4)
  expect_gt(res$SE, 0)
  expect_equal(res$type, "MC (serial mediation)")
  expect_equal(res$level, 0.95)
  expect_equal(res$n.mc, 2000)
  # d_path has length 1 in this 2-mediator fixture.
  expect_identical(res$k, 1L)
})

# ---- 4. error path: unnamed vcov/estimates cannot be resolved ---------------

test_that("unresolvable labels raise an informative error (no silent independence)", {
  vcov_mat <- matrix(
    c(0.01, 0.003, 0,
      0.003, 0.02, 0,
      0, 0, 0.015),
    nrow = 3, ncol = 3
  )  # no dimnames
  md <- medfit::MediationData(
    a_path = 0.5, b_path = 0.4, c_prime = 0.1,
    estimates = c(0.5, 0.4, 0.1),  # no names
    vcov = vcov_mat,
    sigma_m = NULL, sigma_y = NULL,
    treatment = "X", mediator = "M", outcome = "Y",
    mediator_predictors = character(0), outcome_predictors = character(0),
    data = NULL, n_obs = 200L, converged = TRUE, source_package = "manual"
  )
  # The error must be the SPECIFIC informative message about being unable to
  # resolve parameters by name -- not any incidental crash. This matches the
  # resolver's wording for the "no dimnames and unnamed estimates" branch.
  expect_error(
    RMediation::ci(md, type = "dop"),
    regexp = "Cannot resolve path parameters by name"
  )
  expect_error(
    RMediation:::.resolve_path_indices(md, c("a", "b")),
    regexp = "Cannot resolve path parameters by name"
  )
})

test_that("missing path labels raise the specific 'Could not resolve' error", {
  # Names exist but do not include the requested a/b labels.
  vcov_mat <- matrix(
    c(0.01, 0, 0, 0.02), nrow = 2,
    dimnames = list(c("x", "y"), c("x", "y"))
  )
  md <- medfit::MediationData(
    a_path = 0.5, b_path = 0.4, c_prime = 0.1,
    estimates = c(x = 0.5, y = 0.4),
    vcov = vcov_mat,
    sigma_m = NULL, sigma_y = NULL,
    treatment = "X", mediator = "M", outcome = "Y",
    mediator_predictors = character(0), outcome_predictors = character(0),
    data = NULL, n_obs = 200L, converged = TRUE, source_package = "manual"
  )
  expect_error(
    RMediation:::.resolve_path_indices(md, c("a", "b")),
    regexp = "Could not resolve path label\\(s\\): a, b"
  )
})

# ---- 5. non-zero cov(a, b) changes the CI -----------------------------------

test_that("non-zero cov(a, b) yields a different CI than independence", {
  md0 <- make_md(cov_ab = 0)
  md1 <- make_md(cov_ab = 0.006)

  ci0 <- RMediation::ci(md0, type = "dop")$CI
  ci1 <- RMediation::ci(md1, type = "dop")$CI

  expect_false(isTRUE(all.equal(ci0, ci1, tolerance = 1e-6)))
})

# ---- 6. name-based slicing: block-diagonal lm case (a/b non-adjacent) -------

test_that(".extract_path_vcov uses NAME-based slicing (block-diagonal, lm case)", {
  # Block-diagonal vcov with cov(a, b) = 0, and a NUISANCE parameter placed
  # BETWEEN a and b so that a/b sit at non-adjacent positions (1 and 3). A
  # positional 1:2 slice would pick up the nuisance row/col; name resolution
  # must extract exactly the a/b sub-block.
  vcov_mat <- matrix(
    c(0.04, 0.00, 0.00,
      0.00, 0.50, 0.00,
      0.00, 0.00, 0.09),
    nrow = 3, ncol = 3, byrow = TRUE,
    dimnames = list(c("a", "nuisance", "b"),
                    c("a", "nuisance", "b"))
  )
  md <- medfit::MediationData(
    a_path = 0.4, b_path = 0.6, c_prime = 0.1,
    estimates = c(a = 0.4, nuisance = 1.23, b = 0.6),
    vcov = vcov_mat,
    sigma_m = NULL, sigma_y = NULL,
    treatment = "X", mediator = "M", outcome = "Y",
    mediator_predictors = character(0), outcome_predictors = character(0),
    data = NULL, n_obs = 200L, converged = TRUE, source_package = "manual"
  )

  # Indices must come from NAMES (1 and 3), not positions 1 and 2.
  idx <- RMediation:::.resolve_path_indices(md, c("a", "b"))
  expect_equal(idx, c(1L, 3L))

  Sigma <- RMediation:::.extract_path_vcov(md, c("a", "b"))
  expect_equal(dim(Sigma), c(2L, 2L))
  # cov(a, b) == 0 (block-diagonal lm case)
  expect_equal(Sigma["a", "b"], 0)
  expect_equal(Sigma["b", "a"], 0)
  # Diagonal entries are the a/b variances, NOT the nuisance variance (0.50).
  expect_equal(Sigma["a", "a"], 0.04)
  expect_equal(Sigma["b", "b"], 0.09)
  expect_false(any(Sigma == 0.50))
})

# ---- 7. covariates / interspersed a and b columns ---------------------------

test_that(".extract_path_vcov matches the named sub-matrix when a/b are interspersed", {
  # a and b are NOT in positions 1, 2: extra coefficients surround them and
  # cov(a, b) is non-zero. The extracted Sigma must equal the correctly-NAMED
  # a/b sub-matrix, not a positional slice.
  nm <- c("intercept", "a", "covariate", "b", "cp")
  full <- matrix(0, nrow = 5, ncol = 5, dimnames = list(nm, nm))
  diag(full) <- c(0.10, 0.04, 0.20, 0.09, 0.01)
  full["a", "b"] <- 0.015
  full["b", "a"] <- 0.015

  md <- medfit::MediationData(
    a_path = 0.4, b_path = 0.6, c_prime = 0.2,
    estimates = c(intercept = 1, a = 0.4, covariate = 0.3, b = 0.6, cp = 0.2),
    vcov = full,
    sigma_m = NULL, sigma_y = NULL,
    treatment = "X", mediator = "M", outcome = "Y",
    mediator_predictors = "covariate", outcome_predictors = "covariate",
    data = NULL, n_obs = 200L, converged = TRUE, source_package = "manual"
  )

  idx <- RMediation:::.resolve_path_indices(md, c("a", "b"))
  expect_equal(idx, c(2L, 4L)) # name-based positions, not 1:2

  Sigma <- RMediation:::.extract_path_vcov(md, c("a", "b"))
  expected <- full[c("a", "b"), c("a", "b")]
  expect_equal(Sigma, expected)
  expect_equal(Sigma["a", "b"], 0.015)
})

# ---- 8. serial d-label contract is the literal d1..dk ----------------------

test_that(".serial_d_labels returns the literal d1..dk name contract", {
  smd <- make_serial(off_diag = 0)  # d_path length 1 -> "d1"
  expect_equal(RMediation:::.serial_d_labels(smd), "d1")
})

# ---- 9. asymp (delta) method also routes through name-based extraction ------

test_that("ci() type = 'asymp' returns a valid interval via the resolver", {
  # The asymp/delta path is documented but was previously untested. It must
  # produce a finite CI/Estimate/SE built from the name-resolved 2x2 Sigma.
  md <- make_md(cov_ab = 0.003)
  res <- RMediation::ci(md, type = "asymp")

  expect_true(all(c("CI", "Estimate", "SE") %in% names(res)))
  expect_length(as.numeric(res$CI), 2L)
  expect_true(all(is.finite(as.numeric(res$CI))))
  expect_lt(as.numeric(res$CI)[1], as.numeric(res$CI)[2])
  # The delta/asymptotic point estimate of E[ab] is bias-corrected:
  # E[ab] = a*b + cov(a, b) = 0.2 + 0.003. (Not the naive product a*b.)
  expect_equal(res$Estimate, 0.5 * 0.4 + 0.003)
  expect_gt(res$SE, 0)
})

# ---- 10. serial input validation (n.mc, level) -----------------------------

test_that("ci_serial_mediation_data validates n.mc and level", {
  smd <- make_serial(off_diag = 0)

  # n.mc must be a positive count.
  expect_error(
    RMediation::ci(smd, type = "MC", n.mc = -5),
    regexp = "n.mc"
  )
  expect_error(
    RMediation::ci(smd, type = "MC", n.mc = 0),
    regexp = "n.mc"
  )
  # level must be in [0, 1].
  expect_error(
    RMediation::ci(smd, type = "MC", level = 1.5),
    regexp = "level"
  )
})

# ---- 11. three-mediator serial chain (d_path length 2 -> d1, d2) ------------

test_that("serial ci handles a 3-mediator chain (d1, d2) with full covariance", {
  # d_path length 2 exercises the multi-d label contract c("a", "d1", "d2", "b")
  # and a 4x4 covariance sub-matrix -- the 2-mediator fixture only covers d1.
  nm <- c("a", "d1", "d2", "b")
  od <- 0.003
  vcov_mat <- matrix(od, nrow = 4, ncol = 4, dimnames = list(nm, nm))
  diag(vcov_mat) <- c(0.01, 0.02, 0.03, 0.04)

  smd3 <- medfit::SerialMediationData(
    a_path = 0.5, d_path = c(0.3, 0.35), b_path = 0.4, c_prime = 0.1,
    estimates = c(a = 0.5, d1 = 0.3, d2 = 0.35, b = 0.4),
    vcov = vcov_mat,
    sigma_mediators = NULL, sigma_y = NULL,
    treatment = "X", mediators = c("M1", "M2", "M3"), outcome = "Y",
    mediator_predictors = list("X", c("X", "M1"), c("X", "M1", "M2")),
    outcome_predictors = character(0),
    data = NULL, n_obs = 200L, converged = TRUE, source_package = "manual"
  )

  # Labels and resolved indices span all four chain parameters.
  expect_equal(RMediation:::.serial_d_labels(smd3), c("d1", "d2"))
  expect_equal(
    RMediation:::.resolve_path_indices(smd3, c("a", "d1", "d2", "b")),
    1:4
  )

  set.seed(99)
  res <- RMediation::ci(smd3, type = "MC", n.mc = 5e4)
  expect_identical(res$k, 2L)
  expect_equal(res$Estimate, 0.5 * 0.3 * 0.35 * 0.4)
  expect_lt(res$CI[["lower"]], res$CI[["upper"]])

  # Result must reflect the full (non-diagonal) Sigma: compare to a same-seed
  # diagonal-only draw and require it to differ.
  set.seed(99)
  diag_draws <- MASS::mvrnorm(n = 5e4, mu = c(0.5, 0.3, 0.35, 0.4),
                              Sigma = diag(diag(vcov_mat)))
  diag_ci <- unname(stats::quantile(apply(diag_draws, 1, prod),
                                    probs = c(0.025, 0.975)))
  expect_false(isTRUE(all.equal(unname(res$CI), diag_ci, tolerance = 1e-6)))
})
