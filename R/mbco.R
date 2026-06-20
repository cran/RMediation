
S7::method(mbco, list(S7::class_any, S7::class_any)) <- function(
  h0,
  h1,
  R = 10L,
  type = "asymp",
  alpha = .05,
  checkHess = "No",
  checkSE = "No",
  optim = "SLSQP",
  precision = 1e-9,
  ...
) {
  # Check OpenMx availability
  if (!requireNamespace("OpenMx", quietly = TRUE)) {
    stop(
      "Package 'OpenMx' is required for MBCO functions but is not installed.\n",
      "Install it with: install.packages('OpenMx')\n",
      "Note: OpenMx requires compilation and may take several minutes to install.",
      call. = FALSE
    )
  }

  # Input validation (check class without requiring OpenMx at load time)
  if (!"MxModel" %in% class(h0)) stop("h0 must be an MxModel")
  if (!"MxModel" %in% class(h1)) stop("h1 must be an MxModel")
  
  checkmate::assert_count(R, positive = TRUE)
  type <- match.arg(type, c("asymp", "parametric", "semi"))
  checkmate::assert_number(alpha, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_choice(checkHess, c("Yes", "No"))
  checkmate::assert_choice(checkSE, c("Yes", "No"))
  optim <- match.arg(optim, c("NPSOL", "CSOLNP", "SLSQP"))
  checkmate::assert_number(precision, lower = 0, finite = TRUE)

  if (!OpenMx::imxHasNPSOL()) {
    optim <- "SLSQP"
  } # if NPSOL is not available, use SLSQP

  res_list <-
    if (type == "asymp") {
      # Asymptotic MBCO LRT
      mbco_asymp(h0 = h0, h1 = h1, alpha = alpha)
    } else if (type == "parametric") {
      # Parametric bootstrap MBCO LRT
      mbco_parametric(
        h0 = h0,
        h1 = h1,
        R = R,
        alpha = alpha,
        checkHess = checkHess,
        checkSE = checkSE,
        optim = optim,
        precision = precision
      )
    } else if (type == "semi") {
      # Semiparametric bootstrap MBCO LRT
      mbco_semi(
        h0 = h0,
        h1 = h1,
        R = R,
        alpha = alpha,
        checkHess = checkHess,
        checkSE = checkSE,
        optim = optim,
        precision = precision
      )
    }

  # Convert list result to MBCOResult object
  # Assuming res_list has components: chisq, df, p
  MBCOResult(
    statistic = as.numeric(res_list$chisq),
    df = as.numeric(res_list$df),
    p_value = as.numeric(res_list$p),
    type = type
  )
}
