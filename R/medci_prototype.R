# PROTOTYPE: medci() as wrapper around S7 core
# This is a prototype implementation showing how medci() can become a thin wrapper

#' Confidence Interval for the Mediated Effect (PROTOTYPE S7 Wrapper)
#'
#' @inheritParams medci
#' @keywords internal
#' @export
medci_prototype <- function(
  mu.x,
  mu.y,
  se.x,
  se.y,
  rho = 0,
  alpha = .05,
  type = "dop",
  plot = FALSE,
  plotCI = FALSE,
  n.mc = 1e5,
  ...
) {
  # ========== Input Validation (keep existing) ==========
  checkmate::assert_number(mu.x, finite = TRUE)
  checkmate::assert_number(mu.y, finite = TRUE)
  checkmate::assert_number(se.x, lower = 0, finite = TRUE)
  checkmate::assert_number(se.y, lower = 0, finite = TRUE)
  checkmate::assert_number(rho, lower = -1, upper = 1, finite = TRUE)
  checkmate::assert_number(alpha, lower = 0, upper = 1, finite = TRUE)
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1")
  }
  checkmate::assert_logical(plot)
  checkmate::assert_logical(plotCI)
  checkmate::assert_count(n.mc, positive = TRUE)

  type <- tolower(type)
  type <- match.arg(type, c("dop", "mc", "asymp", "all", "prodclin"))

  # ========== Handle Plotting (keep existing plotting code for backward compatibility) ==========
  if (plot == TRUE) {
    mean.v <- c(mu.x, mu.y)
    var.mat <- matrix(
      c(se.x^2, se.x * se.y * rho, se.x * se.y * rho, se.y^2),
      2
    )
    x_y <- matrix(rnorm(2 * n.mc), ncol = n.mc)
    x_y <- crossprod(chol(var.mat), x_y) + mean.v
    x_y <- t(x_y)
    xy <- x_y[, 1] * x_y[, 2]
    se.xy <- sqrt(
      se.y^2 *
        mu.x^2 +
        se.x^2 * mu.y^2 +
        2 * mu.x * mu.y * rho * se.x * se.y +
        se.x^2 * se.y^2 +
        se.x^2 * se.y^2 * rho^2
    )
    mu.xy <- mu.x * mu.y + rho * se.x * se.y
    max1 <- mu.xy + 6 * se.xy
    min1 <- mu.xy - 6 * se.xy
    if (min1 > 0 || max1 < 0) {
      xrange <- round(seq(min1, max1, length = 7), 1)
    } else {
      xrange <- round(
        cbind(seq(min1, 0, length = 3), seq(0, max1, length = 3)),
        1
      )
    }
    xy <- xy[xy > min1 & xy < max1]
    plot(
      density(xy),
      xlab = expression(paste("Product ", (italic(xy)))),
      ylab = "Density",
      axes = FALSE,
      xlim = c(min1, max1),
      main = "",
      ...
    )
    axis(1, xrange)
    axis(2)
    smidge <- par("cin") * abs(par("tcl"))
    text(
      max1 - (max1 - min1) / 7,
      (par("usr")[4]),
      pos = 1,
      bquote(mu == .(round(mu.xy, 3))),
      ...
    )
    text(
      max1 - (max1 - min1) / 7,
      (par("usr")[4] - 1.5 * par("cxy")[2]),
      pos = 1,
      bquote(sigma == .(round(se.xy, 3))),
      ...
    )
    if (plotCI) {
      yci <- par("usr")[3] + diff(par("usr")[3:4]) / 25
      yci <- 0

      # Use S7 core for CI calculation
      Sigma <- matrix(c(se.x^2, rho * se.x * se.y,
                        rho * se.x * se.y, se.y^2), nrow = 2)
      pn <- ProductNormal(mu = c(mu.x, mu.y), Sigma = Sigma)
      s7_result <- ci(pn, level = 1 - alpha, type = "dop")
      MedCI <- list(s7_result$CI)

      arrows(
        MedCI[[1]][1],
        yci,
        MedCI[[1]][2],
        yci,
        length = smidge,
        angle = 90,
        code = 3,
        cex = 1.5,
        ...
      )
      points(mu.xy, yci, pch = 19, cex = 1.5, ...)
      text(
        max1 - (max1 - min1) / 7,
        (par("usr")[4] - 3 * par("cxy")[2]),
        pos = 1,
        paste("LL=", round(MedCI[[1]][1], 3)),
        ...
      )
      text(
        max1 - (max1 - min1) / 7,
        (par("usr")[4] - 4.5 * par("cxy")[2]),
        pos = 1,
        paste("UL=", round(MedCI[[1]][2], 3)),
        ...
      )
    }
  }

  # ========== Create ProductNormal object ==========
  Sigma <- matrix(c(
    se.x^2, rho * se.x * se.y,
    rho * se.x * se.y, se.y^2
  ), nrow = 2)

  pn <- ProductNormal(mu = c(mu.x, mu.y), Sigma = Sigma)

  # ========== Dispatch to S7 method ==========
  level <- 1 - alpha

  # Map prodclin to dop for backward compatibility
  if (type == "prodclin") type <- "dop"

  s7_result <- ci(pn, level = level, type = type, n.mc = n.mc)

  # ========== Format for backward compatibility ==========
  # Legacy format: list with names like "95% CI", "Estimate", "SE"

  if (type == "all") {
    # Return list of results from each method
    legacy_result <- list(
      "Distribution of Product" = .format_medci_result(s7_result$dop, alpha),
      "Monte Carlo" = .format_medci_result(s7_result$mc, alpha),
      "Asymptotic Normal" = .format_medci_result(s7_result$asymp, alpha)
    )
  } else {
    legacy_result <- .format_medci_result(s7_result, alpha)
  }

  return(legacy_result)
}

#' Helper to format S7 result to legacy list format
#' @keywords internal
.format_medci_result <- function(s7_result, alpha) {
  ci_name <- paste0((1 - alpha) * 100, "% CI")

  result <- list(
    s7_result$CI,
    s7_result$Estimate,
    s7_result$SE
  )
  names(result) <- c(ci_name, "Estimate", "SE")

  # Add MC.Error if present
  if (!is.null(s7_result$MC.Error)) {
    result$MC.Error <- s7_result$MC.Error
  }

  return(result)
}
