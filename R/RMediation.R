#' Tidy generic function
#'
#' @param x An object to tidy
#' @param ... Additional arguments passed to methods
#' @return A tibble or data.frame with tidy output
#' @export
tidy <- function(x, ...) {
  UseMethod("tidy")
}
