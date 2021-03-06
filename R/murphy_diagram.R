#' Murphy diagram
#'
#' Murphy diagram of the elementary scoring function for expectiles resp. quantiles at level \code{alpha} for different values of \code{theta}.
#' Can be used to study and compare performance of one or multiple models. If the plot needs to be customized, set \code{plot = FALSE} to get the resulting data instead of the plot.
#'
#' @importFrom graphics matplot legend
#' @param actual Observed values.
#' @param predicted Predicted values.
#' @param w Optional case weights.
#' @param alpha Level of expectile resp. quantile. The default \code{alpha = 0.5} corresponds to the expectation resp. median.
#' @param theta Vector of evaluation points.
#' @param functional Either "expectile" or "quantile".
#' @param plot Should plot (\code{TRUE}) be returned or the data to be plotted?
#' @param ... Further arguments passed to \code{plot}.
#' @return A named list of functions.
#' @export
#' @references
#' Ehm, W., Gneiting, T., Jordan, A. and Krüger, F. (2016), Of quantiles and expectiles: consistent scoring functions, Choquet representations and forecast rankings. J. R. Stat. Soc. B, 78: 505-562, <doi.org/10.1111/rssb.12154>.
#' @examples
#' y <- 1:10
#' predicted <- 1.1 * y
#' murphy_diagram(y, predicted, theta = seq(0.9, 1.2, by = 0.01))
#' two_models <- cbind(m1 = predicted, m2 = 1.2 * y)
#' murphy_diagram(y, two_models, theta = seq(0.9, 1.3, by = 0.01))
#' @seealso \code{\link{elementary_score}}.
murphy_diagram <- function(actual, predicted, w = NULL,
                           alpha = 0.5,
                           theta = seq(-2, 2, length.out = 100),
                           functional = c("expectile", "quantile"),
                           plot = TRUE, ...) {
  # Create list of elementary scores
  functional <- match.arg(functional)
  fun <- switch(functional,
                expectile = elementary_score_expectile,
                quantile = elementary_score_quantile)
  multi <- multi_metric(fun, theta = theta, alpha = alpha)

  # Organize input as data.frame
  if (is.vector(predicted)) {
    predicted <- data.frame(predicted = predicted)
  } else {
    stopifnot(is.matrix(predicted) || is.data.frame(predicted))
    if (is.null(colnames(predicted))) {
      colnames(predicted) <- paste("P", seq_len(ncol(predicted)))
    }
  }
  nms <- colnames(predicted)
  data <- data.frame(actual = actual, predicted)
  if (!is.null(w)) {
    data$w <- w
  }

  # Helper function to get scores
  get_scores <- function(nm) {
    p <- performance(
      data = data,
      actual = "actual",
      predicted = nm,
      w = if (!is.null(w)) "w",
      metrics = multi,
      key = "theta",
      value = "score"
    )
    p$score
  }
  perf <- lapply(nms, get_scores)
  names(perf) <- nms
  out <- do.call(cbind, perf)
  if (isFALSE(plot)) {
    return(data.frame(theta = theta, out))
  }
  matplot(x = theta, y = out, type = "l")
  if (length(nms) >= 2) {
    legend("topleft", legend = nms,
           col = 1 + (seq_along(nms) - 1) %% 6,
           lty = 1 + (seq_along(nms) - 1) %% 5)
  }
}


