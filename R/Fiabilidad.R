#' Omega Reliability of One Factor
#'
#' Fits a one-factor confirmatory model to the items in `vars`, treating them
#' as ordinal (WLSMV estimator) and returns the composite reliability (omega)
#' computed by [semTools::compRelSEM()].
#'
#' @param vars Character vector with the names of the items of the factor, for
#'   example one element of the list returned by [extract_items()].
#' @param data A data frame with the item responses.
#'
#' @return A named numeric value: the omega coefficient of the factor.
#' @seealso [calcula_omega_all()] for several factors at once.
#' @export
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 300
#' eta <- rnorm(n)
#' items <- as.data.frame(sapply(1:4, function(j) as.numeric(cut(
#'   0.7 * eta + rnorm(n, 0, 0.7), c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))))
#' names(items) <- paste0("ANS", 1:4)
#'
#' Fiabilidad(vars = names(items), data = items)
#' }
Fiabilidad <- function(vars, data) {
  temp_data <- data %>% select(all_of(vars))

  model_original <- paste("F1 =~", paste0(vars, collapse = " + "))

  fit.original <- lavaan::cfa(model_original, data = temp_data, estimator = "WLSMV",
                              mimic = "Mplus", ordered = TRUE)

  omega <- semTools::compRelSEM(fit.original, tau.eq = FALSE, ord.scale = TRUE)
  # recent semTools versions return a list; keep the documented numeric value
  stats::setNames(as.numeric(unlist(omega))[1], "F1")
}
