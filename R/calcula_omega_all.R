#' Omega Reliability of Several Factors
#'
#' Applies [Fiabilidad()] to every factor of a list of item names and returns
#' the omega coefficient of each one.
#'
#' @param extracted A named list: each element holds the item names of one
#'   factor, as returned by [extract_items()].
#' @param data A data frame with the item responses.
#'
#' @return A data frame with the columns `Variables` (factor name) and `Omega`.
#' @export
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 300
#' eta <- matrix(rnorm(n * 2), n, 2) %*% chol(0.7 * diag(2) + 0.3)
#' items <- as.data.frame(sapply(1:8, function(j) as.numeric(cut(
#'   0.7 * eta[, ceiling(j / 4)] + rnorm(n, 0, 0.7),
#'   c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)))))
#' names(items) <- c(paste0("ANS", 1:4), paste0("DEP", 1:4))
#'
#' factores <- extract_items(c("Ansiedad: ANS1, ANS2, ANS3, ANS4",
#'                             "Depresion: DEP1, DEP2, DEP3, DEP4"))
#' calcula_omega_all(factores, items)
#' }
calcula_omega_all <- function(extracted, data) {
  resultados <- data.frame(Variables = character(), Omega = numeric())

  for (key in names(extracted)) {
    # compRelSEM() may return a named vector or a one-row list/data frame
    omega <- as.numeric(unlist(Fiabilidad(vars = extracted[[key]], data = data)))[1]
    resultados <- rbind(resultados, data.frame(Variables = key, Omega = omega))
  }
  rownames(resultados) <- NULL
  resultados$Omega <- as.numeric(resultados$Omega)

  resultados
}
