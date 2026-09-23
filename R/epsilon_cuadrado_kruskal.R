#' Kruskal-Wallis Test with Epsilon Squared
#'
#' Runs a Kruskal-Wallis test and adds its effect size, epsilon squared
#' \eqn{\epsilon^2 = H / (n - 1)}, with a verbal interpretation.
#'
#' @param data A data frame.
#' @param formula A formula `outcome ~ group`.
#'
#' @details `n` counts the cases with non-missing outcome and group, the same
#'   cases used by the test. The interpretation is "Grande" for
#'   \eqn{\epsilon^2} >= .50, "Mediano" >= .30, "Pequeño" >= .10 and
#'   "No significativo" otherwise.
#'
#' @return A tibble with the test results (`statistic`, `p.value`,
#'   `parameter`, `method`), `Epsilon` and `Interpretación`.
#' @export
#' @examples
#' df <- data.frame(
#'   grupo = rep(c("A", "B", "C"), each = 5),
#'   puntaje = c(3, 4, 5, 4, 3, 7, 8, 6, 7, 8, 10, 12, 11, 9, 10)
#' )
#' epsilon_cuadrado_kruskal(df, puntaje ~ grupo)
epsilon_cuadrado_kruskal <- function(data, formula) {
  variables <- all.vars(formula)
  if (!all(variables %in% colnames(data))) {
    stop("Las variables de la f\u00f3rmula no existen en los datos: ",
         paste(setdiff(variables, colnames(data)), collapse = ", "))
  }

  n <- sum(stats::complete.cases(data[, variables, drop = FALSE]))

  resultado_glance <- broom::glance(stats::kruskal.test(formula, data = data))

  resultado_glance %>%
    mutate(
      Epsilon = statistic / ((n^2 - 1) / (n + 1)),
      "Interpretaci\u00f3n" = dplyr::case_when(
        Epsilon >= 0.50 ~ "Grande",
        Epsilon >= 0.30 ~ "Mediano",
        Epsilon >= 0.10 ~ "Peque\u00f1o",
        TRUE ~ "No significativo"
      )
    )
}
