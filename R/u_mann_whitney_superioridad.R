#' Mann-Whitney U Test with the Probability of Superiority
#'
#' Runs a Mann-Whitney U test (Wilcoxon rank-sum) between two groups and adds
#' the probability of superiority, \eqn{PS = U / (n_1 n_2)}: the probability
#' that a random case of the first group scores higher than a random case of
#' the second.
#'
#' @param data A data frame.
#' @param formula A formula `outcome ~ group`; the group must have exactly two
#'   levels.
#' @param alternative Alternative hypothesis passed to [stats::wilcox.test()].
#'
#' @details PS = .50 means no effect. The size is judged on the distance from
#'   .50 in either direction, using max(PS, 1 - PS): at least .71 is
#'   "Grande", at least .64 "Mediano", at least .56 "Pequeño" and below that
#'   "No efecto".
#'
#' @return A tibble with the test results (`statistic` is U for the first
#'   group, `p.value`, `method`, `alternative`), `PSest` and `Interpretación`.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(grupo = rep(c("A", "B"), each = 30),
#'                  puntaje = c(rnorm(30, 10), rnorm(30, 11)))
#' u_mann_whitney_superioridad(df, puntaje ~ grupo)
u_mann_whitney_superioridad <- function(data, formula, alternative = "two.sided") {
  variable_respuesta <- all.vars(formula)[1]
  variable_comparacion <- all.vars(formula)[2]

  data <- data[stats::complete.cases(data[, c(variable_respuesta, variable_comparacion)]), ,
               drop = FALSE]
  data[[variable_comparacion]] <- droplevels(as.factor(data[[variable_comparacion]]))

  niveles <- table(data[[variable_comparacion]])
  if (length(niveles) != 2) {
    stop("La variable de comparaci\u00f3n debe tener exactamente 2 niveles.")
  }
  n1 <- as.numeric(niveles[1])
  n2 <- as.numeric(niveles[2])

  resultado <- stats::wilcox.test(formula, alternative = alternative, data = data)

  broom::glance(resultado) %>%
    mutate(
      PSest = statistic / (n1 * n2),
      "Interpretaci\u00f3n" = dplyr::case_when(
        pmax(PSest, 1 - PSest) >= 0.71 ~ "Grande",
        pmax(PSest, 1 - PSest) >= 0.64 ~ "Mediano",
        pmax(PSest, 1 - PSest) >= 0.56 ~ "Peque\u00f1o",
        TRUE ~ "No efecto"
      )
    )
}
