#' Frequencies and Percentages of Categorical Variables
#'
#' For each variable in `columnas`, counts the observations of each category
#' and its percentage of the total.
#'
#' @param data A data frame.
#' @param columnas Character vector with the names of the categorical
#'   variables.
#'
#' @return A named list with one tibble per variable, holding the categories,
#'   their counts (`n`) and percentages (`Porcentaje`).
#' @export
#' @examples
#' df <- data.frame(sexo = c("F", "M", "F", "F", "M"),
#'                  ciclo = c(1, 2, 2, 3, 3))
#' calcular_porcentajes(df, c("sexo", "ciclo"))
calcular_porcentajes <- function(data, columnas) {
  resultados_lista <- lapply(columnas, function(col) {
    data %>%
      group_by(.data[[col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Porcentaje = n / sum(n) * 100)
  })

  stats::setNames(resultados_lista, columnas)
}
