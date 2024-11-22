epsilon_cuadrado_kruskal <- function(data, formula, n) {
  # Cargar librerías necesarias
  if (!requireNamespace("broom", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Por favor, instala los paquetes 'broom' y 'dplyr' antes de usar esta función.")
  }

  # Aplicar la prueba de Kruskal-Wallis
  resultado <- kruskal.test(formula, data = data)

  # Convertir el resultado a un data frame con broom::glance
  resultado_glance <- broom::glance(resultado)

  # Calcular epsilon cuadrado
  resultado_glance <- resultado_glance %>%
    dplyr::mutate(
      Epsilon = statistic / ((n^2 - 1) / (n + 1)),
      Interpretación = dplyr::case_when(
        Epsilon >= 0.50 ~ "Grande",
        Epsilon >= 0.30 ~ "Mediano",
        Epsilon >= 0.10 ~ "Pequeño",
        TRUE ~ "No significativo"
      )
    )

  return(resultado_glance)
}
