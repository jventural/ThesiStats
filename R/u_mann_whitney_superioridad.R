u_mann_whitney_superioridad <- function(data, formula, n1, n2, alternative = "two.sided") {
  # Cargar librerías necesarias
  if (!requireNamespace("broom", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Por favor, instala los paquetes 'broom' y 'dplyr' antes de usar esta función.")
  }

  # Aplicar la prueba U de Mann-Whitney
  resultado <- wilcox.test(formula, alternative = alternative, data = data)

  # Extraer el resultado con broom::glance
  resultado_glance <- broom::glance(resultado)

  # Calcular el tamaño del efecto PS y agregar interpretación
  resultado_glance <- resultado_glance %>%
    dplyr::mutate(
      PSest = statistic / (n1 * n2),  # Cálculo de PS = U / (n1 * n2)
      Interpretación = dplyr::case_when(
        PSest <= 0.0 ~ "No efecto",
        PSest >= 0.71 ~ "Grande",
        PSest >= 0.64 ~ "Mediano",
        PSest >= 0.56 ~ "Pequeño",
        TRUE ~ "No efecto"
      )
    )

  return(resultado_glance)
}
