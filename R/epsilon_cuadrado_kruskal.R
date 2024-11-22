epsilon_cuadrado_kruskal <- function(data, formula) {
  # Cargar librerías necesarias
  if (!requireNamespace("broom", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Por favor, instala los paquetes 'broom' y 'dplyr' antes de usar esta función.")
  }

  # Extraer la variable independiente (categoría)
  variable_independiente <- all.vars(formula)[2]

  # Verificar que la variable independiente exista en los datos
  if (!variable_independiente %in% colnames(data)) {
    stop("La variable independiente especificada en la fórmula no existe en los datos.")
  }

  # Calcular el número total de participantes (n)
  n <- nrow(data)

  # Aplicar la prueba de Kruskal-Wallis
  resultado <- kruskal.test(formula, data = data)

  # Convertir el resultado a un data frame con broom::glance
  resultado_glance <- broom::glance(resultado)

  # Calcular epsilon cuadrado
  resultado_glance <- resultado_glance %>%
    dplyr::mutate(
      Epsilon = statistic / ((n^2 - 1) / (n + 1)),  # Calcular Epsilon cuadrado
      Interpretación = dplyr::case_when(
        Epsilon >= 0.50 ~ "Grande",
        Epsilon >= 0.30 ~ "Mediano",
        Epsilon >= 0.10 ~ "Pequeño",
        TRUE ~ "No significativo"
      )
    )

  return(resultado_glance)
}
