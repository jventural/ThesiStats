detect_Likert <- function(df) {
  # Cargar las bibliotecas necesarias
  library(dplyr)
  library(tidyr)
  # Combinar todas las columnas en una sola columna de texto
  combined <- df %>%
    pivot_longer(everything(), names_to = "column", values_to = "expression") %>%
    select(expression)

  # Obtener las expresiones únicas
  expresiones_unicas <- unique(combined$expression)

  # Retornar las expresiones únicas como una columna en un dataframe
  tibble(Alternativas = expresiones_unicas)
}
