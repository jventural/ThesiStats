detect_Likert <- function(df) {
  library(dplyr)
  library(tidyr)
  library(stringr)  # Para manipulación de cadenas

  # Definir las respuestas de Likert y sus puntuaciones
  likert_responses <- data.frame(
    normalized_expression = c(
      "Totalmente de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Totalmente en desacuerdo",
      "Definitivamente si", "Probablemente si", "Indeciso", "Probablemente no", "Definitivamente no",
      "Nunca", "A veces", "Generalmente", "Muchísimas veces", "Siempre",
      "Nunca", "Rara vez", "Algunas veces", "Casi Siempre", "Siempre",
      "La mayoría de las veces si", "Algunas veces si", "algunas veces no", "La mayoría de las veces no",
      "Completamente verdadero", "Verdadero", "Ni falso ni verdadero", "Falso", "Completamente falso",
      "Para nada", "Varios días", "Más de la mitad los días", "Casi todos los días"
    ),
    score = rep(1:5, length.out = 33)
  )

  # Normalizar las expresiones para comparación
  normalize_expression <- function(expression) {
    expression %>%
      str_to_lower() %>%
      str_replace_all("á", "a") %>%
      str_replace_all("é", "e") %>%
      str_replace_all("í", "i") %>%
      str_replace_all("ó", "o") %>%
      str_replace_all("ú", "u") %>%
      str_replace_all("ü", "u")
  }

  likert_responses$normalized_expression <- sapply(likert_responses$normalized_expression, normalize_expression)

  # Preparar los datos, manteniendo la forma original y creando una versión normalizada para la comparación
  df <- df %>%
    pivot_longer(everything(), names_to = "column", values_to = "original_expression") %>%
    mutate(normalized_expression = normalize_expression(original_expression)) %>%
    distinct(column, normalized_expression, .keep_all = TRUE)

  # Unir con el marco de datos de respuestas Likert para asignar puntuaciones y mantener el orden según la puntuación
  combined <- df %>%
    left_join(likert_responses, by = c("normalized_expression" = "normalized_expression")) %>%
    filter(!is.na(score)) %>%
    select(original_expression, score) %>%
    distinct() %>%
    arrange(desc(score))  # Ordenar en función de la puntuación de manera descendente

  # Devolver las expresiones en su forma original con el orden de las puntuaciones
  tibble(Alternativas = combined$original_expression)
}
