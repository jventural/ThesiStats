detect_expression_Likert <- function(df, start_zero = TRUE) {
  library(dplyr)
  library(tidyr)
  library(stringr)

  detect_Likert2 <- function(df) {
    # Definir las respuestas de Likert y sus puntuaciones
    likert_responses <- data.frame(
      normalized_expression = c(
        "totalmente de acuerdo", "de acuerdo", "indiferente", "en desacuerdo", "totalmente en desacuerdo",
        "definitivamente si", "probablemente si", "indeciso", "probablemente no", "definitivamente no",
        "nunca", "a veces", "generalmente", "muchisimas veces", "siempre",
        "nunca", "rara vez", "algunas veces", "casi siempre", "siempre",
        "la mayoria de las veces si", "algunas veces si", "algunas veces no", "la mayoria de las veces no",
        "completamente verdadero", "verdadero", "ni falso ni verdadero", "falso", "completamente falso",
        "para nada", "varios dias", "mas de la mitad los dias", "casi todos los dias",
        "nunca", "a veces", "casi siempre", "siempre",
        "nunca", "casi nunca", "algunas veces", "regularmente", "bastantes veces", "casi siempre", "siempre",
        "nunca", "algunas veces", "bastantes veces", "siempre",
        "nunca", "casi nunca", "a veces", "casi siempre", "siempre",
        "nunca", "pocas veces", "regular", "muchas veces", "siempre",
        "totalmente en desacuerdo", "en desacuerdo", "de acuerdo", "muy de acuerdo", "totalmente de acuerdo"
      ),
      score = c(
        5, 4, 3, 2, 1,  # actualizando las puntuaciones para "totalmente de acuerdo", "de acuerdo", "indiferente", "en desacuerdo", "totalmente en desacuerdo"
        5, 4, 3, 2, 1,
        0, 1, 3, 4, 5,
        0, 2, 3, 4, 4,
        3, 2, 1, 1,
        5, 4, 3, 2, 1,
        1, 2, 3, 4,
        0, 1, 2, 3,
        1, 2, 3, 4, 5, 6, 7,
        1, 2, 3, 4,
        1, 2, 3, 4, 5,
        0, 1, 2, 3, 4,
        1, 2, 3, 4, 5
      )
    )

    # Eliminar duplicados en likert_responses
    likert_responses <- likert_responses %>%
      distinct(normalized_expression, .keep_all = TRUE)

    # Normalizar las expresiones para comparación
    normalize_expression <- function(expression) {
      expression %>%
        str_to_lower() %>%
        str_replace_all("á", "a") %>%
        str_replace_all("é", "e") %>%
        str_replace_all("í", "i") %>%
        str_replace_all("ó", "o") %>%
        str_replace_all("ú", "u") %>%
        str_replace_all("ü", "u") %>%
        str_trim()  # Eliminar espacios en blanco al principio y al final
    }

    likert_responses$normalized_expression <- sapply(likert_responses$normalized_expression, normalize_expression)

    # Preparar los datos, manteniendo la forma original y creando una versión normalizada para la comparación
    df <- df %>%
      pivot_longer(everything(), names_to = "column", values_to = "original_expression") %>%
      mutate(normalized_expression = normalize_expression(original_expression)) %>%
      distinct(column, normalized_expression, .keep_all = TRUE)

    # Unir con el marco de datos de respuestas Likert para asignar puntuaciones y mantener el orden según la puntuación
    combined <- df %>%
      left_join(likert_responses, by = "normalized_expression") %>%
      filter(!is.na(score)) %>%
      select(original_expression, score) %>%
      distinct() %>%
      arrange(score)  # Ordenar en función de la puntuación de manera ascendente

    # Devolver las expresiones en su forma original con el orden de las puntuaciones y sus valores numéricos
    combined$score <- NULL  # Eliminar la columna de puntuación
    tibble(Alternativas = combined$original_expression)
  }

  agregar_secuencia <- function(df, start_zero = TRUE) {
    if (start_zero) {
      df <- df %>%
        mutate(score = rev(1:n()))
    } else {
      df <- df %>%
        mutate(score = rev(1:n()))
    }
    return(df)
  }

  expresiones_ordenadas_df <- detect_Likert2(df)
  expresiones_ordenadas_df <- agregar_secuencia(expresiones_ordenadas_df, start_zero)
  return(expresiones_ordenadas_df)
}
