detect_expression_Likert <- function(df, start_zero = TRUE) {
  library(dplyr)
  library(tidyr)
  library(stringr)


  # Función para detectar y ordenar expresiones Likert
  detect_Likert2 <- function(df) {
    # Definir las respuestas de Likert y sus puntuaciones
    likert_responses <- data.frame(
      normalized_expression = c(
        # Expresiones del ejemplo de las nuevas imágenes con numeración
        "moderadamente problematico", "muy problematico", "nada problematico", "solo ligeramente problematico",
        "bastante buena", "bastante mala", "muy buena", "muy mala",

        # Expresiones anteriores
        "nunca se queda dormido", "escasa probabilidad de quedarse dormido",
        "moderada probabilidad de quedarse dormido", "alta probabilidad de quedarse dormido",
        "ninguna vez en el ultimo mes", "menos de una vez a la semana",
        "una o dos veces a la semana", "tres o mas veces a la semana",
        "casi nunca", "casi siempre", "con frecuencia", "en ocasiones", "nunca", "siempre",
        "completamente de acuerdo", "completamente en desacuerdo", "de acuerdo",
        "en desacuerdo", "me es indiferente",
        "completamente falso de mi", "la mayor parte falso de mi", "ligeramente mas verdadero que falso",
        "moderadamente verdadero de mi", "la mayor parte verdadero de mi", "me describe perfectamente",

        # Expresiones clásicas de Likert
        "totalmente de acuerdo", "de acuerdo", "indiferente", "en desacuerdo", "totalmente en desacuerdo",
        "definitivamente si", "probablemente si", "indeciso", "probablemente no", "definitivamente no",

        # Expresiones para frecuencia
        "nunca", "a veces", "con frecuencia", "muchísimas veces", "siempre",

        # Otros patrones Likert existentes
        "nunca", "rara vez", "algunas veces", "casi siempre", "siempre",
        "la mayoria de las veces si", "algunas veces si", "algunas veces no", "la mayoria de las veces no",
        "completamente verdadero", "verdadero", "ni falso ni verdadero", "falso", "completamente falso",
        "para nada", "varios dias", "mas de la mitad los dias", "casi todos los dias"
      ),

      # Puntaje correspondiente para cada expresión de menor a mayor intensidad
      score = c(
        # Puntajes asignados a las nuevas expresiones de las imágenes (0 a 3)
        2, 3, 0, 1,  # Para la imagen de CS12 (problemático)
        2, 3, 0, 1,  # Para la imagen de CS13 (buena/mala)

        # Puntajes para el resto de expresiones
        0, 1, 2, 3,
        0, 1, 2, 3,
        1, 5, 3, 2, 0, 4,
        5, 1, 4, 2, 3,
        1, 2, 3, 4, 5, 6,
        5, 4, 3, 2, 1,
        5, 4, 3, 2, 1,
        0, 1, 2, 3, 4,
        0, 1, 2, 3, 4,
        3, 2, 1, 0,
        5, 4, 3, 2, 1,
        0, 1, 2, 3
      )
    )

    # Eliminar duplicados en likert_responses para evitar conflictos
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

  # Función para agregar la secuencia de puntajes si se requiere
  agregar_secuencia <- function(df, start_zero = TRUE) {
    if (start_zero) {
      df <- df %>%
        mutate(score = row_number() - 1)
    } else {
      df <- df %>%
        mutate(score = row_number())
    }
    return(df)
  }

  # Aplicar las funciones a las expresiones detectadas
  expresiones_ordenadas_df <- detect_Likert2(df)
  expresiones_ordenadas_df <- agregar_secuencia(expresiones_ordenadas_df, start_zero)
  return(expresiones_ordenadas_df)
}
