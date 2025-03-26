detect_expression_Likert <- function(df, start_zero = TRUE,
                                     likert_levels = c("Completamente en desacuerdo", "En desacuerdo", "Me es indiferente", "De acuerdo", "Completamente de acuerdo")) {

  library(dplyr)
  library(tidyr)
  library(stringr)

  # Función para normalizar las expresiones
  normalize_expression <- function(expr) {
    expr %>%
      str_to_lower() %>%
      str_replace_all("á", "a") %>%
      str_replace_all("é", "e") %>%
      str_replace_all("í", "i") %>%
      str_replace_all("ó", "o") %>%
      str_replace_all("ú", "u") %>%
      str_replace_all("ü", "u") %>%
      str_trim()
  }

  # Normalizar los niveles de respuesta
  likert_levels_normalized <- sapply(likert_levels, normalize_expression)

  # Convertir el data frame a formato largo y filtrar las respuestas que coincidan
  df_long <- df %>%
    pivot_longer(everything(), names_to = "column", values_to = "original_expression") %>%
    mutate(normalized_expression = normalize_expression(original_expression)) %>%
    filter(normalized_expression %in% likert_levels_normalized)

  # Verificar si faltan niveles de respuesta en los datos
  missing_levels <- likert_levels[!(likert_levels_normalized %in% df_long$normalized_expression)]
  if(length(missing_levels) > 0) {
    warning("Los siguientes niveles Likert no se encontraron en la data: ", paste(missing_levels, collapse = ", "))
  }

  # Convertir las respuestas a factor usando los niveles predefinidos
  df_long <- df_long %>%
    mutate(Alternativas = factor(original_expression, levels = likert_levels))

  # Extraer las alternativas únicas (sin duplicados)
  df_unique <- df_long %>%
    distinct(Alternativas, .keep_all = TRUE) %>%
    arrange(Alternativas)

  # Asignar la secuencia de puntajes
  if (start_zero) {
    df_unique <- df_unique %>% mutate(score = row_number() - 1)
  } else {
    df_unique <- df_unique %>% mutate(score = row_number())
  }

  df_unique %>% select(Alternativas, score)
}
