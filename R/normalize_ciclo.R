normalize_ciclo <- function(df, col_name = "Ciclo") {
  # 0) Dependencias
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  library(dplyr,    quietly = TRUE, warn.conflicts = FALSE)
  library(stringr,  quietly = TRUE, warn.conflicts = FALSE)
  library(readr,    quietly = TRUE, warn.conflicts = FALSE)

  df2 <- df %>%
    mutate(
      .orig       = as.character(.data[[col_name]]),
      # 1) Limpiar: pasar a minúsculas, recortar espacios
      .clean      = str_to_lower(.orig) %>% str_squish(),
      # 2) Quitar la palabra "ciclo(s)"
      .clean      = str_remove_all(.clean, regex("\\b(ciclo|ciclos)\\b", ignore_case = TRUE)) %>% str_squish(),
      # 3) Quitar sólo símbolos de grado
      .clean      = str_remove_all(.clean, "[º°]") %>% str_squish(),
      # 4) Extraer número (mantiene el punto decimal)
      .num_numeric= parse_number(.clean),
      # 5) Detectar números romanos puros
      .roman      = if_else(
        str_detect(.clean,
                   regex("^(m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3}))$",
                         ignore_case = TRUE)
        ),
        .clean,
        NA_character_
      ),
      .num_roman  = suppressWarnings(as.integer(as.roman(str_to_upper(.roman)))),
      # 6) Mapeo de palabras ordinales
      .num_word   = case_when(
        str_detect(.clean, regex("^(primer|primero|1er|1ero)$", ignore_case = TRUE))   ~  1L,
        str_detect(.clean, regex("^(segundo|2do)$",         ignore_case = TRUE))      ~  2L,
        str_detect(.clean, regex("^(tercer|tercero|3er|3ro)$",ignore_case = TRUE))   ~  3L,
        str_detect(.clean, regex("^(cuarto|4to)$",          ignore_case = TRUE))      ~  4L,
        str_detect(.clean, regex("^(quinto|5to)$",          ignore_case = TRUE))      ~  5L,
        str_detect(.clean, regex("^(sexto|6to)$",           ignore_case = TRUE))      ~  6L,
        str_detect(.clean, regex("^(septimo|séptimo|7mo)$", ignore_case = TRUE))      ~  7L,
        str_detect(.clean, regex("^(octavo|8vo)$",          ignore_case = TRUE))      ~  8L,
        str_detect(.clean, regex("^(noveno|9no)$",          ignore_case = TRUE))      ~  9L,
        str_detect(.clean, regex("^(decimo|d[eé]cimo|10mo)$",ignore_case = TRUE))    ~ 10L,
        str_detect(.clean, regex("^(once|11avo|11ero)$",     ignore_case = TRUE))      ~ 11L,
        TRUE                                                                               ~ NA_integer_
      ),
      # 7) Unir todo
      .num        = coalesce(
        as.integer(.num_numeric),
        .num_roman,
        .num_word
      )
    )

  # 8) Reportar casos sin normalizar
  problematic <- df2 %>%
    filter(is.na(.num)) %>%
    select(Original = .orig, Limpio = .clean) %>%
    distinct()

  if (nrow(problematic) > 0) {
    message("Se eliminaron ", nrow(problematic),
            " fila(s) por no poder normalizar '", col_name, "':")
    print(problematic, n = nrow(problematic))
  }

  # 9) Devolver sólo los casos válidos, con la columna reemplazada por entero
  df2 %>%
    filter(!is.na(.num)) %>%
    mutate(
      !!col_name := .num
    ) %>%
    select(-.orig, -.clean, -.num_numeric, -.roman, -.num_roman, -.num_word, -.num)
}
