normalize_ciclo <- function(df, col_name = "Ciclo") {
  # Instala y carga dplyr
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  # Instala y carga stringr
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  library(stringr, quietly = TRUE, warn.conflicts = FALSE)
  # Instala y carga readr
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  library(readr, quietly = TRUE, warn.conflicts = FALSE)

  df %>%
    mutate(
      # 1. Limpieza básica
      .clean = str_to_lower(str_squish(.data[[col_name]])),

      # 2. Extrae cualquier número sin warnings
      .num = suppressWarnings(parse_number(.clean)),

      # 3. Detecta romanos completos
      .roman = str_extract(.clean,
                           "^(?i)(?:m{0,4}(?:cm|cd|d?c{0,3})(?:xc|xl|l?x{0,3})(?:ix|iv|v?i{0,3}))$"
      ),
      .num = coalesce(
        .num,
        if_else(!is.na(.roman),
                as.numeric(as.roman(str_to_upper(.roman))),
                NA_real_)
      ),

      # 4. Mapea ordinales textuales en español, con sufijo opcional ' ciclo'
      .num = case_when(
        str_detect(.clean, "^(primero|1ero)(?: ciclo)?$")       ~ 1,
        str_detect(.clean, "^(segundo|2do)(?: ciclo)?$")        ~ 2,
        str_detect(.clean, "^(tercer|3ro)(?: ciclo)?$")         ~ 3,
        str_detect(.clean, "^(cuarto|4to)(?: ciclo)?$")         ~ 4,
        str_detect(.clean, "^(quinto|5to)(?: ciclo)?$")         ~ 5,
        str_detect(.clean, "^(sexto|6to)(?: ciclo)?$")          ~ 6,
        str_detect(.clean, "^(s[eé]ptimo|7mo)(?: ciclo)?$")     ~ 7,
        str_detect(.clean, "^(octavo|8vo)(?: ciclo)?$")         ~ 8,
        str_detect(.clean, "^(noveno|9no)(?: ciclo)?$")         ~ 9,
        str_detect(.clean, "^(d[eé]cimo|10mo)(?: ciclo)?$")     ~ 10,
        str_detect(.clean, "^(once|11)(?:avo|ero)?(?: ciclo)?$") ~ 11,
        TRUE                                                   ~ .num
      ),

      # 5. Sobrescribe y convierte a entero
      !!sym(col_name) := as.integer(.num)
    ) %>%
    # Limpia columnas temporales
    select(-.clean, -.num, -.roman)
}
