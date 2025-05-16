clean_edad <- function(df, col_name = "Edad", round_decimals = 2) {
  # Instala y carga dplyr, stringr y readr
  for (pkg in c("dplyr", "stringr", "readr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }

  # Número de filas antes
  n_before <- nrow(df)

  df_clean <- df %>%
    # 1) Filtrar filas que contienen al menos un dígito
    filter(stringr::str_detect(.data[[col_name]], "\\d")) %>%
    mutate(
      # 2) Texto limpio: minúsculas y espacios extra reducidos
      .clean = stringr::str_to_lower(stringr::str_squish(.data[[col_name]])),

      # 3) Extraer años (p.ej. "2 años", "un año")
      years = dplyr::case_when(
        stringr::str_detect(.clean, "años?") ~ as.numeric(
          stringr::str_replace(
            stringr::str_extract(.clean, "\\d+[\\.,]?\\d*(?=\\s*años?)"),
            ",", ".")
        ),
        stringr::str_detect(.clean, "\\bun año\\b") ~ 1,
        TRUE ~ NA_real_
      ),

      # 4) Extraer meses explícitos (p.ej. "5 meses")
      months = dplyr::case_when(
        stringr::str_detect(.clean, "meses?") ~ as.numeric(
          stringr::str_replace(
            stringr::str_extract(.clean, "\\d+[\\.,]?\\d*(?=\\s*meses?)"),
            ",", ".")
        ),
        TRUE ~ NA_real_
      ),

      # 5) Extraer cualquier número puro al comienzo o en la cadena
      pure_years = readr::parse_number(.clean),

      # 6) Calcular el total en años: usar 'years' si existe, si no 'pure_years',
      #    y agregar meses/12
      total_years = dplyr::coalesce(years, pure_years, 0) +
        dplyr::coalesce(months, 0) / 12,

      # 7) Redondear y sobrescribir la columna original
      !!col_name := round(total_years, digits = round_decimals)
    ) %>%
    # 8) Eliminar columnas auxiliares
    select(-.clean, -years, -months, -pure_years, -total_years)

  # Filas excluidas
  n_after <- nrow(df_clean)
  excluidas <- n_before - n_after
  message(sprintf(
    "%d fila%s excluíd%s por no contener ningún número en '%s'",
    excluidas,
    ifelse(excluidas == 1, "", "s"),
    ifelse(excluidas == 1, "a", "as"),
    col_name
  ))

  df_clean
}
