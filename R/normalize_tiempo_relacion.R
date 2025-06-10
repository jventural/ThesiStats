normalize_tiempo_relacion <- function(df,
                                      col_name = "Tiempo_Relacion",
                                      remover   = TRUE) {
  # 0) Dependencias
  for (pkg in c("dplyr", "stringr", "readr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }

  df2 <- df %>%
    mutate(
      # Original para comparar
      !!col_name := as.character(.data[[col_name]]),
      .clean = str_to_lower(str_squish(.data[[col_name]])),

      # Casos especiales a priorizar
      meses_especial = case_when(
        .clean == "7 casi 8"                          ~ 8,
        str_replace_all(.clean, '\\s+', '') == "1añoy6meses" ~ 18,
        .clean == "3 años y 8 meses"                  ~ 44,
        .clean == "1 año 5 meses"                     ~ 17,
        str_detect(.clean, "^1 año y tres meses$")     ~ 15,
        str_detect(.clean, "^un año con dos meses$")   ~ 14,
        str_detect(.clean, "^1 año y cuatro meses$")   ~ 16,
        str_detect(.clean, "^1 año con 11 meses$")     ~ 23,
        str_detect(.clean, "^1 y 7 meses$")            ~ 19,
        TRUE                                            ~ NA_real_
      ),

      # 1) "n casi m" → m
      meses_casi = if_else(
        str_detect(.clean, "\\d+\\s+casi\\s+\\d+"),
        as.numeric(str_extract(.clean, "(?<=casi\\s)\\d+")),
        NA_real_
      ),

      # 2) "X años y Y meses" o "X año Y meses" → X*12 + Y
      meses_combo = {
        m <- str_match(
          .clean,
          regex("(?i)^(\\d+)\\s*años?\\s*(?:y\\s*)?(\\d+)\\s*meses?", ignore_case = TRUE)
        )
        x <- as.numeric(m[,2])
        y <- as.numeric(m[,3])
        ifelse(!is.na(m[,1]), x * 12 + y, NA_real_)
      },

      # 3) Semanas → meses (7/30), redondeado a 1 decimal
      meses_semana = if_else(
        str_detect(.clean, "\\d+[\\.,]?\\d*\\s*semanas?"),
        round(
          as.numeric(str_replace(
            str_extract(.clean, "\\d+[\\.,]?\\d*(?=\\s*semanas?)"),
            ",", "."))
          * 7/30,
          1
        ),
        NA_real_
      ),

      # 4) Años y medio y años simples
      years = case_when(
        str_detect(.clean, "\\b(un|una) año[s]? y medio\\b")        ~ 1.5,
        str_detect(.clean, "\\d+\\s*año[s]? y medio\\b")         ~ as.numeric(
          str_extract(.clean, "\\d+(?=\\s*año)")) + 0.5,
        TRUE                                                          ~ NA_real_
      ),
      años = case_when(
        str_detect(.clean, "\\b(un|una) año[s]?\\b")               ~ 1,
        str_detect(.clean, "\\d+\\s*año[s]?\\b")                ~ as.numeric(
          str_extract(.clean, "\\d+(?=\\s*año)")),
        TRUE                                                          ~ NA_real_
      ),

      # 5) Meses explícitos y puros
      meses = case_when(
        str_detect(.clean, "\\b(un|una) mes[es]?\\b")              ~ 1,
        str_detect(.clean, "\\d+[\\.,]?\\d*\\s*meses?\\b")   ~ as.numeric(
          str_replace(
            str_extract(.clean, "\\d+[\\.,]?\\d*(?=\\s*mes)"),
            ",", ".")),
        TRUE                                                         ~ NA_real_
      ),
      num_puro = suppressWarnings(as.numeric(str_replace(.clean, ",", "."))),
      meses_puro = if_else(
        !is.na(num_puro) & !str_detect(.clean, "años?|meses?|semanas?"),
        num_puro,
        NA_real_
      ),

      # 6) "casi" sin número → 0
      meses_casi_solo = if_else(
        str_detect(.clean, "^casi\\b"),
        0,
        NA_real_
      ),

      # 7) Suma en meses, por orden de prioridad
      total_months = coalesce(
        meses_especial,
        meses_casi,
        meses_combo,
        meses_semana,
        years   * 12,
        años    * 12,
        meses,
        meses_puro,
        0
      )
    )

  # 8) Filas problemáticas
  problematic <- df2 %>%
    filter(is.na(total_months) & !is.na(.data[[col_name]])) %>%
    distinct(.data[[col_name]], .clean)

  # 9) Construir salida con columna normalizada y original
  df_out <- df2 %>%
    mutate(!!paste0(col_name, "_norm") := total_months) %>%
    select(-.clean, -meses_especial, -meses_casi, -meses_combo, -meses_semana,
           -years, -años, -meses, -num_puro, -meses_puro,
           -meses_casi_solo, -total_months)

  # 10) Remover o dejar NAs
  if (remover) {
    if (nrow(problematic) > 0) {
      message("Se eliminaron ", nrow(problematic),
              " fila(s) sin normalizar en '", col_name, "':")
      print(problematic, n = Inf)
    }
    df_out <- df_out %>% filter(!is.na(.data[[paste0(col_name, "_norm")]]))
  } else {
    if (nrow(problematic) > 0) {
      message("Se encontraron ", nrow(problematic),
              " fila(s) sin normalizar; se asignó NA en '",
              paste0(col_name, "_norm"), "'.")
    }
  }

  return(as.data.frame(df_out))
}
