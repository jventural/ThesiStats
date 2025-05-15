normalize_tiempo_relacion <- function(df, col_name = "Tiempo_Relacion") {
  # Instala y carga dplyr y stringr
  for (pkg in c("dplyr","stringr")) {
    if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg)
    library(pkg, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE)
  }

  df %>%
    mutate(
      # 1) Texto limpio
      .clean = str_to_lower(str_squish(.data[[col_name]])),

      # 2) Años
      years = case_when(
        str_detect(.clean, "\\baños?\\b") ~ as.numeric(str_replace(
          str_extract(.clean, "\\d+[\\.,]?\\d*(?=\\s*años?)"),
          ",", ".")),
        str_detect(.clean, "\\bun año\\b")   ~ 1,
        TRUE                                ~ NA_real_
      ),

      # 3) Meses explícitos
      months_explicit = case_when(
        str_detect(.clean, "meses?") ~ as.numeric(str_replace(
          str_extract(.clean, "\\d+[\\.,]?\\d*(?=\\s*meses?)"),
          ",", ".")),
        TRUE                         ~ NA_real_
      ),

      # 4) Medio año
      half_year_months = if_else(
        str_detect(.clean, "medio") & str_detect(.clean, "año"),
        6, 0
      ),

      # 5) Valores puramente numéricos → meses
      pure_numeric = str_detect(.clean, "^[0-9]+(?:[\\.,][0-9]+)?$"),
      months_pure = case_when(
        pure_numeric ~ suppressWarnings(
          as.numeric(str_replace(.clean, ",", ".")) ),
        TRUE         ~ NA_real_
      ),

      # 6) Total en meses
      total_months = coalesce(years, 0)*12 +
        coalesce(months_explicit, 0) +
        coalesce(months_pure, 0) +
        half_year_months,

      # 7) Sobrescribe la columna original
      !!sym(col_name) := total_months
    ) %>%
    # 8) Limpia auxiliares
    select(-.clean, -years, -months_explicit, -half_year_months,
           -pure_numeric, -months_pure, -total_months)
}
