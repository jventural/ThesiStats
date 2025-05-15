process_likert_blocks <- function(df, specs) {
  # Carga de librerías necesarias
  library(dplyr)
  library(ThesiStats)

  # 1. Para cada bloque de especificaciones, generar nombres de columnas y aplicar conversión
  exprs_list <- lapply(specs, function(spec) {
    # Generar vectores de nombres: p.ej. AM1, AM2, ..., AM16
    cols <- paste0(spec$prefix, seq_len(spec$n_items))
    # Verificar que existan en df
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Columnas faltantes en df: ", paste(missing_cols, collapse = ", "))
    }
    # Aplicar detección y conversión
    df %>%
      select(all_of(cols)) %>%
      detect_expression_Likert(
        start_zero    = spec$start_zero,
        likert_levels = spec$levels
      ) %>%
      ThesiStats::convert_to_expresions()
  })

  # 2. Construir mapeo de columnas para remplace_alternative_response
  column_mappings <- Map(function(spec, expr_df) {
    start_col <- paste0(spec$prefix, "1")
    end_col   <- paste0(spec$prefix, spec$n_items)
    list(
      c(start_col, end_col),
      expr_df
    )
  }, specs, exprs_list)

  # 3. Aplicar el reemplazo y devolver el dataframe modificado
  remplace_alternative_response(df, column_mappings)
}
