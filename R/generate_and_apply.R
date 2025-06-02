generate_and_apply <- function(df, text_lines, new_name = "df_new") {
  library(dplyr)
  # df:         data.frame original
  # text_lines: carácter vector, cada línea con "Factor: Ítem1, Ítem2, Ítem3, ..."
  # new_name:   nombre (string) para el data.frame resultante en Global Environment

  # 1) Extraer nombres de factor e ítems de cada línea de texto
  factors <- gsub(":.+$", "", text_lines)
  items   <- gsub("^.+:\\s*", "", text_lines)
  items   <- lapply(items, function(x) unlist(strsplit(x, ",\\s*")))

  # 2) Hacemos una copia de df para ir construyendo las columnas nuevas
  df_new <- df

  # 3) Para cada factor, calculamos la suma de los ítems correspondientes
  for (i in seq_along(factors)) {
    fac_name <- factors[i]
    vars_i   <- items[[i]]

    # Verificamos que todas las columnas existan en df_new
    missing_vars <- setdiff(vars_i, names(df_new))
    if (length(missing_vars) > 0) {
      stop(
        sprintf(
          "No se encontraron estas columnas en el data.frame original: %s",
          paste(missing_vars, collapse = ", ")
        )
      )
    }

    # Convertimos las columnas a numéricas si no lo fueran
    df_new <- df_new %>%
      mutate(across(all_of(vars_i), as.numeric))

    # Creamos la columna de suma de ítems:
    # rowSums() ignora NA por defecto si na.rm = TRUE
    df_new[[fac_name]] <- rowSums(df_new[, vars_i, drop = FALSE], na.rm = TRUE)
  }

  # 4) Asignamos df_new al nombre que el usuario haya indicado
  assign(new_name, df_new, envir = .GlobalEnv)

  # 5) Retornamos invisiblemente el data.frame (por si quieren capturarlo)
  invisible(df_new)
}
