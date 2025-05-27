edad_stat <- function(obj, columna) {
  # 1) Cargar dependencias
  pkgs <- c("dplyr", "rlang")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE))
      install.packages(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }

  # 2) Si 'obj' es la salida de convert_age_to_years* extraer el data.frame limpio
  df <- if (is.list(obj) && "cleaned_df" %in% names(obj)) {
    obj$cleaned_df
  } else if (is.data.frame(obj)) {
    obj
  } else {
    stop("El primer argumento debe ser un data.frame o la lista retornada por convert_age_to_years*")
  }

  # 3) Soportar columna como bare name o string
  col_sym <- if (is.character(columna)) {
    rlang::sym(columna)
  } else {
    rlang::enquo(columna)
  }

  # 4) Calcular estadísticas
  df %>%
    summarise(
      Media               = mean(!!col_sym, na.rm = TRUE),
      DesviacionEstandar  = sd(!!col_sym,   na.rm = TRUE),
      Minimo              = min(!!col_sym,  na.rm = TRUE),
      Maximo              = max(!!col_sym,  na.rm = TRUE)
    )
}
