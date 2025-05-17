normalize_universidades <- function(df, col_name) {
  # 1) Instalar y cargar dependencias
  for (pkg in c("dplyr", "stringr", "stringdist")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }

  # 2) Tomar la referencia interna de tu paquete ThesiStats
  ref_df <- ThesiStats::universidades_peruanas

  original <- df[[col_name]]

  # 3) Función que normaliza un solo nombre
  match_one <- function(x) {
    x_clean <- x %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish()

    # calcular distancia Jaro–Winkler a nombres y acrónimos
    d_name <- stringdist::stringdist(x_clean,
                                     tolower(ref_df$Nombre),
                                     method = "jw")
    d_acro <- stringdist::stringdist(x_clean,
                                     tolower(ref_df$Acrónimo),
                                     method = "jw")

    min_name <- min(d_name, na.rm = TRUE); i_name <- which.min(d_name)
    min_acro <- min(d_acro, na.rm = TRUE); i_acro <- which.min(d_acro)

    if      (min_name <= min_acro && min_name <= 0.15) ref_df$Acrónimo[i_name]
    else if (min_acro <  min_name && min_acro <= 0.15) ref_df$Acrónimo[i_acro]
    else                                          NA_character_
  }

  # 4) Aplicar a toda la columna
  matched <- vapply(original, match_one, FUN.VALUE = "", USE.NAMES = FALSE)

  # 5) Reportar los que quedaron sin normalizar
  no_match <- unique(original[is.na(matched)])
  if (length(no_match) > 0) {
    message("No se pudieron normalizar estas universidades y quedaron NA:\n",
            paste0(" • ", no_match, collapse = "\n"))
  } else {
    message("✅ Todas las universidades se normalizaron correctamente.")
  }

  # 6) Insertar la columna normalizada justo después de la original
  df %>%
    mutate(!!paste0(col_name, "_norm") := matched) %>%
    relocate(!!paste0(col_name, "_norm"), .after = all_of(col_name))
}
