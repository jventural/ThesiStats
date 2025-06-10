normalize_carreras <- function(df,
                               col_name    = "Carrera",
                               max_dist    = 0.15,
                               facultad    = FALSE,
                               manual_path = NULL) {
  # 0. instalar/cargar dependencias
  for (pkg in c("dplyr", "stringr", "stringdist", "stringi", "readxl")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }

  # 1. leer catálogo de carreras (interno)
  ref_df <- ThesiStats::carreras_peruanas
  ref_clean <- ref_df$Carrera %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_squish()

  # 2. leer diccionario manual desde Excel si se proporciona ruta
  if (!is.null(manual_path)) {
    manual_df <- readxl::read_excel(manual_path)
    # columnas esperadas: "raw" y "normalized"
    keys <- manual_df$raw %>%
      tolower() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish()
    vals <- manual_df$normalized
    dic_manual <- setNames(vals, keys)
  } else {
    dic_manual <- NULL
  }

  # 3. procesar cada valor original
  original <- df[[col_name]]
  clean0 <- original %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_squish()

  # 4. primer paso: recodificar con diccionario manual
  clean_prep <- if (!is.null(dic_manual)) {
    ifelse(clean0 %in% names(dic_manual), dic_manual[clean0], original)
  } else {
    original
  }

  # 5. función de matching fuzzy
  match_index <- function(x) {
    x_clean <- x %>%
      tolower() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish()
    dists <- stringdist::stringdist(x_clean, ref_clean, method = "jw")
    i_min <- which.min(dists)
    if (length(i_min) == 1 && dists[i_min] <= max_dist) i_min else NA_integer_
  }

  # 6. aplicar matching a cada valor
  idx <- vapply(clean_prep, match_index, integer(1), USE.NAMES = FALSE)

  # 7. armar columnas de salida
  carreras_norm   <- ifelse(is.na(idx), NA_character_, ref_df$Carrera[idx])
  facultades_norm <- if (facultad) {
    ifelse(is.na(idx), NA_character_, ref_df$Facultad[idx])
  }

  # 8. mensaje de no-coincidencia
  no_match <- unique(original[is.na(carreras_norm)])
  if (length(no_match) > 0) {
    message("❗ Las siguientes carreras quedaron NA:\n",
            paste0(" • ", no_match, collapse = "\n"))
  } else {
    message("✅ Todas las carreras se normalizaron.")
  }

  # 9. devolver data.frame con columnas nuevas
  out <- df %>%
    mutate(!!paste0(col_name, "_norm") := carreras_norm) %>%
    relocate(!!paste0(col_name, "_norm"), .after = all_of(col_name))

  if (facultad) {
    out <- out %>%
      mutate(!!paste0(col_name, "_facultad") := facultades_norm) %>%
      relocate(!!paste0(col_name, "_facultad"),
               .after = all_of(paste0(col_name, "_norm")))
  }

  out
}
