normalize_carreras <- function(df, col_name = "Carrera", max_dist = 0.15, facultad = FALSE) {
  # instalar y cargar dependencias
  for (pkg in c("dplyr", "stringr", "stringdist", "stringi")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }

  # catálogo interno: columnas "Carrera" y "Facultad"
  ref_df <- ThesiStats::carreras_peruanas

  # limpiamos el catálogo para comparar
  ref_clean <- ref_df$Carrera %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_squish()

  original <- df[[col_name]]

  # función que devuelve índice de coincidencia o NA
  match_index <- function(x) {
    x_clean <- x %>%
      tolower() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish()

    dists <- stringdist::stringdist(x_clean, ref_clean, method = "jw")
    i_min <- which.min(dists)
    if (length(i_min)==1 && dists[i_min] <= max_dist) i_min else NA_integer_
  }

  # calculamos índices de match
  idx <- vapply(original, match_index, FUN.VALUE = integer(1), USE.NAMES = FALSE)

  # generamos las columnas normalizadas
  carreras_norm   <- ifelse(is.na(idx), NA_character_, ref_df$Carrera[idx])
  facultades_norm <- if (facultad) {
    ifelse(is.na(idx), NA_character_, ref_df$Facultad[idx])
  } else {
    NULL
  }

  # informar no normalizados
  no_match <- unique(original[is.na(carreras_norm)])
  if (length(no_match) > 0) {
    message("❗ Las siguientes carreras quedaron NA:\n",
            paste0(" • ", no_match, collapse = "\n"))
  } else {
    message("✅ Todas las carreras se normalizaron.")
  }

  # añadimos columnas de resultado
  out <- df %>%
    dplyr::mutate(
      !!paste0(col_name, "_norm") := carreras_norm
    ) %>%
    dplyr::relocate(!!paste0(col_name, "_norm"), .after = all_of(col_name))

  if (facultad) {
    out <- out %>%
      dplyr::mutate(
        !!paste0(col_name, "_facultad") := facultades_norm
      ) %>%
      dplyr::relocate(!!paste0(col_name, "_facultad"), .after = all_of(paste0(col_name, "_norm")))
  }

  out
}
