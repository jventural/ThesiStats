#' Normalize Peruvian University Degree Names
#'
#' Matches free-text degree names (for example "psicologia", "Ing. de
#' Sistemas") against the catalog [carreras_peruanas] with fuzzy string
#' matching, and adds a column with the standardized name (and optionally the
#' faculty).
#'
#' @param df A data frame.
#' @param col_name Name of the column with the degree names. Defaults to
#'   `"Carrera"`.
#' @param max_dist Maximum string distance accepted as a match (0 to 1).
#' @param facultad Logical. Also add a column with the faculty.
#' @param manual_path Optional path to an Excel file with the columns `raw`
#'   and `normalized`, a manual dictionary applied before the fuzzy matching
#'   (requires the readxl package).
#' @param force_match Logical. Retry unmatched values with the more permissive
#'   `fallback_dist` (not recommended).
#' @param fallback_dist Distance used when `force_match = TRUE`.
#' @param remove_unmatched Logical. Remove the rows whose degree could not be
#'   normalized.
#'
#' @return `df` with the column `<col_name>_norm` (and `<col_name>_facultad`
#'   when `facultad = TRUE`) placed after `col_name`. Messages report the
#'   values that could not be normalized.
#' @export
#' @examples
#' df <- data.frame(Carrera = c("psicologia", "Derecho", "zzz"))
#' normalize_carreras(df)
normalize_carreras <- function(df,
                               col_name    = "Carrera",
                               max_dist    = 0.15,
                               facultad    = FALSE,
                               manual_path = NULL,
                               force_match = FALSE,  # forzar matching (no recomendado)
                               fallback_dist = 0.4,
                               remove_unmatched = FALSE) { # NUEVO: eliminar no compatibles
  if (!is.null(manual_path) && !requireNamespace("readxl", quietly = TRUE)) {
    stop("manual_path needs the 'readxl' package: install.packages(\"readxl\").",
         call. = FALSE)
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

  # 5. función de matching fuzzy MEJORADA
  match_index <- function(x, use_fallback = FALSE) {
    x_clean <- x %>%
      tolower() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish()

    # Probar diferentes métodos de distancia
    methods <- c("jw", "lv", "cosine", "jaccard")
    current_dist <- if (use_fallback) fallback_dist else max_dist

    best_match <- NA_integer_
    best_distance <- Inf

    for (method in methods) {
      dists <- stringdist::stringdist(x_clean, ref_clean, method = method)
      i_min <- which.min(dists)

      if (length(i_min) == 1 && dists[i_min] <= current_dist && dists[i_min] < best_distance) {
        best_match <- i_min
        best_distance <- dists[i_min]
      }
    }

    # También probar matching por palabras clave
    if (is.na(best_match) || use_fallback) {
      # Extraer palabras clave
      keywords <- str_extract_all(x_clean, "\\b\\w{4,}\\b")[[1]]
      if (length(keywords) > 0) {
        for (keyword in keywords) {
          matches <- str_detect(ref_clean, keyword)
          if (any(matches)) {
            candidates <- which(matches)
            if (length(candidates) == 1) {
              return(candidates[1])
            } else if (length(candidates) > 1) {
              # Si hay múltiples candidatos, elegir el más similar
              sub_dists <- stringdist::stringdist(x_clean, ref_clean[candidates], method = "jw")
              best_candidate <- candidates[which.min(sub_dists)]
              if (min(sub_dists) <= (current_dist * 1.5)) {
                return(best_candidate)
              }
            }
          }
        }
      }
    }

    return(best_match)
  }

  # 6. aplicar matching a cada valor
  idx <- vapply(clean_prep, match_index, integer(1), USE.NAMES = FALSE)

  # 7. Si force_match = TRUE, intentar con parámetros más permisivos para los NA
  if (force_match) {
    na_positions <- which(is.na(idx))
    if (length(na_positions) > 0) {
      message("Intentando matching forzado para ", length(na_positions), " valores...")

      for (pos in na_positions) {
        forced_idx <- match_index(clean_prep[pos], use_fallback = TRUE)
        if (!is.na(forced_idx)) {
          idx[pos] <- forced_idx
          message("Forzado: '", original[pos], "' \u2192 '", ref_df$Carrera[forced_idx], "'")
        }
      }
    }
  }

  # 8. armar columnas de salida
  carreras_norm   <- ifelse(is.na(idx), NA_character_, ref_df$Carrera[idx])
  facultades_norm <- if (facultad) {
    ifelse(is.na(idx), NA_character_, ref_df$Facultad[idx])
  }

  # 9. armar la salida y manejar los valores sin normalizar
  out <- df %>%
    mutate(!!paste0(col_name, "_norm") := carreras_norm) %>%
    relocate(!!paste0(col_name, "_norm"), .after = all_of(col_name))

  if (facultad) {
    out <- out %>%
      mutate(!!paste0(col_name, "_facultad") := facultades_norm) %>%
      relocate(!!paste0(col_name, "_facultad"),
               .after = all_of(paste0(col_name, "_norm")))
  }

  no_match <- unique(original[is.na(carreras_norm)])

  if (length(no_match) > 0) {
    if (remove_unmatched) {
      # Eliminar filas con carreras no normalizadas
      rows_to_remove <- which(is.na(carreras_norm))
      n_removed <- length(rows_to_remove)

      message(" Eliminando ", n_removed, " filas con carreras no compatibles:")
      message(paste0(" \u2022 ", no_match, collapse = "\n"))

      # Filtrar el dataframe
      out <- out[-rows_to_remove, ]

      message("Dataset limpio: ", nrow(out), " filas restantes")

    } else {
      message("Quedaron sin normalizar:\n",
              paste0(" \u2022 ", no_match, collapse = "\n"))

      # Sugerir matches manuales
      message("\nSugerencias de matches manuales:")
      for (nm in head(no_match, 10)) { # Mostrar solo los primeros 10
        nm_clean <- nm %>%
          tolower() %>%
          stringi::stri_trans_general("Latin-ASCII") %>%
          str_replace_all("[[:punct:]]", "") %>%
          str_squish()

        dists <- stringdist::stringdist(nm_clean, ref_clean, method = "jw")
        top3 <- order(dists)[1:3]

        message("   '", nm, "' podr\u00eda ser:")
        for (i in 1:3) {
          message("     - ", ref_df$Carrera[top3[i]], " (dist: ", round(dists[top3[i]], 3), ")")
        }
      }

      message("\nTip: Usa remove_unmatched = TRUE para eliminar estas filas")
    }
  } else {
    message("Todas las carreras se normalizaron.")
  }

  out
}
