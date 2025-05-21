convert_age_to_years <- function(df,
                                 col_name = "Edad",
                                 round_decimals = 2,
                                 drop_missing = FALSE) {
  # 1) Instalar y cargar dependencias
  pkgs <- c("dplyr", "stringr", "tibble", "readr")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }

  raw_vec <- as.character(df[[col_name]])

  convert_years_only <- function(txt) {
    txt2 <- str_to_lower(str_squish(txt))
    if (!str_detect(txt2, "\\d")) return(NA_real_)
    years <- dplyr::case_when(
      str_detect(txt2, "años?")       ~ as.numeric(str_replace(
        str_extract(txt2, "\\d+[\\.,]?\\d*(?=\\s*años?)"),
        ",", ".")),
      str_detect(txt2, "\\bun año\\b") ~ 1,
      TRUE                             ~ NA_real_
    )
    pure  <- parse_number(txt2)
    total <- if (!is.na(years)) years else coalesce(pure, 0)
    round(total, digits = round_decimals)
  }

  cleaned_vec <- vapply(raw_vec, convert_years_only, numeric(1))

  # reconstruir df limpio y df de verificación
  cleaned_df <- df %>% mutate(!!col_name := cleaned_vec)
  verify_df  <- tibble(raw = raw_vec, cleaned = cleaned_vec)

  # opcional: eliminar filas sin edad
  if (drop_missing) {
    cleaned_df <- cleaned_df %>% filter(!is.na(.data[[col_name]]))
  }

  # mensajes de advertencia (si no hicimos drop)
  if (!drop_missing) {
    missing_idx <- which(is.na(cleaned_vec) & !is.na(raw_vec))
    if (length(missing_idx) > 0) {
      message("Filas sin dígitos (NA en cleaned):")
      for (i in missing_idx) {
        message(sprintf("  • fila %d: \"%s\"", i, raw_vec[i]))
      }
    }
  }

  list(cleaned_df = cleaned_df, verify_df = verify_df)
}
