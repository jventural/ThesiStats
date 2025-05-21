convert_age_to_years_months <- function(df,
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

  convert_with_months <- function(txt, i) {
    txt2 <- str_to_lower(str_squish(txt))
    years <- dplyr::case_when(
      str_detect(txt2, "años?")       ~ as.numeric(str_replace(
        str_extract(txt2, "\\d+[\\.,]?\\d*(?=\\s*años?)"),
        ",", ".")),
      str_detect(txt2, "\\bun año\\b") ~ 1,
      TRUE                             ~ NA_real_
    )
    months <- dplyr::case_when(
      str_detect(txt2, "meses?") ~ as.numeric(str_replace(
        str_extract(txt2, "\\d+[\\.,]?\\d*(?=\\s*meses?)"),
        ",", ".")),
      TRUE                        ~ NA_real_
    )
    pure  <- parse_number(txt2)
    total <- dplyr::case_when(
      !is.na(years)                 ~ years + coalesce(months, 0) / 12,
      is.na(years) & !is.na(months) ~ months / 12,
      TRUE                          ~ coalesce(pure, 0)
    )
    res <- round(total, digits = round_decimals)
    if (str_detect(txt2, "meses?") && is.na(years) && !is.na(months)) {
      message(sprintf(
        "\"%s\" → %.2f  (detalle: %d/12 ≈ %.4f)",
        txt, res, months, months/12
      ))
    }
    res
  }

  cleaned_vec <- vapply(seq_along(raw_vec),
                        function(i) convert_with_months(raw_vec[i], i),
                        numeric(1))

  cleaned_df <- df %>% mutate(!!col_name := cleaned_vec)
  verify_df  <- tibble(raw = raw_vec, cleaned = cleaned_vec)

  # opcional: eliminar filas con edad 0 (antes eran NA)
  if (drop_missing) {
    cleaned_df <- cleaned_df %>% filter(.data[[col_name]] != 0)
  }

  list(cleaned_df = cleaned_df, verify_df = verify_df)
}
