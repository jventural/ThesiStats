#' Convert Age Strings to Numeric Years
#'
#' Parses a character column with ages written in Spanish (for example
#' "2 años", "un año" or "3,5") into numeric years. Unlike
#' [convert_age_to_years_months()], months are not added.
#'
#' @param df A data frame.
#' @param col_name Name of the age column. Defaults to `"Edad"`.
#' @param round_decimals Number of decimals of the result. Defaults to 2.
#' @param drop_missing Logical. If `TRUE`, rows whose age could not be
#'   converted are removed; if `FALSE` (default) they are kept as `NA` and
#'   listed in a message.
#'
#' @return A list with `cleaned_df` (the data frame with `col_name` converted)
#'   and `verify_df` (a tibble pairing each original string with its value).
#' @export
#' @examples
#' df <- data.frame(Edad = c("20 años", "un año", "3,5", "sin dato"))
#' res <- convert_age_to_years(df)
#' res$verify_df
convert_age_to_years <- function(df,
                                 col_name = "Edad",
                                 round_decimals = 2,
                                 drop_missing = FALSE) {
  raw_vec <- as.character(df[[col_name]])

  convert_years_only <- function(txt) {
    if (is.na(txt)) return(NA_real_)
    txt2 <- stringr::str_to_lower(stringr::str_squish(txt))
    if (!stringr::str_detect(txt2, "\\d") &&
        !stringr::str_detect(txt2, "\\bun a\u00f1o\\b")) return(NA_real_)
    years <- dplyr::case_when(
      stringr::str_detect(txt2, "\\d+[\\.,]?\\d*\\s*a\u00f1os?") ~ as.numeric(stringr::str_replace(
        stringr::str_extract(txt2, "\\d+[\\.,]?\\d*(?=\\s*a\u00f1os?)"),
        ",", ".")),
      stringr::str_detect(txt2, "\\bun a\u00f1o\\b") ~ 1,
      TRUE ~ NA_real_
    )
    # decimal comma ("3,5") read as a decimal point, never as a thousands mark
    pure <- suppressWarnings(readr::parse_number(stringr::str_replace(txt2, ",", ".")))
    total <- if (!is.na(years)) years else dplyr::coalesce(pure, 0)
    round(total, digits = round_decimals)
  }

  cleaned_vec <- unname(vapply(raw_vec, convert_years_only, numeric(1)))

  cleaned_df <- df %>%
    mutate(!!rlang::sym(col_name) := cleaned_vec)

  verify_df <- tibble::tibble(raw = raw_vec, cleaned = cleaned_vec)

  if (drop_missing) {
    cleaned_df <- cleaned_df %>%
      filter(!is.na(.data[[col_name]]))
  } else {
    missing_idx <- which(is.na(cleaned_vec) & !is.na(raw_vec))
    if (length(missing_idx) > 0) {
      message("Filas sin d\u00edgitos (NA en cleaned):\n",
              paste(sprintf("  - fila %d: \"%s\"", missing_idx, raw_vec[missing_idx]),
                    collapse = "\n"))
    }
  }

  list(cleaned_df = cleaned_df, verify_df = verify_df)
}
