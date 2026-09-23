#' Convert Age Strings with Years and Months to Numeric Years
#'
#' Parses a character column with ages written in Spanish (for example
#' "2 años", "6 meses", "1 año 3 meses" or "3,5") into numeric years,
#' expressing months as fractions of a year.
#'
#' @param df A data frame.
#' @param col_name Name of the age column. Defaults to `"Edad"`.
#' @param round_decimals Number of decimals of the result. Defaults to 2.
#' @param drop_missing Logical. If `TRUE`, rows whose converted age is zero
#'   (entries without any usable number) are removed. Defaults to `FALSE`.
#'
#' @return A list with `cleaned_df` (the data frame with `col_name` converted)
#'   and `verify_df` (a tibble pairing each original string with its value).
#' @export
#' @examples
#' df <- data.frame(Edad = c("2 años", "6 meses", "1 año 3 meses", "3,5"))
#' res <- convert_age_to_years_months(df)
#' res$verify_df
convert_age_to_years_months <- function(df,
                                        col_name = "Edad",
                                        round_decimals = 2,
                                        drop_missing = FALSE) {
  raw_vec <- as.character(df[[col_name]])

  convert_with_months <- function(txt) {
    if (is.na(txt)) return(0)
    txt2 <- stringr::str_to_lower(stringr::str_squish(txt))

    years <- dplyr::case_when(
      stringr::str_detect(txt2, "\\d+[\\.,]?\\d*\\s*a\u00f1os?") ~ as.numeric(stringr::str_replace(
        stringr::str_extract(txt2, "\\d+[\\.,]?\\d*(?=\\s*a\u00f1os?)"),
        ",", ".")),
      stringr::str_detect(txt2, "\\bun a\u00f1o\\b") ~ 1,
      TRUE ~ NA_real_
    )

    months <- dplyr::case_when(
      stringr::str_detect(txt2, "\\d+[\\.,]?\\d*\\s*meses?") ~ as.numeric(stringr::str_replace(
        stringr::str_extract(txt2, "\\d+[\\.,]?\\d*(?=\\s*meses?)"),
        ",", ".")),
      TRUE ~ NA_real_
    )

    # decimal comma ("3,5") read as a decimal point, never as a thousands mark
    pure <- suppressWarnings(readr::parse_number(stringr::str_replace(txt2, ",", ".")))
    total <- dplyr::case_when(
      !is.na(years)                 ~ years + dplyr::coalesce(months, 0) / 12,
      is.na(years) & !is.na(months) ~ months / 12,
      TRUE                          ~ dplyr::coalesce(pure, 0)
    )

    round(total, digits = round_decimals)
  }

  cleaned_vec <- unname(vapply(raw_vec, convert_with_months, numeric(1)))

  cleaned_df <- df %>%
    mutate(!!rlang::sym(col_name) := cleaned_vec)

  verify_df <- tibble::tibble(raw = raw_vec, cleaned = cleaned_vec)

  if (drop_missing) {
    cleaned_df <- cleaned_df %>%
      filter(.data[[col_name]] != 0)
  }

  list(cleaned_df = cleaned_df, verify_df = verify_df)
}
