#' Clean Age Strings into Fractional Years
#'
#' Standardizes a character column with ages written in Spanish (for example
#' "19 años", "1 año 6 meses", "03 meses" or "2.5"): extracts years and
#' months, converts months to fractions of a year and returns a numeric
#' column. Rows without any digit are removed and reported with a message.
#'
#' @param df A data frame.
#' @param col_name Name of the age column. Defaults to `"Edad"`.
#' @param round_decimals Number of decimals of the result. Defaults to 2.
#'
#' @return `df` without the rows that had no digits, and with `col_name`
#'   replaced by the age in years.
#' @export
#' @examples
#' df <- data.frame(Edad = c("19 años", "1 año 6 meses", "03 meses", "2.5",
#'                           "sin dato"))
#' clean_edad(df)
clean_edad <- function(df, col_name = "Edad", round_decimals = 2) {
  n_before <- nrow(df)

  df_clean <- df %>%
    filter(stringr::str_detect(.data[[col_name]], "\\d")) %>%
    mutate(
      .clean = stringr::str_to_lower(stringr::str_squish(.data[[col_name]])),
      years = dplyr::case_when(
        stringr::str_detect(.clean, "a\u00f1os?") ~ as.numeric(
          stringr::str_replace(
            stringr::str_extract(.clean, "\\d+[\\.,]?\\d*(?=\\s*a\u00f1os?)"),
            ",", ".")
        ),
        stringr::str_detect(.clean, "\\bun a\u00f1o\\b") ~ 1,
        TRUE ~ NA_real_
      ),
      months = dplyr::case_when(
        stringr::str_detect(.clean, "meses?") ~ as.numeric(
          stringr::str_replace(
            stringr::str_extract(.clean, "\\d+[\\.,]?\\d*(?=\\s*meses?)"),
            ",", ".")
        ),
        TRUE ~ NA_real_
      ),
      # decimal comma ("2,5") read as a decimal point, not a thousands mark
      pure_years = suppressWarnings(readr::parse_number(stringr::str_replace(.clean, ",", "."))),
      total_years = dplyr::coalesce(years, pure_years, 0) +
        dplyr::coalesce(months, 0) / 12,
      !!col_name := round(total_years, digits = round_decimals)
    ) %>%
    select(-.clean, -years, -months, -pure_years, -total_years)

  excluidas <- n_before - nrow(df_clean)
  message(sprintf(
    "%d fila%s exclu\u00edd%s por no contener ning\u00fan n\u00famero en '%s'",
    excluidas,
    ifelse(excluidas == 1, "", "s"),
    ifelse(excluidas == 1, "a", "as"),
    col_name
  ))

  df_clean
}
