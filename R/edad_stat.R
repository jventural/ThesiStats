#' Summary Statistics of an Age Column
#'
#' Computes the mean, standard deviation, minimum and maximum of an age
#' column.
#'
#' @param obj A data frame, or the list returned by [convert_age_to_years()]
#'   or [convert_age_to_years_months()] (its `cleaned_df` is used).
#' @param columna The age column, as a bare name or a string.
#'
#' @return A one-row data frame with `Media`, `DesviacionEstandar`, `Minimo`
#'   and `Maximo`.
#' @export
#' @examples
#' df <- data.frame(Edad = c(18, 20, 22, 25, 30))
#' edad_stat(df, Edad)
#' edad_stat(df, "Edad")
edad_stat <- function(obj, columna) {
  df <- if (is.list(obj) && !is.data.frame(obj) && "cleaned_df" %in% names(obj)) {
    obj$cleaned_df
  } else if (is.data.frame(obj)) {
    obj
  } else {
    stop("El primer argumento debe ser un data.frame o la lista retornada por convert_age_to_years*")
  }

  col_quo <- rlang::enquo(columna)
  col_sym <- if (rlang::quo_is_symbol(col_quo)) {
    rlang::quo_get_expr(col_quo)
  } else {
    rlang::sym(rlang::eval_tidy(col_quo))
  }

  df %>%
    summarise(
      Media              = mean(!!col_sym, na.rm = TRUE),
      DesviacionEstandar = stats::sd(!!col_sym, na.rm = TRUE),
      Minimo             = min(!!col_sym, na.rm = TRUE),
      Maximo             = max(!!col_sym, na.rm = TRUE)
    )
}
