#' Add the Sum Score of Each Factor to a Data Frame
#'
#' Reads lines of the form `"Factor: Item1, Item2, ..."` and adds to `df` one
#' column per factor with the row sum of its items.
#'
#' @param df A data frame with the item columns.
#' @param text_lines Character vector, one line per factor.
#' @param new_name Deprecated and ignored. Earlier versions also saved the
#'   result in the global environment under this name; assign the returned
#'   data frame instead, for example `df2 <- generate_and_apply(df, lines)`.
#'
#' @return `df` with one new column per factor (the row sum of its items,
#'   ignoring missing values). The item columns are converted to numeric.
#' @export
#' @examples
#' df <- data.frame(A1 = c(1, 2, 3), A2 = c(2, 2, 2), B1 = c(0, 1, 1))
#' generate_and_apply(df, c("Ansiedad: A1, A2", "Estres: B1"))
generate_and_apply <- function(df, text_lines, new_name = NULL) {
  factors <- gsub(":.+$", "", text_lines)
  items   <- gsub("^.+:\\s*", "", text_lines)
  items   <- lapply(items, function(x) unlist(strsplit(x, ",\\s*")))

  df_new <- df

  for (i in seq_along(factors)) {
    fac_name <- factors[i]
    vars_i   <- items[[i]]

    missing_vars <- setdiff(vars_i, names(df_new))
    if (length(missing_vars) > 0) {
      stop(sprintf("No se encontraron estas columnas en el data.frame original: %s",
                   paste(missing_vars, collapse = ", ")))
    }

    df_new <- df_new %>%
      mutate(across(all_of(vars_i), as.numeric))

    df_new[[fac_name]] <- rowSums(df_new[, vars_i, drop = FALSE], na.rm = TRUE)
  }

  df_new
}
