#' Recode Specified Values in Selected Columns
#'
#' Applies recoding rules, written as two-sided formulas `"old" ~ "new"`, to
#' one or more columns of a data frame, replacing exact matches of each old
#' value with the new one.
#'
#' @param df A data frame or tibble.
#' @param cols Columns to recode, in dplyr selection syntax (for example
#'   `Q1:Q5` or `starts_with("Q")`).
#' @param ... One or more two-sided formulas of the form
#'   `"old_value" ~ "new_value"`.
#'
#' @return `df` with every occurrence of each old value replaced by the new
#'   value in the selected columns. Other columns are unchanged.
#' @export
#' @examples
#' df <- data.frame(
#'   Q1 = c("Yes", "No", "yes", "No"),
#'   Q2 = c("Maybe", "maybe", "No", "Yes")
#' )
#' Correct_category(df, Q1:Q2, "yes" ~ "Yes", "maybe" ~ "Maybe")
Correct_category <- function(df, cols, ...) {
  replacements <- list(...)

  if (!all(vapply(replacements, function(f) inherits(f, "formula") && length(f) == 3,
                  logical(1)))) {
    stop('Cada reemplazo debe ser una f\u00f3rmula bidireccional: "viejo" ~ "nuevo".')
  }

  # evaluate each side in the environment where the formula was written
  pairs <- lapply(replacements, function(f) {
    env <- environment(f)
    list(old = eval(f[[2]], envir = env), new = eval(f[[3]], envir = env))
  })

  df %>%
    mutate(
      across(
        {{ cols }},
        function(x) {
          vec <- x
          for (pr in pairs) {
            vec[!is.na(vec) & vec == pr$old] <- pr$new
          }
          vec
        }
      )
    )
}
