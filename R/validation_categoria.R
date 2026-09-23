#' Unique Values of Several Columns
#'
#' Lists, sorted, the distinct values that appear in the selected columns,
#' useful to check the categories before recoding them with
#' [Correct_category()].
#'
#' @param df A data frame.
#' @param cols Columns in dplyr selection syntax.
#'
#' @return A sorted vector with the distinct values.
#' @export
#' @examples
#' df <- data.frame(Q1 = c("Si", "No", "si"), Q2 = c("No", "Tal vez", "Si"))
#' validation_categoria(df, Q1:Q2)
validation_categoria <- function(df, cols) {
  df %>%
    select({{ cols }}) %>%
    unlist(use.names = FALSE) %>%
    unique() %>%
    sort()
}
