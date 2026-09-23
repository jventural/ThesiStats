#' Descriptive Statistics of a Range of Columns
#'
#' Computes, with [psych::describe()], the mean, standard deviation, minimum,
#' maximum, skewness and kurtosis of the columns from `start_col` to
#' `end_col`, plus the mean as a percentage of the maximum.
#'
#' @param data A data frame.
#' @param start_col,end_col Names of the first and last columns of the range.
#'
#' @return A data frame with the columns `Variables`, `Media`, `DE`, `Min.`,
#'   `Max.`, `g1` (skewness), `g2` (kurtosis) and `%`, rounded to two decimals.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(ansiedad = rnorm(50, 20, 4), depresion = rnorm(50, 15, 3))
#' calculate_descriptives(df, "ansiedad", "depresion")
calculate_descriptives <- function(data, start_col, end_col) {
  data %>%
    select(all_of(start_col):all_of(end_col)) %>%
    psych::describe() %>%
    as.data.frame() %>%
    mutate("%" = mean / max * 100) %>%
    select(mean, sd, min, max, skew, kurtosis, "%") %>%
    rename(Media = mean, DE = sd, Min. = min, Max. = max, g1 = skew, g2 = kurtosis) %>%
    round(2) %>%
    tibble::rownames_to_column(var = "Variables")
}
