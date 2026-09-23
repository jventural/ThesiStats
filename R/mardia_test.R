#' Mardia's Test of Multivariate Normality
#'
#' Computes Mardia's multivariate skewness and kurtosis tests with
#' [psych::mardia()] and reports whether each one is compatible with
#' multivariate normality (p >= .05).
#'
#' @param data A data frame or matrix of numeric variables.
#'
#' @return A data frame with the columns `Test`, `Statistic`, `p.value`
#'   (formatted, `"p < .001"` when p <= .001) and `Result` (`"YES"` when
#'   normality is not rejected).
#' @export
#' @examples
#' set.seed(1)
#' df <- as.data.frame(matrix(rnorm(200 * 3), ncol = 3))
#' mardia_test(df)
mardia_test <- function(data) {
  A <- psych::mardia(data, plot = FALSE)

  result <- data.frame(
    Test = c("Mardia Skewness", "Mardia Kurtosis"),
    Statistic = c(A$small.skew, A$kurtosis),
    p.value = c(A$p.small, A$p.kurt),
    Result = c(ifelse(A$p.small < 0.05, "NO", "YES"), ifelse(A$p.kurt < 0.05, "NO", "YES"))
  )

  result$p.value <- round(result$p.value, 3)
  result$p.value <- ifelse(result$p.value <= 0.001, "p < .001", as.character(result$p.value))

  result
}
