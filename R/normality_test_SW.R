#' Shapiro-Wilk Normality Test for Several Variables
#'
#' Applies the Shapiro-Wilk test to each variable and classifies it as
#' `"Normal"` (p >= .05) or `"No-normal"`.
#'
#' @param data A data frame.
#' @param variables Variables to test, in dplyr selection syntax (for example
#'   `c(ansiedad, depresion)` or `ansiedad:estres`).
#'
#' @return A tibble with the columns `Variables`, `Shapiro-Wilk` (the W
#'   statistic), `p.value` (formatted, `"p < .001"` when p < .001) and
#'   `Normality`.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(ansiedad = rnorm(60), depresion = rexp(60))
#' normality_test_SW(df, c(ansiedad, depresion))
normality_test_SW <- function(data, variables) {
  var_names <- names(select(data, {{ variables }}))

  data %>%
    tidyr::pivot_longer(
      cols = all_of(var_names),
      names_to = "Variables",
      values_to = "Ptje_vi"
    ) %>%
    mutate(Variables = factor(Variables, levels = var_names)) %>%
    group_by(Variables) %>%
    summarise(
      test = list(stats::shapiro.test(Ptje_vi)),
      .groups = "drop"
    ) %>%
    mutate(
      `Shapiro-Wilk` = vapply(test, function(tt) unname(tt$statistic), numeric(1)),
      p_num = vapply(test, function(tt) tt$p.value, numeric(1)),
      p.value = ifelse(p_num < 0.001, "p < .001", format(p_num, scientific = FALSE)),
      Normality = ifelse(p_num < 0.05, "No-normal", "Normal")
    ) %>%
    select(Variables, `Shapiro-Wilk`, p.value, Normality)
}
