#' ThesiStats: Statistical Tools for Quantitative Theses
#'
#' Helpers for the analyses that quantitative theses in the social and
#' behavioral sciences repeat: renaming and scoring items, recoding Likert
#' responses, cleaning sociodemographic variables, descriptive statistics,
#' normality checks, reliability, correlations and group comparisons with
#' effect sizes.
#'
#' @keywords internal
#' @importFrom dplyr %>% across all_of any_of arrange case_when coalesce
#'   distinct everything filter group_by if_else left_join mutate n pull
#'   relocate rename rename_with row_number select starts_with summarise
#' @importFrom rlang .data := !! sym
#' @importFrom stringr regex str_detect str_extract str_extract_all str_match
#'   str_remove_all str_replace str_replace_all str_squish str_to_lower
#'   str_to_upper str_trim
#' @importFrom readr parse_number
#' @importFrom stats cor cor.test cov kruskal.test mahalanobis median ppoints
#'   qchisq sd setNames shapiro.test t.test wilcox.test
#' @importFrom utils as.roman head
"_PACKAGE"

utils::globalVariables(c(
  ".", ".clean", ".num", ".num_numeric", ".num_roman", ".num_word", ".orig",
  ".roman", "Alternativas", "Diff", "Epsilon", "ID", "Interpretacion", "M_1",
  "M_2", "Normality", "Observado", "PSest", "Ptje_vi", "Teorico", "Variables",
  "Variables_interes", "anios", "combined", "d_cohen", "estimate",
  "estimate1", "estimate2", "gl", "kurtosis", "max", "mean", "meses",
  "meses_casi", "meses_casi_solo", "meses_combo", "meses_especial",
  "meses_puro", "meses_semana", "min", "months", "normalized_expression",
  "num_puro", "original_expression", "p", "p.value", "parameter",
  "pure_years", "score", "sd_1", "sd_2", "skew", "statistic", "t", "test",
  "p_num", "Shapiro-Wilk",
  "total_months", "total_years", "value", "years"
))
