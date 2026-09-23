#' Detect Likert Response Options and Assign Scores
#'
#' Looks in a data frame for the response options listed in `likert_levels`
#' (ignoring case, accents and extra spaces), keeps those that appear, orders
#' them as in `likert_levels` and assigns each one a score starting at 0 or 1.
#'
#' @param df A data frame whose columns hold Likert responses as text.
#' @param start_zero Logical. Scores start at 0 (`TRUE`, default) or at 1.
#' @param likert_levels Character vector with the response options in
#'   ascending order.
#'
#' @return A tibble with the columns `Alternativas` (the options found, as an
#'   ordered factor) and `score`. A warning lists the options of
#'   `likert_levels` that do not appear in `df`.
#' @export
#' @examples
#' respuestas <- data.frame(
#'   P1 = c("De acuerdo", "En desacuerdo", "Me es indiferente"),
#'   P2 = c("Completamente de acuerdo", "De acuerdo", "En desacuerdo")
#' )
#' suppressWarnings(detect_expression_Likert(respuestas))
detect_expression_Likert <- function(df, start_zero = TRUE,
                                     likert_levels = c("Completamente en desacuerdo",
                                                       "En desacuerdo",
                                                       "Me es indiferente",
                                                       "De acuerdo",
                                                       "Completamente de acuerdo")) {
  normalize_expression <- function(expr) {
    expr <- stringr::str_to_lower(expr)
    expr <- stringi::stri_trans_general(expr, "Latin-ASCII")
    stringr::str_trim(expr)
  }

  likert_levels_normalized <- vapply(likert_levels, normalize_expression, character(1))

  df_long <- df %>%
    mutate(across(everything(), as.character)) %>%
    tidyr::pivot_longer(everything(), names_to = "column", values_to = "original_expression") %>%
    mutate(normalized_expression = normalize_expression(original_expression)) %>%
    filter(normalized_expression %in% likert_levels_normalized)

  missing_levels <- likert_levels[!(likert_levels_normalized %in% df_long$normalized_expression)]
  if (length(missing_levels) > 0) {
    warning("Los siguientes niveles Likert no se encontraron en la data: ",
            paste(missing_levels, collapse = ", "))
  }

  # the factor uses the canonical labels, so accents or case in the data do
  # not turn a detected option into NA
  df_long <- df_long %>%
    mutate(Alternativas = factor(
      likert_levels[match(normalized_expression, likert_levels_normalized)],
      levels = likert_levels))

  df_unique <- df_long %>%
    distinct(Alternativas, .keep_all = TRUE) %>%
    arrange(Alternativas)

  df_unique <- if (start_zero) {
    df_unique %>% mutate(score = row_number() - 1)
  } else {
    df_unique %>% mutate(score = row_number())
  }

  df_unique %>% select(Alternativas, score)
}
